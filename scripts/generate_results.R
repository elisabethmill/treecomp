library(dplyr)
library(ranger)
library(ggplot2)
library(tidyr)
library(xtable)

years_to_first_start <- treecomp::nfl_qbr_by_year %>% 
  group_by(player_name) %>%
  filter(!is.na(draft_year)) %>%
  summarise(
    draft_year = first(draft_year),
    first_start_season = suppressWarnings(min(year[games_started > 0], na.rm = TRUE)),
    years_until_first_start = ifelse(is.infinite(first_start_season),
                                     NA_integer_,
                                     first_start_season - draft_year),
    .groups = "drop"
  )

years_to_first_start %>%
  count(years_until_first_start, name = "n") %>%
  arrange(years_until_first_start)

num_trees <- 1000
set.seed(123)

# Determine mean regression for QBR ----

QBR2_passing_data <- treecomp::nfl_qbr_by_year %>%
  arrange(player_name, year) %>%
  group_by(player_name) %>%
  mutate(qbr_2 = lead(qbr),
         qbr_2 = ifelse(is.na(qbr_2), 0, qbr_2)) %>%
  filter(year < 2023)

QBR2_model <- nls(
  formula = qbr_2 ~ (att * qbr) / (att + c),
  data = QBR2_passing_data,
  start = list(c = 1)  # Starting guess for c
)

summary(QBR2_model)

c <- coef(QBR2_model)[1]

# Tune random forest ----

data <- treecomp::quarterback %>%
  mutate(
    reg_qbr = nfl_att_per_year / (nfl_att_per_year + c) * nfl_qbr_per_year,
    # NA for Heisman voting means you didn't finish in the top 10
    ncaa_heisman_last = ifelse(is.na(ncaa_heisman_last), 11, ncaa_heisman_last),
    ncaa_career_att = ncaa_att_per_year * ncaa_years_career
  )

past_data <- data %>%
  filter(
    ncaa_year_last < 2023,
    ncaa_career_att > 500,
    # Exclude players with very few NFL attempts per season because their QBR will be unstable
    is.na(nfl_att_per_year) | (nfl_att_per_year == 0) | nfl_att_per_year >= 10
  ) %>%
  # Remaining NAs are due to not playing in the NFL
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>%
  # Classification target: did this QB actually have reg_qbr > 0?
  mutate(
    played_nfl = factor(
      ifelse(reg_qbr > 0, "yes", "no"),
      levels = c("no", "yes")
    )
  )

classif_data <- past_data
qbr_data <- past_data %>% filter(played_nfl == "yes", reg_qbr > 0)

set.seed(123)  # For reproducibility

train_years <- c(2006, 2007, 2008, 2010, 2011, 2012, 2014, 2015, 2016, 2018, 2019, 2020, 2022)
test_years  <- c(2009, 2013, 2017, 2021)

train_data <- classif_data %>% filter(ncaa_year_last %in% train_years)
test_data  <- classif_data %>% filter(ncaa_year_last %in% test_years)

train_qbr_data <- qbr_data %>% filter(ncaa_year_last %in% train_years)
test_qbr_data  <- qbr_data %>% filter(ncaa_year_last %in% test_years)

brier <- function(y01, p) mean((y01 - p)^2)

ess_from_similarity <- function(sim_mat) {
  vapply(seq_len(nrow(sim_mat)), function(i) {
    sim_vec <- sim_mat[i, ]
    w <- sim_vec[sim_vec > 0]
    if (length(w) == 0) return(NA_real_)
    w <- w / sum(w)
    (sum(w))^2 / sum(w^2)
  }, numeric(1))
}

make_temporal_folds <- function(df, year_col = "ncaa_year_last",
                                first_test_year = NULL, last_test_year = NULL,
                                min_train_years = 3) {
  yrs <- sort(unique(df[[year_col]]))
  if (!is.null(first_test_year)) yrs <- yrs[yrs >= first_test_year]
  if (!is.null(last_test_year))  yrs <- yrs[yrs <= last_test_year]
  
  folds <- list()
  for (y in yrs) {
    train_idx <- which(df[[year_col]] < y)
    test_idx  <- which(df[[year_col]] == y)
    if (length(train_idx) == 0 || length(test_idx) == 0) next
    if (length(unique(df[[year_col]][train_idx])) < min_train_years) next
    folds[[as.character(y)]] <- list(train = train_idx, test = test_idx)
  }
  folds
}

# temporal folds for training data (uses only train_years)
folds_train_cls <- make_temporal_folds(
  train_data,
  year_col = "ncaa_year_last",
  first_test_year = min(train_years),
  last_test_year = max(train_years),
  min_train_years = 3
)

folds_train_regr <- make_temporal_folds(
  train_qbr_data,
  year_col = "ncaa_year_last",
  first_test_year = min(train_years),
  last_test_year = max(train_years),
  min_train_years = 3
)

# temporal folds for full dataset (uses all years present < 2023)
folds_full_cls <- make_temporal_folds(
  classif_data,
  year_col = "ncaa_year_last",
  first_test_year = 2005,
  last_test_year = 2022,
  min_train_years = 5
)

folds_full_regr <- make_temporal_folds(
  qbr_data,
  year_col = "ncaa_year_last",
  first_test_year = 2005,
  last_test_year = 2022,
  min_train_years = 5
)

grid <- expand.grid(
  min.node.size = c(6, 12, 24, 48, 96),
  mtry = c(1, 2, 4, 8, 16),
  max.depth = c(2, 4, 8, 16, 32),
  stringsAsFactors = FALSE
)

temporal_cv_classifier <- function(df, folds, min.node.size, mtry, max.depth) {
  fold_scores <- list()
  
  fold_names <- names(folds)
  for (k in seq_along(fold_names)) {
    y <- fold_names[k]
    idx <- folds[[y]]
    cv_train <- df[idx$train, ]
    cv_test  <- df[idx$test, ]
    
    # Probability forest for Brier
    rf_prob <- ranger(
      formula = played_nfl ~
        ncaa_yds_per_att_career + ncaa_games_per_year +
        ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
        ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
        ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
        ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
      data = cv_train,
      num.trees = num_trees,
      probability = TRUE,
      keep.inbag = TRUE,
      importance = "impurity",
      min.node.size = min.node.size,
      mtry = mtry,
      max.depth = max.depth
    )
    
    p <- predict(rf_prob, data = cv_test)$predictions[, "yes"]
    y01 <- as.integer(cv_test$played_nfl == "yes")
    br <- brier(y01, p)
    
    # Non-probability forest for similarity/ESS
    rf_sim <- ranger(
      formula = played_nfl ~
        ncaa_yds_per_att_career + ncaa_games_per_year +
        ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
        ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
        ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
        ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
      data = cv_train,
      num.trees = num_trees,
      probability = FALSE,
      keep.inbag = TRUE,
      importance = "impurity",
      min.node.size = min.node.size,
      mtry = mtry,
      max.depth = max.depth
    )
    
    sim <- treecomp::extract_similarity(
      object = rf_sim,
      newdata = cv_test,
      refdata = cv_train,
      match_training = TRUE
    )
    ess <- median(ess_from_similarity(sim), na.rm = TRUE)
    
    fold_scores[[k]] <- data.frame(
      test_year = as.integer(y),
      n = nrow(cv_test),
      brier = br,
      ESS = ess
    )
  }
  
  df_scores <- bind_rows(fold_scores)
  data.frame(
    cv_brier = weighted.mean(df_scores$brier, df_scores$n),
    ESS_median = weighted.mean(df_scores$ESS, df_scores$n)
  )
}

temporal_cv_regressor <- function(df, folds, min.node.size, mtry, max.depth) {
  fold_scores <- list()
  
  fold_names <- names(folds)
  for (k in seq_along(fold_names)) {
    y <- fold_names[k]
    idx <- folds[[y]]
    cv_train <- df[idx$train, ]
    cv_test  <- df[idx$test, ]
    
    if (nrow(cv_train) < 30 || nrow(cv_test) == 0) next
    
    rf <- ranger(
      formula = reg_qbr ~
        ncaa_yds_per_att_career + ncaa_games_per_year +
        ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
        ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
        ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
        ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
      data = cv_train,
      num.trees = num_trees,
      keep.inbag = TRUE,
      importance = "impurity",
      min.node.size = min.node.size,
      mtry = mtry,
      max.depth = max.depth
    )
    
    pred <- predict(rf, data = cv_test)$predictions
    rmse <- sqrt(mean((cv_test$reg_qbr - pred)^2))
    
    sim <- treecomp::extract_similarity(
      object = rf,
      newdata = cv_test,
      refdata = cv_train,
      match_training = TRUE
    )
    ess <- median(ess_from_similarity(sim), na.rm = TRUE)
    
    fold_scores[[k]] <- data.frame(
      test_year = as.integer(y),
      n = nrow(cv_test),
      rmse = rmse,
      ESS = ess
    )
  }
  
  df_scores <- bind_rows(fold_scores)
  data.frame(
    cv_rmse = sqrt(weighted.mean(df_scores$rmse^2, df_scores$n)),
    ESS_median = weighted.mean(df_scores$ESS, df_scores$n)
  )
}

# 1) training data classification model
train_cls_cv <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  pars <- grid[i, ]
  cat("[TRAIN CLS ", i, "/", nrow(grid), "] mns=", pars$min.node.size,
      " mtry=", pars$mtry, " md=", pars$max.depth, "\n", sep = "")
  out <- temporal_cv_classifier(
    df = train_data,
    folds = folds_train_cls,
    min.node.size = pars$min.node.size,
    mtry = pars$mtry,
    max.depth = pars$max.depth
  )
  train_cls_cv[[i]] <- cbind(pars, out)
}
train_cls_cv_df <- bind_rows(train_cls_cv) %>% arrange(cv_brier)

# 2) training data prediction model
train_regr_cv <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  pars <- grid[i, ]
  cat("[TRAIN REGR ", i, "/", nrow(grid), "] mns=", pars$min.node.size,
      " mtry=", pars$mtry, " md=", pars$max.depth, "\n", sep = "")
  out <- temporal_cv_regressor(
    df = train_qbr_data,
    folds = folds_train_regr,
    min.node.size = pars$min.node.size,
    mtry = pars$mtry,
    max.depth = pars$max.depth
  )
  train_regr_cv[[i]] <- cbind(pars, out)
}
train_regr_cv_df <- bind_rows(train_regr_cv) %>% arrange(cv_rmse)

# 3) full dataset classification model
full_cls_cv <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  pars <- grid[i, ]
  cat("[FULL CLS ", i, "/", nrow(grid), "] mns=", pars$min.node.size,
      " mtry=", pars$mtry, " md=", pars$max.depth, "\n", sep = "")
  out <- temporal_cv_classifier(
    df = classif_data,
    folds = folds_full_cls,
    min.node.size = pars$min.node.size,
    mtry = pars$mtry,
    max.depth = pars$max.depth
  )
  full_cls_cv[[i]] <- cbind(pars, out)
}
full_cls_cv_df <- bind_rows(full_cls_cv) %>% arrange(cv_brier)

# 4) full dataset prediction model
full_regr_cv <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  pars <- grid[i, ]
  cat("[FULL REGR ", i, "/", nrow(grid), "] mns=", pars$min.node.size,
      " mtry=", pars$mtry, " md=", pars$max.depth, "\n", sep = "")
  out <- temporal_cv_regressor(
    df = qbr_data,
    folds = folds_full_regr,
    min.node.size = pars$min.node.size,
    mtry = pars$mtry,
    max.depth = pars$max.depth
  )
  full_regr_cv[[i]] <- cbind(pars, out)
}
full_regr_cv_df <- bind_rows(full_regr_cv) %>% arrange(cv_rmse)

print(train_cls_cv_df %>% slice(1))
print(train_regr_cv_df %>% slice(1))
print(full_cls_cv_df %>% slice(1))
print(full_regr_cv_df %>% slice(1))


plot_df <- bind_rows(
  full_cls_cv_df  %>% mutate(model = "classification"),
  full_regr_cv_df %>% mutate(model = "regression")
) %>%
  pivot_longer(
    c(mtry, min.node.size, max.depth),
    names_to = "hyperparameter",
    values_to = "value"
  ) %>%
  mutate(value = factor(value))

{sputil::open_device("figures/ESS_vs_hyperparameter_values.pdf", height = 4, width = 8)
  plot <- ggplot(plot_df, aes(value, ESS_median, fill = model)) +
    geom_boxplot() +
    facet_wrap(~ hyperparameter, scales = "free_x") +
    theme_minimal() +
    labs(
      title = "ESS vs Hyperparameter Values",
      x = "Hyperparameter Value",
      y = "ESS (median across folds)",
      fill = "Model"
    )
  
  print(plot)
  dev.off()}


percent_var_explained <- function(y, y_hat) {
  e <- y - y_hat
  100 * (1 - sum(e^2) / sum((y - mean(y))^2))
}

best_train_cls <- train_cls_cv_df %>% slice(1)
best_train_regr <- train_regr_cv_df %>% slice(1)
best_cls <- full_cls_cv_df %>% slice(1)
best_regr <- full_regr_cv_df %>% slice(1)

rf_play_model <- ranger(
  formula = played_nfl ~
    ncaa_yds_per_att_career + ncaa_games_per_year +
    ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
    ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
    ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
    ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
  data = train_data,
  num.trees = num_trees,
  probability = TRUE,
  importance = "impurity",
  keep.inbag = TRUE,
  min.node.size = best_train_cls$min.node.size,
  mtry = best_train_cls$mtry,
  max.depth = best_train_cls$max.depth
)

rf_qbr_model <- ranger(
  formula = reg_qbr ~
    ncaa_yds_per_att_career + ncaa_games_per_year +
    ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
    ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
    ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
    ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
  data = train_qbr_data,
  num.trees = num_trees,
  importance = "impurity",
  keep.inbag = TRUE,
  min.node.size = best_train_regr$min.node.size,
  mtry = best_train_regr$mtry,
  max.depth = best_train_regr$max.depth
)

lm_qbr_model <- lm(
  formula = reg_qbr ~
    ncaa_yds_per_att_career + ncaa_games_per_year +
    ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
    ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
    ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
    ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
  data = train_data
)

rf_train_pred <- (predict(rf_qbr_model, data = train_data)$predictions 
                  * predict(rf_play_model, data = train_data)$predictions[, 2])
lm_train_pred <- predict(lm_qbr_model, newdata = train_data)

rf_test_pred <- (predict(rf_qbr_model, data = test_data)$predictions 
                 * predict(rf_play_model, data = test_data)$predictions[, 2])
lm_test_pred <- predict(lm_qbr_model, newdata = test_data)

pve_comparison <- data.frame(
  dataset = rep(c("training", "test"), each = 2),
  model = rep(c("Random Forest", "Linear Regression"), times = 2),
  percent_variance_explained = c(
    percent_var_explained(train_data$reg_qbr, rf_train_pred),
    percent_var_explained(train_data$reg_qbr, lm_train_pred),
    percent_var_explained(test_data$reg_qbr, rf_test_pred),
    percent_var_explained(test_data$reg_qbr, lm_test_pred)
  )
)

print(pve_comparison)

rf_play_model_full <- ranger(
  formula = played_nfl ~
    ncaa_yds_per_att_career + ncaa_games_per_year +
    ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
    ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
    ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
    ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
  data = past_data,
  num.trees = num_trees,
  probability = TRUE,
  importance = "impurity",
  keep.inbag = TRUE,
  min.node.size = best_cls$min.node.size,
  mtry = best_cls$mtry,
  max.depth = best_cls$max.depth
)

rf_qbr_model_full <- ranger(
  formula = reg_qbr ~
    ncaa_yds_per_att_career + ncaa_games_per_year +
    ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
    ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
    ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
    ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
  data = qbr_data,
  num.trees = num_trees,
  importance = "impurity",
  keep.inbag = TRUE,
  min.node.size = best_regr$min.node.size,
  mtry = best_regr$mtry,
  max.depth = best_regr$max.depth
)


rgr_importance_scores <- importance(rf_qbr_model_full)
cls_importance_scores <- importance(rf_play_model_full)
variable_display <- c(
  ncaa_yds_per_att_career = "Career Yds/Att",
  ncaa_games_per_year = "Games/Season",
  ncaa_att_per_year = "Attempts/Season",
  ncaa_cmp_per_year = "Completions/Season",
  ncaa_yds_per_year = "Yards/Season",
  ncaa_td_per_year = "Touchdowns/Season",
  ncaa_int_per_year = "Interceptions/Season",
  ncaa_rush_att_per_year = "Rush Attempts/Season",
  ncaa_rush_yds_per_year = "Rush Yards/Season",
  ncaa_rush_td_per_year = "Rush Touchdowns/Season",
  ncaa_sos_last = "Final Strength of Schedule",
  ncaa_games_last = "Final Games",
  ncaa_yds_per_att_last = "Final Yds/Att",
  ncaa_passer_rating_last = "Final Passer Rating",
  ncaa_all_america = "All-America Seasons",
  ncaa_heisman = "Won Heisman Award",
  ncaa_heisman_last = "Final Heisman Voting"
)
rgr_importance_df <- data.frame(
  Variable = variable_display[names(rgr_importance_scores)],
  Importance = rgr_importance_scores
)
cls_importance_df <- data.frame(
  Variable = variable_display[names(cls_importance_scores)],
  Importance = cls_importance_scores
)

{
  sputil::open_device("figures/rgr_variable_importance.pdf", height = 5)
  plot <- ggplot(rgr_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +  
    labs(title = "Regression Model Variable Importance",
         x = "", y = "Importance") +
    theme_minimal()
  print(plot)
  dev.off()
}

{
  sputil::open_device("figures/cls_variable_importance.pdf", height = 5)
  plot <- ggplot(cls_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +  
    labs(title = "Classification Model Variable Importance",
         x = "", y = "Importance") +
    theme_minimal()
  print(plot)
  dev.off()
}


new_plot_data <- past_data %>%
  mutate(predictions = (predict(rf_qbr_model_full, data = past_data)$predictions 
                        * predict(rf_play_model_full, data = past_data)$predictions[, 2]))

{
  sputil::open_device("figures/3d_plot.pdf", height = 5)
  plot <- new_plot_data |>
    ggplot(aes(x = ncaa_att_per_year, y = ncaa_rush_yds_per_year, color = predictions)) +
    geom_point(size = 1) +
    scale_color_viridis_c() +
    labs(
      x = "Passing Attempts per Season (College)",
      y = "Rushing Yards per Season (College)",
      color = "Predicted QBR"
    ) +
    theme_minimal() +
    theme(legend.position = "inside", legend.position.inside = c(0.85, 0.75))
  print(plot)
  dev.off()
  }

{
  sputil::open_device("figures/predicted_vs_actuals.pdf", height = 5, width = 5)
  plot <- ggplot(new_plot_data, aes(x = predictions, y = reg_qbr)) +
    geom_point() +
    labs(
      title = "Predictions vs. Actuals",
      x = "Predicted QBR",
      y = "Actual QBR"
    ) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    xlim(0,100) +
    ylim(0,100) +
    theme_minimal()
  print(plot)
  dev.off()
}


present_data <- data %>%
  filter(ncaa_year_last == 2024)

present_data$predictions <- round((predict(rf_qbr_model_full, data = present_data)$predictions 
                                   * predict(rf_play_model_full, data = present_data)$predictions[, 2]), 3)

present_data %>%
  select(player_name, predictions) %>%
  # Remove draft-ineligible players
  filter(!player_name %in% c("Darian Mensah", "Cade Klubnik", "E.J. Warner", "Cameron Skattebo")) %>%
  arrange(-predictions) %>%
  head(10) %>%
  mutate(predictions = sprintf("%.1f", predictions)) %>%
  sputil::write_latex_table(
    file = "tables/top_ten.tex",
    colnames = c("Player", "Predicted QBR"),
    align = "l|r"
  )


# Produce similarity figures and tables ----

similarity_matrix_cls <- treecomp::extract_similarity(
  object = rf_play_model_full,
  newdata = present_data,
  refdata = past_data
)

similarity_matrix_rgr <- treecomp::extract_similarity(
  object = rf_qbr_model_full,
  newdata = present_data,
  refdata = qbr_data
)

similarity_df_cls <- as.data.frame(similarity_matrix_cls)
colnames(similarity_df_cls) <- past_data$player_name  # Name columns by training players
rownames(similarity_df_cls) <- present_data$player_name  # Name rows by test players

similarity_df_rgr <- as.data.frame(similarity_matrix_rgr)
colnames(similarity_df_rgr) <- qbr_data$player_name  # Name columns by training players
rownames(similarity_df_rgr) <- present_data$player_name  # Name rows by test players

ss_index <- which(present_data$player_name == "Shedeur Sanders")

ss_similarity_scores <- similarity_matrix_rgr[ss_index, ]

ss_plot_data <- data.frame(
  player_name = qbr_data$player_name,
  sim_score = ss_similarity_scores,
  QBR = qbr_data$reg_qbr
) %>%
  filter(sim_score > 0)

{
  sputil::open_device("figures/prospect_histogram_sanders.pdf", height = 3, width = 3)
  plot <- ggplot(ss_plot_data, aes(x = QBR, y = ..density.., weight = sim_score)) +
    geom_histogram(binwidth = 5, color = "blue", fill = "blue", alpha = 0.8) +
    theme_minimal() +
    labs(x = "QBR", y = "Weights",
         title = "Shedeur Sanders") +
    geom_vline(aes(xintercept = present_data[ss_index, ]$predictions), 
               color = "green") +
    coord_cartesian(ylim = c(0, 0.08))
  print(plot)
  dev.off()
  }

{
  sputil::open_device("figures/prospect_similarity_sanders.pdf", height = 3, width = 3)
  plot <- ggplot(ss_plot_data, aes(x = QBR, y = sim_score)) +
    geom_point() +
    ggtitle("Shedeur Sanders") +
    xlab("Training Player QBR Value") +
    ylab("Training Player Similarity Score") +
    coord_cartesian(ylim = c(0, 0.03)) +
    theme_minimal()
  print(plot)
  dev.off()
}

cw_index <- which(present_data$player_name == "Cameron Ward")

cw_similarity_scores <- similarity_matrix_rgr[cw_index, ]

cw_plot_data <- data.frame(
  player_name = qbr_data$player_name,
  sim_score = cw_similarity_scores,
  QBR = qbr_data$reg_qbr
) %>%
  filter(sim_score > 0)

{
  sputil::open_device("figures/prospect_histogram_ward.pdf", height = 3, width = 3)
  plot <- ggplot(cw_plot_data, aes(x = QBR, y = ..density.., weight = sim_score)) +
    geom_histogram(binwidth = 5, color = "blue", fill = "blue", alpha = 0.8) +
    theme_minimal() +
    labs(x = "QBR", y = "Weights", title = "Cam Ward") +
    geom_vline(aes(xintercept = present_data[cw_index, ]$predictions), 
               color = "green") +
    coord_cartesian(ylim = c(0, 0.08))
  print(plot)
  dev.off()
  }

{
  sputil::open_device("figures/prospect_similarity_ward.pdf", height = 3, width = 3)
  plot <- ggplot(cw_plot_data, aes(x = QBR, y = sim_score)) +
    geom_point() +
    ggtitle("Cam Ward") +
    xlab("Training Player QBR Value") +
    ylab("Training Player Similarity Score") +
    coord_cartesian(ylim = c(0, 0.03)) +
    theme_minimal()
  print(plot)
  dev.off()
}

jd_index <- which(present_data$player_name == "Jaxson Dart")

jd_similarity_scores <- similarity_matrix_rgr[jd_index, ]

jd_plot_data <- data.frame(
  player_name = qbr_data$player_name,
  sim_score = jd_similarity_scores,
  QBR = qbr_data$reg_qbr
) %>%
  filter(sim_score > 0)

{
  sputil::open_device("figures/prospect_histogram_dart.pdf", height = 3, width = 3)
  plot <- ggplot(jd_plot_data, aes(x = QBR, y = ..density.., weight = sim_score)) +
    geom_histogram(binwidth = 5, color = "blue", fill = "blue", alpha = 0.8) +
    theme_minimal() +
    labs(x = "QBR", y = "Weights", title = "Jaxson Dart") +
    geom_vline(aes(xintercept = present_data[jd_index, ]$predictions),
               color = "green") +
    coord_cartesian(ylim = c(0, 0.08))
  print(plot)
  dev.off()
  }

{
  sputil::open_device("figures/prospect_similarity_dart.pdf", height = 3, width = 3)
  plot <- ggplot(jd_plot_data, aes(x = QBR, y = sim_score)) +
    geom_point() +
    ggtitle("Jaxson Dart") +
    xlab("Training Player QBR Value") +
    ylab("Training Player Similarity Score") +
    coord_cartesian(ylim = c(0, 0.03)) +
    theme_minimal()
  print(plot)
  dev.off()
}

dg_index <- which(present_data$player_name == "Dillon Gabriel")

dg_similarity_scores <- similarity_matrix_rgr[dg_index, ]

dg_plot_data <- data.frame(
  player_name = qbr_data$player_name,
  sim_score = dg_similarity_scores,
  QBR = qbr_data$reg_qbr
) %>%
  filter(sim_score > 0)

{
  sputil::open_device("figures/prospect_histogram_gabriel.pdf", height = 3, width = 3)
  plot <- ggplot(dg_plot_data, aes(x = QBR, y = ..density.., weight = sim_score)) +
    geom_histogram(binwidth = 5, color = "blue", fill = "blue", alpha = 0.8) +
    theme_minimal() +
    labs(x = "QBR", y = "Weights", title = "Dillon Gabriel") +
    geom_vline(aes(xintercept = present_data[dg_index, ]$predictions),
               color = "green") +
    coord_cartesian(ylim = c(0, 0.08))
  print(plot)
  dev.off()
  }

{
  sputil::open_device("figures/prospect_similarity_gabriel.pdf", height = 3, width = 3)
  plot <- ggplot(dg_plot_data, aes(x = QBR, y = sim_score)) +
    geom_point() +
    ggtitle("Dillon Gabriel") +
    xlab("Training Player QBR Value") +
    ylab("Training Player Similarity Score") +
    coord_cartesian(ylim = c(0, 0.03)) +
    theme_minimal()
  print(plot)
  dev.off()
}

ts_index <- which(present_data$player_name == "Tyler Shough")

ts_similarity_scores <- similarity_matrix_rgr[ts_index, ]

ts_plot_data <- data.frame(
  player_name = qbr_data$player_name,
  sim_score = dg_similarity_scores,
  QBR = qbr_data$reg_qbr
) %>%
  filter(sim_score > 0)

{
  sputil::open_device("figures/prospect_histogram_shough.pdf", height = 3, width = 3)
  plot <- ggplot(ts_plot_data, aes(x = QBR, y = ..density.., weight = sim_score)) +
    geom_histogram(binwidth = 5, color = "blue", fill = "blue", alpha = 0.8) +
    theme_minimal() +
    labs(x = "QBR", y = "Weights", title = "Tyler Shough") +
    geom_vline(aes(xintercept = present_data[dg_index, ]$predictions),
               color = "green") +
    coord_cartesian(ylim = c(0, 0.08))
  print(plot)
  dev.off()
  }

{
  sputil::open_device("figures/prospect_similarity_shough.pdf", height = 3, width = 3)
  plot <- ggplot(ts_plot_data, aes(x = QBR, y = sim_score)) +
    geom_point() +
    ggtitle("Tyler Shough") +
    xlab("Training Player QBR Value") +
    ylab("Training Player Similarity Score") +
    coord_cartesian(ylim = c(0, 0.03)) +
    theme_minimal()
  print(plot)
  dev.off()
}


get_top10_comps_side_by_side <- function(target_name,
                                         player_initials,
                                         similarity_matrix_cls,
                                         similarity_matrix_rgr,
                                         present_data,
                                         past_data,
                                         qbr_data,
                                         qbr_col = "reg_qbr") {
  
  idx <- match(target_name, present_data$player_name)
  if (is.na(idx)) stop("target_name not found in present_data$player_name")
  
  cls_top10 <- tibble(
    cls_player = past_data$player_name,
    cls_sim    = as.numeric(similarity_matrix_cls[idx, , drop = TRUE]),
    cls_played_nfl    = past_data[["played_nfl"]]
  ) %>%
    filter(cls_sim > 0) %>%
    arrange(desc(cls_sim)) %>%
    slice_head(n = 10) %>%
    mutate(rank = row_number())
  
  rgr_top10 <- tibble(
    rgr_player = qbr_data$player_name,
    rgr_sim    = as.numeric(similarity_matrix_rgr[idx, , drop = TRUE]),
    rgr_qbr    = qbr_data[[qbr_col]]
  ) %>%
    filter(rgr_sim > 0) %>%
    arrange(desc(rgr_sim)) %>%
    slice_head(n = 10) %>%
    mutate(rank = row_number())
  
  full_join(cls_top10, rgr_top10, by = "rank") %>%
    select(
      rank,
      cls_player, cls_sim, cls_played_nfl,
      rgr_player, rgr_sim, rgr_qbr
    ) %>%
    transmute(
      !!paste0(player_initials, "_class_name") := cls_player,
      !!paste0(player_initials, "_class_score") := paste0(sprintf("%.1f", 100 * cls_sim), "\\%")
    )
}

cw_df <- get_top10_comps_side_by_side("Cameron Ward", "CW", similarity_matrix_cls,
                                      similarity_matrix_rgr, present_data, past_data,
                                      qbr_data) 
dg_df <- get_top10_comps_side_by_side("Dillon Gabriel", "DG", similarity_matrix_cls,
                                      similarity_matrix_rgr, present_data, past_data,
                                      qbr_data) 
ss_df <- get_top10_comps_side_by_side("Shedeur Sanders", "SS", similarity_matrix_cls,
                                      similarity_matrix_rgr, present_data, past_data,
                                      qbr_data) 
jd_df <- get_top10_comps_side_by_side("Jaxson Dart", "JD", similarity_matrix_cls,
                                      similarity_matrix_rgr, present_data, past_data,
                                      qbr_data) 
ts_df <- get_top10_comps_side_by_side("Tyler Shough", "TS", similarity_matrix_cls,
                                      similarity_matrix_rgr, present_data, past_data,
                                      qbr_data) 

cbind(cw_df, dg_df, ss_df, jd_df, ts_df) %>%
  sputil::write_latex_table(
    file = "tables/side_by_side_similarity.tex",
    colnames = rep(c("Comp", "Score"), times = 5),
    prefix_rows = "
      \\multicolumn{4}{c|}{Cam Ward} &
      \\multicolumn{4}{c|}{Dillon Gabriel} &
      \\multicolumn{4}{c|}{Shedeur Sanders} &
      \\multicolumn{4}{c|}{Jaxson Dart} &
      \\multicolumn{4}{c}{Tyler Shough}
    ",
    align = "lr|lr|lr|lr|lr"
  )
  
