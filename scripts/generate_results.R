library(dplyr)
library(ranger)
library(ggplot2)
library(tidyr)
library(xtable)
library(purrr)

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

test_years <- seq(from = 2004, to = 2020, by = 4)

data_train_classification <- classif_data |>
  dplyr::filter(!ncaa_year_last %in% test_years)
data_train_regression <- qbr_data |>
  dplyr::filter(!ncaa_year_last %in% test_years)
data_test <- classif_data |>
  dplyr::filter(ncaa_year_last %in% test_years)

make_temporal_folds <- function(df, year_col = "ncaa_year_last",
                                first_test_year = NULL, last_test_year = NULL,
                                min_train_years = 3) {
  yrs <- sort(unique(df[[year_col]]))
  if (!is.null(first_test_year)) yrs <- yrs[yrs >= first_test_year]
  if (!is.null(last_test_year))  yrs <- yrs[yrs <= last_test_year]
  
  folds <- list()
  for (y in yrs) {
    # We don't want "proper" temporal CV here because these are not time series data.
    # We just want to train on all years other than the year on which we are validating.
    train_idx <- which(df[[year_col]] != y)
    test_idx  <- which(df[[year_col]] == y)
    if (length(train_idx) == 0 || length(test_idx) == 0) next
#    if (length(unique(df[[year_col]][train_idx])) < min_train_years) next
    folds[[as.character(y)]] <- list(train = train_idx, test = test_idx)
  }
  folds
}

# temporal folds for training data (uses only train_years)
folds_train_cls <- make_temporal_folds(
  data_train_classification,
  year_col = "ncaa_year_last",
  first_test_year = min(data_train_classification$ncaa_year_last),
  last_test_year = max(data_train_classification$ncaa_year_last),
  min_train_years = 0   # because we're not doing "proper" temporal CV
)

folds_train_regr <- make_temporal_folds(
  data_train_regression,
  year_col = "ncaa_year_last",
  first_test_year = min(data_train_classification$ncaa_year_last),
  last_test_year = max(data_train_classification$ncaa_year_last),
  min_train_years = 0   # because we're not doing "proper" temporal CV
)

grid <- expand.grid(
  mtry = c(1, 2, 4, 8, 16),
  min.node.size = c(5, 10, 25, 50, 100),
  max.depth = c(2, 4, 8, 16, 32),
  stringsAsFactors = FALSE
)

train_predict_ranger <- function(data_train,
                                 data_pred,
                                 task = c("classification", "regression"),
                                 ...) {

  task <- match.arg(task)

  if (task == "classification") {
    data_train$outcome <- data_train$played_nfl
  } else {
    data_train$outcome <- data_train$reg_qbr
  }

  fit <- ranger::ranger(
    formula = outcome ~
      ncaa_yds_per_att_career + ncaa_games_per_year +
      ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
      ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
      ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
      ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
    data = data_train,
    probability = task == "classification",
    keep.inbag = TRUE,
    importance = "impurity",
    ...
  )

  if (task == "classification") {
    pred <- predict(fit, data = data_pred)$predictions[, "yes"]
  } else {
    pred <- predict(fit, data = data_pred)$predictions
  }

  return(list(fit = fit, pred = pred))
}

train_predict_glm <- function(data_train,
                             data_pred,
                             task = c("classification", "regression")) {

  task <- match.arg(task)

  if (task == "classification") {
    data_train$outcome <- data_train$played_nfl
    family <- binomial()
  } else {
    data_train$outcome <- data_train$reg_qbr
    family <- gaussian()
  }

  fit <- glm(
    formula = outcome ~
      ncaa_yds_per_att_career + ncaa_games_per_year +
      ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
      ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
      ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
      ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
    data = data_train,
    family = family
  )

  if (task == "classification") {
    pred <- predict(fit, newdata = data_pred, type = "response")
  } else {
    pred <- predict(fit, newdata = data_pred)
  }

  return(list(fit = fit, pred = pred))
}


#pred_classification <- tibble::tibble()
#pred_regression <- tibble::tibble()
#
#for (i in 1:nrow(grid)) {
#
#  if ((i %% 10) == 0) {
#    logger::log_info("Training hyperparameter set {i} of {nrow(grid)}")
#  }
#
#  for (k in 1:length(folds_train_cls)) {
#
#    data_train_classification_k <- data_train_classification |>
#      dplyr::slice(folds_train_cls[[k]]$train)
#
#    data_pred_classification_k <- data_train_classification |>
#      dplyr::slice(folds_train_cls[[k]]$test)
#
#    fit_classification <- train_predict_ranger(
#      data_train = data_train_classification_k,
#      data_pred = data_pred_classification_k,
#      task = "classification",
#      num.trees = num_trees,
#      mtry = grid$mtry[i],
#      min.node.size = grid$min.node.size[i],
#      max.depth = grid$max.depth[i]
#    )
#
#    sim_classification <- treecomp::extract_similarity(
#      object = fit_classification$fit,
#      newdata = data_pred_classification_k,
#      refdata = data_train_classification_k,
#      match_training = TRUE
#    )
#
#    pred_classification <- dplyr::bind_rows(
#      pred_classification,
#      tibble::tibble(
#        mtry = grid$mtry[i],
#        min.node.size = grid$min.node.size[i],
#        max.depth = grid$max.depth[i],
#        index = folds_train_cls[[k]]$test,
#        pred = fit_classification$pred,
#        ess = 1 / rowSums(sim_classification^2),
#        n_train = nrow(data_train_classification_k)
#      )
#    )
#
#    data_train_regression_k <- data_train_regression |>
#      dplyr::slice(folds_train_regr[[k]]$train)
#
#    data_pred_regression_k <- data_train_regression |>
#      dplyr::slice(folds_train_regr[[k]]$test)
#
#    fit_regression <- train_predict_ranger(
#      data_train = data_train_regression_k,
#      data_pred = data_pred_regression_k,
#      task = "regression",
#      num.trees = num_trees,
#      mtry = grid$mtry[i],
#      min.node.size = grid$min.node.size[i],
#      max.depth = grid$max.depth[i]
#    )
#
#    sim_regression <- treecomp::extract_similarity(
#      object = fit_regression$fit,
#      newdata = data_pred_regression_k,
#      refdata = data_train_regression_k,
#      match_training = TRUE
#    )
#
#    pred_regression <- dplyr::bind_rows(
#      pred_regression,
#      tibble::tibble(
#        mtry = grid$mtry[i],
#        min.node.size = grid$min.node.size[i],
#        max.depth = grid$max.depth[i],
#        index = folds_train_regr[[k]]$test,
#        pred = fit_regression$pred,
#        ess = 1 / rowSums(sim_regression^2),
#        n_train = nrow(data_train_regression_k)
#      )
#    )
#  }
#}
validate <- function(args,
                     data_train_classification,
                     data_train_regression,
                     folds_train_cls,
                     folds_train_regr,
                     num_trees,
                     train_predict_ranger) {

  data_train_classification_k <- data_train_classification |>
    dplyr::slice(folds_train_cls[[args$fold]]$train)

  data_pred_classification_k <- data_train_classification |>
    dplyr::slice(folds_train_cls[[args$fold]]$test)

  fit_classification <- train_predict_ranger(
    data_train = data_train_classification_k,
    data_pred = data_pred_classification_k,
    task = "classification",
    num.trees = num_trees,
    mtry = args$mtry,
    min.node.size = args$min.node.size,
    max.depth = args$max.depth
  )

  sim_classification <- treecomp::extract_similarity(
    object = fit_classification$fit,
    newdata = data_pred_classification_k,
    refdata = data_train_classification_k,
    match_training = TRUE
  )

  pred_classification <- tibble::tibble(
    task = "classification",
    mtry = args$mtry,
    min.node.size = args$min.node.size,
    max.depth = args$max.depth,
    index = folds_train_cls[[args$fold]]$test,
    pred = fit_classification$pred,
    ess = 1 / rowSums(sim_classification^2),
    n_train = nrow(data_train_classification_k)
  )

  data_train_regression_k <- data_train_regression |>
    dplyr::slice(folds_train_regr[[args$fold]]$train)

  data_pred_regression_k <- data_train_regression |>
    dplyr::slice(folds_train_regr[[args$fold]]$test)

  fit_regression <- train_predict_ranger(
    data_train = data_train_regression_k,
    data_pred = data_pred_regression_k,
    task = "regression",
    num.trees = num_trees,
    mtry = args$mtry,
    min.node.size = args$min.node.size,
    max.depth = args$max.depth
  )

  sim_regression <- treecomp::extract_similarity(
    object = fit_regression$fit,
    newdata = data_pred_regression_k,
    refdata = data_train_regression_k,
    match_training = TRUE
  )

  pred_regression <- tibble::tibble(
    task = "regression",
    mtry = args$mtry,
    min.node.size = args$min.node.size,
    max.depth = args$max.depth,
    index = folds_train_regr[[args$fold]]$test,
    pred = fit_regression$pred,
    ess = 1 / rowSums(sim_regression^2),
    n_train = nrow(data_train_regression_k)
  )

  return(
    dplyr::bind_rows(
      pred_regression,
      pred_classification
    )
  )
}

args_table <- dplyr::cross_join(grid, tibble::tibble(fold = 1:length(folds_train_cls)))
args_list <- split(args_table, f = 1:nrow(args_table))

cluster <- parallel::makeCluster(parallel::detectCores())
pred_list <- pbapply::pblapply(
  X = args_list,
  FUN = validate,
  cl = cluster,
  data_train_classification = data_train_classification,
  data_train_regression = data_train_regression,
  folds_train_cls = folds_train_cls,
  folds_train_regr = folds_train_regr,
  num_trees = num_trees,
  train_predict_ranger = train_predict_ranger
)
parallel::stopCluster(cluster)

pred <- do.call(dplyr::bind_rows, args = pred_list)

deviance_classification <- data_train_classification |>
  dplyr::mutate(index = 1:dplyr::n()) |>
  dplyr::left_join(dplyr::filter(pred, task == "classification"), by = "index") |>
  # binomial deviance
  dplyr::mutate(deviance = -2 * log(ifelse(played_nfl == "yes", pred, 1 - pred)))
  
cv_classification <- deviance_classification |>
  dplyr::group_by(mtry, min.node.size, max.depth) |>
  dplyr::summarize(
    n = dplyr::n(),
    ess = mean(ess),
    deviance = mean(deviance),
    .groups = "drop"
  )

param_min_classification <- cv_classification |>
  dplyr::arrange(deviance) |>
  dplyr::slice(1)

deviance_regression <- data_train_regression |>
  dplyr::mutate(index = 1:dplyr::n()) |>
  dplyr::left_join(dplyr::filter(pred, task == "regression"), by = "index") |>
  # binomial deviance
  dplyr::mutate(deviance = (reg_qbr - pred)^2)

cv_regression <- deviance_regression |>
  dplyr::group_by(mtry, min.node.size, max.depth) |>
  dplyr::summarize(
    n = dplyr::n(),
    ess = mean(ess),
    deviance = mean(deviance),
    .groups = "drop"
  )

param_min_regression <- cv_regression |>
  dplyr::arrange(deviance) |>
  dplyr::slice(1)


{
  sputil::open_device("figures/ESS_vs_hyperparameter_values.pdf", height = 4, width = 8)
  plot <- dplyr::bind_rows(
    cv_classification |>
      dplyr::mutate(model = "Classification"),
    cv_regression |>
      dplyr::mutate(model = "Regression")
  ) |>
  tidyr::pivot_longer(cols = c(mtry, min.node.size, max.depth), names_to = "hyperparameter") |>
  dplyr::mutate(
    hyperparameter = factor(hyperparameter, levels = c("mtry", "min.node.size", "max.depth")),
    value = as.factor(value)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = value, y = ess, fill = model)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ hyperparameter, scales = "free_x") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Hyperparameter Value",
      y = "Effective Sample Size",
      fill = "Model"
    ) +
    ggplot2::theme(legend.position = "top")
  print(plot)
  dev.off()
}



fit_rf_classification <- train_predict_ranger(
  data_train = data_train_classification,
  data_pred = data_test,
  task = "classification",
  num.trees = num_trees,
  mtry = param_min_classification$mtry,
  min.node.size = param_min_classification$min.node.size,
  max.depth = param_min_classification$max.depth
)

fit_rf_regression <- train_predict_ranger(
  data_train = data_train_regression,
  data_pred = data_test,   # predict for ALL QBs
  task = "regression",
  num.trees = num_trees,
  mtry = param_min_regression$mtry,
  min.node.size = param_min_regression$min.node.size,
  max.depth = param_min_regression$max.depth
)

fit_glm_classification <- train_predict_glm(
  data_train = data_train_classification,
  data_pred = data_test,
  task = "classification"
)

fit_glm_regression <- train_predict_glm(
  data_train = data_train_regression,
  data_pred = data_test,
  task = "regression"
)

data_test_pred <- data_test |>
  dplyr::mutate(
    pred_rf_classification = fit_rf_classification$pred,
    pred_rf_regression = fit_rf_regression$pred,
    pred_rf_qbr = pred_rf_classification * pred_rf_regression,
    pred_glm_classification = fit_glm_classification$pred,
    pred_glm_regression = fit_glm_regression$pred,
    pred_glm_qbr = pred_glm_classification * pred_glm_regression,
  )

data_test_pred |>
  dplyr::summarize(
    deviance_null = mean((reg_qbr - mean(reg_qbr))^2),
    deviance_rf = mean((reg_qbr - pred_rf_qbr)^2),
    deviance_glm = mean((reg_qbr - pred_glm_qbr)^2)
  ) |>
  dplyr::mutate(
    devexp_rf = 1 - deviance_rf / deviance_null,
    devexp_glm = 1 - deviance_glm / deviance_null
  ) |>
  dplyr::select(dplyr::starts_with("devexp_"))

fit_rf_classification_full <- train_predict_ranger(
  data_train = classif_data,
  data_pred = classif_data,
  task = "classification",
  num.trees = num_trees,
  mtry = param_min_classification$mtry,
  min.node.size = param_min_classification$min.node.size,
  max.depth = param_min_classification$max.depth
)

fit_rf_regression_full <- train_predict_ranger(
  data_train = qbr_data,
  data_pred = classif_data,   # predict for ALL QBs
  task = "regression",
  num.trees = num_trees,
  mtry = param_min_regression$mtry,
  min.node.size = param_min_regression$min.node.size,
  max.depth = param_min_regression$max.depth
)



rgr_importance_scores <- importance(fit_rf_regression_full$fit)
cls_importance_scores <- importance(fit_rf_classification_full$fit)
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

# Get out-of-bag predictions for players used to train the regression model
pred_qbr_out_of_bag <- predict(fit_rf_regression_full$fit, data = past_data)$predictions
pred_qbr_out_of_bag[past_data$played_nfl == "yes" & past_data$reg_qbr > 0] <- fit_rf_regression_full$fit$predictions

new_plot_data <- past_data %>%
  mutate(
    pred_qbr = pred_qbr_out_of_bag,
    pred_play = fit_rf_classification_full$fit$predictions[, 2],    # out-of-bag predictions
    predictions = pred_play * pred_qbr
  )

{
  sputil::open_device("figures/3d_plot.pdf", height = 5)
  plot <- new_plot_data |>
    ggplot(aes(x = ncaa_sos_last, y = ncaa_yds_per_att_career, color = predictions)) +
    geom_point(size = 1) +
    scale_color_viridis_c() +
    labs(
      x = "Final Strength of Schedule",
      y = "Career Yds/Att",
      color = "Predicted QBR"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
  print(plot)
  dev.off()
  }

{
  sputil::open_device("figures/predicted_vs_actuals.pdf", height = 5, width = 5)
  plot <- ggplot2::ggplot(new_plot_data, aes(x = predictions, y = reg_qbr, alpha = nfl_att_per_year)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Predicted QBR",
      y = "Actual QBR"
    ) + 
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ggplot2::xlim(0, 100) +
    ggplot2::ylim(0, 100) +
    ggplot2::scale_alpha_continuous(name = "NFL Passing\nAttempts per Year") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.8, 0.3))
  print(plot)
  dev.off()
}


present_data <- data |>
  dplyr::filter(ncaa_year_last == 2024)

present_data <- present_data |>
  dplyr::mutate(
    pred_classification = predict(fit_rf_classification_full$fit, data = present_data)$predictions[, 2],
    pred_regression = predict(fit_rf_regression_full$fit, data = present_data)$predictions,
    predictions = pred_classification * pred_regression
  )

present_data |>
  dplyr::arrange(-predictions) |>
  dplyr::mutate(
    pred_classification = paste0(round(100 * pred_classification), "\\%"),
    pred_regression = sprintf("%.1f", pred_regression),
    predictions = sprintf("%.1f", predictions)
  ) |>
  dplyr::select(player_name, pred_classification, pred_regression, predictions) |>
  # Remove draft-ineligible players
  dplyr::filter(!player_name %in% c("Darian Mensah", "Cade Klubnik", "E.J. Warner", "Cameron Skattebo")) |>
  head(10) |>
  sputil::write_latex_table(
    file = "tables/top_ten.tex",
    colnames = c("Quarterback", "P(QBR $>$ 0)", "E[QBR $|$ QBR $>$ 0]", "Predicted QBR"),
    align = "l|cc|c"
  )


# Produce similarity figures and tables ----

similarity_matrix_cls <- treecomp::extract_similarity(
  object = fit_rf_classification_full$fit,
  newdata = present_data,
  refdata = past_data
)

similarity_matrix_rgr <- treecomp::extract_similarity(
  object = fit_rf_regression_full$fit,
  newdata = present_data,
  refdata = qbr_data
)

similarity_df_cls <- as.data.frame(similarity_matrix_cls)
colnames(similarity_df_cls) <- past_data$player_name  # Name columns by training players
rownames(similarity_df_cls) <- present_data$player_name  # Name rows by test players

similarity_df_rgr <- as.data.frame(similarity_matrix_rgr)
colnames(similarity_df_rgr) <- qbr_data$player_name  # Name columns by training players
rownames(similarity_df_rgr) <- present_data$player_name  # Name rows by test players

get_prospect_plots <- function(player, present_data, similarity_matrix_rgr,
                               qbr_data, player_last_name) {
  index <- which(present_data$player_name == player)
  
  similarity_scores <- similarity_matrix_rgr[index, ]

  prob_zero <- 1 - present_data[index, ]$pred_classification
  weight_zero <- prob_zero / (1 - prob_zero)
  
  plot_data <- data.frame(
    player_name = c(qbr_data$player_name, "Zero"),
    sim_score = c(similarity_scores, weight_zero),
    QBR = c(qbr_data$reg_qbr, 0)
  ) %>%
    filter(sim_score > 0)
  
  {
    sputil::open_device(paste0("figures/prospect_histogram_", player_last_name, ".pdf"), height = 3, width = 3)
    plot <- ggplot(plot_data, aes(x = QBR, y = ..density.., weight = sim_score)) +
      geom_histogram(binwidth = 5, color = "dodgerblue", fill = "dodgerblue", alpha = 0.8) +
      theme_minimal() +
      labs(x = "QBR", y = "Weights",
           title = player) +
      geom_vline(aes(xintercept = present_data[index, ]$predictions), 
                 color = "darkorange", linewidth = 2) +
      coord_cartesian(ylim = c(0, 0.08))
    print(plot)
    dev.off()
    }
  
  {
    sputil::open_device(paste0("figures/prospect_similarity_", player_last_name, ".pdf"), height = 3, width = 3)
    plot <- ggplot(plot_data, aes(x = QBR, y = sim_score)) +
      geom_point() +
      ggtitle(player) +
      xlab("Training Player QBR Value") +
      ylab("Training Player Similarity Score") +
      coord_cartesian(ylim = c(0, 0.03)) +
      theme_minimal()
    print(plot)
    dev.off()
  }
}

get_prospect_plots("Cameron Ward", present_data, similarity_matrix_rgr,
                    qbr_data, "ward")
get_prospect_plots("Shedeur Sanders", present_data, similarity_matrix_rgr,
                   qbr_data, "sanders")
get_prospect_plots("Jaxson Dart", present_data, similarity_matrix_rgr,
                   qbr_data, "dart")
get_prospect_plots("Tyler Shough", present_data, similarity_matrix_rgr,
                   qbr_data, "shough")


get_top10_comps_side_by_side <- function(target_name,
                                         player_initials,
                                         similarity_matrix_rgr,
                                         present_data,
                                         past_data,
                                         qbr_data,
                                         qbr_col = "reg_qbr") {
  
  idx <- match(target_name, present_data$player_name)
  if (is.na(idx)) stop("target_name not found in present_data$player_name")
  
  rgr_top10 <- tibble(
    rgr_player = qbr_data$player_name,
    rgr_sim    = as.numeric(similarity_matrix_rgr[idx, , drop = TRUE]),
    rgr_qbr    = qbr_data[[qbr_col]]
  ) %>%
    filter(rgr_sim > 0) %>%
    arrange(desc(rgr_sim)) %>%
    slice_head(n = 10) %>%
    mutate(rank = row_number()) %>%
    select(
      rank, rgr_player, rgr_sim, rgr_qbr
    ) %>%
    transmute(
      !!paste0(player_initials, "_name") := rgr_player,
      !!paste0(player_initials, "_score") := paste0(sprintf("%.1f", 100 * rgr_sim), "\\%")
    )
}

cw_df <- get_top10_comps_side_by_side("Cameron Ward", "CW", similarity_matrix_rgr, 
                                      present_data, past_data, qbr_data) 
ss_df <- get_top10_comps_side_by_side("Shedeur Sanders", "SS", similarity_matrix_rgr, 
                                      present_data, past_data, qbr_data) 
jd_df <- get_top10_comps_side_by_side("Jaxson Dart", "JD", similarity_matrix_rgr, 
                                      present_data, past_data, qbr_data) 
ts_df <- get_top10_comps_side_by_side("Tyler Shough", "TS", similarity_matrix_rgr, 
                                      present_data, past_data, qbr_data) 

cbind(cw_df, ss_df, jd_df, ts_df) %>%
  sputil::write_latex_table(
    file = "tables/side_by_side_similarity.tex",
    colnames = rep(c("Comp", "Score"), times = 4),
    prefix_rows = "
      \\multicolumn{2}{c|}{Cam Ward} &
      \\multicolumn{2}{c|}{Shedeur Sanders} &
      \\multicolumn{2}{c|}{Jaxson Dart} &
      \\multicolumn{2}{c}{Tyler Shough}
    ",
    align = "lr|lr|lr|lr"
  )


# Helper to compute ENC and cumulative weight stats for a single similarity vector
compute_similarity_stats <- function(sim_vec, player_name, top_k = 10) {
  w <- sim_vec[sim_vec > 0]
  w <- w / sum(w)
  ess = (sum(w))^2 / sum((w^2))
  w_sorted <- sort(w, decreasing = TRUE)
  cum_w <- cumsum(w_sorted)
  n_50 <- which(cum_w >= 0.50)[1]
  n_75 <- which(cum_w >= 0.75)[1]
  n_90 <- which(cum_w >= 0.90)[1]
  k_eff <- min(top_k, length(w_sorted))
  top_k_share <- sum(w_sorted[1:k_eff])
  
  data.frame(
    player_name = player_name,
    ESS = ess,
    comps_50 = n_50,
    comps_75 = n_75,
    comps_90 = n_90,
    top_k = k_eff,
    top_k_share = top_k_share
  )
}

all_similarity_stats <- lapply(seq_len(nrow(similarity_matrix_rgr)), function(i) {
  compute_similarity_stats(
    sim_vec = similarity_matrix_rgr[i, ],
    player_name = present_data$player_name[i],
    top_k = 10
  )
}) %>%
  bind_rows() %>%
  transmute(
    player_name,
    ESS = round(ESS, 1),
    comps_50,
    comps_75,
    comps_90,
    top_k_share_pct = paste0(sprintf("%.1f", 100 * top_k_share), "\\%")
  ) %>%
  filter(player_name %in% c("Cameron Ward", "Shedeur Sanders", "Jaxson Dart", "Tyler Shough"))

all_similarity_stats %>%
  sputil::write_latex_table(
    file = "tables/prospect_ess.tex",
    colnames = c("Player", "ESS", "50\\%", "75\\%", "90\\%", "Top 10\\% Share"),
    align = "l|r|r|r|r|r"
  )

library(purrr)

players_to_plot <- c("Cameron Ward", "Shedeur Sanders", 
                     "Jaxson Dart", "Tyler Shough")

plot_df <- map_dfr(seq_len(nrow(similarity_matrix_rgr)), function(i) {
  
  player <- present_data$player_name[i]
  
  w <- similarity_matrix_rgr[i, ]
  w <- w[w > 0]
  w <- w / sum(w)
  
  w_sorted <- sort(w, decreasing = TRUE)
  cum_w <- cumsum(w_sorted)
  
  tibble(
    player_name = player,
    n_comps = seq_along(cum_w),
    pct_prediction = 100 * cum_w
  )
})


{
  sputil::open_device("figures/comp_pct_plot.pdf", height = 5)
  plot <- plot_df |>
    dplyr::filter(player_name %in% players_to_plot) |>
    dplyr::mutate(
      player_name = factor(
        player_name,
        levels = c("Cameron Ward", "Shedeur Sanders", "Jaxson Dart", "Tyler Shough")
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = n_comps, y = pct_prediction, color = player_name)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_y_continuous(
      limits = c(0, 101),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::labs(
      x = "Number of Comps",
      y = "Percent of Prediction",
      color = "Player"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.8, 0.2))
  print(plot)
  dev.off()
}
