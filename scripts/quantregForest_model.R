library(dplyr)
library(ranger)
library(tuneRanger)
library(mlr)
library(ggplot2)
library(xtable)
library(quantregForest)

num_trees <- 1000

set.seed(123)

# Determine mean regression for QBR ----

QBR2_passing_data <- treecomp::nfl_qbr_by_year %>%
  arrange(player_name, year) %>%
  group_by(player_name) %>%
  mutate(
    qbr_2 = lead(qbr),
    qbr_2 = ifelse(is.na(qbr_2), 0, qbr_2)
  ) %>%
  filter(year != 2024)

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
    ncaa_heisman_last = ifelse(is.na(ncaa_heisman_last), 11, ncaa_heisman_last)
  )

past_data <- data %>%
  filter(
    ncaa_year_last < 2024,
    ncaa_games_career > 6,
    ncaa_att_per_year > 50,
    # Exclude players with very few NFL attempts per season because their QBR will be unstable
    is.na(nfl_att_per_year) | (nfl_att_per_year == 0) | nfl_att_per_year >= 10
  ) %>%
  # Remaining NAs are due to not playing in the NFL
  mutate_all(~ replace(., is.na(.), 0))

set.seed(123)  # For reproducibility

train_indices <- sample(1:nrow(past_data), size = 0.7 * nrow(past_data))
train_data <- past_data[train_indices, ]
test_data  <- past_data[-train_indices, ]

predictor_vars <- c(
  "ncaa_yds_per_att_career", "ncaa_games_per_year",
  "ncaa_att_per_year", "ncaa_cmp_per_year", "ncaa_yds_per_year",
  "ncaa_td_per_year", "ncaa_int_per_year",
  "ncaa_rush_att_per_year", "ncaa_rush_yds_per_year", "ncaa_rush_td_per_year",
  "ncaa_sos_last", "ncaa_games_last", "ncaa_yds_per_att_last",
  "ncaa_passer_rating_last", "ncaa_all_america",
  "ncaa_heisman", "ncaa_heisman_last"
)

# Model (quantile regression forest) ----

y <- train_data$reg_qbr

x <- train_data %>%
  dplyr::select(all_of(predictor_vars))

rf_model <- quantregForest(
  x = x,
  y = y,
  ntree = num_trees,
  importance = TRUE,
  keep.inbag = TRUE
)

# Build test X matrix the same way as train
x_test <- test_data %>%
  dplyr::select(all_of(predictor_vars))

test_data$predictions <- predict(rf_model, newdata = x_test)

rmse <- sqrt(mean((test_data$reg_qbr - test_data$predictions)^2))

total_variance <- var(test_data$reg_qbr)
residuals <- test_data$reg_qbr - test_data$predictions
residual_variance <- var(residuals)
explained_variance <- total_variance - residual_variance

print((explained_variance / total_variance) * 100)

# Re-fit quantile regression forest on full data set ----

y <- past_data$reg_qbr

x <- past_data %>%
  dplyr::select(all_of(predictor_vars))

full_rf_model <- quantregForest(
  x = x,
  y = y,
  ntree = num_trees,
  importance = TRUE,
  keep.inbag = TRUE
)



importance_mat <- full_rf_model$importance

importance_scores <- importance_mat[, 1]
var_names <- rownames(importance_mat)


importance_scores <- as.numeric(importance_scores)
names(importance_scores) <- var_names


## 2. Nice display names for variables ----
variable_display <- c(
  ncaa_yds_per_att_career = "Career Yds/Att",
  ncaa_games_per_year     = "Games/Season",
  ncaa_att_per_year       = "Attempts/Season",
  ncaa_cmp_per_year       = "Completions/Season",
  ncaa_yds_per_year       = "Yards/Season",
  ncaa_td_per_year        = "Touchdowns/Season",
  ncaa_int_per_year       = "Interceptions/Season",
  ncaa_rush_att_per_year  = "Rush Attempts/Season",
  ncaa_rush_yds_per_year  = "Rush Yards/Season",
  ncaa_rush_td_per_year   = "Rush Touchdowns/Season",
  ncaa_sos_last           = "Final Strength of Schedule",
  ncaa_games_last         = "Final Games",
  ncaa_yds_per_att_last   = "Final Yds/Att",
  ncaa_passer_rating_last = "Final Passer Rating",
  ncaa_all_america        = "All-America Seasons",
  ncaa_heisman            = "Won Heisman Award",
  ncaa_heisman_last       = "Final Heisman Voting"
)


## 3. Build importance data frame ----
importance_df <- data.frame(
  Variable   = variable_display[names(importance_scores)],
  Importance = importance_scores,
  check.names = FALSE
)

importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
row.names(importance_df) <- NULL

{
  sputil::open_device(file.path(
    "/Users/elisabethmillington/Fall2025/treecomp project/Code/figures/variable_importance.pdf"), height = 5)
  plot <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +  
    labs(title = "Variable Importance",
         x = "", y = "Importance") +
    theme_minimal()
  print(plot)
  dev.off()
}


x_past <- past_data %>%
  dplyr::select(all_of(predictor_vars))

new_plot_data <- past_data %>%
  mutate(predictions = predict(full_rf_model, newdata = x_past))


{
  sputil::open_device(file.path("/Users/elisabethmillington/Fall2025/treecomp project/Code/figures/3d_plot.pdf"), height = 5)
  plot <- new_plot_data |>
    ggplot(aes(x = ncaa_yds_per_year, y =  ncaa_td_per_year, color = predictions)) +
    geom_point(size = 1) +
    scale_color_viridis_c() +
    labs(
      x = "Passing Yards per Season (College)",
      y = "Passing Touchdowns per Season (College)",
      color = "Predicted QBR"
    ) +
    theme_minimal() +
    theme(legend.position = "inside", legend.position.inside = c(0.85, 0.75))
  print(plot)
  dev.off()
  }

{
  sputil::open_device(file.path("/Users/elisabethmillington/Fall2025/treecomp project/Code/figures/predicted_vs_actuals.pdf"), height = 5, width = 5)
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


# Present (draft) players ----

present_data <- data %>%
  filter(ncaa_year_last == 2024)

present_data_x <- present_data %>%
  dplyr::select(all_of(predictor_vars))

present_data_x <- present_data_x %>% mutate_all(~ replace(., is.na(.), 0))

present_data$predictions <- round(
  predict(full_rf_model, newdata = present_data_x),
  3
)

present_predictions <- as.data.frame(present_data %>%
  group_by(player_name) %>%
  summarise(
    q10 = quantile(predictions, probs = 0.1),
    q50 = quantile(predictions, probs = 0.5),
    q90 = quantile(predictions, probs = 0.9),
    .groups = "drop"
  )) 


top_ten_predictions <- present_data %>%
  select(player_name, predictions) %>%
  filter(!player_name %in% c("Darian Mensah", "Cade Klubnik", "E.J. Warner")) %>%
  group_by(player_name) %>%
  summarise(
    q10 = quantile(predictions, probs = 0.1),
    q50 = quantile(predictions, probs = 0.5),
    q90 = quantile(predictions, probs = 0.9),
    .groups = "drop"
  ) %>%
  arrange(desc(q50)) %>%      # sort by the .5 quantile
  slice_head(n = 10) %>%
  mutate(
    q10 = sprintf("%.1f", q10),
    q50 = sprintf("%.1f", q50),
    q90 = sprintf("%.1f", q90)
  )


ref_x <- past_data %>%
  dplyr::select(
    ncaa_yds_per_att_career, ncaa_games_per_year,
    ncaa_att_per_year, ncaa_cmp_per_year, ncaa_yds_per_year,
    ncaa_td_per_year, ncaa_int_per_year,
    ncaa_rush_att_per_year, ncaa_rush_yds_per_year, ncaa_rush_td_per_year,
    ncaa_sos_last, ncaa_games_last, ncaa_yds_per_att_last,
    ncaa_passer_rating_last, ncaa_all_america,
    ncaa_heisman, ncaa_heisman_last
  ) %>%
  dplyr::select(all_of(predictor_vars)) %>%
  mutate(across(everything(), ~ replace(., is.na(.), 0)))

new_x <- present_data %>%
  dplyr::select(
    ncaa_yds_per_att_career, ncaa_games_per_year,
    ncaa_att_per_year, ncaa_cmp_per_year, ncaa_yds_per_year,
    ncaa_td_per_year, ncaa_int_per_year,
    ncaa_rush_att_per_year, ncaa_rush_yds_per_year, ncaa_rush_td_per_year,
    ncaa_sos_last, ncaa_games_last, ncaa_yds_per_att_last,
    ncaa_passer_rating_last, ncaa_all_america,
    ncaa_heisman, ncaa_heisman_last
  ) %>% 
  dplyr::select(all_of(predictor_vars)) %>%
  mutate(across(everything(), ~ replace(., is.na(.), 0)))


object <- full_rf_model        

# New points you want similarities for
newdata <- new_x 

# Reference points (usually training data predictors)
refdata <- ref_x

extract_similarity <- function(object, newdata, refdata, match_training = TRUE) {
  
  if ("ranger" %in% class(object)) {
    
    if (match_training) {
      if (!is.null(object$inbag.counts)) {
        ref_multiplier <- do.call(cbind, object$inbag.counts)
      } else {
        stop("`object` does not include inbag counts (necessary for `match_training` = TRUE)")
      }
    }
    
    num_trees <- object$num.trees
    terminal_nodes_new <- stats::predict(
      object,
      data = newdata,
      type = "terminalNodes"
    )$predictions
    terminal_nodes_ref <- stats::predict(
      object,
      data = refdata,
      type = "terminalNodes"
    )$predictions
    
  } else if ("randomForest" %in% class(object) || "quantregForest" %in% class(object)) {
    
    # For quantregForest, temporarily treat it as a randomForest for prediction
    rf_object <- object
    if ("quantregForest" %in% class(object)) {
      class(rf_object) <- "randomForest"
    }
    
    if (match_training) {
      if (!is.null(rf_object$inbag)) {
        ref_multiplier <- rf_object$inbag
      } else {
        stop("`object` does not include inbag counts (necessary for `match_training` = TRUE).\n",
             "Refit with keep.inbag = TRUE.")
      }
    }
    
    num_trees <- rf_object$ntree
    terminal_nodes_new <- attr(
      stats::predict(rf_object, newdata = newdata, nodes = TRUE),
      "nodes"
    )
    terminal_nodes_ref <- attr(
      stats::predict(rf_object, newdata = refdata, nodes = TRUE),
      "nodes"
    )
    
  } else {
    stop("`object` must be a fitted `ranger`, `randomForest`, or `quantregForest` model")
  }
  
  if (!match_training) {
    ref_multiplier <- matrix(1, nrow = nrow(refdata), ncol = num_trees)
  }
  
  similarity_matrix <- matrix(0, nrow = nrow(newdata), ncol = nrow(refdata))
  
  for (t in seq_len(num_trees)) {
    
    node_new <- terminal_nodes_new[, t]
    node_ref <- terminal_nodes_ref[, t]
    tree_weight <- ref_multiplier[, t]
    
    node_map_new <- split(seq_len(nrow(newdata)), node_new)
    node_map_ref <- split(seq_len(nrow(refdata)), node_ref)
    
    for (node in intersect(names(node_map_new), names(node_map_ref))) {
      idx_new <- node_map_new[[node]]
      idx_ref <- node_map_ref[[node]]
      
      if (length(idx_ref) > 0) {
        w <- tree_weight[idx_ref]
        w_norm <- (w / sum(w)) / num_trees
        similarity_matrix[idx_new, idx_ref] <- similarity_matrix[idx_new, idx_ref] +
          matrix(w_norm, nrow = length(idx_new), ncol = length(w_norm), byrow = TRUE)
      }
    }
  }
  
  similarity_matrix
}

similarity_matrix <- extract_similarity(object, newdata, refdata)

similarity_df <- as.data.frame(similarity_matrix)
colnames(similarity_df) <- past_data$player_name  # Name columns by training players
rownames(similarity_df) <- present_data$player_name  # Name rows by test players


find_top_ten <- function(player, player_initials, similarity_matrix, refdata) {
  player_index <- which(present_data$player_name == player)
  player_similarity_scores <- similarity_matrix[player_index, ]
  player_similarity_scores <- setNames(player_similarity_scores, refdata$player_name)
  sorted_similarity_scores <- sort(player_similarity_scores, decreasing = TRUE)
  
  top_10 <- data.frame(
    `Player Name` = names(sorted_similarity_scores)[1:10],
    Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
    Base_Player = player
  )
  
  top_10_wide <- top_10 %>%
    select(Player.Name, Similarity) %>%
    setNames(c(paste0(player_initials, "_Player"), paste0(player_initials, "_Similarity")))
  
  
  return(top_10_wide)
}

dg_top_10_wide <- find_top_ten("Dillon Gabriel", "DG", similarity_matrix, past_data)
ss_top_10_wide <- find_top_ten("Shedeur Sanders", "SS", similarity_matrix, past_data)
cw_top_10_wide <- find_top_ten("Cameron Ward", "CW", similarity_matrix, past_data)
jd_top_10_wide <- find_top_ten("Jaxson Dart", "JD", similarity_matrix, past_data)
ts_top_10_wide <- find_top_ten("Tyler Shough", "TS", similarity_matrix, past_data)


cbind(cw_top_10_wide, dg_top_10_wide, ss_top_10_wide, jd_top_10_wide, ts_top_10_wide) %>%
  sputil::write_latex_table(
    file = "/Users/elisabethmillington/Fall2025/treecomp\ project/Code/tables/side_by_side_similarity.tex",
    colnames = rep(c("Comp", "Score"), times = 5),
    prefix_rows = "
      \\multicolumn{2}{c|}{Cam Ward} &
      \\multicolumn{2}{c|}{Dillon Gabriel} &
      \\multicolumn{2}{c|}{Shedeur Sanders} &
      \\multicolumn{2}{c|}{Jaxson Dart} &
      \\multicolumn{2}{c}{Tyler Shough}
    ",
    align = "lr|lr|lr|lr|lr"
  )

get_plots <- function(player, last_name, similarity_matrix, present_data, prediction_data, past_data) {
  player_index <- which(present_data$player_name == player)
  similarity_scores <- similarity_matrix[player_index, ]

  plot_data <- data.frame(
    player_name = past_data$player_name,
    sim_score = similarity_scores,
    QBR = past_data$reg_qbr) %>%
    filter(sim_score > 0)

  {
    sputil::open_device(file.path(
      "/Users/elisabethmillington/Fall2025/treecomp project/Code/figures",
      paste0("prospect_histogram_", last_name, ".pdf")
    ), height = 3, width = 3)
    plot <- ggplot(plot_data, aes(x = QBR, y = after_stat(density), weight = sim_score)) +
      geom_histogram(binwidth = 5, color = "blue", fill = "blue", alpha = 0.8) +
      theme_minimal() +
      labs(x = "QBR", y = "Weights", title = player) +
      geom_vline(
        xintercept = prediction_data$q50[player_index],
        color = "green") +
      coord_cartesian(ylim = c(0, 0.08))
    print(plot)
    dev.off()
    }

  {
    sputil::open_device(file.path(
      "/Users/elisabethmillington/Fall2025/treecomp project/Code/figures",
      paste0("prospect_similarity_", last_name, ".pdf")
    ), height = 3, width = 3)
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

get_plots("Cameron Ward", "ward", similarity_matrix, present_data, present_predictions, past_data)
get_plots("Jaxson Dart", "dart", similarity_matrix, present_data, present_predictions, past_data)
get_plots("Dillon Gabriel", "gabriel", similarity_matrix, present_data, present_predictions, past_data)
get_plots("Shedeur Sanders", "sanders", similarity_matrix, present_data, present_predictions, past_data)

