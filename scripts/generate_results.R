library(dplyr)
library(ranger)
library(tuneRanger)
library(mlr)
library(ggplot2)
library(xtable)

num_trees <- 1000

set.seed(123)


# Determine mean regression for QBR ----

QBR2_passing_data <- treecomp::nfl_qbr_by_year %>%
  arrange(player_name, year) %>%
  group_by(player_name) %>%
  mutate(qbr_2 = lead(qbr),
         qbr_2 = ifelse(is.na(qbr_2), 0, qbr_2)) %>%
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
    ncaa_att_per_year > 5,
    # Exclude players with very few NFL attempts per season because their QBR will be unstable
    is.na(nfl_att_per_year) | (nfl_att_per_year == 0) | nfl_att_per_year >= 10
  ) %>%
  # Remaining NAs are due to not playing in the NFL
  mutate_all(~ replace(., is.na(.), 0))

set.seed(123)  # For reproducibility

train_indices <- sample(1:nrow(past_data), size = 0.7 * nrow(past_data))
train_data <- past_data[train_indices, ]
test_data <- past_data[-train_indices, ]

# Model tuning
task_data <- train_data %>%
  select(
    reg_qbr,
    ncaa_yds_per_att_career, ncaa_games_per_year,
    ncaa_att_per_year, ncaa_cmp_per_year, ncaa_yds_per_year, ncaa_td_per_year, ncaa_int_per_year,
    ncaa_rush_att_per_year, ncaa_rush_yds_per_year, ncaa_rush_td_per_year,
    ncaa_sos_last, ncaa_games_last, ncaa_yds_per_att_last, ncaa_passer_rating_last,
    ncaa_all_america, ncaa_heisman, ncaa_heisman_last
  )
task_data$ncaa_sos_last <- as.numeric(task_data$ncaa_sos_last)
task <- makeRegrTask(data = task_data, target = "reg_qbr")

tuned_model <- tuneRanger(
  task,
  num.trees = num_trees,
  iters = 100,
  tune.parameters = c("mtry", "min.node.size"),
  parameters = list(replace = TRUE)
)

print(tuned_model)

# Model
rf_model <- ranger(
  formula = reg_qbr ~
    ncaa_yds_per_att_career + ncaa_games_per_year +
    ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
    ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
    ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
    ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
  data = train_data,
  num.trees = num_trees,
  importance = "impurity",
  keep.inbag = TRUE,
  min.node.size = tuned_model$recommended.pars$min.node.size,
  mtry = tuned_model$recommended.pars$mtry
)

test_data$predictions <- predict(rf_model, data = test_data)$predictions
rmse <- sqrt(mean((test_data$reg_qbr - test_data$predictions)^2))
total_variance <- var(test_data$reg_qbr)
residuals <- test_data$reg_qbr - test_data$predictions
residual_variance <- var(residuals)
explained_variance <- total_variance - residual_variance
print((explained_variance / total_variance) * 100)


# Re-fit random forest on full data set ----

full_rf_model <- ranger(
  formula = reg_qbr ~
    ncaa_yds_per_att_career + ncaa_games_per_year +
    ncaa_att_per_year + ncaa_cmp_per_year + ncaa_yds_per_year + ncaa_td_per_year + ncaa_int_per_year +
    ncaa_rush_att_per_year + ncaa_rush_yds_per_year + ncaa_rush_td_per_year +
    ncaa_sos_last + ncaa_games_last + ncaa_yds_per_att_last + ncaa_passer_rating_last +
    ncaa_all_america + ncaa_heisman + ncaa_heisman_last,
  data = past_data,
  num.trees = num_trees,
  importance = "impurity",
  keep.inbag = TRUE,
  min.node.size = tuned_model$recommended.pars$min.node.size,
  mtry = tuned_model$recommended.pars$mtry
)

# Model plots
importance_scores <- importance(full_rf_model)
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
importance_df <- data.frame(
  Variable = variable_display[names(importance_scores)],
  Importance = importance_scores
)

{
  sputil::open_device("figures/variable_importance.pdf", height = 5)
  plot <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +  
    labs(title = "Variable Importance",
         x = "", y = "Importance") +
    theme_minimal()
  print(plot)
  dev.off()
}

new_plot_data <- past_data %>%
  mutate(predictions = predict(full_rf_model, data = past_data)$predictions)

{
  sputil::open_device("figures/3d_plot.pdf", height = 5)
  plot <- new_plot_data |>
    ggplot(aes(x = ncaa_yds_per_year, y = ncaa_sos_last, color = predictions)) +
    geom_point(size = 1) +
    scale_color_viridis_c() +
    labs(
      x = "Passing Yards per Season (College)",
      y = "Strength of Schedule (College)",
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

present_data$predictions <- round(predict(full_rf_model, data = present_data)$predictions, 3)

present_data %>%
  select(player_name, predictions) %>%
  # Remove draft-ineligible players
  filter(!player_name %in% c("Darian Mensah", "Cade Klubnik", "E.J. Warner")) %>%
  arrange(-predictions) %>%
  head(10) %>%
  mutate(predictions = sprintf("%.1f", predictions)) %>%
  sputil::write_latex_table(file = "tables/top_ten.tex")


# Produce similarity figures and tables ----

similarity_matrix <- treecomp::extract_similarity(
  object = full_rf_model,
  newdata = present_data,
  refdata = past_data
)

similarity_df <- as.data.frame(similarity_matrix)
colnames(similarity_df) <- past_data$player_name  # Name columns by training players
rownames(similarity_df) <- present_data$player_name  # Name rows by test players

ss_index <- which(present_data$player_name == "Shedeur Sanders")

ss_similarity_scores <- similarity_matrix[ss_index, ]

ss_plot_data <- data.frame(
  player_name = past_data$player_name,
  sim_score = ss_similarity_scores,
  QBR = past_data$reg_qbr
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

cw_similarity_scores <- similarity_matrix[cw_index, ]

cw_plot_data <- data.frame(
  player_name = past_data$player_name,
  sim_score = cw_similarity_scores,
  QBR = past_data$reg_qbr
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

jd_similarity_scores <- similarity_matrix[jd_index, ]

jd_plot_data <- data.frame(
  player_name = past_data$player_name,
  sim_score = jd_similarity_scores,
  QBR = past_data$reg_qbr
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

dg_similarity_scores <- similarity_matrix[dg_index, ]

dg_plot_data <- data.frame(
  player_name = past_data$player_name,
  sim_score = dg_similarity_scores,
  QBR = past_data$reg_qbr
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

cw_index <- which(present_data$player_name == "Cameron Ward")
cw_similarity_scores <- similarity_matrix[cw_index, ]
cw_similarity_scores <- setNames(cw_similarity_scores, past_data$player_name)
sorted_similarity_scores <- sort(cw_similarity_scores, decreasing = TRUE)

cw_top_10 <- data.frame(
  `Player Name` = names(sorted_similarity_scores)[1:10],
  Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
  Base_Player = "Cameron Ward"
)

# Jaxson Dart
jd_index <- which(present_data$player_name == "Jaxson Dart")
jd_similarity_scores <- similarity_matrix[jd_index, ]
jd_similarity_scores <- setNames(jd_similarity_scores, past_data$player_name)
sorted_similarity_scores <- sort(jd_similarity_scores, decreasing = TRUE)

jd_top_10 <- data.frame(
  `Player Name` = names(sorted_similarity_scores)[1:10],
  Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
  Base_Player = "Jaxson Dart"
)

# Dillon Gabriel
dg_index <- which(present_data$player_name == "Dillon Gabriel")
dg_similarity_scores <- similarity_matrix[dg_index, ]
dg_similarity_scores <- setNames(dg_similarity_scores, past_data$player_name)
sorted_similarity_scores <- sort(dg_similarity_scores, decreasing = TRUE)

dg_top_10 <- data.frame(
  `Player Name` = names(sorted_similarity_scores)[1:10],
  Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
  Base_Player = "Dillon Gabriel"
)

# Shedeur Sanders
ss_index <- which(present_data$player_name == "Shedeur Sanders")
ss_similarity_scores <- similarity_matrix[ss_index, ]
ss_similarity_scores <- setNames(ss_similarity_scores, past_data$player_name)
sorted_similarity_scores <- sort(ss_similarity_scores, decreasing = TRUE)

ss_top_10 <- data.frame(
  `Player Name` = names(sorted_similarity_scores)[1:10],
  Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
  Base_Player = "Shedeur Sanders"
)

# Create column-renamed versions of each top 10 table
cw_top_10_wide <- cw_top_10 %>%
  select(Player.Name, Similarity) %>%
  setNames(c("CW_Player", "CW_Similarity"))

jd_top_10_wide <- jd_top_10 %>%
  select(`Player.Name`, Similarity) %>%
  setNames(c("JD_Player", "JD_Similarity"))

dg_top_10_wide <- dg_top_10 %>%
  select(`Player.Name`, Similarity) %>%
  setNames(c("DG_Player", "DG_Similarity"))

ss_top_10_wide <- ss_top_10 %>%
  select(`Player.Name`, Similarity) %>%
  setNames(c("SS_Player", "SS_Similarity"))

cbind(cw_top_10_wide, ss_top_10_wide, jd_top_10_wide, dg_top_10_wide) %>%
  sputil::write_latex_table(file = "tables/side_by_side_similarity.tex")
