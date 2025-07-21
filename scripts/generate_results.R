library(dplyr)
library(ranger)
library(tuneRanger)
library(mlr)
library(ggplot2)
library(xtable)

set.seed(123)


# Determine mean regression for QBR ----

QBR2_passing_data <- treecomp::NFL_QBR_by_year %>%
  arrange(Player, Season) %>%
  group_by(Player) %>%
  mutate(QBR2 = lead(QBR),
         QBR2 = ifelse(is.na(QBR2), 0, QBR2)) %>%
  filter(Season != 2024)

QBR2_model <- nls(
  formula = QBR2 ~ (Att * QBR) / (Att + c),
  data = QBR2_passing_data,
  start = list(c = 1)  # Starting guess for c
)

summary(QBR2_model)

c <- coef(QBR2_model)[1]


# Tune random forest ----

data <- treecomp::quarterback %>%
  mutate(
    reg_QBR = Avg_Att / (Avg_Att + c) * mean_QBR,
    # NA for Heisman voting means you didn't finish in the top 10
    last_H_vote = ifelse(is.na(last_H_vote), 11, last_H_vote)
  )

past_data <- data %>%
  filter(last_season < 2024, c_career_tot_games > 6, c_career_att > 5) %>%
  # Remaining NAs are due to not playing in the NFL
  mutate_all(~ replace(., is.na(.), 0))

set.seed(123)  # For reproducibility

train_indices <- sample(1:nrow(past_data), size = 0.7 * nrow(past_data))
train_data <- past_data[train_indices, ]
test_data <- past_data[-train_indices, ]

# Model tuning
task_data <- train_data %>%
  select(c_career_games, c_career_cmp, c_career_att, c_career_yds, c_career_td, c_career_int, last_games, last_passer_rating, AA, last_H_vote, won_H, last_sos, reg_QBR, yds_per_att, final_yds_per_att, c_rush_att, c_rush_yds, c_rush_td)
task_data$last_sos <- as.numeric(task_data$last_sos)
task <- makeRegrTask(data = task_data, target = "reg_QBR")

tuned_model <- tuneRanger(
  task,
  num.trees = 500,
  iters = 100,
  tune.parameters = c("mtry", "min.node.size"),
  parameters = list(replace = TRUE)
)

print(tuned_model)

# Model
rf_model <- ranger(reg_QBR ~ c_career_games + c_career_cmp + c_career_att + c_career_yds
                   + c_career_td + c_career_int + last_games + last_passer_rating + AA
                   + last_H_vote + won_H + last_sos + yds_per_att
                   + final_yds_per_att + c_rush_att + c_rush_yds + c_rush_td,
                   data = train_data,
                   num.trees = 500, importance = "impurity",
                   keep.inbag = TRUE, min.node.size = 50, mtry = 5)

test_terminal_nodes <- predict(rf_model, data = test_data, type = "terminalNodes")$predictions

test_data$predictions <- predict(rf_model, data = test_data)$predictions

rmse <- sqrt(mean((test_data$reg_QBR - test_data$predictions)^2))
total_variance <- var(test_data$reg_QBR)
residuals <- test_data$reg_QBR - test_data$predictions
residual_variance <- var(residuals)
explained_variance <- total_variance - residual_variance
print((explained_variance / total_variance) * 100)

# Model plots
importance_scores <- importance(rf_model)
importance(rf_model)
names(importance_scores) <- c("games/season", "completions/season", "attempts/season", "yards/season", "touchdowns/season", "interceptions/season", "final season games", "final season passer rating", "All-American seasons", "final season Heisman voting", "won Heisman Award", "final season strength of schedule", "yards/attempt", "final season yards/attempt", "rushing attempts/season", "rushing yards/season", "rushing touchdowns/season")
importance_df <- data.frame(
  Variable = names(importance_scores),
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
  mutate(predictions = predict(rf_model, data = past_data)$predictions)

{
  sputil::open_device("figures/3d_plot.pdf", height = 5)
  plot <- new_plot_data |>
    ggplot(aes(x = c_career_yds, y = last_passer_rating, color = predictions)) +
    geom_point(size = 1) +
    scale_color_viridis_c() +
    labs(
      x = "Passing Yards per Season (College)",
      y = "Final-Season Passer Rating (College)",
      color = "Predicted QBR"
    ) +
    coord_cartesian(xlim = c(0, 6000), ylim = c(0, 250)) +
    theme_minimal() +
    theme(legend.position = "inside", legend.position.inside = c(0.85, 0.25))
  print(plot)
  dev.off()
  }

{
  sputil::open_device("figures/predicted_vs_actuals.pdf", height = 5, width = 5)
  plot <- ggplot(new_plot_data, aes(x = predictions, y = reg_QBR)) +
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


# Re-fit random forest on full data set ----

full_rf_model <- ranger(
  formula = reg_QBR ~ c_career_games + c_career_cmp + c_career_att + c_career_yds + c_career_td +
    c_career_int + last_games + last_passer_rating + AA + last_H_vote + won_H + last_sos + yds_per_att +
    final_yds_per_att + c_rush_att + c_rush_yds + c_rush_td,
  data = past_data,
  num.trees = 500,
  importance = "impurity",
  keep.inbag = TRUE,
  min.node.size = 50,
  mtry = 5
)

present_data <- data %>%
  filter(last_season == 2024)

present_data$predictions <- round(predict(full_rf_model, data = present_data)$predictions, 3)

present_data %>%
  select(Player, predictions) %>%
  # Remove draft-ineligible players
  filter(!Player %in% c("Darian Mensah", "Cade Klubnik", "E.J. Warner")) %>%
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
colnames(similarity_df) <- past_data$Player  # Name columns by training players
rownames(similarity_df) <- present_data$Player  # Name rows by test players

ss_index <- which(present_data$Player == "Shedeur Sanders")

ss_similarity_scores <- similarity_matrix[ss_index, ]

ss_plot_data <- data.frame(
  player_name = past_data$Player,
  sim_score = ss_similarity_scores,
  QBR = past_data$reg_QBR
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

cw_index <- which(present_data$Player == "Cameron Ward")

cw_similarity_scores <- similarity_matrix[cw_index, ]

cw_plot_data <- data.frame(
  player_name = past_data$Player,
  sim_score = cw_similarity_scores,
  QBR = past_data$reg_QBR
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

jd_index <- which(present_data$Player == "Jaxson Dart")

jd_similarity_scores <- similarity_matrix[jd_index, ]

jd_plot_data <- data.frame(
  player_name = past_data$Player,
  sim_score = jd_similarity_scores,
  QBR = past_data$reg_QBR
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

dg_index <- which(present_data$Player == "Dillon Gabriel")

dg_similarity_scores <- similarity_matrix[dg_index, ]

dg_plot_data <- data.frame(
  player_name = past_data$Player,
  sim_score = dg_similarity_scores,
  QBR = past_data$reg_QBR
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

cw_index <- which(present_data$Player == "Cameron Ward")
cw_similarity_scores <- similarity_matrix[cw_index, ]
cw_similarity_scores <- setNames(cw_similarity_scores, past_data$Player)
sorted_similarity_scores <- sort(cw_similarity_scores, decreasing = TRUE)

cw_top_10 <- data.frame(
  `Player Name` = names(sorted_similarity_scores)[1:10],
  Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
  Base_Player = "Cameron Ward"
)

# Jaxson Dart
jd_index <- which(present_data$Player == "Jaxson Dart")
jd_similarity_scores <- similarity_matrix[jd_index, ]
jd_similarity_scores <- setNames(jd_similarity_scores, past_data$Player)
sorted_similarity_scores <- sort(jd_similarity_scores, decreasing = TRUE)

jd_top_10 <- data.frame(
  `Player Name` = names(sorted_similarity_scores)[1:10],
  Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
  Base_Player = "Jaxson Dart"
)

# Dillon Gabriel
dg_index <- which(present_data$Player == "Dillon Gabriel")
dg_similarity_scores <- similarity_matrix[dg_index, ]
dg_similarity_scores <- setNames(dg_similarity_scores, past_data$Player)
sorted_similarity_scores <- sort(dg_similarity_scores, decreasing = TRUE)

dg_top_10 <- data.frame(
  `Player Name` = names(sorted_similarity_scores)[1:10],
  Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
  Base_Player = "Dillon Gabriel"
)

# Shedeur Sanders
ss_index <- which(present_data$Player == "Shedeur Sanders")
ss_similarity_scores <- similarity_matrix[ss_index, ]
ss_similarity_scores <- setNames(ss_similarity_scores, past_data$Player)
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