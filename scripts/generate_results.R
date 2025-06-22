library(stringr)
library(dplyr)
library(ranger)
library(tuneRanger)
library(mlr)
library(ggplot2)
library(xtable)

cfb_data <- read.csv("data/cfb_passing_stats_1984_2024.csv") 

sos_data <- read.csv("data/cfb_schedule_strength_2000_2024.csv")

cfb_rush <- read.csv("data/cfb_rushing_stats_2000_2024.csv") %>%
  mutate(Player = str_replace(Player, "\\*", "")) %>%
  select(Player, Season, Team, rush_att = Att, rush_yds = Yds, rush_td = TD)

cfb_data <- cfb_data %>%
  mutate(Player = str_replace(Player, "\\*", ""),
         all_american = ifelse(grepl("AA", Awards), 1, 0),
         won_heisman = ifelse(grepl("H-1", Awards) & !grepl("H-10", Awards), 1, 0),
         heisman_voting = case_when(
           grepl("H-10", Awards) ~ 10,
           grepl("H-1", Awards) ~ 1,
           grepl("H-2", Awards) ~ 2,
           grepl("H-3", Awards) ~ 3,
           grepl("H-4", Awards) ~ 4,
           grepl("H-5", Awards) ~ 5,
           grepl("H-6", Awards) ~ 6,
           grepl("H-7", Awards) ~ 7,
           grepl("H-8", Awards) ~ 8,
           grepl("H-9", Awards) ~ 9
         )) %>% 
  left_join(cfb_rush, by = c("Player", "Season", "Team")) %>%
  left_join(sos_data, by = c("Team" = "School", "Season")) %>%
  group_by(Player) %>%
  summarise(
    c_career_tot_games = sum(G),
    c_career_games = mean(G, na.rm = TRUE),
    c_career_cmp = mean(Cmp, na.rm = TRUE),
    c_career_att = mean(Att, na.rm = TRUE),
    c_career_yds = mean(Yds, na.rm = TRUE),
    c_career_td = mean(TD, na.rm = TRUE),
    c_career_int = mean(Int, na.rm = TRUE),
    c_rush_att = mean(rush_att, na.rm = TRUE),
    c_rush_yds = mean(rush_yds, na.rm = TRUE),
    c_rush_td = mean(rush_td, na.rm = TRUE),
    yds_per_att = sum(Yds) / sum(Att),
    final_yds_per_att = last(Yds) / last(Att),
    seasons = n(),
    last_season = last(Season),
    last_games = last(G),
    last_passer_rating = last(Rate),
    last_college = last(Team),
    last_conference = last(Conf),
    AA = sum(all_american),
    last_H_vote = last(heisman_voting),
    won_H = sum(won_heisman),
    sos = last(SOS)
  ) 

cfb_career_data <- cfb_data %>%
  filter(last_season < 2024,
         c_career_tot_games > 6,
         c_career_att > 5)

QBR_passing_data <- read.csv("data/QBR_stats_2006_2024.csv") %>%
  filter(Pos == "QB") %>%
  group_by(Player) %>%
  summarise(mean_QBR = mean(QBR),
            Att = sum(Att),
            seasons = n()) %>%
  mutate(
    mean_QBR = ifelse(is.na(mean_QBR), 0, mean_QBR)
  )

QBR_passing_data_combined <- cfb_career_data %>%
  left_join(QBR_passing_data, by = "Player") %>%
  mutate_all(~ replace(., is.na(.), 0)) %>%
  filter(last_season > 2000, Att == 0 | !(Att < 10)) 

set.seed(123)  # For reproducibility

train_indices <- sample(1:nrow(QBR_passing_data_combined), size = 0.7 * nrow(QBR_passing_data_combined))
train_data <- QBR_passing_data_combined[train_indices, ]
test_data <- QBR_passing_data_combined[-train_indices, ]

# Model tuning
task_data <- train_data %>%
  select(c_career_games, c_career_cmp, c_career_att, c_career_yds, c_career_td, c_career_int, last_games, last_passer_rating, AA, last_H_vote, won_H, sos, mean_QBR, yds_per_att, final_yds_per_att, c_rush_att, c_rush_yds, c_rush_td)
task_data$sos <- as.numeric(task_data$sos)
task <- makeRegrTask(data = task_data, target = "mean_QBR")


tuned_model <- tuneRanger(
  task,
  num.trees = 500, 
  iters = 100,
  tune.parameters = c("mtry", "min.node.size"),
  parameters = list(replace = TRUE))

print(tuned_model)

# Model
rf_model <- ranger(mean_QBR ~ c_career_games + c_career_cmp + c_career_att + c_career_yds 
                   + c_career_td + c_career_int + last_games + last_passer_rating + AA 
                   + last_H_vote + won_H + sos + yds_per_att
                   + final_yds_per_att + c_rush_att + c_rush_yds + c_rush_td,
                   data = train_data,  
                   num.trees = 500, importance = "impurity",
                   keep.inbag = TRUE, min.node.size = 67, mtry = 5)

test_terminal_nodes <- predict(rf_model, data = test_data, type = "terminalNodes")$predictions

test_data$predictions <- predict(rf_model, data = test_data)$predictions

rmse <- sqrt(mean((test_data$mean_QBR - test_data$predictions)^2))
total_variance <- var(test_data$mean_QBR)
residuals <- test_data$mean_QBR - test_data$predictions
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

new_plot_data <- QBR_passing_data_combined %>%
  mutate(predictions = predict(rf_model, data = QBR_passing_data_combined)$predictions)

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
  plot <- ggplot(new_plot_data, aes(x = predictions, y = mean_QBR)) +
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

full_rf_model <- ranger(mean_QBR ~ c_career_games + c_career_cmp + c_career_att + c_career_yds 
                        + c_career_td + c_career_int + last_games + last_passer_rating + AA 
                        + last_H_vote + won_H + sos + yds_per_att
                        + final_yds_per_att + c_rush_att + c_rush_yds + c_rush_td,
                        data = QBR_passing_data_combined,  
                        num.trees = 500, importance = "impurity",
                        keep.inbag = TRUE, min.node.size = 67, mtry = 5)

cfb_career_data_current <- cfb_data %>%
  filter(last_season == 2024) %>%
  mutate_all(~ replace(., is.na(.), 0))

cfb_career_data_current$predictions <- round(predict(full_rf_model, data = cfb_career_data_current)$predictions, 3)

cfb_career_data_current %>%
  select(Player, predictions) %>%
  # Remove draft-ineligible players
  filter(!Player %in% c("Darian Mensah", "Cade Klubnik", "E.J. Warner")) %>%
  arrange(-predictions) %>%
  head(10) %>%
  mutate(predictions = sprintf("%.1f", predictions)) %>%
  sputil::write_latex_table(file = "tables/top_ten.tex")


test_names <- cfb_career_data_current$Player
test_terminal_nodes <- predict(full_rf_model, data = cfb_career_data_current, type = "terminalNodes")$predictions
rownames(test_terminal_nodes) <- test_names
train_terminal_nodes <- predict(full_rf_model, data = QBR_passing_data_combined, type = "terminalNodes")$predictions
rownames(train_terminal_nodes) <- QBR_passing_data_combined$Player
inbag_counts <- full_rf_model$inbag.counts
inbag_counts <- do.call(cbind, inbag_counts)

M <- ncol(train_terminal_nodes)  # Number of trees
n_test <- nrow(cfb_career_data_current)  # Number of test samples
n_train <- nrow(QBR_passing_data_combined)  # Number of training samples

# Initialize similarity matrix (rows = test players, cols = training players)
similarity_matrix <- matrix(0, nrow = n_test, ncol = n_train)

# Loop over all trees
for (m in 1:M) {
  
  train_nodes_m <- train_terminal_nodes[, m]  # Terminal nodes for training samples in tree m
  test_nodes_m <- test_terminal_nodes[, m]  # Terminal nodes for test samples in tree m
  
  # Loop over each test sample
  for (t in 1:n_test) {
    
    # Find training samples in the same terminal node as the test sample
    matching_train_samples <- train_nodes_m == test_nodes_m[t]
    
    if (sum(matching_train_samples) > 0) {
      # Extract inbag counts for matching training samples in the current tree
      inbag_weights <- inbag_counts[matching_train_samples, m]
      
      # Normalize weights so they sum to 1 for each tree
      inbag_weights <- inbag_weights / sum(inbag_weights)
      
      # Update similarity matrix (sum similarity scores over all trees)
      similarity_matrix[t, matching_train_samples] <- similarity_matrix[t, matching_train_samples] + inbag_weights
    }
  }
}

# Normalize similarity scores across trees (final similarity score per test/training pair)
similarity_matrix <- similarity_matrix / M

similarity_df <- as.data.frame(similarity_matrix)
colnames(similarity_df) <- QBR_passing_data_combined$Player  # Name columns by training players
rownames(similarity_df) <- cfb_career_data_current$Player  # Name rows by test players

ss_index <- which(cfb_career_data_current$Player == "Shedeur Sanders")

ss_similarity_scores <- similarity_matrix[ss_index, ]

ss_plot_data <- data.frame(
  player_name = QBR_passing_data_combined$Player,
  sim_score = ss_similarity_scores,
  QBR = QBR_passing_data_combined$mean_QBR
) %>%
  filter(sim_score > 0)

{
  sputil::open_device("figures/prospect_histogram_sanders.pdf", height = 3, width = 3)
  plot <- ggplot(ss_plot_data, aes(x = QBR, y = ..density.., weight = sim_score)) +
    geom_histogram(binwidth = 5, color = "blue", fill = "blue", alpha = 0.8) +
    theme_minimal() +
    labs(x = "QBR", y = "Weights",
         title = "Shedeur Sanders") +
    geom_vline(aes(xintercept = cfb_career_data_current[ss_index, ]$predictions), 
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

cw_index <- which(cfb_career_data_current$Player == "Cameron Ward")

cw_similarity_scores <- similarity_matrix[cw_index, ]

cw_plot_data <- data.frame(
  player_name = QBR_passing_data_combined$Player,
  sim_score = cw_similarity_scores,
  QBR = QBR_passing_data_combined$mean_QBR
) %>%
  filter(sim_score > 0)

{
  sputil::open_device("figures/prospect_histogram_ward.pdf", height = 3, width = 3)
  plot <- ggplot(cw_plot_data, aes(x = QBR, y = ..density.., weight = sim_score)) +
    geom_histogram(binwidth = 5, color = "blue", fill = "blue", alpha = 0.8) +
    theme_minimal() +
    labs(x = "QBR", y = "Weights", title = "Cam Ward") +
    geom_vline(aes(xintercept = cfb_career_data_current[cw_index, ]$predictions), 
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

jd_index <- which(cfb_career_data_current$Player == "Jaxson Dart")

jd_similarity_scores <- similarity_matrix[jd_index, ]

jd_plot_data <- data.frame(
  player_name = QBR_passing_data_combined$Player,
  sim_score = jd_similarity_scores,
  QBR = QBR_passing_data_combined$mean_QBR
) %>%
  filter(sim_score > 0)

{
  sputil::open_device("figures/prospect_histogram_dart.pdf", height = 3, width = 3)
  plot <- ggplot(jd_plot_data, aes(x = QBR, y = ..density.., weight = sim_score)) +
    geom_histogram(binwidth = 5, color = "blue", fill = "blue", alpha = 0.8) +
    theme_minimal() +
    labs(x = "QBR", y = "Weights", title = "Jaxson Dart") +
    geom_vline(aes(xintercept = cfb_career_data_current[jd_index, ]$predictions),
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

dg_index <- which(cfb_career_data_current$Player == "Dillon Gabriel")

dg_similarity_scores <- similarity_matrix[dg_index, ]

dg_plot_data <- data.frame(
  player_name = QBR_passing_data_combined$Player,
  sim_score = dg_similarity_scores,
  QBR = QBR_passing_data_combined$mean_QBR
) %>%
  filter(sim_score > 0)

{
  sputil::open_device("figures/prospect_histogram_gabriel.pdf", height = 3, width = 3)
  plot <- ggplot(dg_plot_data, aes(x = QBR, y = ..density.., weight = sim_score)) +
    geom_histogram(binwidth = 5, color = "blue", fill = "blue", alpha = 0.8) +
    theme_minimal() +
    labs(x = "QBR", y = "Weights", title = "Dillon Gabriel") +
    geom_vline(aes(xintercept = cfb_career_data_current[dg_index, ]$predictions),
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

cw_index <- which(cfb_career_data_current$Player == "Cameron Ward")
cw_similarity_scores <- similarity_matrix[cw_index, ]
cw_similarity_scores <- setNames(cw_similarity_scores, QBR_passing_data_combined$Player)
sorted_similarity_scores <- sort(cw_similarity_scores, decreasing = TRUE)

cw_top_10 <- data.frame(
  `Player Name` = names(sorted_similarity_scores)[1:10],
  Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
  Base_Player = "Cameron Ward"
)

# Jaxson Dart
jd_index <- which(cfb_career_data_current$Player == "Jaxson Dart")
jd_similarity_scores <- similarity_matrix[jd_index, ]
jd_similarity_scores <- setNames(jd_similarity_scores, QBR_passing_data_combined$Player)
sorted_similarity_scores <- sort(jd_similarity_scores, decreasing = TRUE)

jd_top_10 <- data.frame(
  `Player Name` = names(sorted_similarity_scores)[1:10],
  Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
  Base_Player = "Jaxson Dart"
)

# Dillon Gabriel
dg_index <- which(cfb_career_data_current$Player == "Dillon Gabriel")
dg_similarity_scores <- similarity_matrix[dg_index, ]
dg_similarity_scores <- setNames(dg_similarity_scores, QBR_passing_data_combined$Player)
sorted_similarity_scores <- sort(dg_similarity_scores, decreasing = TRUE)

dg_top_10 <- data.frame(
  `Player Name` = names(sorted_similarity_scores)[1:10],
  Similarity = paste0(sprintf("%.1f", 100 * sorted_similarity_scores[1:10]), "\\%"),
  Base_Player = "Dillon Gabriel"
)

# Shedeur Sanders
ss_index <- which(cfb_career_data_current$Player == "Shedeur Sanders")
ss_similarity_scores <- similarity_matrix[ss_index, ]
ss_similarity_scores <- setNames(ss_similarity_scores, QBR_passing_data_combined$Player)
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
