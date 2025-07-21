verbose <- TRUE

library(rvest)
library(dplyr)


# Download college passing data ----
if (verbose) {
  logger::log_info("Downloading college passing data (1997-2024)")
}

passing_data <- tibble::tibble()

for (year in 1997:2024) {
  if (verbose) {
    cat(year, "")
  }

  url <- paste0("https://www.sports-reference.com/cfb/years/", year, "-passing.html")
  
  page <- read_html(url)
  
  raw_data <- page %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE)
  
  year_data <- raw_data %>%
    filter(Player != "League Average") %>%
    mutate(
      Player = stringr::str_replace(Player, "\\*", ""),
      Season = year,
    ) %>%
    select(Player, Season, Team, Conf, G, Cmp, Att, Yds, TD, Int, Rate, Awards)
  
  passing_data <- bind_rows(passing_data, year_data)
  
  Sys.sleep(runif(1, 8, 10))
}
if (verbose) {
  cat("\n")
}



# Download college rushing data ----
if (verbose) {
  logger::log_info("Downloading college rushing data (2000-2024)")
}

rushing_data <- tibble::tibble()

for (year in 2000:2024) {
  if (verbose) {
    cat(year, "")
  }

  url <- paste0("https://www.sports-reference.com/cfb/years/", year, "-rushing.html")
  
  page <- read_html(url)
  
  raw_data <- page %>%
    html_node("table") %>%
    html_table(header = FALSE, fill = TRUE)
  
  colnames(raw_data) <- raw_data[2, ]
  year_data <- raw_data[-c(1, 2), c(1:10)] %>%
    filter(Player != "League Average") %>%
    mutate(
      Season = year,
      Player = stringr::str_replace(Player, "\\*", "")
    ) %>%
    group_by(Player, Season, Team) %>%
    slice(1) %>%    # it's possible for a player-season to be duplicated, an apparent S-R bug
    ungroup() %>%
    mutate(rush_att = as.integer(Att), rush_yds = as.integer(Yds), rush_td = as.integer(TD)) %>%
    select(Player, Season, Team, rush_att, rush_yds, rush_td)
  
  rushing_data <- bind_rows(rushing_data, year_data)
  
  Sys.sleep(runif(1, 8, 10))
}
if (verbose) {
  cat("\n")
}



# Download college strength of schedule data ----
if (verbose) {
  logger::log_info("Downloading college stregth of schedule data (2000-2024)")
}

schedule_strength <- tibble::tibble()

for (year in 2000:2024) {
  if (verbose) {
    cat(year, "")
  }

  url <- paste0("https://www.sports-reference.com/cfb/years/", year, "-standings.html")
  
  page <- read_html(url)
  
  raw_data <- page %>%
    html_node("table") %>%
    html_table(header = FALSE, fill = TRUE)
  
  colnames(raw_data) <- raw_data[2, ]
  year_data <- raw_data[-c(1, 2), c(2, 13)] %>%
   filter(!School %in% c("", "School")) %>%   # remove header rows
   mutate(Team = School, Season = year) %>%
   select(Season, Team, SOS)
  
  schedule_strength <- bind_rows(schedule_strength, year_data)
  
  Sys.sleep(runif(1, 8, 10))
}
if (verbose) {
  cat("\n")
}



# Download NFL QBR data ----
if (verbose) {
  logger::log_info("Downloading NFL QBR data (2006-2024)")
}

NFL_QBR_by_year <- tibble::tibble()

for (year in 2006:2024) {
  if (verbose) {
    cat(year, "")
  }

  url <- paste0("https://www.pro-football-reference.com/years/", year, "/passing.htm")
  
  page <- read_html(url)
  
  raw_data <- page %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE)

  year_data <- raw_data %>%
    select(Player, Pos, QBR, Att) %>%
    filter(Pos == "QB", !is.na(QBR)) %>%
    # If a QB plays for multiple teams, they get a row for each team as well as a "total" row.
    # Below, we select the row with the most attempts for each player, which should be the "total".
    group_by(Player) %>%
    arrange(-Att) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(Season = year) %>%
    select(Player, Season, Att, QBR)
  
  NFL_QBR_by_year <- bind_rows(NFL_QBR_by_year, year_data)
  
  Sys.sleep(runif(1, 8, 10))
}
if (verbose) {
  cat("\n")
}



# Join all the data together ----

NFL_QBR_career <- NFL_QBR_by_year %>%
  group_by(Player) %>%
  summarize(
    nfl_seasons = n(),
    Career_Att = sum(Att),
    Avg_Att = mean(Att),
    mean_QBR = mean(QBR)
  )

quarterback <- passing_data %>%
  left_join(rushing_data, by = c("Player", "Season", "Team")) %>%
  left_join(schedule_strength, by = c("Team", "Season")) %>%
  mutate(
    # If you don't appear in the rushing data, it's because you had zero rushing attempts
    across(contains("_rush_"), \(x) coalesce(x, 0)),
    all_american = ifelse(grepl("AA", Awards), 1, 0),
    won_heisman = ifelse(grepl("H-1", Awards) & !grepl("H-10", Awards), 1, 0),
    heisman_voting = case_when(
      grepl("H-1", Awards) ~ 1,
      grepl("H-2", Awards) ~ 2,
      grepl("H-3", Awards) ~ 3,
      grepl("H-4", Awards) ~ 4,
      grepl("H-5", Awards) ~ 5,
      grepl("H-6", Awards) ~ 6,
      grepl("H-7", Awards) ~ 7,
      grepl("H-8", Awards) ~ 8,
      grepl("H-9", Awards) ~ 9,
      grepl("H-10", Awards) ~ 10
    )
  ) %>%
  group_by(Player) %>%
  summarize(
    ncaa_seasons = n(),
    c_career_tot_games = sum(G),
    c_career_games = mean(G, na.rm = TRUE),
    c_career_att = mean(Att, na.rm = TRUE),
    c_career_cmp = mean(Cmp, na.rm = TRUE),
    c_career_yds = mean(Yds, na.rm = TRUE),
    c_career_td = mean(TD, na.rm = TRUE),
    c_career_int = mean(Int, na.rm = TRUE),
    c_rush_att = mean(rush_att, na.rm = TRUE),
    c_rush_yds = mean(rush_yds, na.rm = TRUE),
    c_rush_td = mean(rush_td, na.rm = TRUE),
    yds_per_att = sum(Yds) / sum(Att),
    final_yds_per_att = last(Yds) / last(Att),
    last_season = last(Season),
    last_conference = last(Conf),
    last_college = last(Team),
    last_sos = last(SOS),
    last_games = last(G),
    last_passer_rating = last(Rate),
    AA = sum(all_american),
    won_H = sum(won_heisman),
    last_H_vote = last(heisman_voting)
  ) %>%
  left_join(NFL_QBR_career, by = "Player") %>%
  filter(last_season > 2000)


# Write data to file ----

save(quarterback, file = "data/quarterback.rda")
save(NFL_QBR_by_year, file = "data/NFL_QBR_by_year.rda")
