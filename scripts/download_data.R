library(rvest)
library(dplyr)

# Construct college passing stats dataframe from Sports Reference
passing_data <- data.frame()

for (year in 1984:2024) {
  url <- paste0("https://www.sports-reference.com/cfb/years/", 
                year, "-passing.html")
  
  page <- read_html(url)
  
  data <- page %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE) 
  
  data$Season <- year
  
  passing_data <- bind_rows(passing_data, data)
  
  Sys.sleep(runif(6, 8, 10))
}

head(passing_data)
write.csv(passing_data, "data/cfb_passing_stats_1984_2024.csv", row.names = TRUE)

# Construct college rushing dataframe
rushing_data <- data.frame()

for (year in 2000:2024) {
  url <- paste0("https://www.sports-reference.com/cfb/years/", 
                year, "-rushing.html")
  
  page <- read_html(url)
  
  raw_data <- page %>%
    html_node("table") %>% 
    html_table(header = FALSE, fill = TRUE) 
  
  colnames(raw_data) <- raw_data[2, ] 
  data <- raw_data[-c(1, 2), c(1:10)]
  
  data$Season <- year
  
  rushing_data <- bind_rows(rushing_data, data)
  
  Sys.sleep(runif(6, 8, 10))
  
  print(year)
}

head(rushing_data)
write.csv(rushing_data, "data/cfb_rushing_stats_2000_2024.csv", row.names = TRUE)

# Construct NFL QBR dataframe
QBR_passing_data <- data.frame()

for (year in 2006:2024) {
  url <- paste0("https://www.pro-football-reference.com/years/", 
                year, "/passing.htm")
  
  page <- read_html(url)
  
  data <- page %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE) 
  
  data$Season <- year
  
  QBR_passing_data <- bind_rows(QBR_passing_data, data)
  
  Sys.sleep(runif(6, 8, 10))
}

head(QBR_passing_data)
write.csv(QBR_passing_data, "data/QBR_stats_2006_2024.csv", row.names = TRUE)

schedule_strength <- data.frame()

for (year in 2000:2024) {
  url <- paste0("https://www.sports-reference.com/cfb/years/", 
                year, "-standings.html")
  
  page <- read_html(url)
  
  raw_data <- page %>%
    html_node("table") %>% 
    html_table(header = FALSE, fill = TRUE) 
  
  colnames(raw_data) <- raw_data[2, ] 
  data <- raw_data[-c(1, 2), c(2, 13)]
  
  data$Season <- year
  
  schedule_strength <- bind_rows(schedule_strength, data)
  
  print(year)
  
  Sys.sleep(runif(6, 8, 10))
}

head(schedule_strength)
write.csv(schedule_strength, "cfb_schedule_strength_2000_2024.csv", row.names = TRUE)
