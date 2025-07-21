#' Historical Quarterback Prospects
#' 
#' College careeer statistics and career NFL performance (if applicable) for historical college
#' quarterback prospects from 2001 through 2024. Only NFL performance through 2024 is included.
#' 
#' @format ## `quarterback`
#' A data frame with 5,701 rows and 27 columns:
#' \describe{
#'   \item{Player}{Player name}
#'   \item{ncaa_seasons}{Number of seasons played in college career}
#'   \item{c_career_tot_games}{Total number of games in college career}
#'   \item{c_career_games}{Games per year in college career}
#'   \item{c_career_att}{Passing attempts per year in college career}
#'   \item{c_career_cmp}{Passing completions per year in college career}
#'   \item{c_career_yds}{Passing yards completions per year in college career}
#'   \item{c_career_td}{Passing touchdowns per year in college career}
#'   \item{c_career_int}{Passing interceptions per year in college career}
#'   \item{c_rush_att}{Rushing attempts per year in college career}
#'   \item{c_rush_yds}{Rusing yards per year in college career}
#'   \item{c_rush_td}{Rushing touchdowns per year in college career}
#'   \item{yds_per_att}{Passing yards per attempt in college career}
#'   \item{final_yds_per_att}{Passing yards per attempt in final college season}
#'   \item{last_season}{Year of final college season}
#'   \item{last_conference}{Conference for final college season}
#'   \item{last_college}{Team for final college season}
#'   \item{last_sos}{Strength of schedule for final college season}
#'   \item{last_games}{Games played in final college season}
#'   \item{last_passer_rating}{Passer rating in final college season}
#'   \item{AA}{Numer of All-America recognitions in college career}
#'   \item{won_H}{Number of Heisman awards in college career}
#'   \item{last_H_vote}{Heisman voting finish in final college season}
#'   \item{nfl_seasons}{Number of seasons played in NFL career}
#'   \item{Career_Att}{Total passing attempts in NFL career}
#'   \item{Avg_Att}{Passing attempts per year in NFL career}
#'   \item{mean_QBR}{Average QBR (across years) in NFL career}
#' }
#' 
#' @source <https://www.sports-reference.com/cfb/years/2024-passing.html>
"quarterback"

#' NFL QBR by Year
#' 
#' NFL quarterback performance by year according to ESPN's Total Quarterback Rating, 2006-2024.
#' 
#' @format ## `NFL_QBR_by_year`
#' A data frame with 1,386 rows and 4 columns:
#' \describe{
#'   \item{Player}{Player name}
#'   \item{Season}{Year}
#'   \item{Att}{Total attempts}
#'   \item{QBR}{Total Quarterback Rating}
#' }
#' 
#' @source <https://www.pro-football-reference.com/years/2024/passing.htm>
"NFL_QBR_by_year"