library(tidyverse)

build_matchup_training_data_bt <- function(matchup_training_data, team_bt_ratings) {

  team1_bt <- team_bt_ratings %>%
    rename(
      Team1 = TeamID,
      Team1_BTRating = BTRating)

  team2_bt <- team_bt_ratings %>%
    rename(
      Team2 = TeamID,
      Team2_BTRating = BTRating)

  matchup_training_data_bt <- matchup_training_data %>%
    left_join(team1_bt, by = c("Season", "Team1")) %>%
    left_join(team2_bt, by = c("Season", "Team2")) %>%
    mutate(
      BTRatingDiff = Team1_BTRating - Team2_BTRating)

  return(matchup_training_data_bt)
}
