library(tidyverse)

build_team_games <- function(reg_season_results) {

  winner_rows <- reg_season_results %>%
    transmute(
      Season,
      DayNum,
      TeamID = WTeamID,
      OpponentID = LTeamID,
      TeamScore = WScore,
      OppScore = LScore,
      Win = 1,
      Location = WLoc,
      NumOT)

  loser_rows <- reg_season_results %>%
    transmute(
      Season,
      DayNum,
      TeamID = LTeamID,
      OpponentID = WTeamID,
      TeamScore = LScore,
      OppScore = WScore,
      Win = 0,
      Location = case_when(
        WLoc == "H" ~ "A",
        WLoc == "A" ~ "H", TRUE ~ "N"),
      NumOT)

  bind_rows(winner_rows, loser_rows) %>%
    arrange(Season, DayNum, TeamID)
}
