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
      PointDiff = WScore - LScore,
      Win = 1,
      Location = WLoc,
      NumOT,
      
      TeamFGA = WFGA,
      TeamOR = WOR,
      TeamTO = WTO,
      TeamFTA = WFTA,
      OppFGA = LFGA,
      OppOR = LOR,
      OppTO = LTO,
      OppFTA = LFTA)

  loser_rows <- reg_season_results %>%
    transmute(
      Season,
      DayNum,
      TeamID = LTeamID,
      OpponentID = WTeamID,
      TeamScore = LScore,
      OppScore = WScore,
      PointDiff = LScore - WScore,
      Win = 0,
      Location = case_when(
        WLoc == "H" ~ "A",
        WLoc == "A" ~ "H", TRUE ~ "N"),
      NumOT,
      
      TeamFGA = LFGA,
      TeamOR = LOR,
      TeamTO = LTO,
      TeamFTA = LFTA,
      OppFGA = WFGA,
      OppOR = WOR,
      OppTO = WTO,
      OppFTA = WFTA)

  bind_rows(winner_rows, loser_rows) %>%
    arrange(Season, DayNum, TeamID)
}
