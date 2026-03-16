library(tidyverse)

build_matchup_training_data <- function(tourney_results, team_season_features, seeds) {

  # Standardize tournament games so Team1 always has the lower TeamID
  matchup_base <- tourney_results %>%
    transmute(
      Season,
      Team1 = pmin(WTeamID, LTeamID),
      Team2 = pmax(WTeamID, LTeamID),
      Outcome = if_else(WTeamID < LTeamID, 1, 0))

  # Team season features for Team1
  team1_features <- team_season_features %>%
    rename_with(
      .fn = ~ paste0("Team1_", .x),
      .cols = -Season)

  # Team season features for Team2
  team2_features <- team_season_features %>%
    rename_with(
      .fn = ~ paste0("Team2_", .x),
      .cols = -Season)

  # Clean numeric seed value
  seeds_clean <- seeds %>%
    mutate(
      SeedNum = readr::parse_number(Seed)) %>%
    select(Season, TeamID, Seed, SeedNum)

  team1_seeds <- seeds_clean %>%
    rename(
      Team1 = TeamID,
      Team1_Seed = Seed,
      Team1_SeedNum = SeedNum)

  team2_seeds <- seeds_clean %>%
    rename(
      Team2 = TeamID,
      Team2_Seed = Seed,
      Team2_SeedNum = SeedNum)

  matchup_training_data <- matchup_base %>%
    left_join(team1_features, by = c("Season", "Team1" = "Team1_TeamID")) %>%
    left_join(team2_features, by = c("Season", "Team2" = "Team2_TeamID")) %>%
    left_join(team1_seeds, by = c("Season", "Team1")) %>%
    left_join(team2_seeds, by = c("Season", "Team2")) %>%
    mutate(
      WinPctDiff = Team1_WinPct - Team2_WinPct,
      AvgPointDiffDiff = Team1_AvgPointDiff - Team2_AvgPointDiff,
      AvgTeamScoreDiff = Team1_AvgTeamScore - Team2_AvgTeamScore,
      AvgOppScoreDiff = Team1_AvgOppScore - Team2_AvgOppScore,
      SeedDiff = Team1_SeedNum - Team2_SeedNum) %>%
    arrange(Season, Team1, Team2)

  return(matchup_training_data)
}
