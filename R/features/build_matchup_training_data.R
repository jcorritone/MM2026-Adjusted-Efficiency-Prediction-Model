library(tidyverse)

build_matchup_training_data <- function(tourney_results, team_season_features, seeds, team_strength_ratings_ridge = NULL) {

  valid_seasons <- sort(unique(team_season_features$Season))

  matchup_base <- tourney_results %>%
    filter(Season %in% valid_seasons) %>%
    
    transmute(
      Season,
      Team1 = pmin(WTeamID, LTeamID),
      Team2 = pmax(WTeamID, LTeamID),
      Outcome = if_else(WTeamID < LTeamID, 1, 0))

  team1_features <- team_season_features %>%
    rename_with(
      .fn = ~ paste0("Team1_", .x),
      .cols = -Season)

  team2_features <- team_season_features %>%
    rename_with(
      .fn = ~ paste0("Team2_", .x),
      .cols = -Season)

  seeds_clean <- seeds %>%
    filter(Season %in% valid_seasons) %>%
    
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
      SeedDiff = Team1_SeedNum - Team2_SeedNum,
      OffEffDiff = Team1_OffEff - Team2_OffEff,
      DefEffDiff = Team1_DefEff - Team2_DefEff,
      NetEffDiff = Team1_NetEff - Team2_NetEff,
      AdjTempoDiff = Team1_AdjTempo - Team2_AdjTempo,
      SOSDiff = Team1_SOS - Team2_SOS)
    
  if (!is.null(team_strength_ratings_ridge)) {

    team1_ridge <- team_strength_ratings_ridge %>%
      rename(
        Team1 = TeamID,
        Team1_RidgeRating = RidgeRating)

    team2_ridge <- team_strength_ratings_ridge %>%
      rename(
        Team2 = TeamID,
        Team2_RidgeRating = RidgeRating)

    matchup_training_data <- matchup_training_data %>%
      left_join(team1_ridge, by = c("Season", "Team1")) %>%
      left_join(team2_ridge, by = c("Season", "Team2")) %>%
      mutate(
        RidgeRatingDiff = Team1_RidgeRating - Team2_RidgeRating)
  }

  matchup_training_data <- matchup_training_data %>%
    arrange(Season, Team1, Team2)

  return(matchup_training_data)
}
