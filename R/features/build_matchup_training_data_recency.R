build_matchup_training_data_recency <- function(tourney_results, team_season_features, seeds) {

  valid_seasons <- sort(unique(team_season_features$Season))

  matchup_base <- tourney_results %>%
    filter(Season %in% valid_seasons) %>%
    transmute(
      Season,
      Team1 = pmin(WTeamID, LTeamID),
      Team2 = pmax(WTeamID, LTeamID),
      Outcome = if_else(WTeamID < LTeamID, 1, 0)
    )

  team1_features <- team_season_features %>%
    rename_with(
      .fn = ~ paste0("Team1_", .x),
      .cols = -Season
    )

  team2_features <- team_season_features %>%
    rename_with(
      .fn = ~ paste0("Team2_", .x),
      .cols = -Season
    )

  seeds_clean <- seeds %>%
    filter(Season %in% valid_seasons) %>%
    mutate(
      SeedNum = readr::parse_number(Seed)
    ) %>%
    select(Season, TeamID, Seed, SeedNum)

  team1_seeds <- seeds_clean %>%
    rename(
      Team1 = TeamID,
      Team1_Seed = Seed,
      Team1_SeedNum = SeedNum
    )

  team2_seeds <- seeds_clean %>%
    rename(
      Team2 = TeamID,
      Team2_Seed = Seed,
      Team2_SeedNum = SeedNum
    )

  matchup_training_data_recency <- matchup_base %>%
    left_join(team1_features, by = c("Season", "Team1" = "Team1_TeamID")) %>%
    left_join(team2_features, by = c("Season", "Team2" = "Team2_TeamID")) %>%
    left_join(team1_seeds, by = c("Season", "Team1")) %>%
    left_join(team2_seeds, by = c("Season", "Team2")) %>%
    mutate(
      SeedDiff = Team1_SeedNum - Team2_SeedNum,

      RecencyOffEffDiff = Team1_RecencyOffEff - Team2_RecencyOffEff,
      RecencyDefEffDiff = Team1_RecencyDefEff - Team2_RecencyDefEff,
      RecencyNetEffDiff = Team1_RecencyNetEff - Team2_RecencyNetEff,
      RecencySOSDiff = Team1_RecencySOS - Team2_RecencySOS,
      RecencyAdjTempoDiff = Team1_RecencyAdjTempo - Team2_RecencyAdjTempo,
      TrendNetEffDiff = Team1_TrendNetEff - Team2_TrendNetEff
    ) %>%
    arrange(Season, Team1, Team2)

  return(matchup_training_data_recency)
}
