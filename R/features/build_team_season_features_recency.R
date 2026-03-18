build_team_season_features_recency <- function(team_games) {

  team_games <- team_games %>%
    group_by(Season) %>%
    mutate(
      RecencyWeight = ((DayNum - min(DayNum)) / (max(DayNum) - min(DayNum))) + 0.25
    ) %>%
    ungroup() %>%
    mutate(
      TeamPossessions = TeamFGA - TeamOR + TeamTO + 0.44 * TeamFTA,
      OppPossessions = OppFGA - OppOR + OppTO + 0.44 * OppFTA,
      TeamAdjPossessions = TeamPossessions * (40 / (40 + 5 * NumOT)),
      OppAdjPossessions = OppPossessions * (40 / (40 + 5 * NumOT)),
      OffEff = TeamScore / TeamPossessions,
      DefEff = OppScore / OppPossessions,
      NetEff = OffEff - DefEff
    )

  weighted_mean <- function(x, w) {
    sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])
  }

  team_season_features_recency <- team_games %>%
    group_by(Season, TeamID) %>%
    summarise(
      GamesPlayed = n(),
      Wins = sum(Win),
      WinPct = mean(Win),
      AvgTeamScore = mean(TeamScore),
      AvgOppScore = mean(OppScore),
      AvgPointDiff = mean(PointDiff),
      AdjTempo = mean((TeamAdjPossessions + OppAdjPossessions) / 2),
      OffEff = mean(OffEff),
      DefEff = mean(DefEff),
      NetEff = mean(NetEff),

      RecencyWinPct = weighted_mean(Win, RecencyWeight),
      RecencyAvgPointDiff = weighted_mean(PointDiff, RecencyWeight),
      RecencyAdjTempo = weighted_mean((TeamAdjPossessions + OppAdjPossessions) / 2, RecencyWeight),
      RecencyOffEff = weighted_mean(OffEff, RecencyWeight),
      RecencyDefEff = weighted_mean(DefEff, RecencyWeight),
      RecencyNetEff = weighted_mean(NetEff, RecencyWeight),

      .groups = "drop"
    )

  recency_sos <- team_games %>%
    select(Season, TeamID, OpponentID, RecencyWeight) %>%
    left_join(
      team_season_features_recency %>%
        select(Season, TeamID, OppRecencyNetEff = RecencyNetEff),
      by = c("Season", "OpponentID" = "TeamID")
    ) %>%
    group_by(Season, TeamID) %>%
    summarise(
      RecencySOS = weighted_mean(OppRecencyNetEff, RecencyWeight),
      .groups = "drop"
    )

  team_season_features_recency <- team_season_features_recency %>%
    left_join(recency_sos, by = c("Season", "TeamID")) %>%
    mutate(
      TrendNetEff = RecencyNetEff - NetEff
    )

  return(team_season_features_recency)
}
