library(tidyverse)

build_team_season_features <- function(team_games) {

  team_games <- team_games %>%
    mutate(
      TeamPossessions = TeamFGA - TeamOR + TeamTO + 0.44 * TeamFTA,
      OppPossessions = OppFGA - OppOR + OppTO + 0.44 * OppFTA,
      TeamAdjPossessions = TeamPossessions * (40 / (40 + 5 * NumOT)),
      OppAdjPossessions = OppPossessions * (40 / (40 + 5 * NumOT)),
      OffEff = TeamScore / TeamPossessions,
      DefEff = OppScore / OppPossessions,
      NetEff = OffEff - DefEff)
  
    team_season_features <- team_games %>%
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
      .groups = "drop")

    sos <- team_games %>%
    select(Season, TeamID, OpponentID) %>%
    
    left_join(
      team_season_features %>%
        select(Season, TeamID, OppNetEff = NetEff),
      by = c("Season", "OpponentID" = "TeamID")) %>%
    
    group_by(Season, TeamID) %>%
    
    summarise(
      SOS = mean(OppNetEff, na.rm = TRUE),
      .groups = "drop")

  team_season_features <- team_season_features %>%
    left_join(sos, by = c("Season", "TeamID"))

  return(team_season_features)
}
