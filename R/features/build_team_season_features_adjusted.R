library(tidyverse)

compute_adjusted_efficiency_iterative <- function(team_games,
                                                  max_iter = 100,
                                                  tol = 1e-6,
                                                  damping = 0.5) {

  games <- team_games %>%
    mutate(
      TeamPoss = TeamFGA - TeamOR + TeamTO + 0.44 * TeamFTA,
      OppPoss = OppFGA - OppOR + OppTO + 0.44 * OppFTA,
      RawPoss = (TeamPoss + OppPoss) / 2,
      AdjPoss = RawPoss * (40 / (40 + 5 * NumOT)),
      GameOffEff = TeamScore / AdjPoss,
      GameDefEff = OppScore / AdjPoss
    )

  national_means <- games %>%
    group_by(Season) %>%
    summarise(
      NatOffEff = mean(GameOffEff, na.rm = TRUE),
      NatDefEff = mean(GameDefEff, na.rm = TRUE),
      .groups = "drop"
    )

  ratings <- games %>%
    group_by(Season, TeamID) %>%
    summarise(
      GamesPlayed = n(),
      RawOffEff = mean(GameOffEff, na.rm = TRUE),
      RawDefEff = mean(GameDefEff, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      RawNetEff = RawOffEff - RawDefEff,
      AdjOffEff = RawOffEff,
      AdjDefEff = RawDefEff
    )

  for (iter in seq_len(max_iter)) {

    old_ratings <- ratings %>%
      select(Season, TeamID, AdjOffEff, AdjDefEff)

    game_updates <- games %>%
      left_join(
        old_ratings %>%
          rename(
            OpponentID = TeamID,
            OppAdjOffEff = AdjOffEff,
            OppAdjDefEff = AdjDefEff
          ),
        by = c("Season", "OpponentID")
      ) %>%
      left_join(national_means, by = "Season")

    new_ratings <- game_updates %>%
      group_by(Season, TeamID) %>%
      summarise(
        NewAdjOffEff = mean(GameOffEff - OppAdjDefEff + NatDefEff, na.rm = TRUE),
        NewAdjDefEff = mean(GameDefEff - OppAdjOffEff + NatOffEff, na.rm = TRUE),
        .groups = "drop"
      )

    ratings_next <- ratings %>%
      left_join(new_ratings, by = c("Season", "TeamID")) %>%
      mutate(
        UpdatedAdjOffEff = damping * NewAdjOffEff + (1 - damping) * AdjOffEff,
        UpdatedAdjDefEff = damping * NewAdjDefEff + (1 - damping) * AdjDefEff
      )

    max_change <- ratings_next %>%
      summarise(
        MaxDelta = max(
          abs(UpdatedAdjOffEff - AdjOffEff),
          abs(UpdatedAdjDefEff - AdjDefEff),
          na.rm = TRUE
        )
      ) %>%
      pull(MaxDelta)

    ratings <- ratings_next %>%
      transmute(
        Season,
        TeamID,
        GamesPlayed,
        RawOffEff,
        RawDefEff,
        RawNetEff,
        AdjOffEff = UpdatedAdjOffEff,
        AdjDefEff = UpdatedAdjDefEff
      )

    if (max_change < tol) {
      break
    }
  }

  ratings %>%
    mutate(
      AdjNetEff = AdjOffEff - AdjDefEff
    )
}


build_team_season_features_adjusted <- function(team_games,
                                                max_iter = 100,
                                                tol = 1e-6,
                                                damping = 0.5) {

  library(dplyr)

  games <- team_games %>%
    mutate(
      TeamPoss = TeamFGA - TeamOR + TeamTO + 0.44 * TeamFTA,
      OppPoss = OppFGA - OppOR + OppTO + 0.44 * OppFTA,
      RawPoss = (TeamPoss + OppPoss) / 2,
      AdjPoss = RawPoss * (40 / (40 + 5 * NumOT)),
      GameOffEff = TeamScore / AdjPoss,
      GameDefEff = OppScore / AdjPoss
    )

  base_features <- games %>%
    group_by(Season, TeamID) %>%
    summarise(
      GamesPlayed = n(),
      Wins = sum(Win),
      WinPct = mean(Win),
      AvgTeamScore = mean(TeamScore, na.rm = TRUE),
      AvgOppScore = mean(OppScore, na.rm = TRUE),
      AvgPointDiff = mean(PointDiff, na.rm = TRUE),
      AdjTempo = mean(AdjPoss, na.rm = TRUE),
      .groups = "drop"
    )

  adjusted_ratings <- compute_adjusted_efficiency_iterative(
    team_games = team_games,
    max_iter = max_iter,
    tol = tol,
    damping = damping
  )

  adj_sos <- games %>%
    select(Season, TeamID, OpponentID) %>%
    left_join(
      adjusted_ratings %>%
        select(Season, TeamID, AdjOffEff, AdjDefEff, AdjNetEff) %>%
        rename(
          OpponentID = TeamID,
          OppAdjOffEff = AdjOffEff,
          OppAdjDefEff = AdjDefEff,
          OppAdjNetEff = AdjNetEff
        ),
      by = c("Season", "OpponentID")
    ) %>%
    group_by(Season, TeamID) %>%
    summarise(
      AdjSOS = mean(OppAdjNetEff, na.rm = TRUE),
      AdjSOS_Off = mean(OppAdjDefEff, na.rm = TRUE),
      AdjSOS_Def = mean(OppAdjOffEff, na.rm = TRUE),
      .groups = "drop"
    )

  base_features %>%
    left_join(adjusted_ratings, by = c("Season", "TeamID", "GamesPlayed")) %>%
    left_join(adj_sos, by = c("Season", "TeamID"))
}
