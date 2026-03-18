library(tidyverse)
library(BradleyTerry2)

build_team_strength_ratings_bt <- function(reg_season_results) {

  bt_ratings <- reg_season_results %>%
    group_split(Season) %>%

    map_dfr(function(season_df) {

      season <- unique(season_df$Season)

      teams <- sort(unique(c(season_df$WTeamID, season_df$LTeamID)))

      team_lookup <- tibble(
        TeamID = teams,
        team = factor(teams, levels = teams))

      bt_games <- season_df %>%
        transmute(
          winner = WTeamID,
          loser = LTeamID,
          home_advantage = case_when(
            WLoc == "H" ~ 1,
            WLoc == "A" ~ -1,
            TRUE ~ 0),
          result = 1) %>%
      
        left_join(
          team_lookup %>%
            rename(winner = TeamID, winner_team = team),
          by = "winner") %>%
      
        left_join(
          team_lookup %>%
            rename(loser = TeamID, loser_team = team),
          by = "loser")

      fit <- BradleyTerry2::BTm(
        cbind(result, 1 - result),
        winner_team,
        loser_team,
        ~ home_advantage,
        data = bt_games)

      ability_df <- BradleyTerry2::BTabilities(fit) %>%
        as.data.frame() %>%
      
        rownames_to_column("TeamID") %>%
      
        as_tibble() %>%
      
        transmute(
          Season = season,
          TeamID = as.integer(TeamID),
          BTRating = ability) %>%
      
        mutate(
          BTRating = BTRating - mean(BTRating, na.rm = TRUE))

      ability_df
    })

  return(bt_ratings)
}
