library(tidyverse)
library(here)

load_data <- function() {

  teams <- read_csv(
    here("data", "raw", "MTeams.csv"),
    show_col_types = FALSE)

  seasons <- read_csv(
    here("data", "raw", "MSeasons.csv"),
    show_col_types = FALSE)

  reg_season_results <- read_csv(
    here("data", "raw", "MRegularSeasonDetailedResults.csv"),
    show_col_types = FALSE)

  tourney_results <- read_csv(
    here("data", "raw", "MNCAATourneyCompactResults.csv"),
    show_col_types = FALSE)

  seeds <- read_csv(
    here("data", "raw", "MNCAATourneySeeds.csv"),
    show_col_types = FALSE)

  list(
    teams = teams,
    seasons = seasons,
    reg_season_results = reg_season_results,
    tourney_results = tourney_results,
    seeds = seeds)
}
