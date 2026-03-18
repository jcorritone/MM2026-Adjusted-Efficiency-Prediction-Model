library(tidyverse)

rolling_brier_eval <- function(data, fit_function, test_seasons) {

  rolling_results <- purrr::map_dfr(test_seasons, function(test_season) {

    train_data <- data %>%
      filter(Season < test_season)

    test_data <- data %>%
      filter(Season == test_season)

    fit <- fit_function(train_data)

    test_predictions <- test_data %>%
      mutate(
        PredProb = predict(
          fit,
          newdata = .,
          type = "response"))

    tibble(
      TestSeason = test_season,
      TrainSeasons = paste0(min(train_data$Season), "-", max(train_data$Season)),
      NGames = nrow(test_data),
      Brier = mean((test_predictions$PredProb - test_predictions$Outcome)^2))
  })

  return(rolling_results)
}
