library(tidyverse)

model_01_logistic <- function(matchup_training_data) {

  model_data <- matchup_training_data %>%
    select(
      Outcome,
      SeedDiff,
      WinPctDiff,
      AvgPointDiffDiff) %>%
    drop_na()

  fit <- glm(
    Outcome ~ SeedDiff + WinPctDiff + AvgPointDiffDiff,
    data = model_data,
    family = binomial(link = "logit"))

  return(fit)
}
