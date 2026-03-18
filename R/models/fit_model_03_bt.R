library(tidyverse)
library(BradleyTerry2)

fit_model_03_bt <- function(train_data_bt) {

  model_data <- train_data_bt %>%
    select(
      Outcome,
      SeedDiff,
      BTRatingDiff,
      OffEffDiff,
      DefEffDiff,
      AvgPointDiffDiff) %>%
    drop_na()

  glm(
    Outcome ~ SeedDiff + BTRatingDiff + AvgPointDiffDiff,
    data = model_data,
    family = binomial(link = "logit"))
}
