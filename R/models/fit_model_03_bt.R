library(tidyverse)
library(BradleyTerry2)

fit_model_03_bt <- function(train_data_bt) {

  model_data <- train_data_bt %>%
    select(
      Outcome,
      SeedDiff,
      BTRatingDiff) %>%
    drop_na()

  glm(
    Outcome ~ SeedDiff + BTRatingDiff,
    data = model_data,
    family = binomial(link = "logit"))
}
