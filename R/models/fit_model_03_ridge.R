library(tidyverse)

fit_model_03_ridge <- function(train_data_ridge) {

  model_data <- train_data_ridge %>%
    select(
      Outcome,
      SeedDiff,
      RidgeRatingDiff,
      OffEffDiff,
      DefEffDiff,
      AdjTempoDiff,
      SOSDiff) %>%
    drop_na()

  glm(
    Outcome ~ SeedDiff + RidgeRatingDiff + SOSDiff,
    data = model_data,
    family = binomial(link = "logit"))
}
