fit_model_02_efficiency <- function(train_data) {

  model_data <- train_data %>%
    select(
      Outcome,
      SeedDiff,
      OffEffDiff,
      DefEffDiff,
      AdjTempoMismatch,
      SOSDiff) %>%
    drop_na()

  glm(
    Outcome ~ SeedDiff + OffEffDiff + DefEffDiff + AdjTempoMismatch + SOSDiff,
    data = model_data,
    family = binomial(link = "logit")
  )
}
