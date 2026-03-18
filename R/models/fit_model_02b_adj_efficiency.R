fit_model_02b_adj_efficiency <- function(train_data) {

  model_data <- train_data %>%
    select(
      Outcome,
      SeedDiff,
      AdjNetEffDiff,
      AdjSOSDiff,
      AdjTempoDiff
    ) %>%
    drop_na()

  glm(
    Outcome ~ SeedDiff + AdjNetEffDiff + AdjSOSDiff + AdjTempoDiff,
    data = model_data,
    family = binomial(link = "logit")
  )
}
