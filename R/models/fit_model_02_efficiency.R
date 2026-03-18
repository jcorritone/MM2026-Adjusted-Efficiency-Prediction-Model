fit_model_02_efficiency <- function(train_data) {

  model_data <- train_data %>%
    select(
      Outcome,
      SeedDiff,
      OffEffDiff,
      DefEffDiff,
      NetEffDiff,
      AdjTempoDiff,
      SOSDiff,
      WinPctDiff) %>%
    drop_na()

  glm(
    Outcome ~ SeedDiff + NetEffDiff + SOSDiff + AdjTempoDiff,
    data = model_data,
    family = binomial(link = "logit"))
}
