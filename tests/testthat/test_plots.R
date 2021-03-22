# test the plot outputs

library(mlr3)
library(mlr3learners)
library(ranger)

aq <- na.omit(airquality)

test_that("Test heatmap and network",{
  aq_Task <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
  aq_lrn <- lrn("regr.ranger", importance = "permutation")
  aq_fit <- aq_lrn$train(aq_Task)

  # Default values
  m <- vivi(fit = aq_fit, data = aq, response = "Ozone")

  # test heatmap
  vi <- vivi(fit = aq_fit, data = aq, response = "Ozone", gridSize = 10)
  viH <- viviHeatmap(vi)
  viN <- viviNetwork(vi)

  expect_type(viH, "list")
  expect_type(viN, "list")


})
