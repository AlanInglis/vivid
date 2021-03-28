# Test the matrix

library(ranger)
library(vip)
library(dplyr)



test_that("vip2vivid test class", {
  aq <- na.omit(airquality)

  # create fit
  fr <- ranger(Ozone ~ ., data = aq, importance = "permutation")

  # get vip importance and interactions
  #vipImp <- vi(fr, method = "model")
  #vipInt <- vint(fr, feature_names = names(aq[-1]))

  Variable <- c("Temp", "Wind", "Solar.R", "Month", "Day")
  Importance <- c(10,9,8,7,6)
  vImp <- tibble(Variable, Importance)

  Variables <- c("Wind*Temp", "Solar.R*Temp", "Solar.R*Wind", "Temp*Day", "Temp*Month",
                 "Solar.R*Month", "Wind*Month", "Wind*Day", "Month*Day", "Solar.R*Day")
  Interaction <- c(10,9,8,7,6,5,4,3,2,1)
  vInt <- tibble(Variables, Interaction)

  # create matrix
  m <- vip2vivid(vImp, vInt)

  expect_s3_class(m, c("vivid", "matrix", "array"))

})


test_that("update matrix test", {
  aq <- na.omit(airquality)
  fr <- ranger(Ozone ~ ., data = aq, importance = "permutation")
  m <- vivi(fit = fr, data = aq, response = "Ozone", gridSize = 5)

  # update matrix importance
  corimp <- abs(cor(aq[, -1])[1, ])
  updatedM <- viviUpdate(m, corimp) # use correlation as importance

  expect_s3_class(updatedM, c("vivid", "matrix", "array"))

})


