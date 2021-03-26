# Test the main vivi function

library(mlr3)
library(mlr3learners)
library(ranger)
library(tidymodels)
library(randomForest)
library(MASS)
library(mlr)


aq <- na.omit(airquality)

test_that("Vivi function works for mlr3 example data",{
  aq_Task <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
  aq_lrn <- lrn("regr.ranger", importance = "permutation")
  aq_fit <- aq_lrn$train(aq_Task)

  # Default values
  m <- vivi(fit = aq_fit, data = aq, response = "Ozone")

  expect_s3_class(m,c("vivid","matrix","array"))
  expect_identical(sort(colnames(m)), sort(colnames(aq)[-1]))
  expect_identical(sort(rownames(m)), sort(colnames(aq)[-1]))
  expect_true(ncol(m)==nrow(m))

  # Run for small grid size
  expect_s3_class(vivi(fit = aq_fit, data = aq, response = "Ozone", gridSize = 10),c("vivid","matrix","array"))

  # Try changing importance type FAILS check() test
   expect_s3_class(vivi(fit = aq_fit, data = aq, response = "Ozone", gridSize = 10, importanceType = "agnostic"),c("vivid","matrix","array"))

  # Change number of rows
  expect_s3_class(vivi(fit = aq_fit, data = aq, response = "Ozone", gridSize = 10, nmax = 10),
                  c("vivid","matrix","array"))

  # Set reorder to FALSE
  m <- vivi(fit = aq_fit, data = aq, response = "Ozone", gridSize = 10, reorder = FALSE)
  expect_identical(colnames(m), colnames(aq)[-1])

})

test_that("vivi function works for classification", {
  rf <- ranger(Species ~ ., data = iris, importance = "impurity")
  expect_s3_class(vivi(fit = rf, data = iris, response = "Species"), c("vivid","matrix","array"))

  # Check that the classification value can be changed
  expect_s3_class(vivi(fit = rf, data = iris, response = "Species", class = 2), c("vivid","matrix","array"))

})

test_that("Changing prediction function works", {
  aq <- aq[1:100,]
  lmf <- lm(Ozone ~ ., data = aq)

  vi <- vivi(fit = lmf,
             data = aq,
             response = "Ozone",
             gridSize = 10,
             nmax = 10,
             predictFun = function(fit, data) predict(lmf, aq))

  expect_s3_class(vi,c("vivid","matrix","array"))

})



test_that("Works for old mlr models", {

  rgrTask  <- makeRegrTask(data = aq, target = "Ozone")
  regr.lrn <- makeLearner("regr.randomForest", importance = TRUE)
  mod <- train(regr.lrn,rgrTask)

  m <- vivi(fit = mod, data = aq, response = "Ozone", importanceType = "%IncMSE")
  expect_s3_class(m,c("vivid","matrix","array"))

  m1 <- vivi(fit = mod, data = aq, response = "Ozone", importanceType = "IncNodePurity")
  expect_s3_class(m1, c("vivid","matrix","array"))

  m2 <- vivi(fit = mod, data = aq, response = "Ozone")
  expect_s3_class(m2, c("vivid","matrix","array"))



})

test_that("Works for tidymodels", {
  lm_aq_model <- linear_reg() %>%
    set_engine("lm")

  lm_fit <- lm_aq_model %>%
    fit(Ozone ~ ., data = aq)

  expect_s3_class(vivi(fit = lm_fit, data = aq, response = "Ozone", gridSize = 10),c("vivid","matrix","array"))

})

test_that("Works for lm", {
  lmf <- lm(Ozone ~ ., data = aq)
  vi <- vivi(fit = lmf, data = aq, response = "Ozone")
  expect_s3_class(vi, c("vivid","matrix","array"))

})

test_that("Works for ranger", {
  fr <- ranger(Ozone ~ ., data = aq, importance = "permutation")
  vi <- vivi(fit = fr, data = aq, response = "Ozone")
  expect_s3_class(vi, c("vivid","matrix","array"))

  # change importance type
  fr <- ranger(Ozone ~ ., data = aq, importance = "impurity")
  vi1 <- vivi(fit = fr, data = aq, response = "Ozone")
  expect_s3_class(vi1, c("vivid","matrix","array"))

})

test_that("Works for randomForest", {
  fr <- randomForest(Ozone ~ ., data = aq)
  vi <- vivi(fit = fr, data = aq, response = "Ozone")
  expect_s3_class(vi, c("vivid","matrix","array"))

  # changing importance types
  fr <- randomForest(Ozone ~ ., data = aq, importance = TRUE)
  vi1 <- vivi(fit = fr, data = aq, response = "Ozone", importanceType = "IncNodePurity")
  expect_s3_class(vi1, c("vivid","matrix","array"))

  fr <- randomForest(Ozone ~ ., data = aq, importance = TRUE)
  vi2 <- vivi(fit = fr, data = aq, response = "Ozone", importanceType = "%IncMSE")
  expect_s3_class(vi2, c("vivid","matrix","array"))

  fr <- randomForest(Ozone ~ ., data = aq, importance = TRUE)
  vi3 <- vivi(fit = fr, data = aq, response = "Ozone")
  expect_s3_class(vi3, c("vivid","matrix","array"))

})

test_that("Works for LDA", {
  LDAf <- lda(Species ~ ., data = iris)
  vi <- vivi(fit = LDAf, data = iris, response = "Species")
  expect_s3_class(vi, c("vivid","matrix","array"))

})


test_that("works for mlr3", {
  ozonet <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
  ozonel <- lrn("regr.ranger")
  ozonef <- ozonel$train(ozonet)

  vi <- vivi(fit = ozonef, data = aq, response = "Ozone")
  expect_s3_class(vi, c("vivid","matrix","array"))

  # xboost
  # ozonetXG <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
  # ozonelXG <- lrn("regr.xgboost")
  # ozonefXG <- ozonelXG$train(ozonetXG)
  #
  # vi1 <- vivi(fit = ozonefXG, data = aq, response = "Ozone")
  # expect_s3_class(vi1, c("vivid","matrix","array"))

  # svm
  ozonetSVM <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
  ozonelSVM <- lrn("regr.svm")
  ozonefSVM <- ozonelSVM$train(ozonetSVM)

  vi2 <- vivi(fit = ozonefSVM, data = aq, response = "Ozone")
  expect_s3_class(vi2, c("vivid","matrix","array"))

})



