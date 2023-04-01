# Test the main vivi function

library(mlr3)
library(mlr3learners)
library(ranger)
library(tidymodels)
library(randomForest)
library(MASS)
library(mlr)
library(vivid)


aq <- na.omit(airquality)


test_that("vivi function works for classification", {
  set.seed(1701)
  rf <- ranger(Species ~ ., data = iris, importance = "impurity")
  vi <- vivi(fit = rf, data = iris, response = "Species", nmax = 10, gridSize = 5)
  # check class
  expect_s3_class(vi, c("vivid","matrix","array"))

  # Check that the classification value can be changed
  expect_s3_class(vivi(fit = rf, data = iris, response = "Species", class = 2, nmax = 10, gridSize = 5), c("vivid","matrix","array"))

})

test_that("vivi function output", {
  set.seed(1701)
  rf <- ranger(Species ~ ., data = iris, importance = "impurity")
  vi <- vivi(fit = rf, data = iris, response = "Species", nmax = 10, gridSize = 5)
  expect_type(vi[1,1], "double") # check vimp type
  expect_type(vi[2,1], "double") # check vint type
  expect_length(vi[1,], 4) # check correct length

  # check symmetry of matrix
  viLo <- as.vector(vi[lower.tri(vi)])
  viHi <- as.vector(vi[upper.tri(vi)])
  ord <- match(viHi, viLo)
  viLo <- viLo[ord]
  expect_equal(viLo, viHi)
})

test_that("Changing prediction function works", {
  aq <- aq[1:100,]
  lmf <- lm(Ozone ~ ., data = aq)

  vi <- vivi(fit = lmf,
             data = aq,
             response = "Ozone",
             gridSize = 10,
             nmax = 10,
             predictFun = function(fit, data, prob = TRUE) predict(lmf, aq, prob = TRUE))

  expect_s3_class(vi,c("vivid","matrix","array"))

})

test_that("Check LM output", {
  set.seed(1701)
  aq <- aq[1:100,]
  lmf <- lm(Ozone ~ ., data = aq)

  vi <- vivi(fit = lmf,
             data = aq,
             response = "Ozone",
             gridSize = 10,
             nmax = 10,
             predictFun = function(fit, data, prob = TRUE) predict(lmf, aq, prob = TRUE))
  expect_equal(vi[1,1], 0)

})



test_that("Works for old mlr models", {

  rgrTask  <- makeRegrTask(data = aq, target = "Ozone")
  regr.lrn <- makeLearner("regr.randomForest", importance = TRUE)
  mod <- train(regr.lrn,rgrTask)

  m <- vivi(fit = mod, data = aq, response = "Ozone", importanceType = "%IncMSE", nmax = 10, gridSize = 5)
  expect_s3_class(m,c("vivid","matrix","array"))

  m1 <- vivi(fit = mod, data = aq, response = "Ozone", importanceType = "IncNodePurity", nmax = 10, gridSize = 5)
  expect_s3_class(m1, c("vivid","matrix","array"))

  m2 <- vivi(fit = mod, data = aq, response = "Ozone", nmax = 10, gridSize = 5)
  expect_s3_class(m2, c("vivid","matrix","array"))



})

test_that("Works for tidymodels", {
  lm_aq_model <- linear_reg() %>%
    set_engine("lm")

  lm_fit <- lm_aq_model %>%
    fit(Ozone ~ ., data = aq)

  expect_s3_class(vivi(fit = lm_fit, data = aq,
                       response = "Ozone", nmax = 10,
                       gridSize = 5,
                       importanceType = 'stat',
                       predictFun = condvis2::CVpredict(lm_fit, aq)),c("vivid","matrix","array"))

})



test_that("Works for randomForest", {
  fr <- randomForest(Ozone ~ ., data = aq)
  vi <- vivi(fit = fr, data = aq, response = "Ozone", nmax = 10, gridSize = 5)
  expect_s3_class(vi, c("vivid","matrix","array"))

  # changing importance types
  fr <- randomForest(Ozone ~ ., data = aq, importance = TRUE)
  vi1 <- vivi(fit = fr, data = aq, response = "Ozone", importanceType = "IncNodePurity", nmax = 10, gridSize = 5)
  expect_s3_class(vi1, c("vivid","matrix","array"))

  fr <- randomForest(Ozone ~ ., data = aq, importance = TRUE)
  vi2 <- vivi(fit = fr, data = aq, response = "Ozone", importanceType = "%IncMSE", nmax = 10, gridSize = 5)
  expect_s3_class(vi2, c("vivid","matrix","array"))

  fr <- randomForest(Ozone ~ ., data = aq, importance = TRUE)
  vi3 <- vivi(fit = fr, data = aq, response = "Ozone",nmax = 10, gridSize = 5)
  expect_s3_class(vi3, c("vivid","matrix","array"))

})

test_that("Works for LDA", {
  LDAf <- lda(Species ~ ., data = iris)
  vi <- vivi(fit = LDAf, data = iris, response = "Species", nmax = 10,gridSize = 5)
  expect_s3_class(vi, c("vivid","matrix","array"))

})




