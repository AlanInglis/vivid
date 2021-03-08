# Test the main vivi function

library(mlr3)
library(mlr3learners)
library(ranger)
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

  # Try changing importance type
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

  # Check that the classification value can be changed - didn't make any difference?
  expect_s3_class(vivi(fit = rf, data = iris, response = "Species", class = 2), c("vivid","matrix","array"))

})

test_that("Changing prediction function works", {
  # No tests yet

})

test_that("Works for old mlr models", {
  # No tests yet

})

test_that("Works for tidymodels", {
  # No tests yet

})


