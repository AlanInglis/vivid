# test the plot outputs

library(mlr3)
library(mlr3learners)
library(ranger)
library(e1071)

aq <- na.omit(airquality)

test_that("Test heatmap and network", {

  aq_fit <- ranger(Ozone~., data = aq)


  ## test heatmap
  vi <- vivi(fit = aq_fit, data = aq, response = "Ozone", gridSize = 5)
  viH <- viviHeatmap(vi)
  expect_type(viH, "list")

  # change angle
  viH1 <- viviHeatmap(vi, angle = 45)
  expect_type(viH1, "list")

  # change palettes
  viH2 <- viviHeatmap(vi,
                      intPal = rev(colorspace::sequential_hcl(palette = "Purples 3", n = 100)),
                      impPal = rev(colorspace::sequential_hcl(palette = "Inferno", n = 100)))
  expect_type(viH2, "list")
  # change limits
  viH3 <- viviHeatmap(vi, intLims = c(0,2), impLims = c(0,20))
  expect_type(viH3, "list")

  ## test network
  viN <- viviNetwork(vi)
  expect_type(viN, "list")

  # change threshold
  viN1 <- viviNetwork(vi, intThreshold = 1)
  expect_type(viN1, "list")

  # remove node
  viN2 <- viviNetwork(vi, intThreshold = 1, removeNode = TRUE)
  expect_type(viN2, "list")

  # change limits
  viN3 <- viviNetwork(vi, intLims = c(0, 2), impLims = c(1, 20))
  expect_type(viN3, "list")

  # change layout
  viN4 <- viviNetwork(vi, layout = igraph::layout_as_star)
  expect_type(viN4, "list")

  # cluster using igraph
  viN5 <- viviNetwork(vi, cluster = igraph::cluster_infomap)
  expect_type(viN5, "list")

  # cluster with vector
  viN6 <- viviNetwork(vi, cluster = c(1, 1, 2, 2, 2))
  expect_type(viN6, "list")

  # changing nudge values
  viN7 <- viviNetwork(vi, nudge_x = 0.01, nudge_y = 0.01)
  expect_type(viN7, "list")

  # changing palette
  viN8 <- viviNetwork(vi,
                      intPal = rev(colorspace::sequential_hcl(palette = "Purples 3", n = 100)),
                      impPal = rev(colorspace::sequential_hcl(palette = "Inferno", n = 100)))
  expect_type(viN8, "list")

  # change layout, threshold and remove node
  viN9 <- viviNetwork(vi,layout=cbind(c(1,1,1,2,2), c(1,2,3,1,2)), intThreshold = 1, removeNode = T)
  expect_type(viN9, "list")


})


test_that("Test pdpPairs", {
  # make fit
  fit <- lm(Ozone~., data = aq)

  # test plot
  pp <- pdpPairs(aq, fit, "Ozone",nmax = 5, gridSize = 2)
  expect_type(pp, "list")

  # change vars
  pp1 <- pdpPairs(aq, fit, "Ozone", vars = c("Solar.R", "Wind", "Temp"),  nmax = 5, gridSize = 2)
  expect_type(pp1, "list")

  # change palette
  pp2 <- pdpPairs(aq, fit, "Ozone",  nmax = 5, gridSize = 2, pal = rev(colorspace::sequential_hcl(palette = "Inferno", n = 100)))
  expect_type(pp2, "list")

  # change fitlims
  pp3 <- pdpPairs(aq, fit, "Ozone", fitlims = "all",  nmax = 5, gridSize = 2)
  expect_type(pp3, "list")


  # change no of ice curves
  pp6 <- pdpPairs(aq, fit, "Ozone", nIce = 5,  nmax = 5, gridSize = 2)
  expect_type(pp6, "list")

  # change comboimage
  pp7 <- pdpPairs(aq, fit, "Ozone", comboImage = TRUE, gridSize = 2)
  expect_type(pp7, "list")

  # change convex hull
  pp8 <- pdpPairs(aq, fit, "Ozone", convexHull = T, nmax = 3, gridSize = 5)
  expect_type(pp8, "list")

  # namx to NULL
  pp9 <- pdpPairs(aq, fit, "Ozone", nmax = NULL, gridSize = 2)
  expect_type(pp9, "list")

  # adding own limits ERROR
  pp10 <- pdpPairs(aq, fit, "Ozone", fitlims = c(0,200), nmax = 5, gridSize = 2)
  expect_type(pp10, "list")

  # change class
  rf <- ranger(Species ~ ., data = iris, probability = TRUE)
  ppC <- pdpPairs(iris, rf, "Species", nmax = 5, gridSize = 2) # prediction probs for first class, setosa
  expect_type(ppC, "list")

  ppC1 <-  pdpPairs(iris, rf, "Species", class = "versicolor", nmax = 5, gridSize = 2) # prediction probs versicolor
  expect_type(ppC1, "list")


})


test_that("Test pdpZen", {
  set.seed(1701)
  fit <- lm(Ozone~., data = aq)

  # test plot
  z <- pdpZen(aq, fit, response = "Ozone", nmax = 3, gridSize = 2)
  expect_type(z, "list")

  # change palette
  z1 <- pdpZen(aq, fit, response = "Ozone",  nmax = 3, gridSize = 2, pal =  rev(colorspace::sequential_hcl(palette = "Inferno", n = 100)))
  expect_type(z1, "list")

  # add zpath
  set.seed(1701)
  aq_Task <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
  aq_lrn <- lrn("regr.svm")
  aq_fit <- aq_lrn$train(aq_Task)

  aqVivi <- vivi(aq, aq_fit, "Ozone", 3,  nmax = 3)
  zpath <- zPath(aqVivi, connect = T, method = "strictly.weighted")
  z2 <- pdpZen(aq, fit, "Ozone", zpath = zpath, nmax = 3, gridSize = 2)
  expect_type(z2, "list")

  zpath <- zPath(aqVivi, connect = F, method = "strictly.weighted")
  z21 <- pdpZen(aq, fit, "Ozone", zpath = zpath, nmax = 3, gridSize = 2)
  expect_type(z21, "list")


  # adding own limits
  z3 <- pdpZen(aq, fit, "Ozone", fitlims = c(0,20), nmax = 3, gridSize = 2)
  expect_type(z3, "list")

  # change fitlims
  z31 <- pdpZen(aq, fit, response = "Ozone",  fitlims = "all", nmax = 3, gridSize = 2)
  expect_type(z31, "list")

  # change comboimage
  z6 <- pdpZen(aq, fit, "Ozone", comboImage = TRUE, nmax = 3, gridSize = 2)
  expect_type(z6, "list")

  # change convex hull
  z7 <- pdpZen(aq, fit, "Ozone", convexHull = T, nmax = 10, gridSize = 5)
  expect_type(z7, "list")

  # namx to NULL
  z8 <- pdpZen(aq, fit, "Ozone", nmax = NULL, gridSize = 2)
  expect_type(z8, "list")

  # change class
  rf <- ranger(Species ~ ., data = iris, probability = TRUE)
  zC <- pdpZen(iris, rf, "Species", nmax = 3, gridSize = 2) # prediction probs for first class, setosa
  expect_type(zC, "list")

  zC1 <-  pdpZen(iris, rf, "Species", class = "versicolor", nmax = 3,gridSize = 2) # prediction probs versicolor
  expect_type(zC1, "list")


  })


test_that("Test zpath",{

  aq_Task <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
  aq_lrn <- lrn("regr.svm")
  fit <- aq_lrn$train(aq_Task)

  aqVivi <- vivi(aq, fit, "Ozone", nmax = 3, gridSize = 2)
  zpath <- zPath(aqVivi, cutoff = 1, connect = F, method = "strictly.weighted")
  expect_type(zpath, "list")

  zpath1 <- zPath(aqVivi, cutoff = 1, connect = T, method = "greedy.weighted")
  expect_type(zpath1, "character")

  zpath2 <- zPath(aqVivi, cutoff = "a")
  expect_type(zpath2, "character")

  expect_error(zPath(aqVivi, cutoff = 100))
})


test_that("Test pdpVars",{
  fit <- lm(Ozone ~ ., data = aq)
  p <- pdpVars(aq, fit, "Ozone", nmax = 5, gridSize = 2)
  expect_type(p, "list")

  p1 <- pdpVars(aq, fit, "Ozone", nmax = NULL, gridSize = 2)
  expect_type(p1, "list")

  rfClassif <- ranger(Species ~ ., data = iris, probability = TRUE,)
  p2 <- pdpVars(iris, rfClassif, "Species", class = 2, draw = FALSE, nmax = 5, gridSize = 2)
  expect_type(p2, "list")

  p3 <- pdpVars(iris, rfClassif, "Species", class = 2, colorVar = "Species", nmax = 5, gridSize = 2)
  expect_type(p3, "list")
})






