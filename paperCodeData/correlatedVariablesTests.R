library(ranger)
library(vivid)
library(MASS)
library(ggplot2)

set.seed(1701)

# Normalised Vs Un-Normalised ----------------------------------------------------------

# Data:
fData <- genFriedman(noFeatures = 10, noSamples = 1000, seed = 1701)

# Fit:
fFit <- ranger(y ~ ., data = fData, importance = "permutation")

# vivi Normalized & Unnormalized:
fNormalT <- vivi(fit = fFit, data = fData, response = "y", normalized = TRUE)
fNormalF <- vivi(fit = fFit, data = fData, response = "y", normalized = FALSE)

# Visualisations:
viviHeatmap(fNormalT)
viviHeatmap(fNormalF)


# Correlated Variables Example --------------------------------------------

samples <- 1000
r <- 0.9 # correlation of 2 variabless will be this value
e <- rnorm(samples)

# Create correlated values:
corVals <- mvrnorm(n = samples, mu = c(0, 0), Sigma = matrix(c(1, r, r, 1), nrow = 2), empirical = TRUE)

# Create dataframe:
mvnData <- data.frame(
  X1 = runif(samples, 0 ,1),
  X2 = runif(samples, 0 ,1),
  X3 = runif(samples, 0, 1),
  X4 = corVals[, 1],
  X5 = corVals[, 2],
  X6 = runif(samples, 0 ,1),
  X7 = runif(samples, 0 ,1),
  X8 = runif(samples, 0, 1),
  X9 = runif(samples, 0 ,1),
  X10 = runif(samples, 0 ,1)
)

# Check correlations
corrplot::corrplot(cor(mvnData), type = "lower")

# Equation:
y = (10*sin(pi*mvnData$X1*mvnData$X2) + 20 * (mvnData$X3-0.5)^2 + 10 * mvnData$X4 + 5 * mvnData$X5 + e)
mvnData$y <- y


# Visualisations ----------------------------------------------------------
# Fit:
fFitCorr <- ranger(y ~ ., data = mvnData, importance = "permutation")

# vivi Normalized & Unnormalized:
fNormalTCorr <- vivi(fit = fFitCorr, data = mvnData, response = "y", normalized = TRUE)
fNormalFCorr <- vivi(fit = fFitCorr, data = mvnData, response = "y", normalized = FALSE)

# Heatmap:
viviHeatmap(fNormalTCorr)
viviHeatmap(fNormalFCorr)

# Look at individual PDPs -------------------------------------------------

# Create models with & without influence of correlated variables:
mvnFit_both <- ranger(y~ X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = mvnData, importance = "permutation")
mvnFit_X4 <- ranger(y~ X3 + X4 + X6 + X7 + X8 + X9 + X10, data = mvnData, importance = "permutation")
mvnFit_X5 <- ranger(y~ X3 + X5 + X6 + X7 + X8 + X9 + X10, data = mvnData, importance = "permutation")

# Visualise PDPs for each scenario:
bothX4 <- pdpVars(mvnData, mvnFit_both, "y", nIce = 0, vars = c("X4"), limits = c(-20, 40))
bothX5 <- pdpVars(mvnData, mvnFit_both, "y", nIce = 0, vars = c("X5"), limits = c(-20, 40))
justX4 <- pdpVars(mvnData, mvnFit_X4, "y", nIce = 0,   vars = c("X4"), limits = c(-20, 40))
justX5 <- pdpVars(mvnData, mvnFit_X5, "y", nIce = 0,   vars = c("X5"), limits = c(-20, 40))

# create overlaid plot for X4:
bothX4[[1]]$layers[[2]]$aes_params$colour <- "red"
pX4 <- bothX4[[1]] + justX4[[1]]$layers
aX4 <- annotate("text", x = 2.5, y = 18, label = "Model 3", vjust = -2, hjust = 0.2, colour = "red")
aX4_1 <- annotate("text", x = 2.5, y = 33, label = "Model 1", vjust = -2, hjust = 0.2, colour = "black")
pX4 + aX4 + aX4_1

# create overlaid plot for X5:
bothX5[[1]]$layers[[2]]$aes_params$colour <- "red"
pX5 <- bothX5[[1]] + justX5[[1]]$layers
aX5 <-  annotate("text", x = 2, y = 12, label = "Model 3", vjust = -2, hjust = 0.2, colour = "red")
aX5_1 <- annotate("text", x = 2, y = 31, label = "Model 2", vjust = -2, hjust = 0.2, colour = "black")
pX5 + aX5 + aX5_1

# -------------------------------------------------------------------------


