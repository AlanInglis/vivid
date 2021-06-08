library(ranger)
library(vivid)
library(MASS)

set.seed(1701)

# Normalised Vs Un-Normalised ----------------------------------------------------------

# Data:
fData <- genFriedman(noFeatures = 10, noSamples = 1000)

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
  X5 = corVals[, 2]
)

# Check correlations
cor(mvnData)

# Equation:
y = (10*sin(pi*mvnData$X1*mvnData$X2) + 20 * (mvnData$X3-0.5)^2 + 10 * mvnData$X4 + 5 * mvnData$X5 + e)
mvnData$y <- y

# Create random forest model:
mvnFit <- ranger(y~., data = mvnData, importance = "permutation")

# vivi Normalized & Unnormalized:
mvnNormF <- vivi(fit = mvnFit, data = mvnData, response = "y", normalized = FALSE)
mvnNormT <- vivi(fit = mvnFit, data = mvnData, response = "y", normalized = TRUE)

# Visualisations:
viviHeatmap(mvnNormF)
viviHeatmap(mvnNormT)

# Create models with & without influence of correlated variables:
mvnFit_1 <- ranger(y~ X1 + X2 + X3 + X4, data = mvnData, importance = "permutation")
mvnFit_2 <- ranger(y~ X1 + X2 + X3 + X5, data = mvnData, importance = "permutation")

# Visualise PDPs for each scenario:
pdpVars(mvnData, mvnFit,   "y", nIce = 0, vars = c("X4"), limits = c(-30, 45))
pdpVars(mvnData, mvnFit,   "y", nIce = 0, vars = c("X5"), limits = c(-30, 45))
pdpVars(mvnData, mvnFit_1, "y", nIce = 0, vars = c("X4"), limits = c(-30, 45))
pdpVars(mvnData, mvnFit_2, "y", nIce = 0, vars = c("X5"), limits = c(-30, 45))



