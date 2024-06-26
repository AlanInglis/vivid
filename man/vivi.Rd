% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vivi.R
\name{vivi}
\alias{vivi}
\title{vivi}
\usage{
vivi(
  data,
  fit,
  response,
  gridSize = 50,
  importanceType = "agnostic",
  nmax = 500,
  reorder = TRUE,
  class = 1,
  predictFun = NULL,
  normalized = FALSE,
  numPerm = 4,
  showVimpError = FALSE,
  vars = NULL
)
}
\arguments{
\item{data}{Data frame used for fit.}

\item{fit}{A supervised machine learning model, which understands condvis2::CVpredict}

\item{response}{The name of the response for the fit.}

\item{gridSize}{The size of the grid for evaluating the predictions.}

\item{importanceType}{Used to select the importance metric. By default, an agnostic importance
measure is used. If an embedded metric is available, then setting this argument
to the importance metric will use the selected importance values in the vivid-matrix.
Please refer to the examples given for illustration.
Alternatively, set to equal "agnostic" (the default) to override embedded importance measures and
return agnostic importance values.}

\item{nmax}{Maximum number of data rows to consider. Default is 500. Use all rows if NULL.}

\item{reorder}{If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances.}

\item{class}{Category for classification, a factor level, or a number indicating which factor level.}

\item{predictFun}{Function of (fit, data) to extract numeric predictions from fit. Uses condvis2::CVpredict by default, which works for many fit classes.}

\item{normalized}{Should Friedman's H-statistic be normalized or not. Default is FALSE.}

\item{numPerm}{Number of permutations to perform for agnostic importance. Default is 4.}

\item{showVimpError}{Logical. If TRUE, and \code{numPerm > 1} then a tibble containing the variable names, their importance values,
and the standard error for each importance is printed to the console.}

\item{vars}{A vector of variable names to be assessed.}
}
\value{
A matrix of interaction values, with importance on the diagonal.
}
\description{
Creates a matrix displaying variable importance on the diagonal
and variable interaction on the off-diagonal.
}
\details{
If the argument \code{importanceType = 'agnostic'}, then an agnostic permutation importance (1) is calculated.
Friedman's H statistic (2) is used for measuring the interactions. This measure is based on partial dependence curves
and relates the interaction strength of a pair of variables to the total effect strength of that variable pair.
}
\examples{

aq <- na.omit(airquality)
f <- lm(Ozone ~ ., data = aq)
m <- vivi(fit = f, data = aq, response = "Ozone") # as expected all interactions are zero
viviHeatmap(m)

# Select importance metric
library(randomForest)
rf1 <- randomForest(Ozone~., data = aq, importance = TRUE)
m2 <- vivi(fit = rf1, data = aq, response = 'Ozone',
           importanceType = '\%IncMSE') # select \%IncMSE as the importance measure
viviHeatmap(m2)

\donttest{
library(ranger)
rf <- ranger(Species ~ ., data = iris, importance = "impurity", probability = TRUE)
vivi(fit = rf, data = iris, response = "Species") # returns agnostic importance
vivi(fit = rf, data = iris, response = "Species",
     importanceType = "impurity") # returns selected 'impurity' importance.
}
}
\references{
1: Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful: Variable Importance for Black-Box, Proprietary, or Misspecified Prediction Models, using Model Class Reliance. Arxiv.

2: Friedman, J. H. and Popescu, B. E. (2008). “Predictive learning via rule ensembles.” The Annals of Applied Statistics. JSTOR, 916–54.
}
