#' genFriedman
#'
#' @description Simulate data from the Friedman benchmark problem 1. Mainly used for testing.
#'
#' @param noFeatures Integer specifying the number of features/variables to generate. Default is 10.
#' @param noSamples Integer specifying the number of samples to generate. Default is 100
#' @param sigma Numreic specifying standard deviation of the noise.
#' @param bins Integer specifiying the number of bins to split responce variable into. Setting a value greater than 1 turns this into a classification problem where bins determines the number of classes.
#' @param seed Integer specifying the random seed.
#'
#' @importFrom stats "quantile"
#' @importFrom stats "rnorm"
#' @importFrom stats "runif"
#'
#' @examples
#' genFriedman(noFeatures = 10, noSamples = 100, sigma = 1, seed = NULL)
#' @export

genFriedman <- function(noFeatures = 10, noSamples = 100, sigma = 1, bins = NULL, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Set Values
  n <- noSamples # no of rows
  p <- noFeatures # no of variables
  e <- rnorm(n, sd = sigma)


  # Create matrix of values
  xValues <- matrix(runif(n * p, 0, 1), nrow = n) # Create matrix
  colnames(xValues) <- paste0("x", 1:p) # Name columns
  df <- data.frame(xValues) # Create dataframe


  # Equation:
  # y = 10sin(πx1x2) + 20(x3−0.5)^2 + 10x4 + 5x5 + ε

  y <- (10 * sin(pi * df$x1 * df$x2) + 20 * (df$x3 - 0.5)^2 + 10 * df$x4 + 5 * df$x5 + e)


  # Adding y to df
  df$y <- y

  # Function to bin a numberic vector
  bin <- function(x, bins) {
    x <- df$y
    quantiles <- quantile(x, probs = seq(from = 0, to = 1, length = bins + 1))
    bins <- cut(x, breaks = quantiles, label = FALSE, include.lowest = TRUE)
    as.factor(paste0("class", bins))
  }

  if (!is.null(bins)) {
    bins <- as.integer(bins)
    if (bins < 2) {
      stop("bins should be an integer greater than 1.", call. = FALSE)
    }
    df$y <- bin(df$y, bins = bins)
  }

  df
}
