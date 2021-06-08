###################################################################
# Function to generate data from the Friedman Benchmark 1 problem #
###################################################################

genFriedman <- function(noFeatures = 10,
                        noSamples = 100,
                        sigma = 1,
                        bins = NULL,
                        seed = NULL,
                        showTrueY = FALSE) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Set Values
  n <- noSamples # no of rows
  p <- noFeatures # no of variables
  e <- rnorm(n, sd = sigma)


  # Equation:
  # y = 10sin(πx1x2) + 20(x3−0.5)^2 + 10x4 + 5x5 + ε

  xValues <- matrix(runif(n*p, 0, 1), nrow = n)
  yTrue <- 10 * sin(pi * xValues[, 1] * xValues[, 2]) + 20 * (xValues[, 3] - 0.5)^2 + 10 * xValues[, 4] + 5 * xValues[, 5]
  y <- yTrue + e

  if (showTrueY) {
    df <- data.frame(xValues, y, yTrue)
  } else {
    df <- data.frame(xValues, y)
  }

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
