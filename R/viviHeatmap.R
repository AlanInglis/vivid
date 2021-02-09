#' viviHeatmap
#'
#' @description Plots a Heatmap showing variable importance on the diagonal
#' and variable interaction on the off-diagonal.
#'
#' @param mat A matrix, such as that returned by vivi, of values to be plotted.
#' @param intPal A vector of colours to show interactions, for use with scale_fill_gradientn.
#' @param impPal A vector of colours to show importance, for use with scale_fill_gradientn.
#' @param intLims Specifies the fit range for the color map for interaction strength.
#' @param impLims Specifies the fit range for the color map for importance.
#' @param title Adds title to the plot.
#' @param angle The angle to display the x-axis labels.
#'
#' @importFrom ggplot2 "ggplot"
#' @importFrom ggnewscale "new_scale_fill"
#' @importFrom reshape "melt"
#' @importFrom stats "as.dist"
#' @importFrom colorspace "sequential_hcl"
#'
#' @return A heatmap plot showing variable importance on the diagonal
#' and variable interaction on the off-diagonal.
#'
#' @examples
#' library(ranger)
#' aq <- na.omit(airquality)
#' rF <- ranger(Ozone ~ ., data = aq, importance = "permutation")
#' myMat <- vivi(fit = rF, data = aq, response = "Ozone")
#' viviHeatMap(myMat)
#' @export
# Main plot function -----------------------------------------------------------
viviHeatmap <- function(mat,
                        title = "",
                        intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                        impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                        intLims = NULL,
                        impLims = NULL,
                        angle = NULL) {




  # Small set-up ------------------------------------------------------------

  # get label names
  labelNames <- colnames(mat)

  # set x-axis text angle
  if (is.null(angle)) {
    angle <- 0
  }

  # Limits ------------------------------------------------------------------

  # set the limits for importance
  if (is.null(impLims)) {
    impLims <- range(diag(mat))
    limitsImp <- range(labeling::rpretty(impLims[1], impLims[2]))
  } else {
    limitsImp <- impLims
  }

  # set the limits for interactions
  if (is.null(intLims)) {
    intLims <- range(as.dist(mat))
    limitsInt <- range(labeling::rpretty(intLims[1], intLims[2]))
  } else {
    limitsInt <- intLims
  }


  # Set up plot -------------------------------------------------------

  df <- melt(mat)
  # used in plot
  alphaImp <- as.integer(df$X1 == df$X2)
  alphaInt <- 1 - alphaImp


  # Create Plot ------------------------------------------------------------


  p <- ggplot(df, aes(X1, X2)) +
    geom_tile(aes(fill = value), alpha = alphaInt) +
    scale_x_discrete(limits = rev(levels(df$X1)), position = "top") +
    scale_fill_gradientn(
      colors = intPal, limits = limitsInt, name = "Vint",
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black"
      ), oob = scales::squish
    ) +
    new_scale_fill() +
    geom_tile(aes(fill = value), alpha = alphaImp) +
    scale_fill_gradientn(
      colors = impPal, limits = limitsImp, name = "Vimp",
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black"
      ), oob = scales::squish
    ) +
    xlab("") +
    ylab("") +
    theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(axis.text = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = angle, hjust = 0)) +
    theme(aspect.ratio = 1)

  p
}
