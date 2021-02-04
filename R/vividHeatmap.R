#' viviHeatMap
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
#' @param ...
#'
#' @importFrom ggplot2 "ggplot"
#' @importFrom ggnewscale "new_scale_fill"
#' @importFrom dplyr "as_tibble"
#' @importFrom dplyr "mutate"
#' @importFrom tidyr "pivot_longer"
#' @importFrom reshape "melt"
#' @importFrom stats "reorder"
#' @importFrom stats "as.dist"
#' @importFrom cowplot "get_legend"
#' @importFrom cowplot "plot_grid"
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
viviHeatMap <- function(mat,
                        title = "",
                        intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                        impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                        intLims = NULL,
                        impLims = NULL,
                        angle = NULL,
                        ...) {




  # Small set-up ------------------------------------------------------------

  # get label names
  labelNames <- colnames(mat)

  # used to set up plot below
  nvar <- nrow(mat)
  index <- 1:nvar

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



  #   Warning messages for limits -------------------------------------------

  if (!is.null(intLims) && intLims[1] > min(as.dist(mat))) {
    message("Error: Minimum chosen limit for interaction is larger
  than the minimum measured interaction value. Graphic may not display correctly.
  Please choose a minimum limit value less than or equal to the minimum measured value.")
  }

  if (!is.null(intLims) && intLims[2] < max(as.dist(mat))) {
    message("Error: Maximum chosen limit for interaction is smaller
  than the maximum measured interaction value. Graphic may not display correctly.
  Please choose a maximum limit value greater than or equal to the maximum measured value.")
  }

  if (!is.null(impLims) && impLims[1] > min(diag(mat))) {
    message("Error: Minimum chosen limit for importance is larger
  than the minimum measured importance value. Graphic may not display correctly.
  Please choose a minimum limit value less than or equal to the minimum measured value.")
  }

  if (!is.null(impLims) && impLims[2] < max(diag(mat))) {
    message("Error: Maximum chosen limit for importance is smaller
  than the maximum measured importance value. Graphic may not display correctly.
  Please choose a maximum limit value greater than or equal to the maximum measured value.")
  }

  # Set up plot -------------------------------------------------------

  var_int <- mat %>%
    as_tibble() %>%
    mutate(var_num1 = index) %>%
    pivot_longer(
      cols = index,
      values_to = "Interaction\nStrength"
    ) %>%
    mutate(
      var_num2 = rep(index, nvar),
      alpha_imp = as.integer(var_num1 == var_num2),
      alpha_int = 1 - alpha_imp,
      `Variable\nImportance` = alpha_imp * `Interaction\nStrength`,
      `Interaction\nStrength` = alpha_int * `Interaction\nStrength`
    )


  # Create Plot ------------------------------------------------------------


  p <- ggplot(
    data = var_int,
    mapping = aes(x = var_num1, y = var_num2)
  ) +
    scale_x_continuous(breaks = index, labels = labelNames, position = "top") +
    scale_y_reverse(breaks = index, labels = labelNames) +
    geom_tile(aes(fill = `Interaction\nStrength`), alpha = var_int$alpha_int) +
    scale_fill_gradientn(colors = intPal, limits = limitsInt) +
    labs(title = title) +
    new_scale_fill() +
    geom_tile(aes(fill = `Variable\nImportance`), alpha = var_int$alpha_imp) +
    scale_fill_gradientn(colors = impPal, limits = limitsImp) +
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
