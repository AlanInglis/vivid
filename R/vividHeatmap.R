#' vividHeatap
#'
#' @description Plots a Heatmap-style display showing variable importance on the diagonal
#' and variable interaction on the off-diagonal.
#'
#' @param mat A matrix, such as that returned by vivi, of values to be plotted.
#' @param intPal A colorspace colour palette to display the interaction values.
#' @param impPal A colorspace colour palette to display the importance values.
#' @param fitlimsInt Specifies the fit range for the color map for interaction strength.
#' @param fitlimsImp Specifies the fit range for the color map for importance.
#' @param title Adds title to the plot.
#' @param angle The angle to display the x-axis labels.
#' @param ... Not currently implemented.
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
#' plot(myMat, type = "heatMap")
#'
#'
#' # Main plot function -----------------------------------------------------------
vividHeatMap <- function(mat,
                         title = "",
                         intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                         impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                         fitlimsInt = NULL,
                         fitlimsImp = NULL,
                         angle = NULL,
                         ...) {




  # Small set-up ------------------------------------------------------------

  mat <- mat[,]
  # get label names
  labelNames <- colnames(mat)

  # used to set up plot below
  nvar <- nrow(mat)
  index <- 1:nvar

  # set x-axis text angle
  if (is.null(angle)) {
    angle <- 0
  } else {
    angle <- angle
  }



  # Limits ------------------------------------------------------------------

  # max & min interaction values
  minimumInt <- min(as.dist(mat))
  maximumInt <- max(as.dist(mat))


  # max & min importance values
  vImportance <- diag(mat)
  maximumImp <- max(vImportance)
  minimumImp <- min(vImportance)

  # set the limits for interactions
  if (is.null(fitlimsInt)) {
    limitsInt <- c(minimumInt, maximumInt)
  } else {
    limitsInt <- fitlimsInt
  }

  # set the limits for importance
  if (is.null(fitlimsImp)) {
    limitsImp <- c(minimumImp, maximumImp)
  } else {
    limitsImp <- fitlimsImp
  }

  ## Warning messages:
  # if(minInt > minimumInt){
  #   message(" Warning: Minimum chosen interaction value is larger than
  #           some of the interaction values. These values may not be displayed correctly.
  #           Adjust minInt to rectify.")
  # }
  # if(minImp > minimumImp){
  #   message(" Warning: Minimum chosen importance value is larger than
  #           some of the importance values. These values may not be displayed correctly.
  #           Adjust minImp to rectify.")
  # }


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
    theme(legend.position = "none")

  pp <- ggplot(
    data = var_int,
    mapping = aes(x = var_num1, y = var_num2)
  ) +
    guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5)) +
    scale_x_continuous(breaks = index, labels = labelNames, position = "top") +
    scale_y_reverse(breaks = index, labels = labelNames) +
    geom_tile(aes(fill = `Interaction\nStrength`),
      alpha = var_int$alpha_int
    ) +
    scale_fill_gradientn(colors = intPal, limits = limitsInt) +
    labs(title = title)


  ppp <- ggplot(
    data = var_int,
    mapping = aes(x = var_num1, y = var_num2)
  ) +
    guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5)) +
    geom_tile(aes(fill = `Variable\nImportance`),
      alpha = var_int$alpha_imp
    ) +
    scale_fill_gradientn(colors = impPal, limits = limitsImp) +
    xlab("") +
    ylab("") +
    theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  # Grab the legends using cowplot::get_legend()
  p2_legend <- get_legend(pp)
  p3_legend <- get_legend(ppp)

  # Combine the legends one on top of the other
  legends <- plot_grid(p2_legend, p3_legend, ncol = 1, nrow = 2)

  # Combine the heatmap with the legends
  endPlot <- plot_grid(p, legends,
    ncol = 2, align = "h",
    scale = c(1, 0.8), rel_widths = c(0.9, 0.1)
  )

   endPlot

}
