#' vivid: Plot the interaction/importance matrix created from \code{vivi}
#'
#' @description This function is used to create vivid-based plots. That is, either "heatMap" or "network" plots.
#'
#' @param mat A matrix of class \code{vivid}.
#' @param type Type of plot required. Either "heatMap" or "network".
#' @param intPal A colorspace colour palette to display the interaction values.
#' @param title Adds title to the plot.
#' @param impPal A colorspace colour palette to display the importance values.
#' @param fitlimsInt Specifies the fit range for the color map for interaction strength.
#' @param fitlimsImp Specifies the fit range for the color map for importance.
#' @param angle The angle to display the x-axis labels.
#' @param ... Not currently implemented.
#'
#' @return A plot chosen from the type argument.
#'
#'
#' @examples
#' library(ranger)
#' aq <- na.omit(airquality)
#' rF <- ranger(Ozone ~ ., data = aq, importance = "permutation")
#' myMat <- vivi(fit = rF, data = aq, response = "Ozone")
#' plot(myMat, type = "heatMap")
#' @export

plot.vivid <- function(mat,
                       type = c("heatMap", "network"),
                       # for heatmap
                       title = "",
                       intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                       impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                       fitlimsInt = NULL,
                       fitlimsImp = NULL,
                       angle = NULL,
                       # for network
                       thresholdValue = 0,
                       label = FALSE,
                       labelNudge = 0.05,
                       layout = "circle",
                       cluster = F,
                       clusterType = cluster_optimal,
                       clusterLayout =  layout_with_fr,
                       ...) {


  # Get the specified type
  type <- match.arg(type)

  if ("heatMap" %in% type) {

    vividHeatMap(mat,
                 title = title,
                 intPal,
                 impPal,
                 fitlimsInt = fitlimsInt,
                 fitlimsImp = fitlimsImp,
                 angle = angle,
                 ...
    )
  } else if("network" %in% type){


    plotNetwork(mat,
            model,
            thresholdValue,
            label,
            layout = layout,
            fitlimsInt = fitlimsInt,
            fitlimsImp = fitlimsImp,
            intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
            impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
            labelNudge = labelNudge,
            cluster = cluster,
            clusterType = clusterType,
            clusterLayout = clusterLayout,...)

    }
  }
