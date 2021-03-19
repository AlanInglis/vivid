#' pdpVars
#'
#' @description Displays the individual coditional expectation (ICE) curves and aggregated partial dependence
#' for each variable in a grid.
#'
#' @param data Data frame used for fit.
#' @param fit A supervised machine learning model, which understands condvis2::CVpredict
#' @param response The name of the response for the fit.
#' @param vars The variables to plot (and their order), defaults to all variables other than response.
#' @param pal A vector of colors to show predictions, for use with scale_fill_gradientn
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param nmax Uses sample of nmax data rows for the pdp. Use all rows if NULL.
#' @param class Category for classification, a factor level, or a number indicating which factor level.
#' @param nIce Number of ice curves to be plotted, defaults to 30.
#' @param predictFun Function of (fit, data) to extract numeric predictions from fit. Uses condvis2::CVpredict by default, which works for many fit classes.
#' @param limits A vector determining the limits of the predicted values.
#' @param colorVar Which variable to colour the predictions by.
#' @param draw If FALSE, then the plot will not be drawn. Default is TRUE.
#' @return A grid displaying ICE curves and univariate partial dependence.
#'
#' @importFrom condvis2 CVpredict
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr %>%
#' @importFrom stats na.omit
#' @import ggplot2
#'
#' @examples
#' # Load in the data:
#' aq <- na.omit(airquality)
#' fit <- lm(Ozone ~ ., data = aq)
#' pdpVars(aq, fit, "Ozone")
#'
#' # Classification
#' rfClassif <- ranger(Species ~ ., data = iris, probability = TRUE)
#' pdpVars(iris, rfClassif, "Species", class = 3)
#'
#' pp <- pdpVars(iris, rfClassif, "Species", class = 2, draw = F)
#' pp[[1]]
#' pdpVars(iris, rfClassif, "Species", class = 2, colorVar = "Species")
#'
#' @export

pdpVars <- function(data, fit, response,
                    vars = NULL, pal = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
                    gridSize = 10, nmax = 500, class = 1,
                    nIce = 30, predictFun = NULL, limits = NULL, colorVar = NULL, draw = TRUE) {

  data <- na.omit(data)
  if (is.null(nmax)) nmax <- nrow(data)
  nmax <- max(5, nmax)
  if (is.numeric(nmax) && nmax < nrow(data)) {
    data <- data[sample(nrow(data), nmax), , drop = FALSE]
  }
  gridSize <- min(gridSize, nmax)

  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  if (is.null(predictFun)) predictFun <- vivid:::CVpredictfun(classif, class)


  predData <- predictFun(fit, data)

  vars0 <- names(data)
  vars0 <- vars0[-match(response, vars0)]
  vars <- vars[vars %in% vars0]
  if (is.null(vars)) vars <- vars0

  nIce <- min(nIce, nrow(data))
  sice <- c(NA, sample(nrow(data), nIce)) # for use with iceplots


  data$predData <- predData
  pdplist1 <- vector("list", length = length(vars))
  for (i in 1:length(vars)) {
    px <- vivid:::pdp_data(data, vars[i], gridsize = gridSize)
    px$.pid <- i
    pdplist1[[i]] <- px
  }
  pdplist1 <- bind_rows(pdplist1)
  pdplist1$fit <- predictFun(fit, pdplist1)
  pdplist1 <- split(pdplist1, pdplist1$.pid)

  names(pdplist1) <- vars

  if (is.null(limits)) {
    r <- sapply(pdplist1, function(x) range(x$fit))
    r <- range(c(r, predData))
    limits <- range(labeling::rpretty(r[1], r[2]))
  }

  options(dplyr.summarise.inform = FALSE) # used to suppress dplyr messages
  ice <- function(var) {
    pdp <- pdplist1[[var]]
    aggr <- pdp %>%
      group_by(.data[[var]]) %>%
      summarise(fit = mean(fit))
    pdp1 <- filter(pdp, .id %in% sice)
    if (is.null(colorVar)) {
      p <- pdp1 %>%
        ggplot(aes(x = .data[[var]], y = fit)) +
        geom_line(aes(color = predData, group = .id)) +
        scale_color_gradientn(
          name = "yhat", colors = pal, limits = limits, oob = scales::squish
        )
    } else {
      p <- pdp1 %>%
        ggplot(aes(x = .data[[var]], y = fit)) +
        geom_line(aes(color = .data[[colorVar]], group = .id))
    }

    p <- p +
      geom_line(data = aggr, size = 1, color = "black", lineend = "round", group = 1) +
      theme_bw() + guides(fill = FALSE, color = FALSE) + ylab("   ") + ylim(limits)
    if (var == vars[[1]]) p <- p + ylab("pdp/ice")
    p
  }


  plots <- lapply(vars, ice)
  if (!is.null(colorVar)) {
    legend_y <- lemon::g_legend(plots[[1]] + guides(color = "legend"))
  } else {
    legend_y <- lemon::g_legend(plots[[1]] + guides(color = "colorbar"))
  }
  plots <- c(plots, list(legend_y))
  if (draw) {
    gridExtra::grid.arrange(grobs = plots, widths = c(rep(1, length(vars)), .4))
  }

  invisible(plots)
}

