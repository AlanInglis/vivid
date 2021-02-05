#' pdpPairs
#'
#' @description Creates a pairs plot showing bivariate pdp on upper diagonal, ice/pdp on the diagonal and data on the lower diagonal
#'
#' @param data Data frame used for fit.
#' @param fit A supervised machine learning model, which understands condvis2::CVpredict
#' @param response The name of the response for the fit.
#' @param vars The variables to plot (and their order), defaults to all variables other than response.
#' @param pal A vector of colors to show predictions, for use with scale_fill_gradientn
#' @param fitlims Specifies the fit range for the color map. Options are a numeric vector of length 2,
#'  "pdp" (default), in which cases limits are calculated from the pdp, or "all", when limits are calculated from the observations and pdp.
#'  Predictions outside fitlims are squished on the color scale.
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param nmax Uses sample of nmax data rows for the pdp. Use all rows if NULL.
#' @param class Category for classification, a factor level, or a number indicating which factor level.
#' @param nIce Number of ice curves to be plotted, defaults to 30.
#' @param comboImage If TRUE  draws pdp for mixed variable plots as an image, otherwise an interaction plot.
#' @param predictFun Function of (fit, data) to extract numeric predictions from fit. Uses condvis2::CVpredict by default, which works for many fit classes.
#' @return A matrix of values.
#'
#' @importFrom condvis2 "CVpredict"
#' @importFrom dplyr "bind_rows"
#' @importFrom dplyr "filter"
#'
#' @examples
#' # Load in the data:
#' aq <- na.omit(airquality)
#' f <- lm(Ozone ~ ., data = aq)
#' pdpPairs(aq, f, "Ozone")
#' \dontrun{
#' # Run an mlr ranger model:
#' library(mlr3)
#' library(mlr3learners)
#' library(ranger)
#' library(MASS)
#' Boston1 <- Boston[, c(4:6, 8, 13:14)]
#' Boston1$chas <- factor(Boston1$chas)
#' task <- TaskRegr$new(id = "Boston1", backend = Boston1, target = "medv")
#' learner <- lrn("regr.ranger", importance = "permutation")
#' fit <- learner$train(task)
#' pdpPairs(Boston1[1:30, ], fit, "medv")
#' pdpPairs(Boston1[1:30, ], fit, "medv", comboImage = TRUE)
#' viv <- vivi(Boston1, fit, "medv")
#' # show top variables only
#' pdpPairs(Boston1[1:30, ], fit, "medv", comboImage = TRUE, vars = rownames(viv)[1:4])
#' }
#' \dontrun{
#' library(ranger)
#' rf <- ranger(Species ~ ., data = iris, probability = TRUE)
#' pdpPairs(iris, rf, "Species") # prediction probs for first class, setosa
#' pdpPairs(iris, rf, "Species", class = "versicolor") # prediction probs versicolor
#' # and with mlr3
#' task <- TaskClassif$new(id = "iris", backend = iris, target = "Species")
#' fitm <- lrn("classif.ranger", importance = "permutation", predict_type = "prob")$train(task)
#' pdpPairs(iris, fitm, "Species", class = "versicolor")
#' }
#' @importFrom GGally "ggpairs"
#' @import ggplot2
#' @importFrom dplyr "summarise"
#' @importFrom dplyr "group_by"
#' @importFrom dplyr "%>%"
#' @importFrom GGally "eval_data_col"
#' @importFrom dplyr "bind_rows"
#' @export





pdpPairs <- function(data,
                     fit,
                     response,
                     vars = NULL,
                     pal = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
                     fitlims = "pdp",
                     gridSize = 10,
                     nmax = 500,
                     class = 1,
                     nIce = 30,
                     comboImage = FALSE,
                     predictFun = NULL) {


  data <- na.omit(data)
  if (is.null(nmax)) nmax <- nrow(data)
  nmax <- max(5, nmax)
  if (is.numeric(nmax) && nmax < nrow(data)) {
    data <- data[sample(nrow(data), nmax), , drop = FALSE]
  }
  gridSize <- min(gridSize, nmax)

  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  if (is.null(predictFun)) predictFun <- CVpredictfun(classif, class)

  predData <- predictFun(fit, data)

  vars0 <- names(data)
  vars0 <- vars0[-match(response, vars0)]
  vars <- vars[vars %in% vars0]
  if (is.null(vars)) vars <- vars0

  nIce <- min(nIce, nrow(data))
  sice <- c(NA, sample(nrow(data), nIce)) # for use with iceplots

  # loop through vars and create a list of pdps

  message("Generating ice/pdp fits... waiting...")

  data$predData <- predData
  pdplist1 <- vector("list", length = length(vars))
  for (i in 1:length(vars)) {
    px <- pdp_data(data, vars[i], gridsize = gridSize)
    px$.pid <- i
    pdplist1[[i]] <- px
  }
  pdplist1 <- bind_rows(pdplist1)
  pdplist1$fit <- predictFun(fit, pdplist1)
  pdplist1 <- split(pdplist1, pdplist1$.pid)

  names(pdplist1) <- vars

  # Get names for pairs of variables

  xyvar <- expand.grid(1:length(vars), 1:length(vars))[, 2:1]
  xyvar <- as.matrix(xyvar[xyvar[, 1] < xyvar[, 2], ])
  xyvarn <- cbind(vars[xyvar[, 1]], vars[xyvar[, 2]])


  # loop through vars and create a list of pdps for each pair

  pdplist <- vector("list", length = nrow(xyvarn))
  for (i in 1:nrow(xyvarn)) {
    px <- pdp_data(data, xyvarn[i, ], gridsize = gridSize)
    px$.pid <- i
    pdplist[[i]] <- px
  }

  pdplist <- bind_rows(pdplist)
  pdplist$fit <- predictFun(fit, pdplist)
  pdplist <- split(pdplist, pdplist$.pid)

  for (i in 1:nrow(xyvarn)) {
    pdplist[[i]] <- pdplist[[i]] %>%
      group_by(.data[[xyvarn[i, 1]]], .data[[xyvarn[i, 2]]]) %>%
      summarise(fit = mean(fit))
  }
  names(pdplist) <- paste(xyvarn[, 2], xyvarn[, 1], sep = "pp")

  message("Finished ice/pdp")

  # Set limits for pairs
  if (fitlims == "all") {
    r <- sapply(pdplist, function(x) range(x$fit))
    r <- range(c(r, r1, predData))
    limits <- range(labeling::rpretty(r[1], r[2]))
  }
  else if (fitlims == "pdp") {
    r <- sapply(pdplist, function(x) range(x$fit))
    r <- range(r)
    limits <- range(labeling::rpretty(r[1], r[2]))
  } else {
    limits <- fitlims
  }


  pdpnn <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist[[paste(vars[1], vars[2], sep = "pp")]]
    ggplot(data = pdp, aes(x = .data[[vars[1]]], y = .data[[vars[2]]])) +
      geom_tile(aes(fill = fit)) +
      scale_fill_gradientn(name = "\u0177", colors = pal, limits = limits, oob = scales::squish)
  }

  pdpc <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist[[paste(vars[1], vars[2], sep = "pp")]]
    if (is.factor(pdp[[vars[1]]])) vars <- rev(vars)
    ggplot(data = pdp, aes(x = .data[[vars[1]]], y = fit, color = .data[[vars[2]]])) +
      geom_line()
  }

  ice <- function(data, mapping, ...) {
    var <- quo_name(mapping$x)
    pdp <- pdplist1[[var]]
    aggr <- pdp %>%
      group_by(.data[[var]]) %>%
      summarise(fit = mean(fit))

    filter(pdp, .id %in% sice) %>%
      ggplot(aes(x = .data[[var]], y = fit)) +
      geom_line(aes(color = predData, group = .id)) +
      scale_color_gradientn(name = "\u0177", colors = pal, limits = limits, oob = scales::squish) +
      geom_line(data = aggr, size = 1, color = "black", lineend = "round", group = 1)
  }

  dplotn <- function(data, mapping) {
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    df <- data.frame(x = x, y = y)
    ggplot(df, aes(x = x, y = y, color = predData)) +
      geom_point(shape = 16, size = 1, show.legend = FALSE) +
      scale_colour_gradientn(name = "\u0177", colors = pal, limits = limits, oob = scales::squish)
  }

  dplotm <- function(data, mapping) {
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    df <- data.frame(x = x, y = y)
    jitterx <- if (is.factor(df$x)) .25 else 0
    jittery <- if (is.factor(df$y)) .25 else 0

    ggplot(df, aes(x = x, y = y, color = predData)) +
      geom_jitter(shape = 16, size = 1, show.legend = FALSE, width = jitterx, height = jittery) +
      scale_colour_gradientn(name = "\u0177", colors = pal, limits = limits, oob = scales::squish)
  }


  wlegend <- 1

  p <- ggpairs(data[vars],
    upper = list(continuous = pdpnn, combo = if (comboImage) pdpnn else pdpc, discrete = pdpnn),
    diag = list(continuous = ice, discrete = ice),
    lower = list(continuous = dplotn, combo = dplotm, discrete = dplotm),
    legend = wlegend,
    cardinality_threshold = NULL
  ) +
    theme_bw() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.line = element_line(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 0),
      axis.text.y = element_text(size = 0),
      strip.text = element_text(face = "bold", colour = "red", size = 5)
    )


  suppressMessages(print(p))
  invisible(p)
}




pdp_data <- function(d, var, gridsize = 30) {
  if (length(var) == 1) {
    pdpvar <- d[[var]]
    if (is.factor(pdpvar)) {
      gridvals <- levels(pdpvar)
    } else {
      gridvals <- seq(min(pdpvar, na.rm = T), max(pdpvar, na.rm = T), length.out = gridsize)
    }
    dnew <- do.call(rbind, lapply(gridvals, function(i) {
      d1 <- d
      d1[[var]] <- i
      d1
    }))
    if (is.factor(pdpvar)) dnew[[var]] <- factor(dnew[[var]], levels = levels(pdpvar))
  }
  else {
    pdpvar1 <- d[[var[1]]]
    pdpvar2 <- d[[var[2]]]
    if (is.factor(pdpvar1)) {
      gridvals1 <- levels(pdpvar1)
    } else {
      gridvals1 <- seq(min(pdpvar1, na.rm = T), max(pdpvar1, na.rm = T), length.out = gridsize)
    }
    if (is.factor(pdpvar2)) {
      gridvals2 <- levels(pdpvar2)
    } else {
      gridvals2 <- seq(min(pdpvar2, na.rm = T), max(pdpvar2, na.rm = T), length.out = gridsize)
    }
    gridvals <- expand.grid(gridvals1, gridvals2)

    dnew <- do.call(rbind, lapply(1:nrow(gridvals), function(i) {
      d1 <- d
      d1[[var[1]]] <- gridvals[i, 1]
      d1[[var[2]]] <- gridvals[i, 2]
      d1
    }))
    if (is.factor(pdpvar1)) dnew[[var[1]]] <- factor(dnew[[var[1]]], levels = levels(pdpvar1))
    if (is.factor(pdpvar2)) dnew[[var[2]]] <- factor(dnew[[var[2]]], levels = levels(pdpvar2))
  }
  dnew$.id <- 1:nrow(d)
  rownames(dnew) <- NULL
  dnew
}
