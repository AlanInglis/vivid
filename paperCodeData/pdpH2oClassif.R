pdpPairsH2O <- function(data,
                        fit,
                        response,
                        vars = NULL,
                        pal = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
                        fitlims = "pdp",
                        gridSize = 10,
                        nmax = 500,
                        class = 1,
                        className = NULL,
                        nIce = NULL,
                        comboImage = FALSE,
                        predictFun = NULL,
                        convexHull = FALSE) {

  data <- na.omit(data)
  if (is.null(nmax)) nmax <- nrow(data)
  nmax <- max(5, nmax)
  if (is.numeric(nmax) && nmax < nrow(data)) {
    data <- data[sample(nrow(data), nmax), , drop = FALSE]
  }
  gridSize <- min(gridSize, nmax)

  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  if (is.null(predictFun)) predictFun <- CVpredictfun(classif, class)

  predData <- h2o.predict(fit, as.h2o(data)) ## ADD
  predData <- predData[,c(className)] # ADD for class



  vars0 <- names(data)
  vars0 <- vars0[-match(response, vars0)]
  vars <- vars[vars %in% vars0]
  if (is.null(vars)) vars <- vars0

  if(!is.null(nIce)){
    sice <- c(NA, min(nIce):max(nIce))
  }else{
    nIce <- 30
    nIce <- min(nIce, nrow(data))
    sice <- c(NA, sample(nrow(data), nIce)) # for use with iceplots
  }

  # loop through vars and create a list of pdps

  message("Generating ice/pdp fits... waiting...")


  # getting predicitons from h2o object
  pData <- as.data.frame(predData) # ADD
  data$predData <- pData # ADD


  pdplist1 <- vector("list", length = length(vars))
  for (i in 1:length(vars)) {
    px <- pdp_data(data, vars[i], gridsize = gridSize)
    px$.pid <- i
    pdplist1[[i]] <- px
  }
  pdplist1 <- bind_rows(pdplist1)
  pdpPred <- pdplist1[25] # ADD
  pdpPred <- pdpPred$predData$Cancer # DIRECT REFERENCE
  pdplist1 <- pdplist1[,-25] # ADD
  pdplist1$predData <- pdpPred

  pData_1 <- h2o.predict(fit, as.h2o(pdplist1)) # ADD
  pData_1 <- pData_1[,class] # ADD for class
  pData_1 <- as.data.frame(pData_1) # ADD
  pdplist1$fit <- pData_1$Cancer # DIRECT REFERENCE

  #pdplist1$fit <- predictFun(fit, as.h2o(pdplist1)) # ADD
  pdplist1 <- split(pdplist1, pdplist1$.pid)

  names(pdplist1) <- vars

  # Get names for pairs of variables

  xyvar <- expand.grid(1:length(vars), 1:length(vars))[, 2:1]
  xyvar <- as.matrix(xyvar[xyvar[, 1] < xyvar[, 2], ])
  xyvarn <- cbind(vars[xyvar[, 1]], vars[xyvar[, 2]])


  # loop through vars and create a list of pdps for each pair

  pdplist <- vector("list", length = nrow(xyvarn))
  for (i in 1:nrow(xyvarn)) {
    px <- pdp_data(data, xyvarn[i, ], gridsize = gridSize, convexHull = convexHull)
    px$.pid <- i
    pdplist[[i]] <- px
  }

  pdplist <- bind_rows(pdplist)

  pdpPred_2 <- pdplist[25] # ADD
  pdpPred_2 <- pdpPred_2$predData$Cancer # DIRECT REFERENCE
  pdplist <- pdplist[,-25] # ADD
  pdplist$predData <- pdpPred_2 # ADD

  pData_2 <- h2o.predict(fit, as.h2o(pdplist)) # ADD
  pData_2 <- pData_2[,class] # ADD
  pData_2 <- as.data.frame(pData_2) # ADD
  pdplist$fit <- pData_2$Cancer # DIRECT REFERENCE
  #pdplist$fit <- predictFun(fit, as.h2o(pdplist)) # ADD
  pdplist <- split(pdplist, pdplist$.pid)

  for (i in 1:nrow(xyvarn)) {
    pdplist[[i]] <- pdplist[[i]] %>%
      group_by(.data[[xyvarn[i, 1]]], .data[[xyvarn[i, 2]]]) %>%
      summarise(fit = mean(fit))
  }
  names(pdplist) <- paste(xyvarn[, 2], xyvarn[, 1], sep = "pp")

  message("Finished ice/pdp")

  # Set limits for pairs
  if (fitlims[1] == "all") {
    r <- sapply(pdplist, function(x) range(x$fit))
    r <- range(c(r, predData))
    limits <- range(labeling::rpretty(r[1], r[2]))
  } else if (fitlims[1] == "pdp") {
    r <- sapply(pdplist, function(x) range(x$fit))
    r <- range(r)
    limits <- range(labeling::rpretty(r[1], r[2]))
  } else {
    limits <- fitlims
  }


  pdTest <- data$predData$Cancer # ADD
  data <- dplyr::select(data, - predData) # ADD
  data$predData <- pdTest # ADD




  pdpnn <- function(data, mapping, ...) {
    vars <- c(quo_name(mapping$x), quo_name(mapping$y))
    pdp <- pdplist[[paste(vars[1], vars[2], sep = "pp")]]
    ggplot(data = pdp, aes(x = .data[[vars[1]]], y = .data[[vars[2]]])) +
      geom_tile(aes(fill = fit)) +
      scale_fill_gradientn(name = "y-hat", colors = pal, limits = limits, oob = scales::squish)
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

    filter(pdp, .data[[".id"]] %in% sice) %>%
      ggplot(aes(x = .data[[var]], y = fit)) +
      geom_line(aes(color = .data$predData, group = .data[[".id"]])) + # ADD .data$predData
      scale_color_gradientn(
        name = "y-hat", colors = pal, limits = limits, oob = scales::squish,
        guide = guide_colorbar(
          frame.colour = "black",
          ticks.colour = "black"
        )
      ) +
      geom_line(data = aggr, size = 1, color = "black", lineend = "round", group = 1)
  }


  dplotn <- function(data, mapping, pred = pdTest) { # ADD pred = pdTest
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    df <- data.frame(x = x, y = y, pred = pred) # ADD pre = pred
    ggplot(df, aes(x = x, y = y, color = pred)) + # ADD color = pred
      geom_point(shape = 16, size = 1, show.legend = FALSE) +
      scale_colour_gradientn(name = "y-hat", colors = pal, limits = limits, oob = scales::squish)
  }

  dplotm <- function(data, mapping,  pred = pdTest) { # ADD pred = pdTest
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)
    df <- data.frame(x = x, y = y, pred = pred) # ADD pre = pred
    jitterx <- if (is.factor(df$x)) .25 else 0
    jittery <- if (is.factor(df$y)) .25 else 0

    ggplot(df, aes(x = x, y = y, color = pred)) + # ADD color = pred
      geom_jitter(shape = 16, size = 1, show.legend = FALSE, width = jitterx, height = jittery) +
      scale_colour_gradientn(name = "y-hat", colors = pal, limits = limits, oob = scales::squish)
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
      # axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6),
      strip.text = element_text(face = "bold", colour = "red", size = 7)
    )


  suppressMessages(print(p))
  invisible(p)
}


# pdp_data ---------------------------------------------------------------------


pdp_data <- function(d, var, gridsize = 30, convexHull = FALSE) {
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
    if (is.factor(pdpvar)) dnew[[var]] <- factor(dnew[[var]], levels = levels(pdpvar), ordered = is.ordered(pdpvar))
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

    if (convexHull) {
      if (is.factor(pdpvar1) && is.factor(pdpvar2)) {
        t <- table(pdpvar1, pdpvar2)
        w <- sapply(1:nrow(gridvals), function(i) t[gridvals[i, 1], gridvals[i, 2]] == 0)
        gridvals <- gridvals[!w, ]
      } else if (is.factor(pdpvar1) && is.numeric(pdpvar2)) {
        rangeData <- tapply(pdpvar2, pdpvar1, range)
        w <- sapply(1:nrow(gridvals), function(i) {
          r <- rangeData[[as.character(gridvals[i, 1])]]
          gridvals[i, 2] >= r[1] && gridvals[i, 2] <= r[2]
        })

        gridvals <- gridvals[w, ]
      } else if (is.numeric(pdpvar1) && is.factor(pdpvar2)) {
        rangeData <- tapply(pdpvar1, pdpvar2, range)
        w <- sapply(1:nrow(gridvals), function(i) {
          r <- rangeData[[as.character(gridvals[i, 2])]]
          gridvals[i, 1] >= r[1] && gridvals[i, 1] <= r[2]
        })

        gridvals <- gridvals[w, ]
      } else {
        hpts <- chull(pdpvar1, pdpvar2) # calc CHull
        hpts <- c(hpts, hpts[1]) # close polygon
        pdpvar1CH <- pdpvar1[hpts] # get x-coords of polygon
        pdpvar2CH <- pdpvar2[hpts] # get y-coords of polygon

        # find which are outside convex hull
        res <- sp::point.in.polygon(gridvals$Var1, gridvals$Var2, pdpvar1CH, pdpvar2CH) != 0

        # remove points outside convex hull
        gridvals <- gridvals[res, ]
      }
    }

    dnew <- do.call(rbind, lapply(1:nrow(gridvals), function(i) {
      d1 <- d
      d1[[var[1]]] <- gridvals[i, 1]
      d1[[var[2]]] <- gridvals[i, 2]
      d1
    }))
    if (is.factor(pdpvar1)) dnew[[var[1]]] <- factor(dnew[[var[1]]], levels = levels(pdpvar1), ordered = is.ordered(pdpvar1))
    if (is.factor(pdpvar2)) dnew[[var[2]]] <- factor(dnew[[var[2]]], levels = levels(pdpvar2), ordered = is.ordered(pdpvar2))
  }
  dnew$.id <- 1:nrow(d)
  rownames(dnew) <- NULL
  dnew
}










