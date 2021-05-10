vividH2o <- function(data,
                     fit,
                     response,
                     gridSize = 50,
                     metric = list(rmse = MetricsWeighted::rmse),
                     importanceType = c("permutation", "Geoden"),
                     nmax = NULL,
                     reorder = TRUE,
                     class = 1,
                     predictFun = NULL) {

  # make predict function
  pred_fun <- function(mod, X) as.vector(unlist(h2o.predict(mod, as.h2o(X))))

  # make flashlight object
  flObj <- flashlight(
    metrics = metric,
    model = fit, data = data, y = response, label = "",
    predict_function = pred_fun
  )

  ## Importance:
  if (importanceType == "permutation") {
    print("perm")
    imp <- vividH2oVarImp(fit, data, response, metric = metric)
    importance <- imp$data[, 3:4]
    importance <- setNames(importance$value, as.character(importance$variable))
  } else if (importanceType == "Geoden") {
    print("Geoden")
    importance <- h2o.varimp(fit)
    importance <- setNames(importance$relative_importance, importance$variable)
  }

  ## Interactions

  # get names
  ovars <- data[, !(names(data) %in% response)]
  ovars <- colnames(ovars)

  if (is.null(nmax)) {
    nmax <- nrow(data)
  }

  res <- light_interaction(flObj,
    type = "H",
    pairwise = TRUE,
    grid_size = gridSize,
    normalize = F,
    n_max = nmax
  )$data

  # reorder
  res[["variable"]] <- reorder(res[["variable"]], res[["value"]])

  # create matrix of values
  vars2 <- t(simplify2array(strsplit(as.character(res[["variable"]]), ":"))) # split/get feature names
  mat <- matrix(0, length(ovars), length(ovars)) # create matrix
  rownames(mat) <- colnames(mat) <- ovars # set names
  mat[vars2] <- res[["value"]] # set values
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]

  newimp <- vector("numeric", length = ncol(data) - 1)
  names(newimp) <- names(data)[-match(response, names(data))]
  newimp[names(importance)] <- importance
  diag(mat) <- newimp # set diagonal to equal vImps



  # -------------------------------------------------------------------------

  vividReorder <- function(d) {
    vImp <- diag(d)
    rvImp <- range(vImp)
    if (rvImp[2] != rvImp[1]) {
      vImp <- (vImp - rvImp[1]) / (rvImp[2] - rvImp[1])
    }
    vInt <- as.dist(d)
    rvInt <- range(vInt)
    if (rvInt[2] != rvInt[1]) {
      vInt <- (vInt - rvInt[1]) / (rvInt[2] - rvInt[1])
    }
    score <- apply(as.matrix(vInt), 1, max) + vImp
    o <- DendSer::dser(-vInt, -score, cost = DendSer::costLS)
    res <- d[o, o]
    res
  }
  # reorder
  if (reorder) {
    viviMatrix <- vividReorder(mat)
  } else {
    viviMatrix <- mat
  }

  class(viviMatrix) <- c("vivid", class(viviMatrix))
  return(viviMatrix)
}



vividH2oVarImp <- function(fit,
                           data,
                           response,
                           metric = list(rmse = MetricsWeighted::rmse)) {


  pred_fun <- function(mod, X) as.vector(unlist(h2o.predict(mod, as.h2o(X))))

  flObj <- flashlight(
    metrics = metric,
    model = fit,
    data = data,
    y = response,
    label = "",
    predict_function = pred_fun
  )

  ovars <- data[, !(names(data) %in% response)]
  ovars <- colnames(ovars)

  imp <- light_importance(flObj, v = ovars, type = "permutation")
  return(imp)
}
