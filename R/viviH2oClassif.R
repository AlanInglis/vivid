vividH2oClassif <- function(data,
                            fit,
                            response,
                            gridSize = 50,
                            metric = list(rmse = MetricsWeighted::rmse),
                            importanceType = c("permutation", "Geoden"),
                            nmax = NULL,
                            reorder = TRUE,
                            class = 1
                            ) {

  # make predict function
  pred_fun <- function(mod, X) as.vector(unlist(h2o.predict(mod, as.h2o(X))[,class]))

  # make flashlight object
  flObj <- flashlight(
    model = fit, data = data, y = response, label = "",
    predict_function = pred_fun
  )

  ## Importance:
  if (importanceType == "permutation") {
    print("perm")
    imp <- vividH2oVimpClassif(fit, data, response, metric = metric, class = class)
    importance <- imp$data[, 3:4]
    importance <- setNames(importance$value, as.character(importance$variable))
  } else if (importanceType == "Geoden") {
    print("Geoden")
    importance <- h2o.varimp(fit)
    importance <- setNames(importance$relative_importance, importance$variable)
  }

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




# Reorder -----------------------------------------------------------------


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


# Importance --------------------------------------------------------------


vividH2oVimpClassif <- function(fit,
                           data,
                           response,
                           metric = list(rmse = MetricsWeighted::rmse),
                           class) {

  # extract flashlight importance
  data$DUMMY <- response == class


  pred_fun_imp <- function(mod, X, what = class) {
    out <- h2o.predict(mod, as.h2o(X))
    return(as.vector(out[, what]))
  }

  fl_imp <- flashlight(model = fit,
                       data = data,
                       y = "DUMMY",
                       label = "",
                       metrics = metric,
                       predict_function = pred_fun_imp)

  ovars <- data[, !(names(data) %in% response)]
  howMany <- length(ovars)
  ovars <- ovars[-howMany]
  ovars <- colnames(ovars)

  imp <- light_importance(fl_imp, v = ovars, lower_is_better = FALSE, type = "permutation")

  return(imp)
}


# example -----------------------------------------------------------------
# iris_hf <- as.h2o(iris)
# iris_dl <- h2o.deeplearning(x = 1:4, y = "Species", training_frame = iris_hf, seed=123456)
#
# vi <- vividH2oClassif(iris, iris_dl, "Species", gridSize = 5, class = "setosa")
# viviHeatmap(vi)




