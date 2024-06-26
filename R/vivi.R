#' vivi
#'
#' @description Creates a matrix displaying variable importance on the diagonal
#'  and variable interaction on the off-diagonal.
#'
#' @details  If the argument `importanceType = 'agnostic'`, then an agnostic permutation importance (1) is calculated.
#' Friedman's H statistic (2) is used for measuring the interactions. This measure is based on partial dependence curves
#' and relates the interaction strength of a pair of variables to the total effect strength of that variable pair.
#'
#' @references 1: Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful: Variable Importance for Black-Box, Proprietary, or Misspecified Prediction Models, using Model Class Reliance. Arxiv.
#' @references 2: Friedman, J. H. and Popescu, B. E. (2008). “Predictive learning via rule ensembles.” The Annals of Applied Statistics. JSTOR, 916–54.
#'
#' @param fit A supervised machine learning model, which understands condvis2::CVpredict
#' @param data Data frame used for fit.
#' @param response The name of the response for the fit.
#' @param vars A vector of variable names to be assessed.
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param importanceType  Used to select the importance metric. By default, an agnostic importance
#' measure is used. If an embedded metric is available, then setting this argument
#' to the importance metric will use the selected importance values in the vivid-matrix.
#' Please refer to the examples given for illustration.
#' Alternatively, set to equal "agnostic" (the default) to override embedded importance measures and
#' return agnostic importance values.
#' @param nmax Maximum number of data rows to consider. Default is 500. Use all rows if NULL.
#' @param reorder If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances.
#' @param class Category for classification, a factor level, or a number indicating which factor level.
#' @param predictFun Function of (fit, data) to extract numeric predictions from fit. Uses condvis2::CVpredict by default, which works for many fit classes.
#' @param normalized Should Friedman's H-statistic be normalized or not. Default is FALSE.
#' @param numPerm Number of permutations to perform for agnostic importance. Default is 4.
#' @param showVimpError Logical. If TRUE, and `numPerm > 1` then a tibble containing the variable names, their importance values,
#' and the standard error for each importance is printed to the console.
#' @return A matrix of interaction values, with importance on the diagonal.
#'
#' @importFrom flashlight flashlight
#' @importFrom flashlight light_importance
#' @importFrom flashlight light_interaction
#' @importFrom condvis2 CVpredict
#' @importFrom stats reorder
#' @importFrom stats setNames
#'
#' @examples
#'
#' aq <- na.omit(airquality)
#' f <- lm(Ozone ~ ., data = aq)
#' m <- vivi(fit = f, data = aq, response = "Ozone") # as expected all interactions are zero
#' viviHeatmap(m)
#'
#' # Select importance metric
#' library(randomForest)
#' rf1 <- randomForest(Ozone~., data = aq, importance = TRUE)
#' m2 <- vivi(fit = rf1, data = aq, response = 'Ozone',
#'            importanceType = '%IncMSE') # select %IncMSE as the importance measure
#' viviHeatmap(m2)
#'
#' \donttest{
#' library(ranger)
#' rf <- ranger(Species ~ ., data = iris, importance = "impurity", probability = TRUE)
#' vivi(fit = rf, data = iris, response = "Species") # returns agnostic importance
#' vivi(fit = rf, data = iris, response = "Species",
#'      importanceType = "impurity") # returns selected 'impurity' importance.
#'}
#' @export

# Main vivi function ------------------------------------------------------

vivi <- function(data,
                 fit,
                 response,
                 gridSize = 50,
                 importanceType = "agnostic",
                 nmax = 500,
                 reorder = TRUE,
                 class = 1,
                 predictFun = NULL,
                 normalized = FALSE,
                 numPerm = 4,
                 showVimpError = FALSE,
                 vars = NULL) {

  # check for predict function
  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  if (is.null(predictFun)) {
    pFun <- CVpredictfun(classif, class)
  } else {
    pFun <- predictFun
  }

  # Call the importance function
  if (!is.null(importanceType) && importanceType == "agnostic") {
    if (is.null(predictFun) & classif) {
      data1 <- data
      if (is.factor(data[[response]]) & is.numeric(class)) class <- levels(data[[response]])[class]
      data1[[response]] <- as.numeric(data1[[response]] == class)
      pFun1 <- CVpredictfun(TRUE, class)
    } else {
      data1 <- data
      pFun1 <- pFun
    }
    vImp <- vividImportance.default(
      data = data1,
      fit = fit,
      response = response,
      importanceType = importanceType,
      predictFun = pFun1,
      numPerm = numPerm,
      showVimpError = showVimpError,
      vars = vars
    )
  } else {
    vImp <- vividImportance(
      data = data,
      fit = fit,
      response = response,
      importanceType = importanceType,
      predictFun = pFun,
      numPerm = numPerm,
      showVimpError = showVimpError,
      vars = vars
    )
  }
  vImp1 <- vector("numeric", length = ncol(data) - 1)
  vImp1[] <- NA
  names(vImp1) <- names(data)[-match(response, names(data))]
  vImp1[names(vImp)] <- vImp


  # Call the interaction function
  vInt <- vividInteraction(
    data = data,
    fit = fit,
    response = response,
    interactionType = NULL,
    nmax = nmax,
    gridSize = gridSize,
    predictFun = pFun,
    normalized = normalized,
    vars = vars
  )

  viviUpdate(vInt, vImp1, reorder = reorder)
}



# vividReorder ------------------------------------------------------------

#' vividReorder
#' @description Reorders a square matrix so that values of high importance and
#' interaction strength are pushed to the top left of the matrix.
#' @param d A matrix such as that returned by vivi
#' @name vividReorder
#' @return A reordered version of d.
#' @importFrom DendSer dser
#' @importFrom DendSer costLS
#' @export
#'
#' @examples
#' f <- lm(Sepal.Length ~ ., data = iris[, -5])
#' m <- vivi(fit = f, data = iris[, -5], response = "Sepal.Length")
#' corimp <- abs(cor(iris[, -5])[1, -1])
#' viviUpdate(m, corimp) # use correlation as importance and reorder
#'
vividReorder <- function(d) {
  d1 <- d
  d1[is.na(d)] <- 0
  vImp <- diag(d1)
  rvImp <- range(vImp)
  if (rvImp[2] != rvImp[1]) {
    vImp <- (vImp - rvImp[1]) / (rvImp[2] - rvImp[1])
  }
  vInt <- as.dist(d1)
  rvInt <- range(vInt)
  if (rvInt[2] != rvInt[1]) {
    vInt <- (vInt - rvInt[1]) / (rvInt[2] - rvInt[1])
  }
  score <- apply(as.matrix(vInt), 1, max) + vImp
  o <- DendSer::dser(-vInt, -score, cost = DendSer::costLS)
  res <- d[o, o]
  class(res) <- class(d)
  res
}




# Variable Importance Measures: -------------------------------------------

# vividImportance ---------------------------------------------------------

# Main vImp function:
vividImportance <- function(fit, data, response = NULL,
                            importanceType = NULL,
                            predictFun = NULL,
                            numPerm = 4,
                            showVimpError = FALSE,
                            vars = NULL,
                            ...) {
  if (importanceType == "agnostic") {
    vividImportance.default(fit, data, response = response,
                            predictFun = predictFun,
                            numPerm = 4,
                            showVimpError = FALSE,
                            vars = NULL,
                            ...)
  } else {
    UseMethod("vividImportance", fit)
  }
}




# Default flashlight ------------------------------------------------------
#' @export
#' @method vividImportance default
vividImportance.default <- function(fit,
                                    data,
                                    response = NULL,
                                    importanceType = NULL,
                                    predictFun = NULL,
                                    numPerm = 4,
                                    showVimpError = FALSE,
                                    vars = NULL,
                                    ...) {
  message("Agnostic variable importance method used.")


  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")


  # create flashlight
  fl <- flashlight(
    model = fit, data = data, y = response, label = "",
    predict_function = function(fit, data) predictFun(fit, data, prob = TRUE)
  )


  # Extract importance
  suppressWarnings(
    imp <- light_importance(fl, m_repetitions = numPerm, v = vars)
  )
  # Column names and order in imp$data:
  #   label  metric  variable  value  error  (flashlight < 1.0.0)
  #   label_ metric_ variable_ value_ error_ (flashlight >=1.0.0)
  impDf <- imp$data[, 3:5]
  names(impDf) <- c('Variable', 'Importance', 'Std_Error')

  if(showVimpError) {
    print(impDf)
  }

  if (any(is.nan(impDf$Importance))) {
    impDf$Importance <- 1
    message("Flashlight importance works for numeric and numeric binary response only; setting importance to 1.")
  }
  importance <- setNames(impDf$Importance, as.character(impDf$Variable)) # turn into named vector

  return(importance)
}



# ranger ----------------------------------------------------------------
#' @export
#' @method vividImportance ranger
vividImportance.ranger <- function(fit,
                                   data,
                                   response = NULL,
                                   importanceType = NULL,
                                   predictFun = NULL,
                                   numPerm = 4,
                                   showVimpError = FALSE,
                                   vars = NULL,
                                   ...) {
  # If no importance mode selected, then default! Else, extract importance type
  if (fit$importance.mode == "none") {
    message("No variable importance mode selected.")
    vividImportance.default(fit, data, response, importanceType, predictFun = predictFun)
  } else if (fit$importance.mode == "permutation") {
    importance <- fit$variable.importance
    message("Embedded permutation variable importance method used.")
    return(importance)
  } else if (fit$importance.mode == "impurity") {
    importance <- fit$variable.importance
    message("Embedded impurity variable importance method used.")
    return(importance)
  } else if (fit$importance.mode == "impurity_corrected") {
    importance <- fit$variable.importance
    message("Embedded impurity_corrected variable importance method used.")
    return(importance)
  }
}



# randomForest ------------------------------------------------------------
#' @export
#' @method vividImportance randomForest
vividImportance.randomForest <- function(fit,
                                         data,
                                         response = NULL,
                                         importanceType = NULL,
                                         predictFun = NULL,
                                         numPerm = 4,
                                         showVimpError = FALSE,
                                         vars = NULL,
                                         ...) {
  fitImp <- dim(fit$importance)
  importanceData <- randomForest::importance(fit, scale = FALSE)
  st <- colnames(importanceData)

  if (fit$type == "classification") {
    if (all(st != importanceType) && !is.null(importanceType)) {
      message("Stated importanceType not found. Returning MeanDecreaseGini importance values")
      importance <- importanceData[, "MeanDecreaseGini"]
    } else {
      if (!is.null(importanceType)) {
        importance <- importanceData[, importanceType]
        message(importanceType, " importance selected.")
      } else if (is.null(importanceType) && fitImp[2] != 1) {
        message("No importanceType selected. Returning MeanDecreaseGini importance values")
        importance <- importanceData[, "MeanDecreaseGini"]
      } else {
        message("No importanceType selected. Returning MeanDecreaseGini importance values")
        importance <- importanceData[, "MeanDecreaseGini"]
      }
    }
  }




  if (fit$type == "regression") {
    if (!is.null(importanceType)) {
      importance <- importanceData[, importanceType]
      message(importanceType, " importance selected.")
    } else if (is.null(importanceType) && fitImp[2] != 1) {
      message("No importanceType selected. Returning IncNodePurity importance values")
      importance <- importanceData[, "IncNodePurity"]
    } else {
      message("No importanceType selected. Returning IncNodePurity importance values")
      importance <- importanceData[, "IncNodePurity"]
    }
  }

  return(importance)
}


# mlr3 learner ------------------------------------------------------------
#' @export
#' @method vividImportance Learner
vividImportance.Learner <- function(fit,
                                    data,
                                    response = NULL,
                                    importanceType = NULL,
                                    predictFun = NULL,
                                    numPerm = 4,
                                    showVimpError = FALSE,
                                    vars = NULL,
                                    ...) {
  # if no importance mode selected, use default
  if (fit$packages == "ranger" && fit$model$importance.mode == "none") {
    message("No variable importance mode selected.")
    vividImportance.default(fit, data, response, importanceType, predictFun = predictFun)
  } else {
    # get data names without response
    featureNames <- names(data[, !(names(data) %in% response)])

    # check object properties
    lrnID <- fit$properties

    # if learner doesnt have an embedded vImp method then use default
    if (length(lrnID) == 0) {
      message("No variable importance mode available.")
      vividImportance.default(fit, data, response, importanceType, predictFun = predictFun)
    } else if (fit$packages == "xgboost") {
      # mlr3 xgboost omits some features. Here we are adding the omitted feature back
      # with a value of 0
      importance <- fit$importance()
      message("Embedded variable importance method used.")
      impNames <- names(importance)
      differernceNames <- setdiff(featureNames, impNames)
      importance <- c(importance, setNames(0, differernceNames))
      return(importance)
    } else {
      importance <- fit$importance()
      message("Embedded variable importance method used.")
      return(importance)
    }
  }
}



# mlr original ------------------------------------------------------------
#' @export
#' @method vividImportance WrappedModel
vividImportance.WrappedModel <- function(fit,
                                         data,
                                         response = NULL,
                                         importanceType = NULL,
                                         predictFun = NULL,
                                         numPerm = 4,
                                         showVimpError = FALSE,
                                         vars = NULL,
                                         ...) {
  data <- as.data.frame(data)

  # get data names without response
  featureNames <- names(data[, !(names(data) %in% response)])

  lrnID <- fit$learner$properties
  testString <- "featimp"


  logID <- logical(length(lrnID))
  for (i in seq_along(lrnID)) {
    logID[i] <- grepl(lrnID[i], testString, fixed = TRUE)
  }

  if (any(logID) == TRUE) {
    if (fit$learner$id == "regr.ranger") {
      importance <- fit$learner.model$variable.importance
    } else if (fit$learner$id == "regr.randomForest") {
      fitImp <- dim(fit$learner.model$importance)
      if (!is.null(importanceType) && fitImp[2] == 2) {
        if (importanceType == "%IncMSE") {
          message("%IncMSE variable importance method used.")
          importance <- fit$learner.model$importance[, 1]
        } else if (importanceType == "IncNodePurity") {
          message("IncNodePurity variable importance method used.")
          importance <- fit$learner.model$importance[, 2]
        }
      } else if (is.null(importanceType) && fitImp[2] == 2) {
        message("No importanceType selected. Returning %IncMSE importance values")
        importance <- fit$learner.model$importance[, 1]
      } else {
        importance <- fit$learner.model$importance[, 1]
      }
    } else {
      importance <- vividImportance.default(fit, data, response, importanceType, predictFun = predictFun)
    }
  }
  return(importance)
}


# tidyModels --------------------------------------------------------------
#' @export
#' @method vividImportance model_fit

vividImportance.model_fit <- function(fit,
                                      data,
                                      response = NULL,
                                      importanceType = NULL,
                                      predictFun = NULL,
                                      numPerm = 4,
                                      showVimpError = FALSE,
                                      vars = NULL,
                                      ...) {
  vImp <- vip::vi_model(fit, type = importanceType)
  vImp <- vImp[, 1:2]
  importance <- vImp$Importance
  names(importance) <- vImp$Variable
  return(importance)
}


#  Space for more ML models here ------------------------------------------







# Interaction Calculation: ------------------------------------------------

# vividInteraction --------------------------------------------------------

vividInteraction <- function(fit,
                             data,
                             response = NULL,
                             interactionType = NULL,
                             nmax = 500,
                             gridSize = 10,
                             predictFun = NULL,
                             normalized = FALSE,
                             vars = NULL,
                             ...) {
  UseMethod("vividInteraction")
}



# default flashlight interactions -----------------------------------------
#' @export
#' @method vividInteraction default
vividInteraction.default <- function(fit,
                                     data,
                                     response = NULL,
                                     interactionType = NULL,
                                     nmax = 500,
                                     gridSize = 50,
                                     predictFun = NULL,
                                     normalized = FALSE,
                                     vars = NULL,
                                     ...) {
  message("Calculating interactions...")

  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")

  # remove response column
  ovars <- data[, !(names(data) %in% response)]
  ovars <- colnames(ovars)

  if (is.null(nmax)) nmax <- nrow(data)
  nmax <- max(5, nmax)
  gridSize <- min(gridSize, nmax)

  ### Interaction Matrix:
  res <- NULL

  # create flashlight
  if (any(class(fit) == "model_fit")) {
    fl <- flashlight(
      model = fit, data = data, y = response, label = "",
      predict_function = function(fit, data) condvis2::CVpredict(fit, as.data.frame(data))
    )
  } else {
    fl <- flashlight(
      model = fit, data = data, y = response, label = "",
      predict_function = function(fit, data) predictFun(fit, as.data.frame(data), prob = normalized)
    )
  }


  # calculate interactions
  res <- light_interaction(fl,
                           pairwise = TRUE, type = "H", grid_size = gridSize,
                           normalize = normalized, n_max = nmax, v = vars
  )$data

  # Column names and order in res:
  #   label  variable  value  error  (flashlight < 1.0.0)
  #   label_ variable_ value_ error_ (flashlight >=1.0.0)

  variable <- res[[2L]]
  value <- res[[3L]]

  # reorder
  variable <- reorder(variable, value)

  # create matrix of values
  vars2 <- t(simplify2array(strsplit(as.character(variable), ":"))) # split/get feature names
  mat <- matrix(0, length(ovars), length(ovars)) # create matrix
  rownames(mat) <- colnames(mat) <- ovars # set names
  mat[vars2] <- value # set values
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  return(mat)
}
