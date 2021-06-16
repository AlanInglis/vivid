#' vivi
#'
#' @description Creates a matrix displaying variable importance on the diagonal
#'  and variable interaction on the off-diagonal.
#'
#' @param fit A supervised machine learning model, which understands condvis2::CVpredict
#' @param data Data frame used for fit.
#' @param response The name of the response for the fit.
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param importanceType  One of either "%IncMSE" or "IncNodePurity" for use with randomForest. Or set to equal "agnostic" to override
#' embedded importance measures and return agnostic importance values.
#' @param nmax Maximum number of data rows to consider.
#' @param reorder If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances.
#' @param class Category for classification, a factor level, or a number indicating which factor level.
#' @param predictFun Function of (fit, data) to extract numeric predictions from fit. Uses condvis2::CVpredict by default, which works for many fit classes.
#' @param normalized Should Friedman's H-statistic be normalized or not. Default is FALSE.
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
#'\donttest{
#' # Run an mlr ranger model:
#' library(mlr3)
#' library(mlr3learners)
#' library(ranger)
#' aq <- na.omit(airquality)
#' aq_Task <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn <- lrn("regr.ranger", importance = "permutation")
#' aq_fit <- aq_lrn$train(aq_Task)
#'
#' m <- vivi(fit = aq_fit, data = aq, response = "Ozone")
#' viviHeatmap(m)
#' }
#'\donttest{
#' library(ranger)
#' rf <- ranger(Species ~ ., data = iris, importance = "impurity", probability = TRUE)
#' vivi(fit = rf, data = iris, response = "Species")
#' }
#' @export


# Main vivi function ------------------------------------------------------

vivi <- function(data,
                 fit,
                 response,
                 gridSize = 50,
                 importanceType = NULL,
                 nmax = 500,
                 reorder = TRUE,
                 class = 1,
                 predictFun = NULL,
                 normalized = FALSE) {

  # check for predict function
  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  if (is.null(predictFun)) {
    pFun <- CVpredictfun(classif, class)
  }
  else pFun <- predictFun

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
      predictFun = pFun1
    )
  }
  else {
    vImp <- vividImportance(
      data = data,
      fit = fit,
      response = response,
      importanceType = importanceType,
      predictFun = pFun
    )
  }



  # Call the interaction function
  vInt <- vividInteraction(
    data = data,
    fit = fit,
    response = response,
    interactionType = NULL,
    nmax = nmax,
    gridSize = gridSize,
    predictFun = pFun,
    normalized = normalized
  )

  # perserve original data order
  newimp <- vector("numeric", length = ncol(data)-1)
  names(newimp) <- names(data)[-match(response, names(data))]
  newimp[names(vImp)]<- vImp
  diag(vInt) <- newimp # set diagonal to equal vImps


  # reorder
  if (reorder) {
    viviMatrix <- vividReorder(vInt)
  } else {
    viviMatrix <- vInt
  }

  class(viviMatrix) <- c("vivid", class(viviMatrix))
  return(viviMatrix)
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
  class(res)<- class(d)
  res
}




# Variable Importance Measures: -------------------------------------------

# vividImportance ---------------------------------------------------------

# Main vImp function:
vividImportance <- function(fit, data, response = NULL, importanceType = NULL, predictFun = NULL, ...) {
  UseMethod("vividImportance", fit)
}




# Default flashlight ------------------------------------------------------

vividImportance.default <- function(fit,
                                    data,
                                    response = NULL,
                                    importanceType = NULL,
                                    predictFun = NULL) {

  message("Agnostic variable importance method used.")


  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")


    # create flashlight
    fl <- flashlight(
      model = fit, data = data, y = response, label = "",
      predict_function = function(fit, data) predictFun(fit, data, prob = TRUE)
    )




  # extract importance
  suppressWarnings(
    imp <- light_importance(fl, m_repetitions = 4)
  )

  importance <- imp$data[, 3:4]

  if (any(is.nan(importance$value))) {
    importance$value <- 1
    message("Flashlight importance works for numeric and numeric binary response only; setting importance to 1.")
  }
  importance <- setNames(importance$value, as.character(importance$variable)) # turn into named vector

  return(importance)
}



# ranger ----------------------------------------------------------------

vividImportance.ranger <- function(fit,
                                   data,
                                   response = NULL,
                                   importanceType = NULL,
                                   predictFun = NULL) {


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

vividImportance.randomForest <- function(fit,
                                         data,
                                         response = NULL,
                                         importanceType = NULL,
                                         predictFun = NULL) {

 fitImp <- dim(fit$importance)
 importanceData <- randomForest::importance(fit)

 if (fit$type == "classification") {
   if (!is.null(importanceType)) {
     if (importanceType == "MeanDecreaseAccuracy") {
       message("MeanDecreaseAccuracy variable importance method used.")
       importance <- importanceData[, 4]
     } else if (importanceType == "MeanDecreaseGini") {
       message("MeanDecreaseGini variable importance method used.")
       importance <- importanceData[, 5]
     } else if (is.numeric(importanceType)) {
       message(colnames(importanceData)[importanceType], " importance selected.")
       importance <- importanceData[, importanceType]
     }
   } else if (is.null(importanceType) && fitImp[2] != 1) {
     message("No importanceType selected. Returning MeanDecreaseGini importance values")
     importance <- importanceData[, 5]
   } else {
     message("No importanceType selected. Returning MeanDecreaseGini importance values")
     importance <- importanceData[, 1]
   }
 }


  if (fit$type == "regression") {
    if (!is.null(importanceType)) {
      if (importanceType == "%IncMSE") {
        message("%IncMSE variable importance method used.")
        importance <- importanceData[, 1]
      } else if (importanceType == "IncNodePurity") {
        message("IncNodePurity variable importance method used.")
        importance <- importanceData[, 2]
      }
    } else if (is.null(importanceType) && fitImp[2] != 1) {
      message("No importanceType selected. Returning IncNodePurity importance values")
      importance <- importanceData[, 2]
    } else {
      message("No importanceType selected. Returning IncNodePurity importance values")
      importance <- importanceData[, 1]
    }
}

  return(importance)
}


# mlr3 learner ------------------------------------------------------------

vividImportance.Learner <- function(fit,
                                    data,
                                    response = NULL,
                                    importanceType = NULL,
                                    predictFun = NULL) {


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
vividImportance.WrappedModel <- function(fit,
                                         data,
                                         response = NULL,
                                         importanceType = NULL,
                                         predictFun = NULL) {
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

vividImportance.model_fit <- function(fit,
                                      data,
                                      response = NULL,
                                      importanceType = NULL,
                                      predictFun = NULL) {

  vImp <- vip::vi_model(fit)
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
                             ...) {
  UseMethod("vividInteraction")
}



# default flashlight interactions -----------------------------------------

vividInteraction.default <- function(fit,
                                     data,
                                     response = NULL,
                                     interactionType = NULL,
                                     nmax = 500,
                                     gridSize = 50,
                                     predictFun = NULL,
                                     normalized = FALSE) {
  message("Calculating interactions...")

  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")

  # remove response column
  ovars <- data[, !(names(data) %in% response)]
  ovars <- colnames(ovars)

  ### Interaction Matrix:
  res <- NULL

  # create flashlight
  fl <- flashlight(
    model = fit, data = data, y = response, label = "",
    predict_function = function(fit, data) predictFun(fit, as.data.frame(data), prob = normalized)
  )


  # calculate interactions
  res <- light_interaction(fl,
                           pairwise = TRUE, type = "H", grid_size = gridSize,
                           normalize = normalized, n_max = nmax
  )$data

  # reorder
  res[["variable"]] <- reorder(res[["variable"]], res[["value"]])

  # create matrix of values
  vars2 <- t(simplify2array(strsplit(as.character(res[["variable"]]), ":"))) # split/get feature names
  mat <- matrix(0, length(ovars), length(ovars)) # create matrix
  rownames(mat) <- colnames(mat) <- ovars # set names
  mat[vars2] <- res[["value"]] # set values
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  return(mat)
}
