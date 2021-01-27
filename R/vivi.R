#' vivi
#'
#' @description Creates a matrix displaying variable importance on the diagonal
#'  and variable interaction on the off-diagonal.
#'
#' @param data Data frame used for fit
#' @param fit A supervised machine learning model, which understands condvis2::CVpredict
#' @param response The name of the response for the fit
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param importanceType passed to vividImportance
#' @param nmax Maximum number of data rows to consider.
#' @param reorder If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances.
#' @param class Category for classification, a factor level, or a number indicating which factor level.
#' @param predictFun Function of (fit, data) to extract numeric predictions from fit. Uses condvis2::CVpredict by default, which works for many fit classes.
#' @return A matrix of values, of class vivid
#'
#' @importFrom flashlight "flashlight"
#' @importFrom flashlight "light_importance"
#' @importFrom flashlight "light_interaction"
#' @importFrom condvis2 "CVpredict"
#' @examples
#' aq <- data.frame(airquality)
#' aq <- na.omit(airquality)
#' f <- lm(Ozone ~ ., data=aq)
#' plot(vivi(aq,f, "Ozone")) # as expected all interactions are zero

#' \dontrun{
#' # Run an mlr ranger model:
#' library(mlr3)
#' library(mlr3learners)
#' library(ranger)
#' aq <- na.omit(airquality)
#' aq_Task <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn <- lrn("regr.ranger", importance = "permutation")
#' aq_fit <- aq_lrn$train(aq_Task)
#'
#' m <- vivi(aq, aq_fit, "Ozone")
#' plot(m, type = "heatMap")
#' }
#' \dontrun{
#' library(ranger)
#' rf <- ranger(Species ~ ., data= iris)
#' plot(vivi(iris,rf, "Species"))
#' rf <- ranger(Species ~ ., data= iris, importance="impurity")
#' plot(vivi(iris,rf, "Species"))
#' }
#' @export


vivi <- function(data, fit,  response, gridSize = 10, importanceType = NULL, nmax = 500,
                 reorder = TRUE, class = 1, predictFun = NULL, ...){

  # check for predict function
  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  if (is.null(predictFun)){
    predictFun <- CVpredictfun(classif, class)
  }

  # Call the importance function
  vImp <- vividImportance(data = data,
                          fit  = fit,
                          response = response,
                          importanceType = importanceType,
                          predictFun = predictFun)

  # Call the interaction function
  vInt <- vividInteraction(data = data,
                           fit = fit,
                           response = response,
                           class = class,
                           interactionType = NULL,
                           nmax = nmax,
                           gridSize = gridSize,
                           predictFun = predictFun)


  orderNames <- names(vImp)
  vInt <- vInt[orderNames, orderNames] # make sure the order of vImp & vInt match
  diag(vInt) <- vImp   # set diagonal to equal vImps


  # reorder
  if(reorder == TRUE){
   viviMatrix <-  vividReorder(vInt)
  }else {viviMatrix <- vInt}

  # Set class to vivid matrix and return
  class(viviMatrix) <- c("vivid", class(viviMatrix))

  return(viviMatrix)

}


# -------------------------------------------------------------------------

#' vividReorder
#' @description Reorders a square matrix so that values of high importance and
#' interaction strength are pushed to the top left of the matrix.
#' @param d A matrix such as that returned by vivi
#' @param newImp A named vector of variable importances.
#' @param reorder If TRUE, uses vividReorder on the matrix
#' @name vividReorder
#' @return A reordered version of d
#' @importFrom DendSer "dser"
#' @importFrom DendSer "costLS"
#' @export
#'
#' @examples
#' f <- lm(Sepal.Length ~ ., data=iris[,-5])
#' m <- vivi(iris[,-5], f, "Sepal.Length")
#' corimp <- abs(cor(iris[,-5])[1,-1])
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
  o <- dser(-vInt, -score, cost = DendSer::costLS)
  res <- d[o, o]
  res
}


# -------------------------------------------------------------------------
# Variable Importance Measures
# -------------------------------------------------------------------------


#' @export
# Main vImp function:
vividImportance <-function (fit, data, response = NULL, importanceType = NULL, predictFun = NULL,...) {
  UseMethod("vividImportance", fit)
}



# -------------------------------------------------------------------------
# Default flashlight

#' @export
vividImportance.default <- function (fit,
                                     data,
                                     response = NULL,
                                     importanceType = NULL,
                                     predictFun = NULL) {


  print("DEFAULT IMP")

  # check for predict function
  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  if (is.null(predictFun)){
    predictFun <- CVpredictfun(classif, class)
  }

  # create flashlight
  fl <- flashlight(model = fit, data = data, y = response, label = "",
                   predict_function = function(fit, data) predictFun(fit, data))

  # extract importance
  imp <- light_importance(fl, m_repetitions = 4)
  importance <- imp$data[,3:4]
  importance <- setNames(importance$value, as.character(importance$variable)) # turn into named vector
  message("Agnostic variable importance method used.")
  return(importance)

}


# -------------------------------------------------------------------------
# ranger

#' @export
vividImportance.ranger <- function (fit,
                                    data,
                                    response = NULL,
                                    importanceType = NULL,
                                    predictFun = NULL){

  print("ranger imp")

  # If no importance mode selected, then default! Else, extract importance type
  if (fit$importance.mode == "none") {
    print("No variable importance mode selected. Using agnostic method.")
    vividImportance.default(fit, data, response)
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


# -------------------------------------------------------------------------
# mlr3 learner

#' @export
vividImportance.Learner <- function (fit,
                                     data,
                                     response = NULL,
                                     importanceType = NULL,
                                     predictFun = predictFun){
  print("learner Imp")

  # check object properties
  lrnID <- fit$properties

  # if learner doesnt have an embedded vImp method then use default
   if(length(lrnID) == 0){
     print("No variable importance mode available. Using agnostic method.")
     vividImportance.default(fit, data, response)
   }else{
      ovars <- colnames(data)
      importance <- fit$importance()
      message("Embedded variable importance method used.")
      return(importance)
   }
}


# -------------------------------------------------------------------------
# randomForest


#' @export
vividImportance.randomForest <- function (fit,
                                    data,
                                    response = NULL,
                                    importanceType = NULL,
                                    predictFun = NULL){

  print("randomForest Imp")

  # check to see if if randomForest importance is TRUE
  fitCall <- as.character(fit$call)

  # if importance = T, then choose between mse and nodePurity
  if (any(fitCall == "T")) {
    if (is.null(importanceType)) {
      stop("importanceType must not be NULL. Choose either 'mse' or 'nodePurity'.")
    }
    if (importanceType == "mse") {
      importance <- fit$importance[, 1]
    } else if (importanceType == "nodePurity") {
      importance <- fit$importance[, 2]
    }
  } else {
    importance <- as.vector(fit$importance)
    names(importance) <- rownames(fit$importance)
  }

  return(importance)
}





# -------------------------------------------------------------------------
# space for more ML models here



# -------------------------------------------------------------------------
# Interaction Calculation:
# -------------------------------------------------------------------------

# Main interaction function
#' @export
vividInteraction <- function (fit,data,
                              response = NULL,
                              class = 1,
                              interactionType = NULL,
                              nmax = 500,
                              gridSize = 10,
                              predictFun = NULL, ...) {
  UseMethod("vividInteraction")
}


# -------------------------------------------------------------------------
# default flashlight

#' @export
vividInteraction.default <- function (fit,
                                      data,
                                      response = NULL,
                                      class = 1,
                                      interactionType = NULL,
                                      nmax = 500,
                                      gridSize = 10,
                                      predictFun = NULL) {

    message("Calculating interactions...")

  # Check if regr or classif.
  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  if(classif){
    print("classif Int")
  }else{
    print("default Int")
  }

  # check for predict function
  if (is.null(predictFun)){
    predictFun <- CVpredictfun(classif, class)
  }

  # remove response column
  drops <- response
  ovars  <- data[ , !(names(data) %in% drops)]
  ovars <- colnames(ovars)

  ### Interaction Matrix:
  res  <- NULL

  # create flashlight
  fl <- flashlight(model = fit, data = data, y = response, label = "",
                   predict_function = function(fit, data) predictFun(fit, data))


  # calculate interactions
  res <- light_interaction(fl, pairwise = TRUE, type = "H", grid_size = gridSize,
                           normalize = F, n_max = nmax)$data

  # reorder
  res[["variable"]]<- reorder(res[["variable"]], res[["value"]])

  # create matrix of values
  vars2 <- t(simplify2array(strsplit(as.character(res[["variable"]]),":"))) # split/get feature names
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[["value"]]                                     # set values
  dinteraction[lower.tri(dinteraction)] = t(dinteraction)[lower.tri(dinteraction)]

  # as mlr3 xgboost models omit features that arent used
  # here we omit feature(s) if necessary
  if(!is.null(fit$packages) && fit$packages == "xgboost"){
    impColNames <- names(fit$importance())    # get importance feature names
    intColNames <- colnames(dinteraction)     # get interaction feature names
    difference <- setdiff(intColNames,impColNames) # get name of removed features
    print(difference)
    # remove feature(s) from matrix
    dinteraction <- dinteraction[rownames(dinteraction) != difference, colnames(dinteraction) != difference]
  }
  return(dinteraction)
}
