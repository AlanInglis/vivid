#' vivi
#'
#' @description Creates a matrix displaying variable importance on the diagonal
#'  and variable interaction on the off-diagonal.
#'
#' @param data Data frame used for fit
#' @param fit A supervised machine learning model, which understands condvis2::CVpredict
#' @param response The name of the response for the fit
#' @param gridSize The size of the grid for evaluating the predictions.
#' @param main Define main category for classification.
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


vivi <- function(data, fit,  response, gridSize = 10, main = NULL, importanceType = NULL, nmax = 500,
                 reorder = TRUE, class = 1, predictFun = NULL, ...){

  # Call the importance function
  Vimp <- vividImportance(data = data, fit = fit, response = response)

  # Call the interaction function
  Vint <- vividInteraction(data = data, fit = fit, response = response,  main = main)

  # as mlr3 xgboost removes some variables, the length of Vint and Vimp may not be equal
  # Here we remove feature(s) if necessary
  if(length(colnames(Vint)) == length(Vimp)){
      col.order <- names(Vimp)
      Vint <- Vint[,col.order] # make sure the order of Vimp & Vint match
      diag(Vint) <- Vimp   # set diagonal to equal Vimps
  }else{
      impColNames <- names(Vimp)    # get importance feature names
      intColNames <- colnames(Vint) # get interaction feature names
      difference <- setdiff(intColNames,impColNames) # get name of removed features
      # remove feature(s) from matrix
      Vint <- Vint[rownames(Vint) != difference, colnames(Vint) != difference]
      diag(Vint) <- Vimp   # set diagonal to equal Vimps
  }


  # reorder
  if(reorder == TRUE){
   viviMatrix <-  vividReorder(Vint)
  }else {viviMatrix <- Vint}

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
  vimp <- diag(d)
  rvimp <- range(vimp)
  if (rvimp[2] != rvimp[1]) {
    vimp <- (vimp - rvimp[1]) / (rvimp[2] - rvimp[1])
  }
  vint <- as.dist(d)
  rvint <- range(vint)
  if (rvint[2] != rvint[1]) {
    vint <- (vint - rvint[1]) / (rvint[2] - rvint[1])
  }
  score <- apply(as.matrix(vint), 1, max) + vimp
  o <- dser(-vint, -score, cost = DendSer::costLS)
  res <- d[o, o]
  res
}


# -------------------------------------------------------------------------
# Variable Importance Measures
# -------------------------------------------------------------------------


#' @export
# Main Vimp function:
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

  # create flashlight
  fl <- flashlight(model = fit, data = data, y = response, label = "")
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

  # If no importance mode selected, then stop! Else, extract importance
  if(fit$importance.mode == "none"){
    print("No variable importance mode selected.")
    stop()
    }else{
    importance <- fit$variable.importance
    names(importance) <- NULL
    message("Embedded variable importance method used.")
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

  # if learner doesnt have an embedded Vimp method then use default
   if(length(lrnID) == 0){
     print("No variable importance mode available. Using agnostic method.")
     vividImportance.default(fit, data, response)
   }else{

  # create a string called importance
  testString <- "importance"

  # Check if any of the properties equals string
  logID <- logical(length(lrnID))
  for(i in seq_along(lrnID)){
    logID[i] <- grepl(lrnID[i], testString, fixed = TRUE)
  }

  # if above is true, then extract importance
    if(any(logID) == TRUE){
      ovars <- colnames(data)
      importance <- fit$importance()
      importance <- importance[order(factor(names(importance), levels = ovars))] # reorder to match data
      #names(importance) <- NULL
      message("Embedded variable importance method used.")
      return(importance)

    }
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
  importance <- fit$importance
  return(importance)
}





# -------------------------------------------------------------------------
# space for more ML models here



# -------------------------------------------------------------------------
# Interaction Calculation:
# -------------------------------------------------------------------------

# Main interaction function
#' @export
vividInteraction <- function (fit,data, response = NULL, main = NULL, interactionType = NULL, ...) {
  UseMethod("vividInteraction")
}


# -------------------------------------------------------------------------
# default flashlight

#' @export
vividInteraction.default <- function (fit,
                                      data,
                                      response = NULL,
                                      main = NULL,
                                      interactionType = NULL,
                                      nmax = 500,
                                      gridSize = 10,
                                      predictFun = NULL) {

  # Check if regr or classif. If classif use different method
  if(!is.null(main)){
    vividInteraction.classif(fit, data, response, main)
  }else{
  message("Calculating interactions...")
  print("default Int")


  # remove response column
  drops <- response
  ovars  <- data[ , !(names(data) %in% drops)]
  ovars <- colnames(ovars)

  # Interaction Matrix:
  res  <- NULL

  # create flashlight
  fl <- flashlight(model = fit, data = data, y = response, label = "",
                   predict_function = function(fit, data) CVpredict(fit, data))

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
  return(dinteraction)
  }
}


# -------------------------------------------------------------------------
# classification flashlight

#' @export
vividInteraction.classif <- function (fit,
                                      data,
                                      response = NULL,
                                      main = NULL,
                                      interactionType = NULL,
                                      nmax = 500,
                                      gridSize = 10,
                                      predictFun = NULL) {

  message("Calculating interactions...")
  print("classif Int")


  # remove response column
  drops <- response
  ovars  <- data[ , !(names(data) %in% drops)]
  ovars <- colnames(ovars)

  # Interaction Matrix:
  res  <- NULL

  # create flashlight
  fl <- flashlight(
    model = fit,
    data = data,
    y = response,
    label = "",
    predict_function = function(m, X) predict(m, X) == main)



  # calculate interactions
  res <- light_interaction(fl, pairwise = TRUE, type = "H", grid_size = gridSize,
                           normalize = F, n_max = nmax)$data

  ## Removing rows containing target and adding df back to FL object
   # get data
  res_edit <- res
  # remove target
  res_edit <- res_edit[!grepl(response, res_edit$variable),]
  # add back into FL object
  res <- res_edit
  # reorder
  res[["variable"]]<- reorder(res[["variable"]], res[["value"]])

  # create matrix of values
  vars2 <- t(simplify2array(strsplit(as.character(res[["variable"]]),":"))) # split/get feature names
  dinteraction <- matrix(0, length(ovars), length(ovars))                   # create matrix
  rownames(dinteraction) <- colnames(dinteraction) <- ovars                 # set names
  dinteraction[vars2] <- res[["value"]]                                     # set values
  dinteraction[lower.tri(dinteraction)] = t(dinteraction)[lower.tri(dinteraction)]
  return(dinteraction)
}


# -------------------------------------------------------------------------
