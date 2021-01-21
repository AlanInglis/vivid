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


vivi <- function(data, fit,  response, gridSize = 10, importanceType=NULL, nmax = 500,
                          reorder = TRUE, class=1,predictFun = NULL, ...){


}


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
vividReorder <- function(d){
  vimp <- diag(d)
  rvimp <- range(vimp)
  if (rvimp[2] != rvimp[1])
    vimp <- (vimp - rvimp[1])/(rvimp[2] - rvimp[1])
  vint <- as.dist(d)
  rvint <- range(vint)
  if (rvint[2] != rvint[1])
    vint <-  (vint - rvint[1])/(rvint[2] - rvint[1])
  score <- apply(as.matrix(vint), 1,max) + vimp
  o <- dser( -vint , -score, cost=DendSer::costLS)
  res<- d[o,o]
  res
}


# could export these too

vividImportance <-function (fit,data, response=NULL, importanceType=NULL,predictFun=NULL,...) {
  UseMethod("vividImportance")
}


vividImportance.default <- function (fit,data,response=NULL, importanceType=NULL, predictFun=NULL) {
}

vividImportance.ranger <- function (fit,data,response=NULL, importanceType=NULL, predictFun=NULL){
}


vividImportance.Learner <- function (fit,data,response=NULL, importanceType=NULL,predictFun=predictFun){
}


vividInteraction <- function (fit,data, response=NULL, interactionType=NULL, ...) {
  UseMethod("vividInteraction")
}

vividInteraction.default <- function (fit,data,response=NULL, interactionType=NULL,
                                      nmax = 500, gridSize=10, predictFun=NULL) {

}


