#' vip2vivid
#'
#' @description  Takes measured importance and interactions
#' from the vip package and turns them into a matrix
#' which can be used for plotting. Accepts any of the variable importance
#' methods supplied by vip.
#'
#' @param importance Measured importance from the vip package using \code{vi} function.
#' @param interaction Measured interaction from the vip package using \code{vint} function.
#' @param reorder If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances.
#'
#' @return A matrix of interaction values, with importance on the diagonal.
#'
#'
#' @examples
#' \dontrun{
#' library(ranger)
#' library(vip)
#' aq <- na.omit(airquality) # get data
#' nameAq <- names(aq[-1]) # get feature names
#'
#' rF <- ranger(Ozone ~ ., data = aq, importance = "permutation") # create ranger random forest fit
#' vImp <- vi(rF) # vip importance
#' vInt <- vint(rF, feature_names = nameAq) # vip interaction
#'
#' vip2vivid(vImp, vInt)
#' }
#' @export

vip2vivid <- function(importance, interaction, reorder = TRUE) {

  # Importance --------------------------------------------------------------


  # get importance and make named vector
  importanceTibble <- importance[, 1:2]

  vImp <- importanceTibble$Importance
  names(vImp) <- importanceTibble$Variable

  varNames <- importanceTibble$Variable
  # interactions ------------------------------------------------------------

  # get interaction and make named vector
  interactionTibble <- interaction[, 1:2]

  vars <- t(simplify2array(strsplit(as.character(interactionTibble[["Variables"]]), "*", fixed = TRUE)))
  mat <- matrix(0, length(varNames), length(varNames)) # create matrix
  rownames(mat) <- colnames(mat) <- varNames # set names
  mat[vars] <- interactionTibble[["Interaction"]]
  mat[vars[, 2:1, drop = FALSE]] <- interactionTibble[["Interaction"]]


  mat <- mat[varNames, varNames] # make sure the order of vImp & vInt match
  diag(mat) <- vImp

  if (reorder) {
    mat <- vividReorder(mat)
  }

  if(class(mat)[1] != "vivid"){
    class(mat) <- c("vivid", class(mat))
  }

  return(mat)
}
