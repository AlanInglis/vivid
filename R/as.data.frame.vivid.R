#' as.data.frame.vivid
#'
#'  @description Takes a matrix of class \code{vivid} and turn it into a data frame
#'  containing variable names, Vimp and Vint values, and the row and column index from the original
#'  matrix.
#'
#' @param x A matrix of class 'vivid' to be converted to a data frame.
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Logical. If TRUE, setting row names and converting column names (to syntactic names: see
#' make.names) is optional. Note that all of R's base package as.data.frame() methods use optional
#' only for column names treatment, basically with the meaning of data.frame(*, check.names = !optional). See also the make.names argument of the matrix method.
#' @param ... Additional arguments to be passed to or from methods.
#' @return A data frame of Vimp and Vint values and their index from the vivid matrix.
#'
#'
#' @examples
#' library(ranger)
#' aq <- na.omit(airquality)
#' rF <- ranger(Ozone ~ ., data = aq, importance = "permutation")
#' myMat <- vivi(fit = rF, data = aq, response = "Ozone")
#' myDf <- as.data.frame(myMat)
#' myDf
#' @export


as.data.frame.vivid <- function(x, row.names = NULL, optional = FALSE, ...) {

  matrix <- x
  df <- cbind(expand.grid(dimnames(matrix), stringsAsFactors = FALSE), value = as.vector(matrix) )

  # get the row and colum index
  Row <- as.vector(row(matrix))
  Col <- as.vector(col(matrix))

  # Create measure column
  df$Measure <- with(df, ifelse(Var1 == Var2, "Vimp", "Vint"))

  # cbind them together
  viviDataFrame <- cbind(df, Row, Col)

  # set names
  names(viviDataFrame)[1] <- "Variable_1"
  names(viviDataFrame)[2] <- "Variable_2"
  names(viviDataFrame)[3] <- "Value"

  return(viviDataFrame)
}


