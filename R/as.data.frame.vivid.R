#' as.data.frame.vivid
#'
#'  @description Takes a matrix of class \code{vivid} and turn it into a data frame
#'  containing variable names, Vimp and Vint values, and the row and column index from the original
#'  matrix.
#'
#' @param matrix A matrix of class 'vivid' to be converted to a data frame.
#'
#' @return A data frame of Vimp and Vint values and their index from the vivid matrix.
#'
#'
#' @examples
#' library(ranger)
#' aq <- na.omit(airquality)
#' rF <- ranger(Ozone ~ ., data = aq, importance = "permutation")
#' myMat <- vivi(fit = rF, data = aq, response = "Ozone")
#' myDf <- as.data.frame(myMat, class = "vivid")
#' myDf
#' @export


as.data.frame.vivid <- function(matrix) {

  # melt the matrix
  df <- cbind(expand.grid(dimnames(matrix)), value = as.vector(matrix))

  # get the row and colum index
  Row <- as.vector(row(matrix))
  Col <- as.vector(col(matrix))

  # Create measure column
  df$Measure <- with(df, ifelse(Var1 == Var2, "Vimp", "Vint"))

  # cbind them together
  viviDataFrame <- cbind(df, Row, Col)

  # set names
  colnames(viviDataFrame)[1] <- "Variable_1"
  colnames(viviDataFrame)[2] <- "Variable_2"
  colnames(viviDataFrame)[3] <- "Value"

  return(viviDataFrame)
}


