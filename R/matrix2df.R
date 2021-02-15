#' matrix2df
#'
#'  @description Takes a matrix, such as that created by \code{vivi}, and turn it into a data frame
#'  containing variable names, Vimp and Vint values, and the row and column index from the original
#'  matrix.
#'
#' @param matrix A matrix to be converted to a data frame.
#'
#' @importFrom reshape "melt"
#'
#' @return A data frame of Vimp and Vint values and their index from the matrix.
#'
#'
#' @examples
#' library(ranger)
#' aq <- na.omit(airquality)
#' rF <- ranger(Ozone ~ ., data = aq, importance = "permutation")
#' myMat <- vivi(fit = rF, data = aq, response = "Ozone")
#' myDf <- matrix2df(myMat)
#' myDf
#' @export


# Turn matrix into df with index ------------------------------------------

matrix2df <- function(matrix) {

  # melt the matrix
  df <- melt(matrix)
  # get the row and colum index
  ind <- which(!is.na(matrix), arr.ind = TRUE)
  # get position of importance values
  impIndex <- as.integer(df$X1 == df$X2)
  # get position of interaction values
  intIndex <- 1 - impIndex
  # set Vimp values
  Vimp <- impIndex * df$value
  # set Vint values
  Vint <- intIndex * df$value
  # cbind them together
  viviDataFrame <- cbind(df[-(3)], Vimp, Vint, ind)
  # convert to dataframe
  viviDataFrame <- as.data.frame(viviDataFrame)
  # set names
  rownames(viviDataFrame) <- NULL
  colnames(viviDataFrame)[1] <- "Variable_1"
  colnames(viviDataFrame)[2] <- "Variable_2"

  return(viviDataFrame)
}
