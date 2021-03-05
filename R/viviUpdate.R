#' viviUpdate
#'
#' @description Creates a matrix displaying updated variable importance on the diagonal
#' and variable interaction on the off-diagonal.
#'
#' @param mat A matrix, such as that returned by \code{vivi}.
#' @param newImp A named vector of variable importances.
#' @return A matrix of values, of class vivid, with updated variable importances.
#'
#' @examples
#' f <- lm(Sepal.Length ~ ., data = iris[, -5])
#' m <- vivi(iris[, -5], f, "Sepal.Length")
#' corimp <- abs(cor(iris[, -5])[1, -1])
#' viviUpdate(m, corimp) # use correlation as updated importance
#' @export

viviUpdate <- function(mat, newImp) {
  orderNames <- names(newImp)
  viviMatrix <- mat[orderNames, orderNames] # make sure the order of vImp & vInt match
  diag(viviMatrix) <- newImp # set diagonal to equal vImps

  if(class(viviMatrix)[1] != "vivid"){
  class(viviMatrix) <- c("vivid", class(viviMatrix))
  }

  return(viviMatrix)

}
