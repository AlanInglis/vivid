#' viviUpdate
#'
#' @description Creates a matrix displaying updated variable importance on the diagonal
#' and variable interaction on the off-diagonal.
#'
#' @param mat A matrix, such as that returned by \code{vivi}.
#' @param newImp A named vector of variable importances.
#' @param reorder If TRUE (default) uses DendSer to reorder the matrix of interactions and variable importances.
#' @return A matrix of values, of class vivid, with updated variable importances.
#'
#' @examples
#' f <- lm(Sepal.Length ~ ., data = iris[, -5])
#' m <- vivi(iris[, -5], f, "Sepal.Length")
#' corimp <- abs(cor(iris[, -5])[1, -1])
#' viviUpdate(m, corimp) # use correlation as updated importance
#' @export

viviUpdate <- function(mat, newImp, reorder = TRUE) {
  if (is.null(names(newImp))) {
    names(newImp) <- rownames(mat)[1:length(newImp)]
  }

  newnames <- intersect(rownames(mat), names(newImp))
  if (length(newnames) != nrow(mat)) {
    n <- setdiff(rownames(mat), newnames)
    message(paste("Importance values not provided for", paste0(n, collapse = " ")))
  }

  matdiag <- diag(mat)
  matdiag[newnames] <- newImp[newnames]
  diag(mat) <- matdiag
  if (reorder) mat <- vividReorder(mat)

  if (class(mat)[1] != "vivid") {
    class(mat) <- c("vivid", class(mat))
  }

  return(mat)
}
