#' vivi_importance
#'
#' @description Compute variable importance only, without interactions.
#'
#' @param data Data frame used for `fit`.
#' @param fit A supervised ML model understood by `condvis2::CVpredict` (or supply `predictFun`).
#' @param response Name of the response column in `data`.
#' @param importanceType Importance metric to use. Defaults to "agnostic"
#' (permutation via flashlight). If an embedded metric exists, set this to that
#' metric name to extract it instead.
#' @param class Classification level (factor level or 1-based index) when `response` is a factor.
#' @param predictFun Optional prediction function of signature `(fit, data, prob = TRUE/FALSE)`.
#' If `NULL`, an internal CVpredict-based function is used.
#' @param numPerm Number of permutations for agnostic importance. Default 4.
#' @param showVimpError If TRUE and `numPerm > 1`, print standard errors.
#' @param vars Optional character vector of feature names to restrict the calculation.
#' @param as_matrix If TRUE, return a square matrix with importances on the diagonal
#' and zeros elsewhere; otherwise return a named numeric vector. Default FALSE.
#' @param reorder If `as_matrix = TRUE`, optionally reorder the matrix with `vividReorder()`. Default FALSE.
#'
#' @return Named numeric vector of importances, or a square matrix if `as_matrix = TRUE`.
#'
#' @examples
#' # Example 1 — importance as a named vector
#' aq <- na.omit(airquality)
#' fit_lm <- lm(Ozone ~ ., data = aq)
#' imp_vec <- vivi_importance(data = aq, fit = fit_lm, response = "Ozone")
#' head(imp_vec)
#'
#' # Example 2 — importance as a diagonal matrix for plotting
#' imp_mat <- vivi_importance(data = aq, fit = fit_lm, response = "Ozone",
#'                            as_matrix = TRUE)
#' # viviHeatmap(imp_mat)  # if you want to visualise the diagonal
#'
#' # Example 3 — embedded importance from a random forest (if available)
#' \donttest{
#' if (requireNamespace("randomForest", quietly = TRUE)) {
#'   library(randomForest)
#'   rf <- randomForest(Ozone ~ ., data = aq, importance = TRUE)
#'   vivi_importance(data = aq, fit = rf, response = "Ozone",
#'                   importanceType = "%IncMSE")
#' }
#' }
#'
#' # Example 4 — classification model with ranger using embedded impurity importance
#' \donttest{
#' if (requireNamespace("ranger", quietly = TRUE)) {
#'   library(ranger)
#'   fit_rf <- ranger(Species ~ ., data = iris,
#'                    importance = "impurity", probability = TRUE)
#'   vivi_importance(data = iris, fit = fit_rf, response = "Species",
#'                   importanceType = "impurity")
#' }
#' }
#'
#' @export
vivi_importance <- function(data,
                            fit,
                            response,
                            importanceType = "agnostic",
                            class = 1,
                            predictFun = NULL,
                            numPerm = 4,
                            showVimpError = FALSE,
                            vars = NULL,
                            as_matrix = FALSE,
                            reorder = FALSE) {

  # choose prediction function consistent with vivi()
  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  pFun <- if (is.null(predictFun)) CVpredictfun(classif, class) else predictFun

  # compute importance (agnostic or embedded)
  if (!is.null(importanceType) && importanceType == "agnostic") {
    if (is.null(predictFun) & classif) {
      data1 <- data
      if (is.factor(data[[response]]) & is.numeric(class)) {
        class <- levels(data[[response]])[class]
      }
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
      predictFun = pFun1,
      numPerm = numPerm,
      showVimpError = showVimpError,
      vars = vars
    )
  } else {
    vImp <- vividImportance(
      data = data,
      fit = fit,
      response = response,
      importanceType = importanceType,
      predictFun = pFun,
      numPerm = numPerm,
      showVimpError = showVimpError,
      vars = vars
    )
  }

  # return as vector (default)
  if (!as_matrix) {
    return(vImp)
  }

  # or as square matrix with diagonal = importance
  all_feats <- setdiff(names(data), response)
  feature_names <- if (is.null(vars)) all_feats else intersect(all_feats, vars)
  m <- matrix(0, length(feature_names), length(feature_names),
              dimnames = list(feature_names, feature_names))
  # align and place importances
  vImp_full <- setNames(rep(NA_real_, length(feature_names)), feature_names)
  vImp_full[names(vImp)] <- vImp
  diag(m) <- vImp_full[feature_names]

  if (reorder) m <- vividReorder(m)
  m
}



#' vivi_interaction
#'
#' @description Compute pairwise interactions only (Friedman's H), without importance.
#'
#' @param data Data frame used for `fit`.
#' @param fit A supervised ML model understood by `condvis2::CVpredict` (or supply `predictFun`).
#' @param response Name of the response column in `data`.
#' @param nmax Maximum number of rows to consider for grids. Default 500. Use all rows if `NULL`.
#' @param gridSize Grid size for evaluating partial dependence. Default 50.
#' @param predictFun Optional prediction function `(fit, data, prob = TRUE/FALSE)`.
#' If `NULL`, an internal CVpredict-based function is used.
#' @param normalized Should H be normalised. Default FALSE.
#' @param vars Optional character vector of feature names to restrict the calculation.
#' @param reorder If TRUE, reorder the resulting matrix with `vividReorder()`. Default TRUE.
#'
#' @return Square interaction matrix with zero diagonal.
#'
#' @examples
#' # Example 1 — interactions with a linear model
#' aq <- na.omit(airquality)
#' fit_lm <- lm(Ozone ~ ., data = aq)
#' int_mat <- vivi_interaction(data = aq, fit = fit_lm, response = "Ozone")
#' dim(int_mat); int_mat[1:3, 1:3]
#' # viviHeatmap(int_mat)  # if you want to visualise interactions only
#'
#' # Example 2 — classification with ranger
#' \donttest{
#' if (requireNamespace("ranger", quietly = TRUE)) {
#'   library(ranger)
#'   fit_rf <- ranger(Species ~ ., data = iris,
#'                    importance = "impurity", probability = TRUE)
#'   vivi_interaction(data = iris, fit = fit_rf, response = "Species")
#' }
#' }
#'
#' @export
vivi_interaction <- function(data,
                             fit,
                             response,
                             nmax = 500,
                             gridSize = 50,
                             predictFun = NULL,
                             normalized = FALSE,
                             vars = NULL,
                             reorder = TRUE) {

  # choose prediction function consistent with vivi()
  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  pFun <- if (is.null(predictFun)) CVpredictfun(classif, 1) else predictFun

  vInt <- vividInteraction(
    data = data,
    fit = fit,
    response = response,
    interactionType = NULL,
    nmax = nmax,
    gridSize = gridSize,
    predictFun = pFun,
    normalized = normalized,
    vars = vars
  )

  diag(vInt) <- 0
  if (reorder) vInt <- vividReorder(vInt)
  vInt
}
