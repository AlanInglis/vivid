
CVpredictfun <- function(classif = FALSE, class = 1) {
  if (classif) {
    function(fit, data, prob = FALSE) {
      pred <- tryCatch(CVpredict(fit, data, ptype = "probmatrix"),
        error = function(e) NULL, warning = function(w) {}
      )
      if (is.null(pred)) {
        predNullMarker <- TRUE
        pred <- CVpredict(fit, data)
        if (is.numeric(class) & is.factor(pred)) class <- levels(pred)[class]
        pred <- as.numeric(pred == class)
      }else{
        predNullMarker <- FALSE
      }

      if (predNullMarker) {
        pred
      } else if (prob) {
        pred[, class]
      } else {
        mEpsilon <- .Machine$double.eps
        pred_1 <- log(ifelse(pred > 0, pred, mEpsilon))
        predLogit <- pred_1[, class] - rowMeans(pred_1)
        predLogit
      }
    }
  } else {
    CVpredict
  }
}
