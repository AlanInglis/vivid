
CVpredictfun <- function(classif = FALSE, class = 1) {
  if (classif) {
    function(fit, data, prob = FALSE) {
      pred <- tryCatch(CVpredict(fit, data, ptype = "probmatrix"),
                       error = function(e) NULL, warning = function(w) {}
      )
      if (is.null(pred)) {
        if(prob == FALSE){
          message("logits not available, as probabilites are not available.")
        }
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
        if (ncol(pred)== 2) {
          if (inherits(fit, "glm") && fit$family$family == "binomial")
            predLogit <- predict(fit, data)
          else  predLogit <- pred_1[, 1] - pred_1[, 2]
          if (identical(pred_1[, class],  pred_1[, 2]))
            predLogit <- -predLogit
        } else
          predLogit <- pred_1[, class] - rowMeans(pred_1)
        predLogit
      }
    }
  } else {
    CVpredict
  }
}
