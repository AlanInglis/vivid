CVpredictfun <- function(classif=FALSE, class=1){
  if (classif)
    function(fit, data){
      pred <- tryCatch(CVpredict(fit,data, ptype="probmatrix")[,class],
                       error = function(e) NULL,warning = function(w) {})
      if (is.null(pred)){
        pred <- CVpredict(fit,data)
        if (is.numeric(class) & is.factor(pred)) class <- levels(pred)[class]
        pred <- as.numeric(pred==class)
      }
      pred
    }
  else CVpredict
}

