library(ggplot2)
library(condvis2)
library(vivid)
library(GGally)
library(dplyr)
library(h2o)


h2o.init()
h2o.no_progress()


# predict function --------------------------------------------------------

CVpredictfun <- function(classif = FALSE, class = 1) {
  if (classif) {
    function(fit, data) {
      pred <- tryCatch(CVpredict(fit, data, ptype = "probmatrix")[, class],
                       error = function(e) NULL, warning = function(w) {}
      )
      if (is.null(pred)) {
        pred <- CVpredict(fit, data)
        if (is.numeric(class) & is.factor(pred)) class <- levels(pred)[class]
        pred <- as.numeric(pred == class)
      }
      pred
    }
  } else {
    CVpredict
  }
}



# air quality -------------------------------------------------------------


aq <- na.omit(airquality)
x <- names(aq[,-1])
y <- "Ozone"



set.seed(1234)
aqDL <- h2o.deeplearning(x,
                         y,
                         as.h2o(aq)
)


pdpPairsH2O(aq, aqDL, "Ozone", convexHull = T)




# boston ------------------------------------------------------------------

library(MASS)
Boston1 <- Boston[, c(4:6, 8, 13:14)]
Boston1$chas <- factor(Boston1$chas)

x <- names(Boston1[,-6])
y <- "medv"

set.seed(1234)
bostonDL <- h2o.deeplearning(x,
                             y,
                             as.h2o(Boston1)
)


pdpPairsH2O(Boston1, bostonDL, "medv", convexHull = T)



# iris --------------------------------------------------------------------

# DOESNT WORK AT ALL FOR CLASSIFICATION

# data
yC <- "Species"
xC <- names(iris[,-5])

set.seed(1234)
irisDL <- h2o.deeplearning(xC,
                           yC,
                           as.h2o(iris)
)


pdpPairsH2O(iris, irisDL, "Species", class = "setosa")


h2o.shutdown()
Y

