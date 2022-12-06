#####################
# VIVID Speed Tests #
#####################

set.seed(1701)

# load libraries ----------------------------------------------------------

library('vivid')
library('MASS')
library('ranger')
library('randomForest')
library('mlr3') # used for kknn
library("mlr3learners") # used for kknn
library('e1071')
library('nnet')
library('xgboost')
library('microbenchmark')


# Load data ---------------------------------------------------------------


data("Boston")



# -------------------------------------------------------------------------
# Build Models ------------------------------------------------------------
# -------------------------------------------------------------------------

# randomForest
rf <- randomForest(medv ~ ., data = Boston)

# ranger
rang <- ranger(medv~., data = Boston)


# kknn
knnT <- TaskRegr$new(id = "knn", backend = Boston, target = "medv")
knnL <- lrn("regr.kknn")
knnMod <- knnL$train(knnT)

# svm
suppvm <- svm(medv~., data = Boston)


# nnet
net <- nnet(medv~., data = Boston, size = 2)


# xgboost
bst <- xgboost(
  data = as.matrix(Boston[,c(1:13)]),
  label =  as.matrix(Boston[,14]),
  nrounds = 100)




# -------------------------------------------------------------------------
# Speed Tests -------------------------------------------------------------
# -------------------------------------------------------------------------


# predict functions
pFunXgb <- function(fit, data, prob = FALSE) predict(fit, as.matrix(data[,1:13]))
pFun <- function(fit, data, prob = FALSE) predict(fit, data[,1:13])


mb <- microbenchmark(
  viviRf   = vivi(fit = rf,    importanceType = 'agnostic', data = Boston, response = "medv"),
  viviRang = vivi(fit = rang,  importanceType = 'agnostic', data = Boston, response = "medv"),
  viviKnn  = vivi(fit = knnMod,importanceType = 'agnostic', data = Boston, response = "medv"),
  viviSvm  = vivi(fit = suppvm,importanceType = 'agnostic', data = Boston, response = 'medv', predictFun = pFun),
  viviNnet = vivi(fit = net,   importanceType = 'agnostic', data = Boston, response = 'medv', predictFun = pFun),
  viviXgb  = vivi(fit = bst,   importanceType = 'agnostic', data = Boston, response = 'medv', predictFun = pFunXgb),
  times = 5
)

print(mb, unit = "s", order = "mean")


