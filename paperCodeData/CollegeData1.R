#==============================================================================
# College enrollment visualisations
#==============================================================================

library("vivid")
library("ISLR") # for data
library("mlr3") # to create model
library("mlr3learners") # to create model
library("randomForest") # to create model



# Data --------------------------------------------------------------------


myData <- College

tvars <- c("Accept", "Apps", "Enroll", "F.Undergrad", "P.Undergrad", "Outstate", "Room.Board", "Books", "Personal")

# taking log values of skewed data
myData[, tvars]<- log(myData[, tvars])



# Model fits --------------------------------------------------------------

# fit a randomforest model
set.seed(101)
rf <- randomForest(Enroll~., data = myData)

# create sorted vivid matrix
set.seed(101)
system.time(myMatrix1 <- vivi(myData, rf, "Enroll",  gridSize = 50))
# unsorted matrix
set.seed(101)
myMatrixUnsorted <- vivi(myData, rf, "Enroll",  gridSize = 100, reorder = F)
# with agnostic Vimp
set.seed(101)
myMatrixAg <- vivi(myData, rf, "Enroll", gridSize = 50, importanceType = "agnostic")

# visualisations
viviHeatmap(myMatrix1, angle = 45) # sorted heatmap
viviHeatmap(myMatrixUnsorted, angle = 45) # unsorted heatmap
viviHeatmap(myMatrixAg, angle = 45) # sorted heatmap
viviNetwork(myMatrix1) # full network
viviNetwork(myMatrix1, intThreshold = 0.01, removeNode = T) # thresholded network

# -------------------------------------------------------------------------
# GPDP on filtered college data

nam <- colnames(myMatrix1) # get names
nam <- nam[1:7] # filter names


# create GPDP
set.seed(1701)
pdpPairs(myData, rf, "Enroll", gridSize = 20, vars = nam, convexHull = T)


# -------------------------------------------------------------------------
# zenplot

zpath <- zPath(myMatrix1, 0.01) # calc zpath

# create ZPDP using zpath
set.seed(1701)
pdpZen(myData, rf, "Enroll", gridSize = 20, zpath = zpath, convexHull = T)



# -------------------------------------------------------------------------

## mlr3 knn

myData1 <- myData
myData1$Private <- as.numeric(myData1$Private)

set.seed(11)
exT <- TaskRegr$new(id = "myData1", backend = myData1, target = "Enroll")
set.seed(11)
exL <- lrn("regr.kknn")
set.seed(11)
exMod <- exL$train(exT)

set.seed(101)
exMat <- vivi(fit = exMod, data = myData1, response = "Enroll", gridSize = 100, nmax = 100, importanceType = "agnostic")

# create visulisations
viviHeatmap(exMat, angle = 45)
# setting same same limits as agnostic plot
viviHeatmap(exMat, angle = 45, impLims = c(0, 0.5))

