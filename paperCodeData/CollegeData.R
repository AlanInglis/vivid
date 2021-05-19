# ==============================================================================
# College enrollment script
# ==============================================================================
# This script is used for creating the models and visualisations in:
# Section 2:  VISUALISING VARIABLE IMPORTANCE AND INTERACTION
# Section 3:  VISUALISING PARTIAL DEPENDENCE AND CONDITIONAL EXPECTATION
# ==============================================================================

# Load relevent packages:
library("vivid") # for visualisations
library("ISLR") # for data
library("mlr3") # to create model
library("mlr3learners") # to create model
library("randomForest") # to create model
library("condvis2") # for predict function
library("Metrics") # to get metrics


# Data --------------------------------------------------------------------

# Load data:
myData <- as.data.frame(College)

# Taking log values of skewed data:
myData$Accept <- log(myData$Accept)
myData$Apps <- log(myData$Apps)
myData$Enroll <- log(myData$Enroll)
myData$F.Undergrad <- log(myData$F.Undergrad)
myData$P.Undergrad <- log(myData$P.Undergrad)
myData$Outstate <- log(myData$Outstate)
myData$Room.Board <- log(myData$Room.Board)
myData$Books <- log(myData$Books)
myData$Personal <- log(myData$Personal)

# Split data into train and test
set.seed(123)
train <- sample(x = 777, size = 544) # split 70-30
collegeTrain <- myData[train, ]
collegeTest <- myData[-train, ]

# Model fits --------------------------------------------------------------

# Fit a random forest model
# Used throughout Section 2:
set.seed(101)
rf <- randomForest(Enroll ~ ., data = collegeTrain)

# Fit an mlr3 knn model
# Used in Section 2.3:
myData1 <- collegeTrain
myData1$Private <- as.numeric(myData1$Private)

set.seed(11)
knnT <- TaskRegr$new(id = "myData1", backend = myData1, target = "Enroll")
set.seed(11)
knnL <- lrn("regr.kknn")
set.seed(11)
knnMod <- knnL$train(knnT)


# Create vivid matrix -----------------------------------------------------

# Create unsorted vivid matrix for random forest fit:
# Used for Figure 1(a):
set.seed(101)
myMatrix <- vivi(collegeTrain, rf, "Enroll", gridSize = 40, reorder = FALSE)

# Sort and turn the matrix into vivid matrix:
# Used for Figure 1(b):
myMatrixSorted <- vividReorder(myMatrix)

# Get agnostic VImp values instead of using random forests embedded VImps
# Used for Figure 2(b):
collegeVImps <- vivid:::vividImportance.default(rf,
  collegeTrain,
  "Enroll",
  importanceType = "agnostic",
  predictFun = CVpredict
)

# Update the matrix with the new VImp values and sort:
myMatrixSorted_1 <- viviUpdate(myMatrixSorted, collegeVImps)
myMatrixSorted_1 <- vividReorder(myMatrixSorted_1)

# Create vivid matix for mlr3 knn fit using agnostic VImp
# Used for figure 2(a):
set.seed(101)
knnMat <- vivi(
  fit = knnMod,
  data = myData1,
  response = "Enroll",
  gridSize = 40,
  importanceType = "agnostic"
)


# ==============================================================================
# Visualisations
# ==============================================================================

# Visualisations for Section 2 --------------------------------------------

# Figure 1(a):
viviHeatmap(myMatrix, angle = 45) # unsorted heatmap
# Figure 1(b):
viviHeatmap(myMatrixSorted, angle = 45) # sorted heatmap

# Figure 2(a):
viviHeatmap(knnMat, angle = 45, impLims = c(0, 0.5)) # setting same VImp limits as Figure 2(b)
# Figure 2(b)
viviHeatmap(myMatrixSorted_1, angle = 45) # agnostic VImp measures

# Figure 3(a)
viviNetwork(myMatrixSorted)
# Figure 3(b)
viviNetwork(myMatrixSorted, intThreshold = 0.01, removeNode = T)


# Visualisation for Section 3.2 ---------------------------------------------

# Filter matrix:
nam <- colnames(myMatrixSorted) # get names
nam <- nam[1:7] # filter names

# Create GPDP for Figure 4:
set.seed(1701)
pdpPairs(collegeTrain,
  rf, "Enroll",
  gridSize = 20,
  vars = nam,
  convexHull = T
)


# Visualisation for Section 3.3 ---------------------------------------------

# Calculate the zpath using same threshold as Figure 3(b):
zpath <- zPath(myMatrixSorted, 0.01)

# Create ZPDP using zpath for Figure 5:
set.seed(1701)
pdpZen(collegeTrain,
  rf,
  "Enroll",
  gridSize = 20,
  zpath = zpath,
  convexHull = T
)



# -------------------------------------------------------------------------
# Check random forest fit
# -------------------------------------------------------------------------

yTrain <- collegeTrain$Enroll
yTest <- collegeTest$Enroll

# Make prediction
predictions <- predict(rf, collegeTest)


# Get some metrics
print(paste0("MAE: ", mae(yTest, predictions)))
print(paste0("MSE: ", caret::postResample(predictions, yTest)["RMSE"]^2))
print(paste0("R2: ", caret::postResample(predictions, yTest)["Rsquared"]))
