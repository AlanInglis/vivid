# Run an h2o model on the higgs 10k data from Kaggle
# https://www.kaggle.com/c/higgs-boson
# https://higgsml.lal.in2p3.fr/files/2014/04/documentation_v1.8.pdf - variable description

library(mlr)
library(tidyverse)

# Load in data ------------------------------------------------------------

higgs <- read.csv('https://raw.githubusercontent.com/h2oai/h2o-tutorials/master/tutorials/data/higgs_10k.csv', header = FALSE) %>%
  mutate(V1 = as.factor(V1))

testHiggs <- read.csv('https://raw.githubusercontent.com/h2oai/h2o-tutorials/master/tutorials/data/higgs_test_5k.csv', header = FALSE) %>%
  mutate(V1 = as.factor(V1))

higgsAll <- rbind(higgs, testHiggs)

higgsTrain <- higgsAll[1:10000, ]
higgsTest  <- higgsAll[10001:nrow(higgsAll), ]

# Fit h2o model -----------------------------------------------------------

set.seed(11)
higgsTask <- makeClassifTask(data = higgsTrain, target = "V1")

# check if data is imbalanced
table(getTaskTargets(higgsTask))

# Create learner
set.seed(11)
higgsLrn <- makeLearner("classif.h2o.deeplearning", predict.type = "prob")
#higgsLrn <- makeLearner("classif.h2o.gbm", predict.type = "prob")

set.seed(11)
higgsMod <- train(higgsLrn, higgsTask)

# Test predictions
pred1 <- predict(higgsMod, newdata = higgsTest)
# Evaluate performance accuracy, area under curve and mean misclassification error
performance(pred1, measures = list(acc, mmce, auc), model = canMod)
# False and true positive rates and mean misclassification error
predAnalysis <- generateThreshVsPerfData(pred1, measures = list(fpr, tpr, mmce))
plotROCCurves(predAnalysis)

# Prep for vivid ----------------------------------------------------------

set.seed(11)
higgsVIVI <- vivi(higgsTrain, higgsMod, response = "V1", class = "1",
                gridSize = 10)

higgsTrain$DUMMY <-  higgsTrain$V1 == "1"

predFun_Imp <- function(mod, data){
  out <- predict(mod, newdata = data)
  return(out$data[,2])
}

set.seed(12)
higgsImp <- vivid:::vividImportance.default(higgsMod, higgsTrain,
                                            "DUMMY",
                                            predictFun = function(mod, data) predFun_Imp(mod = mod, data = data))

# Remove biopsy column
higgsImp <- higgsImp[-1]

# Add VImps into canVIVI and turn into vivid matrix
higgsVIVI <- viviUpdate(higgsVIVI, higgsImp)
higgsVIVI <- vividReorder(higgsVIVI)

# ==============================================================================
# Visualisations
# ==============================================================================

# Figure 6:
viviHeatmap(higgsVIVI, angle = 50)

# Figure 7:
set.seed(11)
viviNetwork(higgsVIVI, intThreshold = 0.008, removeNode = TRUE,
            cluster = igraph::cluster_fast_greedy)

# Figure 8:
varNames <- colnames(higgsVIVI[,1:5]) # Using the vars from cluster in Figure 7
set.seed(11)
canGPDP <- pdpPairs(higgsTrain,
                    higgsMod,
                    "V1",
                    nmax = length(higgs$V1),
                    nIce = 50,
                    vars = varNames,
                    convexHull = TRUE)


# Figure 9:
zpath <- zPath(higgsVIVI, cutoff = 0.015)
pdpZen(higgsTrain,
       higgsMod,
       "V1",
       zpath,
       convexHull = TRUE,
       fitlims = c(0, 0.7))


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

