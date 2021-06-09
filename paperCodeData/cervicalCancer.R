#==============================================================================
# Cervical cancer classification visualisations
#==============================================================================
# Source:
# http://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29
# Paper: http://www.inescporto.pt/~jsc/publications/conferences/2017KelwinIBPRIA.pdf
#==============================================================================
# This script is used for creating the model and visualisations in:
# Section 4:  CASE STUDY: CERVICAL CANCER RISK CLASSIFICATION
#==============================================================================

# install the development version of vivid:
devtools::install_github("AlanInglis/vivid")

# Load relevent packages:
library("vivid") # for visualistions and measuring VIVI
library("h2o") # to create model
library("mlr") # to create model
library("dplyr")
library("mice")
# Data --------------------------------------------------------------------

# Get data:
cervical <- read.csv("https://raw.githubusercontent.com/AlanInglis/vivid/master/paperCodeData/cervicalCancer.csv",
                         sep=",",  na.strings = c('?'), stringsAsFactors = FALSE)



# Remove cancer tests, similar and constant variables:
cervical <- dplyr::select(cervical, -Citology, -Schiller, -Hinselmann,
                          -Dx.CIN, -Dx, -Horm_Cont,
                          -Smokes, -IUD, -STDs, -STDs_No_diag, -STDs_AIDS,
                          -STDs.Hep_B, -STDs_cerv_condy, -STDs_Time_first_diag,
                          -STDs_Time_last_diag, -STDs_pel_inf,
                          -STDs_gen_h, -STDs_m_c,
                          -STDs.HPV, -STDs_vag_condy)


# Taking the log(x+1) of skewed variables:
cervical <- cervical %>%
   mutate(log(cervical[,1:8] + 1))

# Adding biopsy as a factor with labels:
cervical$Biopsy <- factor(cervical$Biopsy, levels = c(0, 1), labels=c('Healthy', 'Cancer'))
biopsy <- cervical$Biopsy


# Impute using mice
cImp <- mice(cervical[,-16], seed = 1701, method = "cart")
cervical <- complete(cImp)
cervical$Biopsy <- biopsy


# Turn dummy variables into factors:
factorNames <- c("STDs_condy",
                 "STDs_vp_condy",
                 "STDs_syph",
                 "STDs_HIV",
                 "Dx.HPV",
                 "Dx.Cancer")

cervical[factorNames] <- lapply(cervical[factorNames], factor)

#  Split data into train and test
set.seed(1701)
trainCervical <- sample(x = 858, size = 600)
cervicalTrain <- cervical[trainCervical, ]
cervicalTest  <- cervical[-trainCervical, ]

# Create Deep Learner -----------------------------------------------------
canTask <- makeClassifTask(data = cervicalTrain, target = "Biopsy")

# check if data is imbalanced
table(getTaskTargets(canTask))

# Balance the data using SMOTE
set.seed(1701)
taskSmote <- smote(canTask, rate = 12, nn = 10)
table(getTaskTargets(taskSmote))

# Create learner
canLrn <- makeLearner("classif.h2o.gbm",
                      predict.type = "prob",
                      par.vals = list(
                         max_depth = 25,
                         ntrees = 100,
                         nbins = 100,
                         learn_rate = 0.1,
                         seed = 1
                      ))


canMod <- train(canLrn, taskSmote)


# # Test predictions
pred1 <- predict(canMod, newdata = cervicalTest)
# # Evaluate performance accuracy, area under curve and mean misclassification error
performance(pred1, measures = list(acc, auc))


# Create vivid matrix -----------------------------------------------------

# vivid matrix
set.seed(1701)
canVIVI <- vivi(cervicalTrain, canMod, response = "Biopsy", class = "Cancer",
                gridSize = 10, importanceType = "agnostic")


# ==============================================================================
# Visualisations
# ==============================================================================

# Figure 6:
viviHeatmap(canVIVI, angle = 50)

# Figure 7:
set.seed(1701)
viviNetwork(canVIVI, intThreshold = 0.01, removeNode = TRUE,
            cluster = igraph::cluster_fast_greedy)


# Figure 8:
# Subsetting the variables that appear in the cluster in Figure 7:
varNames <- colnames(canVIVI)[1:5]

# sample 50 ice curves - 25 from positive class, 25 from negative class:
yesRows <- sample(which(cervicalTrain$Biopsy == "Cancer"), 25)
noRows <- sample(which(cervicalTrain$Biopsy == "Healthy"), 25)

set.seed(1701)
canGPDP <- pdpPairs(data = cervicalTrain,
                    fit = canMod,
                    response = "Biopsy",
                    class = "Cancer",
                    nmax = length(cervical$Age),
                    nIce = c(yesRows, noRows),
                    vars = varNames,
                    convexHull = TRUE)


# Figure 9:
zpath <- zPath(canVIVI, cutoff = 0.01) # same cutoff as Figure 7.
set.seed(1701)
pdpZen(data = cervicalTrain,
       fit = canMod,
       response = "Biopsy",
       class = "Cancer",
       zpath = zpath,
       convexHull = TRUE
      )


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
