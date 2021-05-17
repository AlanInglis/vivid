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

# Load relevent packages:
library("vivid") # for visualistions and measuring VIVI
library("mlr") # to create model

# Data --------------------------------------------------------------------

# Get data:
cervical <- read.csv("https://raw.githubusercontent.com/AlanInglis/vivid/master/paperCodeData/cervicalCancer.csv",
                         sep=",",  na.strings = c('?'), stringsAsFactors = FALSE)



# Remove cancer tests, similar and constant variables:
cervical <- dplyr::select(cervical, -Citology, -Schiller, -Hinselmann,
                          -Dx.CIN, -Dx, -Horm_Cont,
                          -Smokes, -IUD, -STDs, -STDs_No_diag, -STDs_AIDS,
                          -STDs.Hep_B, -STDs_cerv_condy, -STDs_Time_first_diag,
                          -STDs_Time_last_diag)


# Taking the log(x+1) of skewed variables:
cervical$Age <- log(cervical$Age + 1)
cervical$No_sex_par <- log(cervical$No_sex_par + 1)
cervical$First_sex_inter <- log(cervical$First_sex_inter + 1)
cervical$No_preg <- log(cervical$No_preg +1)
cervical$Smokes_pcks_yr <- log(cervical$Smokes_pcks_yr + 1)
cervical$Smokes_yrs <- log(cervical$Smokes_yrs +1)
cervical$Horm_Cont_yrs <- log(cervical$Horm_Cont_yrs + 1)
cervical$IUD_yrs <- log(cervical$IUD_yrs + 1)
cervical$STDs_No <- log(cervical$STDs_No + 1)

# Adding biopsy as a factor with labels:
cervical$Biopsy <- factor(cervical$Biopsy, levels = c(0, 1), labels=c('Healthy', 'Cancer'))

# Impute NAs by mode:
imputer <- mlr::imputeMode()
cervical_impute <- mlr::impute(cervical, classes = list(numeric = mlr::imputeMode()))
cervical <- cervical_impute$data
cervical_impute_1 <-  mlr::impute(cervical, classes = list(integer = mlr::imputeMode()))
cervical <- cervical_impute_1$data

# Turn dummy variables into factors:
factorNames <- c("STDs_condy", "STDs_vag_condy",
                 "STDs_vp_condy", "STDs_syph", "STDs_pel_inf",
                 "STDs_gen_h", "STDs_m_c", "STDs_HIV",
                 "STDs.HPV", "Dx.HPV", "Dx.Cancer")

cervical[factorNames] <- lapply(cervical[factorNames], factor)

#  Split data into train and test
set.seed(11)
trainCervical <- sample(x = 858, size = 600)
cervicalTrain <- cervical[trainCervical, ]
cervicalTest  <- cervical[-trainCervical, ]

# Create Deep Learner -----------------------------------------------------
set.seed(11)
canTask <- makeClassifTask(data = cervicalTrain, target = "Biopsy")

# check if data is imbalanced
table(getTaskTargets(canTask))

# Balance the data
set.seed(11)
taskSmote <- smote(canTask, rate = 14, nn = 10)
table(getTaskTargets(taskSmote))

# Create learner
set.seed(11)
canLrn <- makeLearner("classif.h2o.deeplearning", predict.type = "prob")

# Tune model
param_set <- makeParamSet(
  makeNumericParam("epochs", default = 50, lower = 1, upper = Inf),
  makeNumericParam("score_interval", default = 1),
  makeDiscreteParam("stopping_metric", default = "misclassification",
                    values = "misclassification"),
  makeNumericParam("stopping_tolerance", default = 1e-3)
)
canLrn$par.set$pars$epochs <- param_set$pars$epochs
canLrn$par.set$pars$score_interval <- param_set$pars$score_interval
canLrn$par.set$pars$stopping_metric <- param_set$pars$stopping_metric
canLrn$par.set$pars$stopping_tolerance<- param_set$pars$stopping_tolerance

set.seed(11)
canMod <- train(canLrn, taskSmote)

# Test predictions
pred1 <- predict(canMod, newdata = cervicalTest)
# Evaluate performance accuracy, area under curve and mean misclassification error
performance(pred1, measures = list(acc, mmce, auc), model = canMod)
# False and true positive rates and mean misclassification error
predAnalysis <- generateThreshVsPerfData(pred1, measures = list(fpr, tpr, mmce))
plotROCCurves(predAnalysis)
plotThreshVsPerf(predAnalysis)

# Create vivid matrix -----------------------------------------------------

# vivid matrix
set.seed(11)
canVIVI <- vivi(cervicalTrain, canMod, response = "Biopsy", class = "Cancer",
                gridSize = 10)


# Calc importance ---------------------------------------------------------

cervicalTrain$DUMMYcancer <-  cervicalTrain$Biopsy == "Cancer"

predFun_Imp <- function(mod, data){
  out <- predict(mod, newdata = data)
  return(out$data[,2])
}

set.seed(12)
cervicalImp <- vivid:::vividImportance.default(canMod, cervicalTrain, "DUMMYcancer",
                                predictFun = function(mod, data) predFun_Imp(mod = mod, data = data))

# Remove biopsy column
cervicalImp <- cervicalImp[-2]

# Add VImps into canVIVI and turn into vivid matrix
canVIVI <- viviUpdate(canVIVI, cervicalImp)
canVIVI <- vividReorder(canVIVI)
class(canVIVI) <- c("vivid", class(canVIVI))


# ==============================================================================
# Visualisations
# ==============================================================================

# Figure 6:
viviHeatmap(canVIVI, angle = 50)

# Figure 7:
set.seed(11)
viviNetwork(canVIVI, intThreshold = 0.008, removeNode = TRUE,
            cluster = igraph::cluster_fast_greedy)

# Figure 8:
varNames <- colnames(canVIVI[,1:5]) # Using the vars from cluster in Figure 7
set.seed(11)
canGPDP <- pdpPairs(cervical,
         canMod,
         "Biopsy",
         nmax = length(cervical$Age),
         nIce = 50,
         vars = varNames,
         convexHull = TRUE)


# Figure 9:
zpath <- zPath(canVIVI, cutoff = 0.015)
pdpZen(cervical,
       canMod,
       "Biopsy",
       zpath,
       convexHull = TRUE,
       fitlims = c(0, 0.7))






