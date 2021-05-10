#==============================================================================
# Cervical cancer classification deep learner visualisations
#==============================================================================
# Source:
# http://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29
# Paper: http://www.inescporto.pt/~jsc/publications/conferences/2017KelwinIBPRIA.pdf



library(dplyr)
library(flashlight)
library(h2o)
library(vivid)
library(imbalance)

# get data
cervicalReal <- read.csv("/Users/alaninglis/Desktop/cervicalCancerCopy.csv", sep=",",  na.strings = c('?'), stringsAsFactors = FALSE)



# remove cancer types and extra variables
cervical <- dplyr::select(cervicalReal, -Citology, -Schiller, -Hinselmann)
cervical <- dplyr::select(cervical, -Dx.Cancer, -Dx.CIN, -Dx.HPV, -Dx, -Horm_Cont, -Smokes,
                          -IUD, -STDs, - STDs_No_diag)


# taking the log(x+1)
cervical <- log(cervical[,-24] + 1)



# Adding biopsy
cervical$Biopsy <- factor(cervicalReal$Biopsy, levels = c(0, 1), labels=c('Healthy', 'Cancer'))


# Impute NAs by mode
imputer <- mlr::imputeMode()
cervical_impute <- mlr::impute(cervical, classes = list(numeric = mlr::imputeMode()))
cervical <- cervical_impute$data
cervical_impute_1 <-  mlr::impute(cervical, classes = list(integer = mlr::imputeMode()))
cervical <- cervical_impute_1$data


# Balance the data
imbalanceRatio(cervical, classAttr = "Biopsy") # check imbalance ratio
set.seed(1701)
cervical <- oversample(cervical, ratio = 0.9, method = "SMOTE",  classAttr = "Biopsy")
imbalanceRatio(cervical, classAttr = "Biopsy") # check imbalance ratio again



# create h2o deep learner -------------------------------------------------

# data
y <- "Biopsy"
x <- names(cervical[,-24])


h2o.init()
h2o.no_progress()


# Fit the model
set.seed(12345)
fit_NN <- h2o.deeplearning(x,
                           y,
                           as.h2o(cervical),
                           epochs = 50,
                           variable_importances = T)
set.seed(12345)
vi <- vividH2oClassif(data = cervical,
                      fit = fit_NN,
                      response = "Biopsy",
                      gridSize = 10,
                      importanceType = "permutation",
                      class = "Cancer")

viviHeatmap(vi, angle = 50)


h2o.shutdown()
Y
