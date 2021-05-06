#==============================================================================
# Cervical cancer classification PDPs
#==============================================================================
# Source:
# http://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29
# Paper: http://www.inescporto.pt/~jsc/publications/conferences/2017KelwinIBPRIA.pdf

library(vivid)
library(ranger)
library(imbalance) # to balance data


cervical <-  read.csv("/Users/alaninglis/Desktop/cervicalCancer.csv", sep=",",  na.strings = c('?'), stringsAsFactors = FALSE)


cervicalData = dplyr::select(cervical, -Citology, -Schiller, -Hinselmann)

# taking the log(x +1) =
cervicalData <- log(cervicalData[,-33] + 1)

# put Biopsy back in
cervicalData$Biopsy = factor(cervical$Biopsy, levels = c(0, 1), labels=c('Healthy', 'Cancer'))

## subset variables to top 5 (taken from the  top 5 Vimps on heatmap)
cervicalData = dplyr::select(cervicalData,
                             Age,
                             No_sex_par,
                             First_sex_inter,
                             No_preg,
                             Horm_Cont_yrs,
                             Biopsy)

# NA imputation
imputer = mlr::imputeMode()


cervical_imputeA = mlr::impute(cervicalData, classes = list(numeric = mlr::imputeMode()))
cervicalData = cervical_imputeA$data
cervical_impute_B <-  mlr::impute(cervicalData, classes = list(integer = mlr::imputeMode()))
cervicalData <- cervical_impute_B$data

# Balance the data
# imbalanceRatio(cervical, classAttr = "Biopsy") # check imbalance ratio
# set.seed(1701)
# cervical <- oversample(cervical, ratio = 0.9, method = "SMOTE",  classAttr = "Biopsy")
# imbalanceRatio(cervical, classAttr = "Biopsy") # check imbalance ratio again


# random forest
set.seed(1701)
cervicalRF_1 <- ranger(Biopsy~., data = cervicalData, probability = TRUE, importance = "impurity")

# PDPs

# set the order
varNames <- c("Horm_Cont_yrs",  "No_sex_par",  "No_preg","First_sex_inter",  "Age")
set.seed(1701)
pdpPairs(cervicalData, cervicalRF_1, "Biopsy",  vars = varNames,
         gridSize = 20, class = "Cancer", convexHull = TRUE,
         nmax = nrow(cervicalData))



set.seed(1701)
pdpZen(cervicalData, cervicalRF_1, "Biopsy", gridSize = 20,
       nmax = nrow(cervicalData),
       class = "Cancer", convexHull = TRUE,
       zpath = varNames)






