#==============================================================================
# Cervical cancer classification visualisations
#==============================================================================
# Source:
# http://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29
# Paper: http://www.inescporto.pt/~jsc/publications/conferences/2017KelwinIBPRIA.pdf

library(vivid)
library(ranger)
library(imbalance) # to balance data


# get data
cervicalReal <- read.csv("/Users/alaninglis/Desktop/cervicalCancer.csv", sep=",",  na.strings = c('?'), stringsAsFactors = FALSE)



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


# create random forest & vivi ----------------------------------------------------
set.seed(12345)
cervicalRF <- ranger(Biopsy~., data = cervical, probability = TRUE, importance = "impurity")

# check model fit
require(pROC)
votes <- predict(cervicalRF, cervical)$predictions
rfROC <- roc(cervical$Biopsy, votes[,1])
plot(rfROC)
auc(rfROC)
cervicalRF$prediction.error

# vivi
set.seed(1701)
cervicalVIVI <- vivi(fit = cervicalRF, data = cervical, response = "Biopsy", class = "Cancer")

# visualisations
viviHeatmap(cervicalVIVI, angle = 45)
viviNetwork(cervicalVIVI)

#  top 10 -----------------------------------------------------------------
cerMat <- cervicalVIVI[1:10, 1:10]
# turn into vivid matrix
class(cerMat) <- c("vivid", class(cerMat))

# visualisations
viviHeatmap(cerMat, angle = 45)
viviNetwork(cerMat)
viviNetwork(cerMat,  cluster = igraph::cluster_louvain)



