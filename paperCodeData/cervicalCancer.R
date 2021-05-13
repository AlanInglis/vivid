#==============================================================================
# Cervical cancer classification visualisations
#==============================================================================
# Source:
# http://archive.ics.uci.edu/ml/datasets/Cervical+cancer+%28Risk+Factors%29
# Paper: http://www.inescporto.pt/~jsc/publications/conferences/2017KelwinIBPRIA.pdf

library(vivid)
library(ranger)
library(imbalance) # to balance data
library(ranger)

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


#training Sample with 500 observations
set.seed(123)
trainCervical <- sample(x = 1526, size = 500)
cervicalTrain <- cervical[trainCervical, ]
xTrain <- cervicalTrain %>% select(-Biopsy)
yTrain <- cervicalTrain$Biopsy


cervicalTest  <- cervical[-trainCervical, ]
xTest <-  cervicalTest %>% select(-Biopsy)
yTest <- cervicalTest$Biopsy

# create random forest & vivi ----------------------------------------------------
set.seed(12345)
cervicalRF <- ranger(Biopsy~., data = cervicalTrain, probability = TRUE, importance = "impurity")


# vivi
set.seed(1701)
cervicalVIVI <- vivi(fit = cervicalRF, data = cervical,
                     response = "Biopsy", class = "Cancer",
                     reorder = F)


cv <- vivid:::vividReorder(cervicalVIVI)
class(cv) <- c("vivid", class(cv))


# visualisations
viviHeatmap(cv, angle = 45)
viviNetwork(cv)
viviNetwork(cv, intThreshold = 0.01, removeNode = T,
            cluster = c(1,1,1,1,1,2))

#  top 10 -----------------------------------------------------------------
cerMat <- cervicalVIVI[1:10, 1:10]
cerMat <- cv[1:6, 1:6]

# turn into vivid matrix
class(cerMat) <- c("vivid", class(cerMat))

# visualisations
viviHeatmap(cerMat, angle = 45)
viviNetwork(cerMat)
viviNetwork(cerMat,  cluster = c(1,1,1,1,2,2))



