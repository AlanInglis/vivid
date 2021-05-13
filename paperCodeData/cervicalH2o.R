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
cervical <- dplyr::select(cervical, -Dx.Cancer, -Dx.HPV, -Dx.CIN, -Dx, -Horm_Cont, -Smokes,
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


# create h2o deep learner -------------------------------------------------

# data
y <- "Biopsy"
x <- names(cervical[,-24])


h2o.init()
h2o.no_progress()


set.seed(1234)
fit_NN_1 <- h2o.deeplearning(x,
                           y,
                           as.h2o(cervicalTrain),
                           epochs = 50,
                           nfolds = 3,                            #used for early stopping
                           score_interval = 1,                    #used for early stopping
                           stopping_rounds = 5,                   #used for early stopping
                           stopping_metric = "misclassification", #used for early stopping
                           stopping_tolerance = 1e-3,             #used for early stopping
                           variable_importances = T)

dl_perf2 <- h2o.performance(model = fit_NN_1, newdata = as.h2o(cervicalTest))

# Retreive test set MSE
h2o.mse(dl_perf2)


# Get the CV models from the `fit_NN_1` object
cv_models <- sapply(fit_NN_1@model$cross_validation_models,
                    function(i) h2o.getModel(i$name))

# Plot the scoring history over time
plot(cv_models[[1]],
     timestep = "epochs",
     metric = "classification_error")



# create vivi matrix
set.seed(12345)
vi <- vividH2oClassif(data = cervical,
                      fit = fit_NN_1,
                      response = "Biopsy",
                      gridSize = 10,
                      importanceType = "permutation",
                      class = "Cancer")

viviHeatmap(vi, angle = 50)
set.seed(54321)
viviNetwork(vi, intThreshold = 0.024, removeNode = T,
            cluster = igraph::cluster_sp)


h2o.shutdown()
Y
