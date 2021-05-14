

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


varNames <- c("Horm_Cont_yrs", "No_sex_par", "First_sex_inter" , "No_preg" , "Age")
pdpPairsH2O(cervical, fit_NN_1, "Biopsy", class = 2, className = "Cancer",
            convexHull = T, vars = varNames, nmax = length(cervical$Age))
