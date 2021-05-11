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


# create h2o deep learner -------------------------------------------------

# data
y <- "Biopsy"
x <- names(cervical[,-24])


h2o.init()
h2o.no_progress()

cervicalH2O <- as.h2o(cervical)


# Fit the model
set.seed(1234)
fit_NN <- h2o.deeplearning(x,
                           y,
                           as.h2o(cervical),
                           variable_importances = T)

# PDPs --------------------------------------------------------------------

# testing predict functions
pred_fun <- function(mod, X) as.vector(unlist(h2o.predict(mod, as.h2o(X))))
pred_fun(fit_NN, cervicalH2O)

pred_fun_2 <- function(mod, X, what = class) {
  out <- h2o.predict(mod, as.h2o(X))
  return(as.vector(out[, what]))
}
pred_fun_2(fit_NN, cervicalH2O, "Cancer")


# set the order
varNames <- c("Horm_Cont_yrs",  "No_sex_par",  "No_preg","First_sex_inter",  "Age")
set.seed(1701)
pdpPairs(cervicalH2O,
         fit_NN,
         "Biopsy",
         vars = varNames,
         gridSize = 5,
         class = "Cancer",
         convexHull = TRUE,
         nmax = nrow(cervical),
         predictFun = h2o.predict(fit_NN, cervicalH2O))


h2o.shutdown()
Y



