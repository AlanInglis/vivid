# Load relevant packages:
library("vivid") # for visualisations
library("randomForest") # to create model
library('xgboost') # to create model
library("ggplot2") # for visualisations
library('MASS') # for data


set.seed(1701) # for reproducibility

data("Boston")


# Model fits --------------------------------------------------------------

# random forest:
rf <- randomForest(medv ~ ., data = Boston, importance = TRUE)

# gbm:
gbst <- xgboost(
  data = as.matrix(Boston[,c(1:13)]),
  label =  as.matrix(Boston[,14]),
  nrounds = 100
)


# vivi function -----------------------------------------------------------

# vivi for rf
set.seed(1701)

viviRf <- vivi(fit = rf,
               data = Boston,
               response = "medv",
               reorder = FALSE,
               normalized = FALSE,
               importanceType = 'agnostic',
               gridSize = 50,
               nmax = 500,
               class = 1,
               predictFun = NULL)



# predict function for gbm
pFun <- function(fit, data, prob = FALSE) predict(fit, as.matrix(data[,1:13]))

# vivi for GBM
set.seed(1701)
viviGBst <- vivi(fit = gbst,
                 data = Boston,
                 response = "medv",
                 reorder = FALSE,
                 normalized = FALSE,
                 predictFun = pFun)



# vip2vivid ---------------------------------------------------------------

library("vip")
# get model specific VImps using vip package
vipVImp <- vi(rf, method = 'model')
# get VInts using vip package
vipVInt <- vint(rf, feature_names = names(Boston[-14]))

# turn into vivi-matrix
vipViviMat <- vip2vivid(importance = vipVImp, interaction = vipVInt)



# vivi matrix -------------------------------------------------------------

# average over matrices and seriate to get common ordering
viviAvg <- (viviRf + viviGBst) / 2
viviAvgReorder <- vividReorder(viviAvg)

# reorder vivi-matrices
ord <- colnames(viviAvgReorder)
viviRf <- viviRf[ord,ord]
viviGBst <- viviGBst[ord,ord]



# Figure 2 -----------------------------------------------------------------

# heatmap for random forest
viviHeatmap(viviRf, angle = 45, intLims = c(0,1), impLims = c(0,8))

# heatmap for GBM
viviHeatmap(viviGBst, angle = 45, intLims = c(0,1), impLims = c(0,8))




# Figure 3 -----------------------------------------------------------------

# default network plot for GBM shown in Figure 2 (a)
viviNetwork(viviGBst)

# clustered and filtered network for GBM shown in Figure 2 (b)
intVals <- viviGBst
diag(intVals) <- NA

# select VIVI values above median value
sv <- which(diag(viviGBst) > median(diag(viviGBst)) |
              apply(intVals, 1, max, na.rm=TRUE) > median(apply(intVals, 1, max, na.rm=TRUE)))

# perform hierarchical clustering
h <- hclust(-as.dist(viviGBst[sv,sv]), method="single")

# plot graph clustering by high VIVI
viviNetwork(viviGBst[sv,sv],
            intLims = c(0,1),
            impLims = c(0,8),
            cluster = cutree(h, k = 3), # specify number of groups
            layout = igraph::layout_as_star)



# vivid data frame --------------------------------------------------------

head(as.data.frame(viviRf), 4)


# Figure 4 ----------------------------------------------------------------

# create PDPs for GBM
pdpVars(data = Boston,
        fit = gbst,
        response = 'medv',
        vars = colnames(viviGBst),
        predictFun = pFun)



# Figure 5 ----------------------------------------------------------------

# filter matrix:
filteredVars <- colnames(viviGBst)[1:5]

# select rows to plot associated ICE curves:
rmHigh <- sample(which(Boston$rm > mean(Boston$rm)), 25)
lstatLow <- sample(which(Boston$lstat < mean(Boston$lstat)), 25)

# create GPDP for gbm:
set.seed(1701)
pdpPairs(data = Boston,
         fit = gbst,
         response = "medv",
         gridSize = 20,
         nIce = c(rmHigh, lstatLow),
         var = filteredVars,
         convexHull = TRUE,
         fitlims = "pdp",
         predictFun = pFun)





# Figure 6 ----------------------------------------------------------------

# create ZPDP for gbm:
pdpZen(data = Boston,
       fit = gbst,
       response = "medv",
       convexHull = TRUE,
       zpath = colnames(viviGBst)[1:5],
       predictFun = pFun)



# Figure 7 ----------------------------------------------------------------

# find the 90% quantile of the interactions
qVIntBst <- quantile(intVals, 0.9, na.rm=TRUE)

# set zpaths with different parameters
zpGw  <- zPath(viv = viviGBst, cutoff = qVIntBst, method = 'greedy.weighted')
zpSw  <- zPath(viv = viviGBst, cutoff = qVIntBst, connect = FALSE, method = 'strictly.weighted')

# plots
pdpZen(data = Boston,
       fit = gbst,
       response = "medv",
       zpath = zpGw,
       convexHull = TRUE,
       predictFun = pFun)

pdpZen(data = Boston,
       fit = gbst,
       response = "medv",
       zpath = zpSw,
       convexHull = TRUE,
       predictFun = pFun)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------










