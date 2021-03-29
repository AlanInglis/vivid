[![codecov](https://codecov.io/gh/AlanInglis/vivid/branch/master/graph/badge.svg?token=IJTT3ZW1RP)](https://codecov.io/gh/AlanInglis/vivid)


# vividPackage

The `zenplots` package requires the `graph` package from BioConductor. To install the `graph` and `zenplots` packages use:

```
if (!requireNamespace("graph", quietly = TRUE)){
  install.packages("BiocManager")
  BiocManager::install("graph")
}
install.packages("zenplots")
```

Variable importance, interaction measures and partial dependence plots are important summaries in the interpretation of statistical and machine learning models. In our R package `vivid` (variable importance and variable interaction displays) we create new visualisation techniques for exploring these model summaries. We construct heatmap and graph-based displays showing variable importance and interaction jointly, which are carefully designed to highlight important aspects of the fit. We also construct a new matrix-type layout showing all single and bivariate partial dependence plots, and an alternative layout based on graph Eulerians focusing on key subsets. Our new visualisations are model-agnostic and are applicable to regression and classification supervised learning settings. They enhance interpretation even in situations where the number of variables is large and the interaction structure complex.

To install vivid use:

`install.packages("vivid")`


Alternatively you can install the latest development version of the package in R with the commands:

```
if(!require(remotes)) install.packages('remotes')
remotes::install_github('AlanInglis/vivid')
```

You can then load the package with:

`library(vivid)`
