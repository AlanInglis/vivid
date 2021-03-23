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

