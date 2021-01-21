#' zPath
#'
#' #' Construct a path of indices to visit to order variables
#'
#' @description Constructs a zenpath for connecting and displaying pairs.
#'
#' @param viv A matrix of class 'vivid' to be used to calculate the path.
#' @param cutoff Do not include any variables that are below the cutoff interaction value.
#' @param method String indicating the method to use. The available methods are:
#' "greedy.weighted": Sort all pairs according to a greedy (heuristic) Euler path with x as weights visiting each edge precisely once.
#' "strictly.weighted": Strictly respect the order of the weights - so the first, second, third, and so on, adjacent pair of numbers
#' of the output of zenpath() corresponds to the pair with largest, second-largest, third-largest, and so on, weight.
#' see zenpath
#' @param connect If connect is TRUE, connect the edges from separate eulerians (strictly.weighted only).
#'
#'
#' @return Returns a zpath from viv showing pairs with viv entry over the cutoff
#'
#' @importFrom zenplots "zenpath"
#' @importFrom zenplots "connect_pairs"
#'
#'
#' @examples
#' \dontrun{
#' aq <- na.omit(airquality)*1.0
#'
#' # Run an mlr3 ranger model:
#' library(mlr3)
#' library(mlr3learners)
#' library(ranger)
#' ozonet  <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' ozonel  <- lrn("regr.ranger", importance = "permutation")
#' ozonef  <- ozonel$train(ozonet)
#'
#' viv <- vivi(aq, ozonef, "Ozone")
#'
#'# Calculate Zpath:
#' zpath<-zPath(viv,.8)
#' zpath
#'}
#' @export



zPath <- function(viv, cutoff=NULL, method=c("greedy.weighted", "strictly.weighted"), connect=TRUE){


  method <- match.arg(method)

  diag(viv)<- NA
  viv[upper.tri(viv)] <- NA
  # find the off-diagonal entries in viv that are bigger than some number
  if (!is.numeric(cutoff)) cutoff <- quantile(viv, .8, na.rm=TRUE)
  viv[is.na(viv)]<- 0
  w <- viv>cutoff
if (sum(w) == 0) stop("No off diagonal entries in 'viv' exceed 'cutoff'.")
  zinfo <- cbind(viv[w],  row(viv)[w],col(viv)[w])

  # form an eulerian path with these pairs of variables
  if (method=="greedy.weighted"){
    zpath <- tryCatch(zpath <- zenpath(zinfo[,1], pairs=zinfo[,-1], method="greedy.weighted"),
                      error = function(e) NULL,warning = function(w) {})
  }
  if (method=="strictly.weighted"| is.null(zpath)| length(zpath)==0){
    zpath <- zenpath(zinfo[,1], pairs=zinfo[,-1], method="strictly.weighted")
    zpath <-  connect_pairs(zpath)
    if (connect) zpath <- unlist(zpath)
  }

  if (is.numeric(zpath))
    zpath <- rownames(viv)[zpath]
  else if (is.list(zpath)) zpath <- lapply(zpath, function(z) rownames(viv)[z])

  zpath
}
