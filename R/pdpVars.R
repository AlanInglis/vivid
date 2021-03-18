library(colorspace)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(lemon)

library(gridExtra)
library(ranger)
# rf <- ranger(Species ~ ., data = iris, probability = TRUE)
# pdpVars(iris, rf, "Species",  class=3)
# pp <- pdpVars(iris, rf, "Species",  class=2, draw=F)
# pp[[1]]
# pdpVars(iris, rf, "Species", class=2, colorVar="Species")

pdpVars <- function(data, fit,response,
                    vars = NULL, pal=rev(RColorBrewer::brewer.pal(11,"RdYlBu")),
                    gridSize = 10, nmax=500,class = 1,
                    nIce=30, predictFun=NULL, limits=NULL, colorVar=NULL, draw=TRUE) {

  data <- na.omit(data)
  if (is.null(nmax)) nmax <- nrow(data)
  nmax <- max(5,nmax)
  if (is.numeric(nmax) && nmax < nrow(data) ){
    data <- data[sample(nrow(data), nmax), , drop = FALSE]
  }
  gridSize <- min(gridSize, nmax)

  classif <- is.factor(data[[response]]) | inherits(fit, "LearnerClassif")
  if (is.null(predictFun)) predictFun <- vividTemp:::CVpredictfun(classif, class)


  predData <- predictFun(fit,data)

  vars0 <- names(data)
  vars0 <- vars0[- match(response, vars0)]
  vars <- vars[vars %in% vars0]
  if (is.null(vars)) vars <- vars0

  nIce <- min(nIce, nrow(data))
  sice <- c(NA, sample(nrow(data), nIce)) # for use with iceplots


  data$predData <- predData
  pdplist1 <- vector("list", length=length(vars))
  for (i in 1:length(vars)){
    px <-vividTemp:::pdp_data(data, vars[i], gridsize=gridSize)
    px$.pid <- i
    pdplist1[[i]] <-px
  }
  pdplist1 <- bind_rows(pdplist1)
  pdplist1$fit <- predictFun(fit, pdplist1)
  pdplist1<- split(pdplist1, pdplist1$.pid)

  names(pdplist1)  <- vars

  if (is.null(limits)){
    r <- sapply(pdplist1, function(x) range(x$fit))
    r <- range(c(r,predData))
    limits <- range(labeling::rpretty(r[1],r[2]))
  }

  ice <- function(var) {
    pdp <- pdplist1[[var]]
    aggr <- pdp %>% group_by(.data[[var]])   %>% summarise(fit = mean(fit))
    pdp1 <- filter(pdp, .id %in% sice)
    if (is.null(colorVar))
      p <- pdp1 %>%
      ggplot(aes(x=.data[[var]],y=fit)) +
      geom_line( aes(color=predData,group=.id))+
      scale_color_gradientn(name = "yhat",colors = pal, limits = limits,oob=scales::squish)
    else
      p <- pdp1 %>%
      ggplot(aes(x=.data[[var]],y=fit)) +
      geom_line( aes(color=.data[[colorVar]],group=.id))

    p <- p+
      geom_line(data = aggr, size = 1, color = "black", lineend = "round", group=1)+
      theme_bw()+guides(fill=FALSE, color=FALSE)+ ylab("   ") + ylim(limits)
     if (var == vars[[1]]) p<-p+ ylab("pdp/ice")
    p
  }

  plots <- lapply(vars, ice)
  if (!is.null(colorVar))
    legend_y <- lemon::g_legend(plots[[1]]+ guides(color="legend"))
  else
  legend_y <- lemon::g_legend(plots[[1]]+ guides(color="colorbar"))
  plots <- c(plots, list(legend_y))
  if (draw)
    gridExtra::grid.arrange(grobs=plots, widths=c(rep(1, length(vars)),.4))
  invisible(plots)
}
