#' vividNetwork
#'
#'   @description Create a Network style plot displaying Variable
#'  and Variable Interaction.
#'
#' @param mat A matrix of values to be plotted. Either added by the user or created using the prepFunc() function.
#' @param thresholdValue A value chosen by the user which will show all the edges with weights (i.e., the interacions) above that value. For example, if thresholdValue = 0.2, then only the the interacions greater than 0.2 will be displayed.
#' @param label If label = TRUE the numerical value for the interaction strength will be displayed.
#' @param intPal A colorspace colour palette to display the interaction values.
#' @param impPal A colorspace colour palette to display the importance values.
#' @param labelNudge A value, set by the user, to determine the y_postioning of the variables names. A higher value will postion the label farther above the nodes.
#' @param layout Determines the shape, or layout, of the plotted graph.
#' @param cluster If cluster = TRUE, then the data is clustered in groups.
#' @param clusterType = Network-based clustering. Any of the appropriate cluster types from the igraph package are allowed.
#' @param clusterLayout = Determines the shape, or layout, of the clustered plotted graph.
#' @param ... Not currently implemented.
#'
#' @return A newtwork style plot displaying interaction strength between variables on the edges and variable importance on the nodes.
#'
#' @import igraph
#' @importFrom igraph "sample_pa"
#' @importFrom igraph "as_data_frame"
#' @importFrom igraph "make_graph"
#' @import ggplot2
#' @importFrom GGally "ggnet2"
#' @importFrom ggnewscale "new_scale_fill"
#' @importFrom reshape "melt"
#' @importFrom ggalt "geom_encircle"
#' @importFrom cowplot "get_legend"
#' @importFrom cowplot "plot_grid"
#' @importFrom colorspace "sequential_hcl"
#'
#' @examples
#' # Load in the data:
#' aq <- data.frame(airquality)
#' aq <- na.omit(aq)
#'
#' # Run an mlr ranger model:
#' library(mlr3)
#' library(mlr3learners)
#' library(ranger)
#' aq_Task <- TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn <- lrn("regr.ranger", importance = "permutation")
#' aq_Mod <- aq_lrn$train(aq_Task)
#'
#' # Create matrix
#' myMat <- vividMatrix(task = aq_Task, model = aq_Mod)
#'
#' # Create plot:
#' plot(myMat, type = "network")
#'
#'
#'
#' # Plotting Function -------------------------------------------------------
plotNetwork <- function(mat,
                        model,
                        thresholdValue = 0,
                        label,
                        fitlimsInt = NULL,
                        fitlimsImp = NULL,
                        intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                        impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                        labelNudge = 0.05,
                        layout = "circle",
                        cluster = F,
                        clusterType = cluster_optimal,
                        clusterLayout = layout_with_fr,
                        ...) {


  # setting up of variables -------------------------------------------


  # Get importance values
  imp <- diag(mat) # used to scale the size of points
  impFill <- imp # used in plotting for geom_point


  # Sort interaction values
  sortInt <- t(mat)[lower.tri(t(mat), diag = FALSE)] # get upper triangle of the matrix by row order
  sorted_Int <- sort(sortInt, index.return = TRUE) # Sort values whilst preserving the index
  int <- sorted_Int$x
  nam <- colnames(mat) # Get feature names



  # Limits ------------------------------------------------------------------

  # max min int vals
  minimumInt <- min(as.dist(mat))
  maximumInt <- max(as.dist(mat))

  # max min imp vals
  vImportance <- diag(mat)
  maxImportance <- max(vImportance)
  minImportance <- min(vImportance)

  if (is.null(fitlimsInt)) {
    limitsInt <- c(minimumInt, maximumInt)
  } else {
    limitsInt <- fitlimsInt
  }

  if (is.null(fitlimsImp)) {
    limitsImp <- c(minImportance, maxImportance)
  } else {
    limitsImp <- fitlimsImp
  }

  # Warning messages --------------------------------------------------------

  if(!is.null(fitlimsInt) && fitlimsInt[1] > minimumInt){
    stop("Error: Minimum chosen limit for interaction is larger
  than the minimum measured interaction value.
  Please chose a minimum limit value less than or equal to the minimum measured value.")
  }

  if(!is.null(fitlimsInt) && fitlimsInt[2] < maximumInt){
    stop("Error: Maximum chosen limit for interaction is smaller
  than the maximum measured interaction value.
  Please chose a maximum limit value greater than or equal to the maximum measured value.")
  }

  if(!is.null(fitlimsImp) && fitlimsImp[1] > minImportance){
    stop("Error: Minimum chosen limit for importance is larger
  than the minimum measured importance value.
  Please chose a minimum limit value less than or equal to the minimum measured value.")
  }

  if(!is.null(fitlimsImp) && fitlimsImp[2] < maxImportance){
    stop("Error: Maximum chosen limit for importance is smaller
  than the maximum measured importance value.
  Please chose a maximum limit value greater than or equal to the maximum measured value.")
  }

  # Setting up graph properties ---------------------------------------------

  # Set path direction of graph:
  to <- NULL
  g <- sample_pa(length(nam), m = length(nam)) # generate scale free graph
  df <- igraph::as_data_frame(g) # create graph from data frame
  gDF <- dplyr::arrange(df, to) # arrange rows by column name
  gDFL <- rbind(gDF$from, gDF$to) # combine
  matched_gDFL <- gDFL[, sorted_Int$ix]

  # Create network graph:
  net.bg <- make_graph(matched_gDFL, length(nam))

  # Scale and round values:
  E(net.bg)$weight <- int # set edge weight to equal interaction values
  impScaled <- (5 - 1) * ((imp - min(imp)) / (max(imp) - min(imp))) + 1 # scale between 1-5


  # Set the edge colours
  if (is.null(fitlimsInt)) {
    colfunction <- intPal # col palette
    edgeColour <- (E(net.bg)$weight) # edge weights
    cut_int <- cut(edgeColour, 10) # cut
    edgeCols <- colfunction[cut_int]
  } else {
    edgeColour <- (E(net.bg)$weight) # edge weights
    ## Use n equally spaced breaks to assign each value to n-1 equal sized bins
    ii <- cut(edgeColour,
      breaks = seq(min(fitlimsInt), max(fitlimsInt), len = 10),
      include.lowest = TRUE
    )
    colfunction <- intPal[ii]
    edgeCols <- colfunction
  }

  # Get edge weights
  weightDF <- get.data.frame(net.bg) # get df of graph attributes
  edgeWidth1 <- weightDF$weight # select edge weight
  edgeWidthScaled <- (5 - 1) * ((edgeWidth1 - min(edgeWidth1)) / (max(edgeWidth1) - min(edgeWidth1))) + 1 # scale between 1-5 for graphic



  # THRESHOLDING ------------------------------------------------------------



  if (thresholdValue > 0) {
    a <- sort(int, decreasing = TRUE)
    # Warning message if threshold value is set too high or too low
    if (thresholdValue > max(a)) {
      stop("Selected threshold value is larger than maximum interaction strength")
    } else if (thresholdValue < 0) {
      stop("Selected threshold value is less than minimum interaction strength")
    }
    idx <- which(a > thresholdValue)
    cut.off <- a[1:max(idx)]
    # Thresholded colours
    indexCol <- rev(edgeCols)
    edgeCols <- indexCol[idx]
    edgeCols <- rev(edgeCols)
    # Thresholded edge weights
    indexWeight <- rev(edgeWidthScaled)
    edgeW <- indexWeight[idx]
    edgeW <- rev(edgeW)
    # Thresholded edge labels
    indexLabel <- rev(edgeWidth1)
    edgeL <- indexLabel[idx]
    edgeL <- rev(edgeL)
    # Thresholded network
    `%notin%` <- Negate(`%in%`)
    net.sp <- delete_edges(net.bg, E(net.bg)[E(net.bg)$weight %notin% cut.off])

    # Delete vertex that have no edges (if thresholding)
    Isolated <- which(igraph::degree(net.sp) == 0)
    if (length(Isolated) == 0) {
      net.sp <- net.sp
    } else {
      net.sp <- igraph::delete.vertices(net.sp, Isolated)
      impFill <- impFill[-c(Isolated)]
      impScaled <- impScaled[-c(Isolated)]
      nam <- nam[-c(Isolated)]
    }
  } else {
    net.sp <- net.bg
    weightDF <- get.data.frame(net.sp) # get df of graph attributes
    edgeL <- weightDF$weight # select edge weight
    edgeW <- (5 - 1) * ((edgeWidth1 - min(edgeWidth1)) / (max(edgeWidth1) - min(edgeWidth1))) + 1 # scale between 1-5
  }




  # Set plot properties -----------------------------------------------------

  # set layout
  l <- layout

  # Whether to show edge label
  if (label == T) {
    edgeL <- edgeL
    edgeL <- round(edgeL, 3)
  } else {
    edgeL <- NULL
  }


  # Plot graph ----------------------------------------------------

  if (cluster) {
    l_1 <- clusterLayout(net.sp)
    # l_1 <- layout_in_circle(net.sp)
    com <- clusterType(net.sp)
    V(net.sp)$color <- com$membership
    group <- V(net.sp)$color
    group <- factor(group)


    # g <- set_graph_attr(net.sp, "layout", layout_in_circle(net.sp))
    colrs <- adjustcolor(c(
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green",
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green"
    ))
    colorC <- colrs[group]


    pcl <- ggnet2(net.sp,
      mode = l_1,
      size = 0,
      edge.size = edgeW,
      edge.label = edgeL,
      edge.color = edgeCols
    ) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam), nudge_y = labelNudge) +
      geom_point(aes(fill = impFill), size = impScaled * 2, col = "transparent", shape = 21) +
      scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits = limitsImp) +
      theme(legend.position = "none")


    ppcl <- ggnet2(net.sp,
      mode = l_1,
      size = 0,
      edge.size = edgeW,
      edge.label = edgeL,
      edge.color = edgeCols,
      palette = intPal
    ) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam), nudge_y = labelNudge) +
      geom_point(aes(fill = impFill), size = impScaled * 2, col = "transparent", shape = 21) +
      scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits = limitsImp) +
      guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))


    intDFcl <- as.data.frame(int)
    pppcl <- ggplot(intDFcl) +
      geom_tile(aes(x = 0, y = 0, fill = int), size = -1) +
      scale_fill_gradientn(name = "Interaction\nStrength", colors = intPal, limits = limitsInt) +
      guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))


    # Group clusters
    groupV <- as.vector(group)
    fillCols <- c(
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green",
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green"
    )
    colCluster <- fillCols[group]
    colCluster <- as.vector(colCluster)
    pcl <- pcl + geom_encircle(aes(group = groupV),
      spread = 0.01,
      alpha = 0.2,
      expand = 0.03,
      fill = colCluster
    )


    # Grab the legends using cowplot::get_legend()
    pcl2_legend <- get_legend(ppcl)
    pcl3_legend <- get_legend(pppcl)

    # Combine the legends one on top of the other
    legendsCl <- plot_grid(pcl2_legend, pcl3_legend, ncol = 1, nrow = 2)

    # Combine the heatmap with the legends
    endPlotCl <- plot_grid(pcl, legendsCl,
      ncol = 2, align = "h",
      scale = c(1, 0.8), rel_widths = c(0.9, 0.1)
    )


    return(endPlotCl)
  } else {
    p <- ggnet2(net.sp,
      mode = l,
      size = 0,
      edge.size = edgeW,
      edge.label = edgeL,
      edge.color = edgeCols
    ) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam), nudge_y = labelNudge) +
      geom_point(aes(fill = impFill), size = impScaled * 2, colour = "transparent", shape = 21) +
      scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits = limitsImp) +
      theme(legend.position = "none")




    pp <- ggnet2(net.sp,
      mode = l,
      size = 0,
      edge.size = edgeW,
      edge.label = edgeL,
      edge.color = edgeCols
    ) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam), nudge_y = labelNudge) +
      geom_point(aes(fill = impFill), size = impScaled * 2, colour = "transparent", shape = 21) +
      scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits = limitsImp) +
      guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))

    intDF <- as.data.frame(int)

    ppp <- ggplot(intDF) +
      geom_tile(aes(x = 0, y = 0, fill = int), size = -1) +
      scale_fill_gradientn(name = "Interaction\nStrength", colors = intPal, limits = limitsInt) +
      guides(fill = guide_colorbar(frame.colour = "gray", frame.linewidth = 1.5))


    # Grab the legends using cowplot::get_legend()
    p2_legend <- get_legend(pp)
    p3_legend <- get_legend(ppp)

    # Combine the legends one on top of the other
    legends <- plot_grid(p2_legend, p3_legend, ncol = 1, nrow = 2)

    # Combine the network with the legends
    endPlot <- plot_grid(p, legends,
      ncol = 2, align = "h",
      scale = c(1, 0.8), rel_widths = c(0.9, 0.1)
    )

    return(endPlot)
  }
}
