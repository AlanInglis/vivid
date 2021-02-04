#' viviNetwork
#'
#'  @description Create a Network  plot displaying Variable
#'  and Variable Interaction.
#'
#' @param mat A matrix, such as that returned by vivi, of values to be plotted.
#' @param threshold Remove edges with weight below this value if provided.
#' @param intPal A vector of colours to show interactions, for use with scale_fill_gradientn.
#' @param impPal A vector of colours to show importance, for use with scale_fill_gradientn.
#' @param intLims Specifies the fit range for the color map for interaction strength.
#' @param impLims Specifies the fit range for the color map for importance.
#' @param labelNudge A value to determine the y_postioning of the variables names. A higher value will postion the label farther above the nodes.
#' @param layout Layout of the plotted graph.
#' @param cluster Either a vector of cluster memberships for nodes or an igraph clustering function.
#' @param ...
#'
#' @return A plot displaying interaction strength between variables on the edges and variable importance on the nodes.
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
#' library(ranger)
#' aq <- na.omit(airquality)
#' rF <- ranger(Ozone ~ ., data = aq, importance = "permutation")
#' myMat <- vivi(fit = rF, data = aq, response = "Ozone")
#' viviNetwork(myMat)
#' @export
# Plotting Function -------------------------------------------------------
viviNetwork <- function(mat,
                        threshold = 0,
                        intLims = NULL,
                        impLims = NULL,
                        intPal = rev(sequential_hcl(palette = "Blues 3", n = 11)),
                        impPal = rev(sequential_hcl(palette = "Reds 3", n = 11)),
                        labelNudge = 0.05,
                        layout = "circle",
                        cluster = NULL,
                        ...) {


  # setting up of variables -------------------------------------------


  # Get importance values
  imp <- diag(mat) # used to scale the size of points
  impFill <- imp # used in plotting for geom_point


  # Sort interaction values
  sortInt <- t(mat)[lower.tri(t(mat), diag = FALSE)] # get upper triangle of the matrix by row order
  sorted_Int <- sort(sortInt, index.return = TRUE) # Sort values whilst preserving the index
  int <- sorted_Int$x # used to set edge weight
  nam <- colnames(mat) # Get feature names



  # Limits ------------------------------------------------------------------

  # set the limits for importance
  if (is.null(impLims)) {
    impLims <- range(diag(mat))
    limitsImp <- range(labeling::rpretty(impLims[1], impLims[2]))
  } else {
    limitsImp <- impLims
  }

  # set the limits for interactions
  if (is.null(intLims)) {
    intLims <- range(as.dist(mat))
    limitsInt <- range(labeling::rpretty(intLims[1], intLims[2]))
  } else {
    limitsInt <- intLims
  }

  # Warning messages --------------------------------------------------------

  if (!is.null(intLims) && intLims[1] > min(as.dist(mat))) {
    stop("Error: Minimum chosen limit for interaction is larger
  than the minimum measured interaction value.
  Please choose a minimum limit value less than or equal to the minimum measured value.")
  }

  if (!is.null(intLims) && intLims[2] < max(as.dist(mat))) {
    stop("Error: Maximum chosen limit for interaction is smaller
  than the maximum measured interaction value.
  Please choose a maximum limit value greater than or equal to the maximum measured value.")
  }

  if (!is.null(impLims) && impLims[1] > min(diag(mat))) {
    stop("Error: Minimum chosen limit for importance is larger
  than the minimum measured importance value.
  Please choose a minimum limit value less than or equal to the minimum measured value.")
  }

  if (!is.null(impLims) && impLims[2] < max(diag(mat))) {
    stop("Error: Maximum chosen limit for importance is smaller
  than the maximum measured importance value.
  Please choose a maximum limit value greater than or equal to the maximum measured value.")
  }

  # Setting up graph properties ---------------------------------------------

  # Set path direction of graph:
  to <- NULL
  g <- sample_pa(length(nam), m = length(nam)) # generate scale free graph
  df <- igraph::as_data_frame(g) # create data frame from graph
  gDF <- dplyr::arrange(df, to) # arrange rows by column name
  gDFL <- rbind(gDF$from, gDF$to) # combine
  matched_gDFL <- gDFL[, sorted_Int$ix]

  # Create network graph:
  net.bg <- make_graph(matched_gDFL, length(nam))

  # Scale and round values:
  E(net.bg)$weight <- int # set edge weight to equal interaction values
  impScaled <- (5 - 1) * ((imp - min(imp)) / (max(imp) - min(imp))) + 1 # scale between 1-5


  # Set the edge colours
  if (is.null(intLims)) {
    edgeColour <- (E(net.bg)$weight) # edge weights
    cut_int <- cut(edgeColour, 10) # cut
    edgeCols <- intPal[cut_int]
  } else {
    edgeColour <- (E(net.bg)$weight) # edge weights
    ## Use n equally spaced breaks to assign each value to n-1 equal sized bins
    ii <- cut(edgeColour,
      breaks = seq(min(intLims), max(intLims), len = 10),
      include.lowest = TRUE
    )
    edgeCols <- intPal[ii]
  }

  # Get edge weights
  weightDF <- get.data.frame(net.bg) # get df of graph attributes
  edgeWidth1 <- weightDF$weight # select edge weight
  edgeWidthScaled <- (5 - 1) * ((edgeWidth1 - min(edgeWidth1)) / (max(edgeWidth1) - min(edgeWidth1))) + 1 # scale between 1-5 for graphic



  # THRESHOLDING ------------------------------------------------------------



  if (threshold > 0) {
    a <- sort(int, decreasing = TRUE)
    # Warning message if threshold value is set too high or too low
    if (threshold > max(a)) {
      stop("Selected threshold value is larger than maximum interaction strength")
    } else if (threshold < 0) {
      stop("Selected threshold value is less than minimum interaction strength")
    }
    idx <- which(a > threshold)
    cut.off <- a[1:max(idx)]
    # Thresholded colours
    indexCol <- rev(edgeCols)
    edgeCols <- indexCol[idx]
    edgeCols <- rev(edgeCols)
    # Thresholded edge weights
    indexWeight <- rev(edgeWidthScaled)
    edgeW <- indexWeight[idx]
    edgeW <- rev(edgeW)
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
    edgeW <- (5 - 1) * ((edgeWidth1 - min(edgeWidth1)) / (max(edgeWidth1) - min(edgeWidth1))) + 1 # scale between 1-5
  }





  # Plot graph ----------------------------------------------------

  # set layout
  l <- layout

  if (!is.null(cluster)) {
    l_1 <- l

    # add numeric vector to cluster by, else use igraph clustering
    if (is.numeric(cluster)) {
      group <- factor(cluster)
    } else {
      com <- cluster(net.sp)
      V(net.sp)$color <- com$membership
      group <- V(net.sp)$color
      group <- factor(group)
    }


    # g <- set_graph_attr(net.sp, "layout", layout_in_circle(net.sp))
    colrs <- adjustcolor(c(
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green",
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green"
    ))
    colorC <- colrs[group]

    # plot clustered graph
    pcl <- ggnet2(net.sp,
      mode = l_1,
      size = 0,
      edge.size = edgeW,
      edge.color = edgeCols
    ) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam), nudge_y = labelNudge) +
      geom_point(aes(fill = impFill), size = impScaled * 2, col = "transparent", shape = 21) +
      scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits = limitsImp) +
      new_scale_fill() +
      geom_point(aes(x = 0, y = 0, fill = impFill), size = -1) +
      scale_fill_gradientn(name = "Interaction\nStrength", colors = intPal, limits = limitsInt)

    # Group clusters
    groupV <- as.vector(group)
    fillCols <- c(
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green",
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green"
    )

    # encircle groups
    colCluster <- fillCols[group]
    colCluster <- as.vector(colCluster)
    pcl <- pcl + geom_encircle(aes(group = groupV),
      spread = 0.01,
      alpha = 0.2,
      expand = 0.03,
      fill = colCluster
    )
    return(pcl)
  } else {
    p <- ggnet2(net.sp,
      mode = l,
      size = 0,
      edge.size = edgeW,
      edge.color = edgeCols
    ) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam), nudge_y = labelNudge) +
      geom_point(aes(fill = impFill), size = impScaled * 2, colour = "transparent", shape = 21) +
      scale_fill_gradientn(name = "Variable\nImportance", colors = impPal, limits = limitsImp) +
      new_scale_fill() +
      geom_point(aes(x = 0, y = 0, fill = impFill), size = -1) +
      scale_fill_gradientn(name = "Interaction\nStrength", colors = intPal, limits = limitsInt)

    return(p)
  }
}
