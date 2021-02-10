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
#'
#' @return A plot displaying interaction strength between variables on the edges and variable importance on the nodes.
#'
#' @import igraph
#' @import ggplot2
#' @importFrom GGally "ggnet2"
#' @importFrom ggnewscale "new_scale_fill"
#' @importFrom reshape "melt"
#' @importFrom ggalt "geom_encircle"
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
                        cluster = NULL) {


  # Set up ------------------------------------------------------------------


  # get names
  nam <- colnames(mat)

  # get imp values and scale
  imp <- diag(mat)
  impScaled <- (5 - 1) * ((imp - min(imp)) / (max(imp) - min(imp))) + 1 # scale between 1-5 for graphic


  # get int values and scale
  intV <- as.dist(mat)
  df <- melt(as.matrix(intV), varnames = c("row", "col")) # turn into df
  df <- df[-seq(1, NROW(df), by = (length(nam)+1)), ] # remove imp vals
  df <- df[!duplicated(t(apply(df, 1, sort))), ] # remove duplicates
  int <- df$value # extract interaction values
  edgeWidthScaled <- (5 - 1) * ((int - min(int)) / (max(int) - min(int))) + 1 # scale between 1-5 for graphic

  # Set up & create graph ---------------------------------------------------

  # create all pairs and turn into vector for graph edges
  pairs <- expand.grid(1:length(nam), 1:length(nam)) # create all pairs
  pairs <- pairs[!pairs$Var1 == pairs$Var2, ] # remove matching rows
  pairs <- pairs[!duplicated(t(apply(pairs, 1, sort))), ] # remove duplicates
  ed <- as.vector(t(pairs)) # turn into vecotr


  # create graph
  g <- make_empty_graph(n = ncol(mat))
  g <- add_edges(graph = g, edges = ed)

  # add edge weight
  E(g)$weight <- int


  # Edge colour set up -------------------------------------------------------------

  # Set the edge colours
  if (is.null(intLims)) {
    edgeColour <- (E(g)$weight) # edge weights
    cut_int <- cut(edgeColour, 10) # cut
    edgeCols <- intPal[cut_int]
  } else {
    edgeColour <- (E(g)$weight) # edge weights
    ## Use n equally spaced breaks to assign each value to n-1 equal sized bins
    ii <- cut(edgeColour,
      breaks = seq(min(intLims), max(intLims), len = 10),
      include.lowest = TRUE
    )
    edgeCols <- intPal[ii]
  }

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


  # THRESHOLDING ------------------------------------------------------------

  if (threshold > 0) {
    a <- sort(unique(int), decreasing = TRUE)
    # Warning message if threshold value is set too high or too low
    if (threshold > max(a)) {
      stop("Selected threshold value is larger than maximum interaction strength")
    } else if (threshold < 0) {
      stop("Selected threshold value is less than minimum interaction strength")
    }
    idx <- which(a > threshold)
    cut.off <- a[1:max(idx)]
    # Thresholded colours
    indexCol <- edgeCols
    edgeCols <- indexCol[idx]
    # Thresholded edge weights
    indexWeight <- edgeWidthScaled
    edgeWidthScaled <- indexWeight[idx]
    # Thresholded network
    `%notin%` <- Negate(`%in%`)
    g <- delete_edges(g, E(g)[E(g)$weight %notin% cut.off])

    # Delete vertex that have no edges (if thresholding)
    Isolated <- which(igraph::degree(g) == 0)
    if (length(Isolated) == 0) {
      g <- g
    } else {
      g <- igraph::delete.vertices(g, Isolated)
      imp <- imp[-c(Isolated)]
      impScaled <- impScaled[-c(Isolated)]
      nam <- nam[-c(Isolated)]
    }
  }

  # Plot graph ----------------------------------------------------

  if (is.null(cluster)) {
    # plotting
    p <- ggnet2(g,
      mode = layout,
      size = 0,
      edge.size = edgeWidthScaled,
      edge.color = edgeCols
    ) +
      theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam), nudge_y = labelNudge) +
      geom_point(aes(fill = imp), size = impScaled * 2, colour = "transparent", shape = 21) +
      scale_fill_gradientn(
        name = "Vimp", colors = impPal, limits = limitsImp,
        guide = guide_colorbar(
          frame.colour = "black",
          ticks.colour = "black"
        ), oob = scales::squish
      ) +
      new_scale_fill() +
      geom_point(aes(x = 0, y = 0, fill = imp), size = -1) +
      scale_fill_gradientn(
        name = "Vint", colors = intPal, limits = limitsInt,
        guide = guide_colorbar(
          frame.colour = "black",
          ticks.colour = "black"
        ), oob = scales::squish
      ) +
      theme(aspect.ratio = 1)
    return(p)
  } else {
    ## Clustering plot
    # add numeric vector to cluster by, else use igraph clustering
    if (is.numeric(cluster)) {
      group <- factor(cluster)
    } else {
      com <- cluster(g)
      V(g)$color <- com$membership
      group <- V(g)$color
      group <- factor(group)
    }


    # g <- set_graph_attr(g, "layout", layout_in_circle(g))
    colrs <- adjustcolor(c(
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green",
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green"
    ))
    colorC <- colrs[group]

    # plot clustered graph
    pcl <- ggnet2(g,
      mode = layout,
      size = 0,
      edge.size = edgeWidthScaled,
      edge.color = edgeCols
    ) + theme(legend.text = element_text(size = 10)) +
      geom_label(aes(label = nam), nudge_y = labelNudge) +
      geom_point(aes(fill = imp), size = impScaled * 2, colour = "transparent", shape = 21) +
      scale_fill_gradientn(
        name = "Vimp", colors = impPal, limits = limitsImp,
        guide = guide_colorbar(
          frame.colour = "black",
          ticks.colour = "black"
        ), oob = scales::squish
      ) +
      new_scale_fill() +
      geom_point(aes(x = 0, y = 0, fill = imp), size = -1) +
      scale_fill_gradientn(
        name = "Vint", colors = intPal, limits = limitsInt,
        guide = guide_colorbar(
          frame.colour = "black",
          ticks.colour = "black"
        ), oob = scales::squish
      ) +
      theme(aspect.ratio = 1)

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
  }
}
