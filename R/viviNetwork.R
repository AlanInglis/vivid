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

  # convert to df
  df <- matrix2df(mat)

  # get names
  nam <- colnames(mat)

  # get imp values and scale
  imp <- diag(mat)
  # dfImp <- which(df$Variable_1 == df$Variable_2)
  # imp <- df$Vimp[dfImp]
  # imp <- setNames(imp, df$Variable_1[dfImp])
  impScaled <- (5 - 1) * ((imp - min(imp)) / (max(imp) - min(imp))) + 1 # scale between 1-5 for graphic




  # get int values and scale
  dfInt <- data.frame(
    Variable_1 = df$Variable_1,
    Variable_2 = df$Variable_2,
    Vint = df$Vint,
    row = df$row,
    col = df$col
  )

  dfInt <- dfInt[-seq(1, NROW(dfInt), by = (length(nam) + 1)), ] # remove imp vals
  dfInt <- dfInt[!duplicated(t(apply(dfInt, 1, sort))), ] # remove duplicates

  dfInt <- dfInt[with(dfInt, order(-Vint)), ]
  int <- dfInt$Vint # extract interaction values
  edgeWidthScaled <- (5 - 1) * ((int - min(int)) / (max(int) - min(int))) + 1 # scale between 1-5 for graphic
  edgeWidthScaled <- rev(edgeWidthScaled) # reversing for drawing graphic

  # Set up & create graph ---------------------------------------------------

  # create all pairs for graph edges
  pairs <- data.frame(row = dfInt$row, col = dfInt$col)
  pairs <- rev(as.vector(t(pairs)))


  # create graph
  g <- make_empty_graph(n = ncol(mat))
  g <- add_edges(graph = g, edges = pairs)

  # add edge weight
  E(g)$weight <- int


  # Edge colour set up -------------------------------------------------------------

  # Set the edge colours
  if (is.null(intLims)) {
    edgeColour <- (E(g)$weight) # edge weights
    cut_int <- cut(edgeColour, 10) # cut
    edgeCols <- rev(intPal[cut_int])
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
    a <- sort(int, decreasing = TRUE)
    # Warning message if threshold value is set too high or too low
    if (threshold > max(a)) {
      stop("Selected threshold value is larger than maximum interaction strength")
    } else if (threshold < 0) {
      stop("Selected threshold value is less than minimum interaction strength")
    }

    idx <- which(a > threshold)

    # getting edge weight
    revEdgeWeight <- rev(E(g)$weight)
    g <- delete.edges(g, which(revEdgeWeight < threshold))

    # Thresholded colours
    edgeCols <- rev(edgeCols)[idx]
    edgeCols <- rev(edgeCols)

    # Thresholded edge weights
    indexWeight <- rev(edgeWidthScaled)[idx]
    edgeWidthScaled <- rev(indexWeight)


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


  ## Clustering plot
  if (!is.null(cluster)) {

    # add numeric vector to cluster by, else use igraph clustering
    if (is.numeric(cluster)) {
      group <- factor(cluster)
    } else {
      com <- cluster(g)
      V(g)$color <- com$membership
      group <- V(g)$color
      group <- factor(group)
    }

    # encircle groups
    colPal <- c(
      "yellow", "red", "blue", "black", "purple",
      "orange", "pink", "green"
    )

    colPal <- rep(colPal, times = length(group)) # get correct amount of aesthetics
    colCluster <- colPal[group] # select colours

    # plot
    p <- p + geom_encircle(aes(group = group),
      spread = 0.01,
      alpha = 0.2,
      expand = 0.03,
      fill = colCluster
    )
  }
  return(p)
}
