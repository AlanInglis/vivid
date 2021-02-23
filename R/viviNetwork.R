#' viviNetwork
#'
#'  @description Create a Network plot displaying variable importance
#'  and variable interaction.
#'
#' @param mat A matrix, such as that returned by vivi, of values to be plotted.
#' @param threshold Remove edges with weight below this value if provided.
#' @param intPal A vector of colours to show interactions, for use with scale_fill_gradientn.
#' @param impPal A vector of colours to show importance, for use with scale_fill_gradientn.
#' @param intLims Specifies the fit range for the color map for interaction strength.
#' @param impLims Specifies the fit range for the color map for importance.
#' @param removeNode If TRUE, then removes nodes with no connecting edges when thresholding interaction values.
#' @param layout Layout of the plotted graph.
#' @param cluster Either a vector of cluster memberships for nodes or an igraph clustering function.
#'
#' @return A plot displaying interaction strength between variables on the edges and variable importance on the nodes.
#'
#' @import igraph
#' @import ggplot2
#' @importFrom GGally "ggnet2"
#' @importFrom ggnewscale "new_scale_fill"
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
                        removeNode = FALSE,
                        layout = "circle",
                        cluster = NULL) {

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
  # Set up ------------------------------------------------------------------

  # convert to df
  df <- as.data.frame(mat, class = "vivid")

  # get names
  nam <- colnames(mat)

  # get imp values and scale
  imp <- diag(mat)
  impScaled <- (5 - 1) * ((imp - min(imp)) / (max(imp) - min(imp))) + 1 # scale between 1-5 for graphic

  # get int vals and scale
  df$Variable_1 <- as.character(df$Variable_1)
  df$Variable_2 <- as.character(df$Variable_2)

  result <- transform(df, Variable_1 = pmin(Variable_1, Variable_2), Variable_2 = pmax(Variable_1, Variable_2))
  dfInt <- subset(result, (!duplicated(result[1:2])) & Measure != "Vimp")

  dfInt <- dfInt[with(dfInt, order(Value)), ]
  int <- dfInt$Value # extract interaction values
  edgeWidthScaled <- (5 - 1) * ((int - min(int)) / (max(int) - min(int))) + 1 # scale between 1-5 for graphic

  # Set up & create graph ---------------------------------------------------

  # create all pairs for graph edges
  pairs <- dfInt[c("Row", "Col")]
  pairs <- as.vector(t(pairs))

  # create graph
  g <- make_empty_graph(n = ncol(mat))
  g <- add_edges(graph = g, edges = pairs)

  # add edge weight
  E(g)$weight <- int


  # Edge colour set up -------------------------------------------------------------

  # Set the edge colours
  if (is.null(intLims)) {
    cut_int <- cut(int, length(intPal)) # cut
    edgeCols <- intPal[cut_int]
  } else {
    ## Use n equally spaced breaks to assign each value to n-1 equal sized bins
    ii <- cut(int,
      breaks = seq(min(intLims), max(intLims), len = length(intPal)),
      include.lowest = TRUE
    )
    edgeCols <- intPal[ii]
  }



  # THRESHOLDING ------------------------------------------------------------

  if (threshold > 0) {

    # Warning message if threshold value is set too high or too low
    if (threshold > max(int)) {
      warning("Selected threshold value is larger than maximum interaction strength")
    } else if (threshold < 0) {
      warning("Selected threshold value is less than minimum interaction strength")
    }

    idx <- which(int > threshold)

    dfRemove <- dfInt[dfInt$Value < threshold, ]
    pairsRemove <- dfRemove[c("Row", "Col")]
    # g1 <- delete.edges(g, pairsRemove)

    # delete edges
    # g <- delete.edges(g, which(int < threshold)) OG

    edgesRemove <- apply(pairsRemove, 1, paste, collapse = "|")
    edgesRemove <- edges(edgesRemove)

    # remove edges from graph
    g <- g - edgesRemove

    # Thresholded colours
    edgeCols <- rev(edgeCols)
    edgeCols <- rev(edgeCols)[idx]

    # Thresholded edge weights
    indexWeight <- rev(edgeWidthScaled)
    edgeWidthScaled <- rev(indexWeight)[idx]


    if (removeNode) {
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
    geom_label(aes(label = nam)) +
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
    )


  ## Clustering plot
  if (!is.null(cluster)) {

    # add numeric vector to cluster by, else use igraph clustering
    if (is.numeric(cluster)) {
      group <- cluster
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

    colPal <- rep(colPal, length.out = length(group)) # get correct amount of aesthetics
    colCluster <- colPal[group] # select colours

    # plot
    p <- p + geom_encircle(aes(group = group),
      spread = 0.01,
      alpha = 0.2,
      expand = 0.03,
      fill = colCluster
    )
  }


  # Move labels to outside --------------------------------------------------

  geoms <- sapply(p$layers, function(x) class(x$geom)[1]) # get geoms
  segments <- p$layers[[which(geoms == "NewGeomSegment")]] # select segment
  labels <- p$layers[[which(geoms == "NewGeomLabel")]] # select layers

  # set some values
  segments$data <- segments$data - 0.5
  p$data$x <- p$data$x - 0.5
  p$data$y <- p$data$y - 0.5

  labels$position$y <- 0


  labels$data <- p$data
  labels$data$x <- labels$data$x * 1.1
  labels$data$y <- labels$data$y * 1.1

  p$scales$scales <- lapply(p$scales$scales, function(x) {
    if (class(x)[1] == "ScaleContinuousPosition") ScaleContinuousPosition else x
  })
  p <- p + theme(axis.text = element_blank()) +
    theme_void() +
    theme(aspect.ratio = 1)
  return(p)
}
