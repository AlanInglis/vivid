#' viviNetwork
#'
#' @description Create a Network plot displaying variable importance
#'  and variable interaction.
#'
#' @param mat A matrix, such as that returned by vivi, of values to be plotted.
#' @param intThreshold Remove edges with weight below this value if provided.
#' @param intPal A vector of colours to show interactions, for use with scale_fill_gradientn.
#' @param impPal A vector of colours to show importance, for use with scale_fill_gradientn.
#' @param intLims Specifies the fit range for the color map for interaction strength.
#' @param impLims Specifies the fit range for the color map for importance.
#' @param removeNode If TRUE, then removes nodes with no connecting edges when thresholding interaction values.
#' @param layout Layout of the plotted graph.
#' @param cluster Either a vector of cluster memberships for nodes or an igraph clustering function.
#' @param nudge Move labels a small distance from the nodes they are labelling. Ideal values are between 0 and 1.
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
                        intThreshold = 0,
                        intLims = NULL,
                        impLims = NULL,
                        intPal = rev(sequential_hcl(palette = "Blues 3", n = 100)),
                        impPal = rev(sequential_hcl(palette = "Reds 3", n = 100)),
                        removeNode = FALSE,
                        layout = "circle",
                        cluster = NULL,
                        nudge = 0.5) {


  # Set up ------------------------------------------------------------------

  # convert to df
  df <- as.data.frame(mat)

  # get names
  nam <- colnames(mat)

  # get imp values and scale
  imp <- diag(mat)

  # get int vals and scale
  # remove duplicates
  dfInt <- df[-which(df$Row < df$Col), ]
  # remove imp vals
  dfInt <- dfInt[which(dfInt$Measure == "Vint"), ]
  # sort
  dfInt <- dfInt[with(dfInt, order(Value)), ]
  dfLimsInt <- dfInt$Value # used to set the limits
  int <- dfInt$Value # extract interaction values

  # Thresholding ------------------------------------------------------------
  if (intThreshold > 0) {
    # Warning message if intThreshold value is set too high or too low
    if (intThreshold > max(int)) {
      warning("Selected threshold value is larger than maximum interaction strength")
    } else if (intThreshold < min(int)) {
      warning("Selected threshold value is less than minimum interaction strength")
    }

    idx <- which(int > intThreshold)
    dfInt <- dfInt[idx, ]
    int <- int[idx]
  }

  # Set up & create graph ---------------------------------------------------

  # create all pairs for graph edges
  pairs <- dfInt[c("Row", "Col")]
  pairs <- as.vector(t(pairs))

  # create graph
  g <- make_empty_graph(n = ncol(mat))
  g <- add_edges(graph = g, edges = pairs)

  # add edge weight
  E(g)$weight <- int


  # Limits ------------------------------------------------------------------

  # set the limits for importance
  if (is.null(impLims)) {
    impLimits <- range(imp)
    impLimits <- range(labeling::rpretty(impLimits[1], impLimits[2]))
  } else {
    impLimits <- impLims
  }

  # set the limits for interactions
  if (is.null(intLims)) {
    intLimits <- range(dfLimsInt)
    intLimits <- range(labeling::rpretty(intLimits[1], intLimits[2]))
  } else {
    intLimits <- intLims
  }


  mapinto <- function(x, lims, v) {
    x <- pmin(pmax(x, lims[1]), lims[2])
    i <- cut(x, breaks = seq(lims[1], lims[2], length = length(v) + 1), include.lowest = TRUE)
    v[i]
  }

  edgeCols <- mapinto(int, intLimits, intPal) # set edge cols
  edgeWidthScaled <- mapinto(int, intLimits, c(1:4)) # scaling for graphic
  impScaled <- mapinto(imp, impLimits, c(1:5)) # scaling for graphic


  # Remove unconnected nodes ------------------------------------------------

  # Delete vertex that have no edges (if thresholding)
  if (removeNode) {
    Isolated <- which(igraph::degree(g) == 0)
    if (length(Isolated) > 0) {
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
    geom_point(aes(fill = imp), size = impScaled * 2, colour = "transparent", shape = 21) +
    scale_fill_gradientn(
      name = "Vimp", colors = impPal, limits = impLimits,
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black"
      ), oob = scales::squish
    ) +
    new_scale_fill() +
    geom_point(aes(x = 0, y = 0, fill = imp), size = -1) +
    scale_fill_gradientn(
      name = "Vint", colors = intPal, limits = intLimits,
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black"
      ), oob = scales::squish
    )

  ## Clustering plot
  if (!is.null(cluster)) {

    # add numeric vector to cluster by, else use igraph clustering
    if (is.numeric(cluster)) {
      group <- as.numeric(factor(cluster))
    } else {
      group <- cluster(g)$membership
    }


    # encircle groups
    colPal <- rainbow(length(unique(group)))
    colCluster <- colPal[group]

    # plot
    p <- p + geom_encircle(aes(group = group),
      spread = 0.01,
      alpha = 0.2,
      expand = 0.03,
      fill = colCluster
    )
  }

  # Reposition labels -------------------------------------------------------

  if (nudge < 0 || nudge > 1) {
    warning("Recommended nudge values are between 0 and 1. Values outside this range may not
            display labels correctly.")
  }
  nudge <- nudge / 10
  labelPosition <- function(labCoord, nudge) {
    LmatX <- as.vector(scale(labCoord$x, center = (max(labCoord$x) + min(labCoord$x)) / 2, scale = (max(labCoord$x) - min(labCoord$x)) / 2))
    LmatY <- as.vector(scale(labCoord$y, center = (max(labCoord$y) + min(labCoord$y)) / 2, scale = (max(labCoord$y) - min(labCoord$y)) / 2))

    LmatX <- ifelse(abs(LmatX) <= 1e-10, 0, LmatX)
    LmatY <- ifelse(abs(LmatY) <= 1e-10, 0, LmatY)

    newX <- ifelse(LmatX > 0, labCoord$x + nudge, ifelse(LmatX < 0, labCoord$x - nudge, labCoord$x))
    newY <- ifelse(LmatY > 0, labCoord$y + nudge, ifelse(LmatY < 0, labCoord$y - nudge, labCoord$y))

    labDf <- data.frame(newX, newY)
    return(labDf)
  }

  labelDf <- labelPosition(p$data, nudge)


  # plot with repositioned labels
  suppressMessages({
    p <- p +
      geom_label(data = labelDf, aes(x = newX, y = newY, label = nam)) +
      theme(axis.text = element_blank()) +
      theme_void() +
      theme(aspect.ratio = 1) +
      xlim(c(-0.1, 1.1)) +
      ylim(c(-0.1, 1.1))
  })

  return(p)
}
