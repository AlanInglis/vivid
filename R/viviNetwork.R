#' viviNetwork
#'
#'  @description Create a Network plot displaying variable importance
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
                        cluster = NULL) {


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
  int <- dfInt$Value # extract interaction values

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
    intLimits <- range(int)
    intLimits <- range(labeling::rpretty(intLimits[1], intLimits[2]))
  } else {
    intLimits <- intLims
  }


  mapinto <- function(x, lims, v) {

    x <- pmin(pmax(x, lims[1]), lims[2])
    i <- cut(x, breaks = seq(lims[1], lims[2], length = length(v) + 1), include.lowest = TRUE)
    v[i]
  }

  edgeCols<- mapinto(int, intLimits, intPal) # set edge cols
  edgeWidthScaled <- mapinto(int, intLimits, c(1:4)) # scaling for graphic
  impScaled <- mapinto(imp, impLimits, c(1:5)) # scaling for graphic


  # THRESHOLDING ------------------------------------------------------------

  if (intThreshold > 0) {

    # Warning message if intThreshold value is set too high or too low
    if (intThreshold > max(int)) {
      warning("Selected threshold value is larger than maximum interaction strength")
    } else if (intThreshold < min(int)) {
      warning("Selected threshold value is less than minimum interaction strength")
    }

    idx <- which(int > intThreshold)

    # delete edges
    g <- delete.edges(g, which(int < intThreshold))


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
