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
#' @param layout igraph layout function or a numeric matrix with two columns, one row per node. Defaults to igraph::layout_as_circle
#' @param cluster Either a vector of cluster memberships for nodes or an igraph clustering function.
#' @param nudge_x Nudge (centered) labels by this amount, outward horizontally.
#' @param nudge_y Nudge (centered) labels by this amount, outward vertically.
#' @param edgeWidths A vector specifying the scaling of the edges for the displayed graph. Values must be positive.
#'
#' @return A plot displaying interaction strength between variables on the edges and variable importance on the nodes.
#'
#' @import igraph
#' @import ggplot2
#' @importFrom GGally ggnet2
#' @importFrom ggnewscale new_scale_fill
#' @importFrom ggalt geom_encircle
#' @importFrom grDevices rainbow
#' @importFrom colorspace sequential_hcl
#' @examples
#'\donttest{
#' library(ranger)
#' aq <- na.omit(airquality)
#' rF <- ranger(Ozone ~ ., data = aq, importance = "permutation")
#' myMat <- vivi(fit = rF, data = aq, response = "Ozone")
#' viviNetwork(myMat)
#'}
#' @export

# Plotting Function -------------------------------------------------------
viviNetwork <- function(mat,
                        intThreshold = NULL,
                        intLims = NULL,
                        impLims = NULL,
                        intPal = rev(colorspace::sequential_hcl(palette = "Purples 3", n = 100)),
                        impPal = rev(colorspace::sequential_hcl(palette = "Greens 3", n = 100)),
                        removeNode = FALSE,
                        layout = igraph::layout_in_circle,
                        cluster = NULL,
                        nudge_x = .05,
                        nudge_y = .03,
                        edgeWidths = 1:4) {



  nnodes <- nrow(mat)
  if (nnodes == 1) stop("Only one node provided, no graph drawn")

  if (is.numeric(cluster) && length(cluster) != nnodes) cluster <- NULL
  if (is.numeric(layout) && !identical(dim(layout), as.integer(c(nnodes, 2)))) {
    layout <- igraph::layout_in_circle
  }

  df <- as.data.frame.vivid(mat)

  dfImp <- df[df$Measure == "Vimp", ]

  dfInt <- df[df$Measure == "Vint", ]
  dfInt <- dfInt[-which(dfInt$Row < dfInt$Col), ]
  dfInt <- dfInt[with(dfInt, order(Value)), ]

  # Limits ------------------------------------------------------------------

  # set the limits for importance
  if (is.null(impLims)) {
    impLimits <- range(dfImp$Value)
    impLimits <- range(labeling::rpretty(impLimits[1], impLimits[2]))
  } else {
    impLimits <- impLims
  }

  # set the limits for interactions
  if (is.null(intLims)) {
    intLimits <- range(dfInt$Value)
    intLimits <- range(labeling::rpretty(intLimits[1], intLimits[2]))
  } else {
    intLimits <- intLims
  }

  # Thresholding ------------------------------------------------------------
  dfInt1 <- dfInt

  # thresholding
  if (!is.null(intThreshold)) {
    if (intThreshold > max(dfInt$Value) | intThreshold < min(dfInt$Value)) {
      warning("Interaction threshold value is outside range of interaction values and will be ignored")
      intThreshold <- NULL
    }
    if (!is.null(intThreshold)) {
      dfInt1 <- dfInt[dfInt$Value > intThreshold, ]
    }
  }


  # Set up & create graph ---------------------------------------------------

  g <- make_empty_graph(nnodes, directed = FALSE)
  g <- add_edges(graph = g, edges = as.vector(t(dfInt1[c("Row", "Col")])))
  E(g)$weight <- dfInt1$Value

  # Delete vertex that have no edges (if thresholding)
  if (removeNode) {
    rnode <- igraph::degree(g) == 0
    g <- igraph::delete.vertices(g, rnode)
    dfImp <- dfImp[!rnode, ]
    if (is.numeric(cluster)) cluster <- cluster[!rnode]
    if (is.numeric(layout)) layout <- layout[!rnode, , drop = F]
  }

  if (is.function(layout)) {
    glayout <- layout(g)
    if (identical(layout, igraph::layout_in_circle)) glayout <- glayout[, 2:1]
  } else {
    glayout <- layout
  }

  m1 <- apply(glayout, 2, min)
  r <- apply(glayout, 2, max) - m1
  glayout <- -1 + 2 * scale(glayout, m1, r)
  attr(glayout, "scaled:scale") <- NULL
  attr(glayout, "scaled:center") <- NULL
  if (r[1] == 0) glayout[, 1] <- seq(-1, 1, length.out = nrow(glayout))
  if (r[2] == 0) glayout[, 2] <- seq(-1, 1, length.out = nrow(glayout))

  mapinto <- function(x, lims, v) {
    x <- pmin(pmax(x, lims[1]), lims[2])
    i <- cut(x, breaks = seq(lims[1], lims[2], length = length(v) + 1), include.lowest = TRUE)
    v[i]
  }

  edgeCols <- mapinto(dfInt1$Value, intLimits, intPal) # set edge cols
  edgeWidthScaled <- mapinto(dfInt1$Value, intLimits, sort(edgeWidths)) # scaling for graphic
  impScaled <- mapinto(dfImp$Value, impLimits, c(1:5)) # scaling for graphic

  glayout[abs(glayout) < .0001] <- 0
  nudged <- sign(glayout)
  nudged[nudged[, 2] == 0, 2] <- 1



  nodeSize <- mapinto(dfImp$Value, impLimits, seq(1, 2.4, length.out = 10))
  nudged[, 1] <- nudged[, 1] * nodeSize * nudge_x
  nudged[, 2] <- nudged[, 2] * nodeSize * nudge_y

  # Plot graph ----------------------------------------------------
  xlim <- c(-0.05 + min(nudged[, 1]), 1.05 + max(nudged[, 1]))
  ylim <- c(-0.05 + min(nudged[, 2]), 1.05 + max(nudged[, 2]))

  suppressMessages(
    p <- ggnet2(g,
      mode = glayout,
      size = 0,
      edge.label = NULL,
      edge.size = edgeWidthScaled,
      edge.color = edgeCols
    ) +
      xlim(xlim) +
      ylim(ylim) +
      geom_label(aes(label = dfImp$Variable_1), size = 4.5,
        nudge_x = nudged[, 1], nudge_y = nudged[, 2],
        hjust = "middle", vjust = "middle",
        label.size = NA
      ) +
      geom_point(aes(fill = dfImp$Value), size = impScaled * 2, colour = "transparent", shape = 21) +
      scale_fill_gradientn(
        name = "Vimp", colors = impPal, limits = impLimits,
        guide = guide_colorbar(
          order = 2,
          frame.colour = "black",
          ticks.colour = "black"
        ), oob = scales::squish
      ) +
      new_scale_fill() +
      geom_point(aes(x = 0, y = 0, fill = dfImp$Value), size = -1) +
      scale_fill_gradientn(
        name = "Vint", colors = intPal, limits = intLimits,
        guide = guide_colorbar(
          order = 1,
          frame.colour = "black",
          ticks.colour = "black"
        ), oob = scales::squish
      ) +
      theme_void() + theme(aspect.ratio = 1)
  )
  if (!is.null(cluster)) {
    # add numeric vector to cluster by, else use igraph clustering
    if (!is.numeric(cluster)) {
      cluster <- cluster(g)$membership
    }

    # encircle groups
    colPal <- rainbow(length(unique(cluster)))
    colCluster <- colPal[cluster]

    p <- p + geom_encircle(aes(group = cluster),
      spread = 0.01,
      alpha = 0.2,
      expand = 0.03,
      fill = colCluster
    )
  }
  p
}
