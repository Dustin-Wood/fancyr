#' Plot a mediation path diagram from medXonAllY output
#'
#' @description
#' Takes a single \code{parameterestimates()} data frame from \code{medXonAllY}
#' (accessed via \code{xEffects$modelEstimates[["item name"]]}) and renders a
#' standard mediation path diagram: X at top center, Y1 (baseline item score)
#' at left, Y2 (follow-up item score) at right, and any control variables
#' stacked below-left. Covariances (Y1 with controls, and between controls)
#' are drawn as curved dashed double-headed arrows arcing out to the left.
#' Full item and experience variable labels are placed outside the nodes.
#'
#' @param pe A single \code{parameterestimates()} data frame from
#'   \code{out$xEffects$modelEstimates[["item name"]]}.
#' @param item_label Full display name for the item (shown below Y1 and Y2,
#'   since these are the same construct measured at two time points).
#' @param x_label Full display name for the experience/predictor variable
#'   (shown above the X node).
#' @param control_labels Character vector of display names for control variables,
#'   in the order they appear in \code{pe}. If \code{NULL} (default), sanitized
#'   variable names extracted from \code{pe} are used.
#' @param digits Integer. Number of decimal places for path coefficients.
#'   Default is \code{2}.
#' @param show_pvalues Logical. If \code{TRUE}, append the p-value in
#'   parentheses to each edge label. Default is \code{FALSE}.
#' @param title Optional character string passed to \code{qgraph}'s
#'   \code{title} argument.
#'
#' @return Invisibly returns the \code{qgraph} object (which contains the
#'   final layout coordinates in \code{$layout}).
#'
#' @export
#' @importFrom qgraph qgraph
#'
#' @examples
#' \dontrun{
#' out <- xEffects(P25CB.4, F25CB.4,
#'                 commonitems = NL110.F25set,
#'                 xVar = "NL110fall",
#'                 xFile = LEADcourses,
#'                 id_col = "Random Id",
#'                 controls = c("genderNum", "SAT Math"),
#'                 zY = TRUE, zX = TRUE, NA_to_0 = TRUE)
#'
#' item_name <- NL110.F25set[1]
#' plotMedX(
#'   pe             = out$xEffects$modelEstimates[[item_name]],
#'   item_label     = item_name,
#'   x_label        = "NL110 Fall Course",
#'   control_labels = c("Gender", "SAT Math")
#' )
#' }
plotMedX <- function(pe, item_label, x_label,
                     control_labels = NULL,
                     digits = 2,
                     show_pvalues = FALSE,
                     title = NULL) {

  # ── 1. Detect controls ──────────────────────────────────────────────────────
  ctrl_names <- pe$rhs[pe$lhs == "X" & pe$op == "~" & pe$rhs != "Y1"]

  if (is.null(control_labels)) {
    control_labels <- ctrl_names
  } else if (length(control_labels) != length(ctrl_names)) {
    warning("Length of control_labels (", length(control_labels),
            ") does not match number of controls detected in pe (",
            length(ctrl_names), "). Using sanitized names from pe.")
    control_labels <- ctrl_names
  }

  n_ctrl <- length(ctrl_names)

  # ── 2. Extract path and covariance estimates ─────────────────────────────────
  get_est <- function(lhs_val, rhs_val) {
    row <- pe[pe$lhs == lhs_val & pe$rhs == rhs_val & pe$op == "~", ]
    if (nrow(row) == 0) return(list(est = NA_real_, pvalue = NA_real_))
    list(est = row$est[1], pvalue = row$pvalue[1])
  }

  get_cov <- function(var1, var2) {
    row <- pe[((pe$lhs == var1 & pe$rhs == var2) |
               (pe$lhs == var2 & pe$rhs == var1)) & pe$op == "~~", ]
    if (nrow(row) == 0) return(list(est = NA_real_, pvalue = NA_real_))
    list(est = row$est[1], pvalue = row$pvalue[1])
  }

  bX1 <- get_est("X",  "Y1")
  b2X <- get_est("Y2", "X")
  b21 <- get_est("Y2", "Y1")

  ctrl_to_X  <- lapply(ctrl_names, function(c) get_est("X",  c))
  ctrl_to_Y2 <- lapply(ctrl_names, function(c) get_est("Y2", c))

  # Y1 <-> each control covariance
  y1_ctrl_cov <- lapply(ctrl_names, function(c) get_cov("Y1", c))

  # All pairs of controls covariance
  ctrl_pairs    <- if (n_ctrl > 1) combn(seq_len(n_ctrl), 2, simplify = FALSE) else list()
  ctrl_pair_cov <- lapply(ctrl_pairs,
                          function(p) get_cov(ctrl_names[p[1]], ctrl_names[p[2]]))

  # ── 3. Format coefficient helper ─────────────────────────────────────────────
  fmt <- function(path_info) {
    est <- path_info$est
    if (is.na(est)) return("")
    lbl <- sprintf(paste0("%.", digits, "f"), est)
    if (show_pvalues && !is.na(path_info$pvalue))
      lbl <- paste0(lbl, "\n(p=", sprintf("%.3f", path_info$pvalue), ")")
    lbl
  }

  # ── 4. Build node list ───────────────────────────────────────────────────────
  node_names  <- c("Y1", "X", "Y2", ctrl_names)
  n_nodes     <- length(node_names)
  idx         <- setNames(seq_along(node_names), node_names)
  node_labels <- c("Y1", "X", "Y2", paste0("C", seq_len(n_ctrl)))

  # ── 5. Regression-only edge list (covariances drawn manually later) ───────────
  make_edge <- function(from_name, to_name, path_info) {
    if (is.na(path_info$est)) return(NULL)
    list(edge = c(idx[from_name], idx[to_name]), label = fmt(path_info))
  }

  edge_list <- Filter(Negate(is.null), list(
    make_edge("Y1", "X",  bX1),
    make_edge("X",  "Y2", b2X),
    make_edge("Y1", "Y2", b21)
  ))
  for (i in seq_along(ctrl_names)) {
    r <- make_edge(ctrl_names[i], "X",  ctrl_to_X[[i]])
    if (!is.null(r)) edge_list <- c(edge_list, list(r))
    r <- make_edge(ctrl_names[i], "Y2", ctrl_to_Y2[[i]])
    if (!is.null(r)) edge_list <- c(edge_list, list(r))
  }

  edges   <- lapply(edge_list, `[[`, "edge")
  elabels <- sapply(edge_list, `[[`, "label")

  edge_mat <- do.call(rbind, edges)

  # ── 6. Layout coordinates ────────────────────────────────────────────────────
  layout_mat <- matrix(NA_real_, nrow = n_nodes, ncol = 2)
  rownames(layout_mat) <- node_names

  layout_mat["Y1", ] <- c(-1.2,  0.0)
  layout_mat["X",  ] <- c( 0.0,  1.2)
  layout_mat["Y2", ] <- c( 1.2,  0.0)
  for (i in seq_len(n_ctrl))
    layout_mat[ctrl_names[i], ] <- c(-1.2, -0.8 * i)

  # ── 7. Render regression paths with qgraph ───────────────────────────────────
  q <- qgraph::qgraph(
    input          = edge_mat,
    directed       = TRUE,
    layout         = layout_mat,
    labels         = node_labels,
    shape          = "rectangle",
    edge.labels    = elabels,
    edge.label.cex = 0.85,
    node.width     = 0.9,
    node.height    = 0.55,
    mar            = c(6, 10, 6, 6),
    title          = title,
    DoNotPlot      = FALSE
  )

  lyt <- q$layout   # layout coords match the live plot's user coordinate system

  # ── 8. Match qgraph's edge style, then draw covariance arcs ─────────────────

  # Extract the color and lwd qgraph actually used for edges so the arcs match
  edge_col <- tryCatch({
    ec <- q$graphAttributes$Edges$color
    if (length(ec) > 0 && !is.na(ec[1])) ec[1] else "grey50"
  }, error = function(e) "grey50")

  edge_lwd <- tryCatch({
    ew <- q$graphAttributes$Edges$lwd
    if (length(ew) > 0 && !is.na(ew[1])) ew[1] else 1
  }, error = function(e) 1)

  # Allow drawing into the margin area (arcs bow left of the plot region)
  old_xpd <- par(xpd = NA)
  on.exit(par(old_xpd), add = TRUE)

  # Each arc is a quadratic bezier starting/ending at the LEFT EDGE of its node
  # so no line is visible inside the box. Curvature scales with vertical distance
  # so farther-apart arcs bow out further left, keeping them clear of shorter arcs.
  node_hw <- 0.13   # approximate half-width of a node box in layout coordinates

  draw_cov_arc <- function(node_a, node_b, path_info) {
    if (is.na(path_info$est)) return(invisible(NULL))

    ra <- which(node_names == node_a)
    rb <- which(node_names == node_b)

    x1 <- lyt[ra, 1] - node_hw;  y1 <- lyt[ra, 2]
    x2 <- lyt[rb, 1] - node_hw;  y2 <- lyt[rb, 2]

    vert_dist   <- abs(y1 - y2)
    left_offset <- 0.18 + vert_dist * 0.22

    cx <- min(x1, x2) - left_offset
    cy <- (y1 + y2) / 2

    t  <- seq(0, 1, length.out = 200)
    bx <- (1 - t)^2 * x1 + 2 * (1 - t) * t * cx + t^2 * x2
    by <- (1 - t)^2 * y1 + 2 * (1 - t) * t * cy + t^2 * y2

    lines(bx, by, lty = 2, lwd = edge_lwd, col = edge_col)

    arrows(bx[8],   by[8],   bx[1],   by[1],   length = 0.08,
           angle = 20, code = 2, lwd = edge_lwd, col = edge_col)
    arrows(bx[193], by[193], bx[200], by[200], length = 0.08,
           angle = 20, code = 2, lwd = edge_lwd, col = edge_col)

    lbl <- fmt(path_info)
    if (nchar(lbl) > 0)
      text(bx[100] - 0.05, by[100], lbl, cex = 0.85, adj = c(1, 0.5))
  }

  for (i in seq_along(ctrl_names))
    draw_cov_arc("Y1", ctrl_names[i], y1_ctrl_cov[[i]])

  for (k in seq_along(ctrl_pairs)) {
    p <- ctrl_pairs[[k]]
    draw_cov_arc(ctrl_names[p[1]], ctrl_names[p[2]], ctrl_pair_cov[[k]])
  }

  # ── 9. Add full text labels outside nodes ────────────────────────────────────
  add_label <- function(node_name, label, y_offset, adj_x = 0.5,
                        font = 1, cex = 1.05) {
    ri <- which(node_names == node_name)
    text(x      = lyt[ri, 1],
         y      = lyt[ri, 2] + y_offset,
         labels = label,
         adj    = c(adj_x, if (y_offset > 0) 0 else 1),
         font   = font,
         cex    = cex)
  }

  add_label("X",  x_label,    y_offset =  0.18, font = 3)
  add_label("Y1", item_label, y_offset = -0.18)
  add_label("Y2", item_label, y_offset = -0.18)
  for (i in seq_len(n_ctrl))
    add_label(ctrl_names[i], control_labels[i], y_offset = -0.18)

  invisible(q)
}
