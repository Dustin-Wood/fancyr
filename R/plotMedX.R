#' Plot a mediation path diagram from medXonAllY output
#'
#' @description
#' Takes a single \code{parameterestimates()} data frame from \code{medXonAllY}
#' (accessed via \code{xEffects$modelEstimates[["item name"]]}) and renders a
#' standard mediation path diagram: X at top center, Y1 (baseline item score)
#' at left, Y2 (follow-up item score) at right, and any control variables
#' stacked below-left. Full item and experience variable labels are placed
#' outside the nodes.
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
  # Controls are rhs values where X is regressed on something, excluding Y1
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

  # ── 2. Extract path estimates ────────────────────────────────────────────────
  get_est <- function(lhs_val, rhs_val) {
    row <- pe[pe$lhs == lhs_val & pe$rhs == rhs_val & pe$op == "~", ]
    if (nrow(row) == 0) return(list(est = NA_real_, pvalue = NA_real_))
    list(est = row$est[1], pvalue = row$pvalue[1])
  }

  bX1   <- get_est("X",  "Y1")   # Y1 → X  (baseline predicts experience)
  b2X   <- get_est("Y2", "X")    # X  → Y2 (experience predicts follow-up)
  b21   <- get_est("Y2", "Y1")   # Y1 → Y2 (residual stability)

  ctrl_to_X  <- lapply(ctrl_names, function(c) get_est("X",  c))
  ctrl_to_Y2 <- lapply(ctrl_names, function(c) get_est("Y2", c))

  # ── 3. Format edge label helper ──────────────────────────────────────────────
  fmt <- function(path_info) {
    est <- path_info$est
    if (is.na(est)) return("")
    lbl <- sprintf(paste0("%.", digits, "f"), est)
    if (show_pvalues && !is.na(path_info$pvalue)) {
      lbl <- paste0(lbl, "\n(p=", sprintf("%.3f", path_info$pvalue), ")")
    }
    lbl
  }

  # ── 4. Build node list ───────────────────────────────────────────────────────
  # Node order: Y1, X, Y2, [controls]
  node_names <- c("Y1", "X", "Y2", ctrl_names)
  n_nodes    <- length(node_names)

  idx <- setNames(seq_along(node_names), node_names)

  # ── 5. Build edge list and labels ────────────────────────────────────────────
  edges  <- list()
  elabels <- character(0)

  add_edge <- function(from_name, to_name, path_info) {
    if (!is.na(path_info$est)) {
      edges[[length(edges) + 1]] <<- c(idx[from_name], idx[to_name])
      elabels <<- c(elabels, fmt(path_info))
    }
  }

  add_edge("Y1", "X",  bX1)
  add_edge("X",  "Y2", b2X)
  add_edge("Y1", "Y2", b21)

  for (i in seq_along(ctrl_names)) {
    add_edge(ctrl_names[i], "X",  ctrl_to_X[[i]])
    add_edge(ctrl_names[i], "Y2", ctrl_to_Y2[[i]])
  }

  edge_mat <- do.call(rbind, edges)

  # ── 6. Layout coordinates ────────────────────────────────────────────────────
  layout_mat <- matrix(NA_real_, nrow = n_nodes, ncol = 2)
  rownames(layout_mat) <- node_names

  layout_mat["Y1", ] <- c(-1.2,  0.0)
  layout_mat["X",  ] <- c( 0.0,  1.2)
  layout_mat["Y2", ] <- c( 1.2,  0.0)

  for (i in seq_len(n_ctrl)) {
    layout_mat[ctrl_names[i], ] <- c(-1.2, -0.8 * i)
  }

  # Short display labels inside nodes (just node IDs)
  node_labels <- c("Y1", "X", "Y2", paste0("C", seq_len(n_ctrl)))

  # ── 7. Render with qgraph ────────────────────────────────────────────────────
  q <- qgraph::qgraph(
    input         = edge_mat,
    directed      = TRUE,
    layout        = layout_mat,
    labels        = node_labels,
    edge.labels   = elabels,
    edge.label.cex = 0.9,
    node.width    = 1.2,
    node.height   = 0.8,
    mar           = c(5, 5, 5, 5),
    title         = title,
    DoNotPlot     = FALSE
  )

  # ── 8. Add full text labels via base R text() ────────────────────────────────
  lyt <- q$layout  # normalized plot coordinates, rows match node_names order

  # Helper to add label relative to a node position
  add_label <- function(node_name, label, y_offset, adj_x = 0.5,
                        font = 1, cex = 0.85) {
    ri <- which(node_names == node_name)
    text(x    = lyt[ri, 1],
         y    = lyt[ri, 2] + y_offset,
         labels = label,
         adj  = c(adj_x, if (y_offset > 0) 0 else 1),
         font = font,
         cex  = cex)
  }

  # X node: label above, italicized
  add_label("X",  x_label,    y_offset =  0.20, font = 3)

  # Y1 node: item label below
  add_label("Y1", item_label, y_offset = -0.20)

  # Y2 node: item label below
  add_label("Y2", item_label, y_offset = -0.20)

  # Control nodes: display labels below each
  for (i in seq_len(n_ctrl)) {
    add_label(ctrl_names[i], control_labels[i], y_offset = -0.20)
  }

  invisible(q)
}
