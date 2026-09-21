# Draw one item's stability path diagram (internal; reached via
# plot.fancyStability(x, item = ...), which documents the arguments).
#
# Mediators sit across the top, Y1 at left, Y2 at right, and controls stack
# below-left. Covariances are curved double-headed arrows: Y1-with-control and
# control-with-control arcs bow out to the left, mediator-with-mediator arcs bow
# upward. Variables modelled as latent (a reliability was supplied) are drawn as
# ellipses, with their reliability noted under the name. `sp` is one element of
# a fancyStability object's $fits, i.e. a fitModel() result.
plotMedX <- function(sp, item_label = NULL, x_label = NULL,
                     control_labels = NULL,
                     show_controls = TRUE,
                     suppress_control_cov = FALSE,
                     show_estimates = TRUE,
                     label_cex = 1.05,
                     digits = 2,
                     show_pvalues = FALSE,
                     title = NULL) {

  # ── 1. Unpack the fitted model ──────────────────────────────────────────────
  if (!is.list(sp) || is.null(sp$varmap) || is.null(sp$coefficients))
    stop("`sp` must be a fitModel() result (with $varmap and $coefficients).")
  if (!isTRUE(sp$converged))
    stop("This model did not converge; there is nothing to plot.")

  vm      <- sp$varmap
  rel_of  <- if ("reliability" %in% names(vm))
    stats::setNames(vm$reliability, vm$original) else
    stats::setNames(rep(NA_real_, nrow(vm)), vm$original)
  y1_name <- vm$original[vm$role == "Y1"]
  y2_name <- vm$original[vm$role == "Y2"]
  med_names  <- vm$original[vm$role == "mediator"]
  ctrl_names <- vm$original[vm$role == "control"]
  if (!show_controls) ctrl_names <- character(0)

  n_med  <- length(med_names)
  n_ctrl <- length(ctrl_names)

  check_labels <- function(lbls, n, what) {
    if (is.null(lbls) || n == 0) return(NULL)
    if (length(lbls) != n) {
      warning("Length of ", what, " (", length(lbls),
              ") does not match the number of variables in the model (", n,
              "). Using original variable names.")
      return(NULL)
    }
    lbls
  }
  # Nodes carry the variable names themselves, so an unsupplied label falls back
  # to the column name rather than to a role placeholder -- a blank node would
  # otherwise be all that is left.
  control_labels <- check_labels(control_labels, n_ctrl, "control_labels")
  if (is.null(control_labels)) control_labels <- ctrl_names
  x_label <- check_labels(x_label, n_med, "x_label")
  if (is.null(x_label)) x_label <- med_names
  item_label <- check_labels(item_label, 1, "item_label")
  y1_disp <- if (is.null(item_label)) y1_name else item_label
  y2_disp <- if (is.null(item_label)) y2_name else item_label

  # ── 2. Extract path and covariance estimates ─────────────────────────────────
  cf <- sp$coefficients

  get_est <- function(lhs_val, rhs_val) {
    row <- cf[cf$lhs == lhs_val & cf$rhs == rhs_val & cf$op == "~", ]
    if (nrow(row) == 0) return(list(est = NA_real_, pvalue = NA_real_))
    list(est = row$est[1], pvalue = row$pvalue[1])
  }

  get_cov <- function(var1, var2) {
    row <- cf[((cf$lhs == var1 & cf$rhs == var2) |
               (cf$lhs == var2 & cf$rhs == var1)) & cf$op == "~~", ]
    if (nrow(row) == 0) return(list(est = NA_real_, pvalue = NA_real_))
    list(est = row$est[1], pvalue = row$pvalue[1])
  }

  b21 <- get_est(y2_name, y1_name)
  y1_to_med <- lapply(med_names, function(v) get_est(v, y1_name))
  med_to_y2 <- lapply(med_names, function(v) get_est(y2_name, v))

  ctrl_to_med <- lapply(med_names,
                        function(v) lapply(ctrl_names, function(cc) get_est(v, cc)))
  ctrl_to_y2  <- lapply(ctrl_names, function(cc) get_est(y2_name, cc))

  y1_ctrl_cov <- lapply(ctrl_names, function(cc) get_cov(y1_name, cc))

  ctrl_pairs    <- if (n_ctrl > 1) combn(seq_len(n_ctrl), 2, simplify = FALSE) else list()
  ctrl_pair_cov <- lapply(ctrl_pairs,
                          function(p) get_cov(ctrl_names[p[1]], ctrl_names[p[2]]))

  med_pairs    <- if (n_med > 1) combn(seq_len(n_med), 2, simplify = FALSE) else list()
  med_pair_cov <- lapply(med_pairs,
                         function(p) get_cov(med_names[p[1]], med_names[p[2]]))

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
  node_names <- c(y1_name, med_names, y2_name, ctrl_names)
  n_nodes    <- length(node_names)
  idx        <- setNames(seq_along(node_names), node_names)

  # qgraph draws the boxes empty; the variable names are written at node centre
  # in step 9 with text(). Handing qgraph blank labels is what keeps every box
  # exactly node.width by node.height regardless of how long a name is.
  node_labels <- rep("", n_nodes)

  # ── 5. Regression-only edge list (covariances drawn manually later) ───────────
  make_edge <- function(from_name, to_name, path_info) {
    if (is.na(path_info$est)) return(NULL)
    list(edge = c(idx[from_name], idx[to_name]), label = fmt(path_info))
  }

  edge_list <- Filter(Negate(is.null), list(make_edge(y1_name, y2_name, b21)))
  for (j in seq_len(n_med)) {
    r <- make_edge(y1_name, med_names[j], y1_to_med[[j]])
    if (!is.null(r)) edge_list <- c(edge_list, list(r))
    r <- make_edge(med_names[j], y2_name, med_to_y2[[j]])
    if (!is.null(r)) edge_list <- c(edge_list, list(r))
  }
  for (i in seq_len(n_ctrl)) {
    for (j in seq_len(n_med)) {
      r <- make_edge(ctrl_names[i], med_names[j], ctrl_to_med[[j]][[i]])
      if (!is.null(r)) edge_list <- c(edge_list, list(r))
    }
    r <- make_edge(ctrl_names[i], y2_name, ctrl_to_y2[[i]])
    if (!is.null(r)) edge_list <- c(edge_list, list(r))
  }

  edges    <- lapply(edge_list, `[[`, "edge")
  elabels  <- vapply(edge_list, `[[`, character(1), "label")
  edge_mat <- do.call(rbind, edges)

  # ── 6. Layout coordinates ────────────────────────────────────────────────────
  layout_mat <- matrix(NA_real_, nrow = n_nodes, ncol = 2)
  rownames(layout_mat) <- node_names

  layout_mat[y1_name, ] <- c(-1.2, 0.0)
  layout_mat[y2_name, ] <- c( 1.2, 0.0)

  # one mediator sits dead centre; several spread evenly across the top
  med_x <- if (n_med == 1) 0 else seq(-0.8, 0.8, length.out = max(n_med, 1))
  for (j in seq_len(n_med)) layout_mat[med_names[j], ] <- c(med_x[j], 1.2)
  for (i in seq_len(n_ctrl)) layout_mat[ctrl_names[i], ] <- c(-1.2, -0.8 * i)

  # ── 7. Render regression paths with qgraph ───────────────────────────────────
  q <- qgraph::qgraph(
    input          = edge_mat,
    directed       = TRUE,
    layout         = layout_mat,
    labels         = node_labels,
    # SEM convention: observed variables are boxes, latent ones ellipses
    shape          = ifelse(is.na(rel_of[node_names]), "rectangle", "ellipse"),
    edge.labels    = if (show_estimates) elabels else FALSE,
    edge.label.cex = 0.85,
    node.width     = 0.9,
    node.height    = 0.55,
    # extra margin only on the sides where covariance arcs need room to bow out.
    # The Y1-with-control arcs on the left survive suppression; the upward
    # mediator-with-mediator arcs do not, so that headroom is reclaimed.
    mar            = c(6,
                       if (n_ctrl > 0) 10 else 6,
                       if (n_med > 1 && !suppress_control_cov) 10 else 6,
                       6),
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

  edge_lty <- tryCatch({
    el <- q$graphAttributes$Edges$lty
    if (length(el) > 0 && !is.na(el[1])) el[1] else 1
  }, error = function(e) 1)

  # Match qgraph's edge-label styling: labels default to the edge color, and
  # qgraph rescales label.cex by device size (normalize = TRUE), so reproduce
  # that factor here or the arc labels come out a different size than the rest.
  elab_col <- tryCatch({
    lc <- q$graphAttributes$Edges$label.color
    if (length(lc) > 0 && !is.na(lc[1])) lc[1] else edge_col
  }, error = function(e) edge_col)

  elab_font <- tryCatch({
    lf <- q$graphAttributes$Edges$label.font
    if (length(lf) > 0 && !is.na(lf[1])) lf[1] else 1
  }, error = function(e) 1)

  # qgraph computed its normC while plotting under par(mar = c(0,0,0,0)), where
  # pin equals the figure region; it restores mar before returning, so use fin
  # (unchanged by that restore) rather than the now-shrunken pin.
  normC <- sqrt(sum(par("fin")^2)) / sqrt(7^2 + 7^2)
  elab_cex <- tryCatch({
    lx <- q$graphAttributes$Edges$label.cex
    if (length(lx) > 0 && !is.na(lx[1])) lx[1] else 0.85
  }, error = function(e) 0.85) * normC

  # Allow drawing into the margin area (arcs bow outside the plot region)
  old_xpd <- par(xpd = NA)
  on.exit(par(old_xpd), add = TRUE)

  # Each arc is a quadratic bezier starting/ending at the EDGE of its node so no
  # line is visible inside the box. Curvature scales with the distance between
  # nodes, so farther-apart arcs bow out further and stay clear of shorter ones.
  node_hw <- 0.13   # approximate half-width of a node box in layout coordinates
  node_hh <- 0.08   # approximate half-height

  draw_cov_arc <- function(node_a, node_b, path_info, dir = c("left", "up")) {
    if (is.na(path_info$est)) return(invisible(NULL))
    dir <- match.arg(dir)

    ra <- which(node_names == node_a)
    rb <- which(node_names == node_b)

    if (dir == "left") {
      x1 <- lyt[ra, 1] - node_hw; y1 <- lyt[ra, 2]
      x2 <- lyt[rb, 1] - node_hw; y2 <- lyt[rb, 2]
      offset <- 0.18 + abs(y1 - y2) * 0.22
      cx <- min(x1, x2) - offset
      cy <- (y1 + y2) / 2
    } else {
      x1 <- lyt[ra, 1]; y1 <- lyt[ra, 2] + node_hh
      x2 <- lyt[rb, 1]; y2 <- lyt[rb, 2] + node_hh
      offset <- 0.18 + abs(x1 - x2) * 0.22
      cx <- (x1 + x2) / 2
      cy <- max(y1, y2) + offset
    }

    t  <- seq(0, 1, length.out = 200)
    bx <- (1 - t)^2 * x1 + 2 * (1 - t) * t * cx + t^2 * x2
    by <- (1 - t)^2 * y1 + 2 * (1 - t) * t * cy + t^2 * y2

    lines(bx, by, lty = edge_lty, lwd = edge_lwd, col = edge_col)

    arrows(bx[8],   by[8],   bx[1],   by[1],   length = 0.08,
           angle = 20, code = 2, lwd = edge_lwd, col = edge_col)
    arrows(bx[193], by[193], bx[200], by[200], length = 0.08,
           angle = 20, code = 2, lwd = edge_lwd, col = edge_col)

    lbl <- if (show_estimates) fmt(path_info) else ""
    if (nchar(lbl) > 0) {
      if (dir == "left")
        text(bx[100] - 0.05, by[100], lbl, cex = elab_cex, col = elab_col,
             font = elab_font, adj = c(1, 0.5))
      else
        text(bx[100], by[100] + 0.04, lbl, cex = elab_cex, col = elab_col,
             font = elab_font, adj = c(0.5, 0))
    }
  }

  for (i in seq_along(ctrl_names))
    draw_cov_arc(y1_name, ctrl_names[i], y1_ctrl_cov[[i]], dir = "left")

  # Everything above involves Y1 and is part of the stability decomposition.
  # What follows -- control-with-control, mediator-with-mediator -- is estimated
  # but decomposes nothing, so it is the first thing to drop when the diagram
  # gets crowded.
  if (!suppress_control_cov) {
    for (p in seq_along(ctrl_pairs)) {
      pr <- ctrl_pairs[[p]]
      draw_cov_arc(ctrl_names[pr[1]], ctrl_names[pr[2]], ctrl_pair_cov[[p]],
                   dir = "left")
    }

    for (p in seq_along(med_pairs)) {
      pr <- med_pairs[[p]]
      draw_cov_arc(med_names[pr[1]], med_names[pr[2]], med_pair_cov[[p]],
                   dir = "up")
    }
  }

  # ── 9. Write variable names at node centres ─────────────────────────────────
  # Drawn over the boxes qgraph already laid down, so a name longer than its box
  # overflows it rather than resizing it. Use `label_cex` to shrink text to fit.
  add_label <- function(node_name, label, font = 1) {
    ri  <- which(node_names == node_name)
    rel <- rel_of[[node_name]]
    # a latent node carries its assumed reliability just under its name
    y_off <- if (is.na(rel)) 0 else 0.025
    text(x      = lyt[ri, 1],
         y      = lyt[ri, 2] + y_off,
         labels = label,
         adj    = c(0.5, 0.5),
         font   = font,
         cex    = label_cex)
    if (!is.na(rel))
      text(x = lyt[ri, 1], y = lyt[ri, 2] - 0.045,
           labels = sprintf("rel = %.2f", rel),
           adj = c(0.5, 0.5), cex = label_cex * 0.7, col = "grey30")
  }

  # Always drawn: a box with no name in it is not a diagram of anything.
  for (j in seq_len(n_med)) add_label(med_names[j], x_label[j], font = 3)
  add_label(y1_name, y1_disp)
  add_label(y2_name, y2_disp)
  for (i in seq_len(n_ctrl)) add_label(ctrl_names[i], control_labels[i])

  invisible(q)
}
