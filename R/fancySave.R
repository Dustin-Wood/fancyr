#' Save a fancyr Plot to a File
#'
#' @description
#' Saves a plot from \code{plot()} on a \code{\link{stabilityPaths}},
#' \code{\link{crossLagPaths}} or \code{\link{reliabilitySensitivity}} result,
#' at a size suited to that kind of plot and at print resolution. The format
#' comes from the file extension: \code{.png}, \code{.jpg}, \code{.tiff}
#' (raster) or \code{.pdf}, \code{.svg} (vector, sharp at any size, best for
#' papers).
#'
#' Raster files are drawn with \pkg{ragg} when it is installed, which gives
#' crisper text and lines than the default Windows devices; PDFs with
#' \code{cairo_pdf} where available, which embeds fonts properly.
#'
#' @param plot The plot. A \pkg{ggplot2} object (the bar chart, the effects
#'   scatterplot, the sensitivity plots), or for a path diagram, which is drawn
#'   with base graphics, a function that draws it:
#'   \code{function() plot(sp, item = "Powerful_role")}.
#' @param file File name, with the extension giving the format.
#' @param width,height Size in inches. By default the width is 6.5, the
#'   text width of a letter page with 1-inch margins, so the figure goes into
#'   a manuscript without rescaling and its text prints at the size drawn.
#'   The height suits the plot: 7.8 for the effects scatterplot (square
#'   panel plus legend), enough room per item for the bar chart, 4.9 for a
#'   path diagram and 4.6 otherwise. Text is a fixed point size, so a larger
#'   canvas gives crowded labels more room.
#' @param dpi Resolution for raster files. 300 suits print; 150 is enough for
#'   slides and the web.
#' @param ... Passed to \code{\link[ggplot2]{ggsave}} for a ggplot.
#' @return \code{file}, invisibly.
#' @seealso \code{\link{plot.fancyStability}}
#' @examples
#' \dontrun{
#' p <- plot(sp, type = "effects", labels = function(i) sub(" \\(.*$", "", i))
#' fancySave(p, "effects.png")
#' fancySave(p, "effects.pdf")
#' fancySave(function() plot(sp, item = "Powerful_role"), "path.png")
#' }
#' @export
fancySave <- function(plot, file, width = NULL, height = NULL, dpi = 300, ...) {
  ext <- tolower(sub(".*\\.", "", basename(file)))
  raster <- ext %in% c("png", "jpg", "jpeg", "tif", "tiff")
  if (!raster && !ext %in% c("pdf", "svg"))
    stop("`file` must end in .png, .jpg, .tiff, .pdf or .svg.")

  isGG <- inherits(plot, "ggplot")
  if (!isGG && !is.function(plot))
    stop("`plot` must be a ggplot or, for a path diagram, a function that draws it.")
  if (is.null(width)) width <- 6.5
  if (is.null(height)) height <- if (!isGG) 4.9
    else if (inherits(plot$coordinates, "CoordFixed")) 7.8
    else if (is.factor(plot$data$item) && !"reliability" %in% names(plot$data))
      max(4, 1.8 + .3 * nlevels(plot$data$item))   # bar chart: one row per item
    else 4.6

  ragg <- raster && requireNamespace("ragg", quietly = TRUE)
  device <- switch(ext,
    png = if (ragg) ragg::agg_png else grDevices::png,
    jpg = , jpeg = if (ragg) ragg::agg_jpeg else grDevices::jpeg,
    tif = , tiff = if (ragg) ragg::agg_tiff else grDevices::tiff,
    pdf = if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf,
    svg = grDevices::svg)

  if (isGG) {
    ggplot2::ggsave(file, plot, device = device, width = width, height = height,
                    units = "in", dpi = dpi, bg = "white", ...)
  } else {
    if (raster) device(file, width = width, height = height, units = "in", res = dpi)
    else device(file, width = width, height = height)
    on.exit(grDevices::dev.off())
    plot()
  }
  invisible(file)
}
