#' Fetch a Qualtrics Survey with Value Labels and a Trimmed Column Map
#' @description
#' One-call wrapper around the usual Qualtrics setup. It fetches a survey in
#' numeric form and builds an enhanced column map (a human-readable codebook)
#' with \code{\link{extract_colmapPlus}}, trimmed to the essential columns. The
#' survey metadata is fetched once and shared, so only a single
#' \code{qualtRics::metadata()} call is made.
#'
#' By default the returned \code{data} is left as plain numeric columns. The
#' full code-to-label codebook still travels with the survey -- in the
#' \code{colmap}'s \code{value_labels} column -- so nothing is lost. Set
#' \code{labelled = TRUE} only when you specifically want \code{haven_labelled}
#' value labels embedded in the data itself (e.g.\ for SPSS export).
#' @param surveyID Character. The Qualtrics survey ID.
#' @param labelled Logical. If \code{TRUE}, embed \code{haven_labelled} value-
#'   and variable-label attributes in \code{data} via
#'   \code{\link{attach_value_labels}}. Defaults to \code{FALSE}, which returns
#'   plain numeric columns. See \emph{Details} for why \code{FALSE} is the
#'   default.
#' @param ... Additional arguments passed to \code{qualtRics::fetch_survey()}.
#'   Note that \code{label}, \code{convert}, and \code{breakout_sets} are fixed
#'   to \code{FALSE} so the returned data holds numeric response codes; do not
#'   pass these three.
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{data}}{The survey data frame. Plain numeric by default;
#'       \code{haven_labelled} value- and variable-label attributes are
#'       attached only when \code{labelled = TRUE}.}
#'     \item{\code{colmap}}{The enhanced column map, keeping only the
#'       \code{qname}, \code{main}, \code{sub}, and \code{value_labels}
#'       columns.}
#'   }
#' @details
#' Bundling the data and its codebook in one list keeps them together as a
#' single object — convenient for passing a survey around or holding several
#' surveys side by side. \code{colmap} is the human-readable companion,
#' dropping the columns of \code{qualtRics::extract_colmap()} that are rarely
#' useful.
#'
#' \strong{Why \code{labelled = FALSE} by default.} \code{haven_labelled} is a
#' transitional import format: the values stay numeric, but the class trips up
#' numeric-matrix consumers. Under recent \pkg{vctrs}, operations such as
#' \code{as.numeric()}, \code{data.matrix()}, and \code{as.matrix()} on a
#' labelled column raise "Can't convert <haven_labelled> to <double>", which
#' breaks exactly the analysis tools this package targets (\pkg{psych},
#' \pkg{lavaan}, base \code{cor()}/\code{factanal()}). Because the same
#' code-to-label mapping is preserved in \code{colmap$value_labels}, delivering
#' plain numeric data costs nothing and avoids those failures. When you do need
#' the embedded labels (SPSS/\pkg{sjlabelled} workflows), pass
#' \code{labelled = TRUE} or call \code{\link{attach_value_labels}} yourself.
#' @seealso \code{\link{attach_value_labels}}, \code{\link{extract_colmapPlus}}
#' @examples
#' \dontrun{
#' library(qualtRics)
#' sid <- subset(all_surveys(), name == "ReTurk Survey")$id
#'
#' # Default: plain numeric data, codebook in the colmap.
#' survey <- fetch_survey_plus(sid)
#' head(survey$colmap)                       # qname, main, sub, value_labels
#' survey$colmap$value_labels[1]             # "1=Strongly Disagree | 2=..."
#'
#' # Opt in to embedded haven value labels (e.g. for SPSS export):
#' survey_lab <- fetch_survey_plus(sid, labelled = TRUE)
#' attr(survey_lab$data$Q589_1, "labels")    # named numeric: codes -> labels
#' }
#' @export
fetch_survey_plus <- function(surveyID, labelled = FALSE, ...) {
  if (!requireNamespace("qualtRics", quietly = TRUE)) {
    stop("Package 'qualtRics' is required for fetch_survey_plus(). ",
         "Install it from CRAN.", call. = FALSE)
  }

  meta <- qualtRics::metadata(surveyID)

  survey <- qualtRics::fetch_survey(surveyID, label = FALSE, convert = FALSE,
                                    breakout_sets = FALSE, ...)

  # The codebook reads only column metadata, not the response labels, so it is
  # identical whether or not value labels are attached to `survey`.
  colmap <- extract_colmapPlus(survey, surveyID, meta = meta)
  colmap <- colmap[c("qname", "main", "sub", "value_labels")]

  # Embed haven value labels only on request; plain numeric is the default to
  # keep `data` interoperable with numeric-matrix consumers (see Details).
  if (labelled) {
    survey <- attach_value_labels(survey, surveyID, meta = meta)
  }

  list(data = survey, colmap = colmap)
}
