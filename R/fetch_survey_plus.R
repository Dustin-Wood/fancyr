#' Fetch a Qualtrics Survey with Value Labels and a Trimmed Column Map
#' @description
#' One-call wrapper around the usual Qualtrics setup. It fetches a survey in
#' numeric form, embeds value labels into the data with
#' \code{\link{attach_value_labels}}, builds an enhanced column map with
#' \code{\link{extract_colmapPlus}}, and trims that map down to the essential
#' columns. The survey metadata is fetched once and shared between the two
#' helpers, so only a single \code{qualtRics::metadata()} call is made.
#' @param surveyID Character. The Qualtrics survey ID.
#' @param ... Additional arguments passed to \code{qualtRics::fetch_survey()}.
#'   Note that \code{label}, \code{convert}, and \code{breakout_sets} are fixed
#'   to \code{FALSE} so the returned data holds numeric response codes ready
#'   for \code{attach_value_labels()}; do not pass these three.
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{data}}{The survey data frame, with \code{haven_labelled}
#'       value- and variable-label attributes attached.}
#'     \item{\code{colmap}}{The enhanced column map, keeping only the
#'       \code{qname}, \code{main}, \code{sub}, and \code{value_labels}
#'       columns.}
#'   }
#' @details
#' Bundling the data and its codebook in one list keeps them together as a
#' single object — convenient for passing a survey around or holding several
#' surveys side by side. The \code{data} element behaves numerically in
#' analyses but carries its codebook with it; \code{colmap} is the
#' human-readable companion, dropping the columns of
#' \code{qualtRics::extract_colmap()} that are rarely useful.
#' @seealso \code{\link{attach_value_labels}}, \code{\link{extract_colmapPlus}}
#' @examples
#' \dontrun{
#' library(qualtRics)
#' sid <- subset(all_surveys(), name == "ReTurk Survey")$id
#' survey <- fetch_survey_plus(sid)
#'
#' head(survey$colmap)
#' attr(survey$data$Q589_1, "labels")   # named numeric: codes -> labels
#' }
#' @export
fetch_survey_plus <- function(surveyID, ...) {
  if (!requireNamespace("qualtRics", quietly = TRUE)) {
    stop("Package 'qualtRics' is required for fetch_survey_plus(). ",
         "Install it from CRAN.", call. = FALSE)
  }

  meta <- qualtRics::metadata(surveyID)

  survey <- qualtRics::fetch_survey(surveyID, label = FALSE, convert = FALSE,
                                    breakout_sets = FALSE, ...)
  survey <- attach_value_labels(survey, surveyID, meta = meta)

  colmap <- extract_colmapPlus(survey, surveyID, meta = meta)
  colmap <- colmap[c("qname", "main", "sub", "value_labels")]

  list(data = survey, colmap = colmap)
}
