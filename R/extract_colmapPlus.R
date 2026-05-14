#' Enhanced Qualtrics Column Map with Value Labels
#' @description
#' Extends \code{qualtRics::extract_colmap()} by adding a \code{value_labels}
#' column that records, for each question, the mapping between numeric response
#' codes and their displayed labels (e.g.\ \code{"1=Strongly Disagree | 2=Somewhat Disagree | ..."}).
#'
#' Labels are pulled directly from the survey definition via
#' \code{qualtRics::metadata()}, so every response option defined in Qualtrics
#' is included — even ones nobody selected. For matrix or multi-part questions
#' where every sub-item shares the same choice scale, the label string is
#' repeated on every row of the colmap belonging to that question.
#' @param survey A survey fetched with \code{qualtRics::fetch_survey()}. The
#'   numeric or labelled version both work; only the embedded column-map
#'   metadata is read, not the responses.
#' @param surveyID Character. Survey ID, used to query
#'   \code{qualtRics::metadata()}. Required.
#' @param sep Character. Separator placed between \code{code=label} pairs in
#'   the assembled string. Defaults to \code{" | "}.
#' @param meta Optional. The result of \code{qualtRics::metadata(surveyID)}.
#'   Supply this when the metadata has already been fetched (e.g.\ inside
#'   \code{fetch_survey_plus()}) to avoid a redundant API call. Defaults to
#'   querying the Qualtrics API.
#' @return A data frame: the output of \code{qualtRics::extract_colmap(survey)}
#'   with one additional column, \code{value_labels}. Questions that have no
#'   choice scale (text entry, file upload, etc.) get \code{NA} in this column.
#' @details
#' The function matches colmap rows to questions via the \code{ImportId}
#' column, stripping any sub-item suffix (e.g.\ \code{"QID3_1"} or
#' \code{"QID3_TEXT"} both map to \code{"QID3"}). If \code{ImportId} is not
#' present in the colmap, all rows receive \code{NA}.
#'
#' Numeric codes are taken from each choice's \code{$recode} field, which is
#' what Qualtrics actually writes to the response data (e.g.\ a five-point
#' scale recoded as 4--8 will appear as 4--8, not 1--5). When \code{$recode}
#' is absent the choice key is used as a fallback.
#' @examples
#' \dontrun{
#' library(qualtRics)
#' sid <- subset(all_surveys(), name == "ReTurk Survey")$id
#' survey <- fetch_survey(surveyID = sid,
#'                        label = FALSE, convert = FALSE,
#'                        breakout_sets = FALSE)
#' cm <- extract_colmapPlus(survey, sid)
#' head(cm[, c("qname", "value_labels")])
#' }
#' @export
extract_colmapPlus <- function(survey, surveyID, sep = " | ",
                               meta = qualtRics::metadata(surveyID)) {
  if (!requireNamespace("qualtRics", quietly = TRUE)) {
    stop("Package 'qualtRics' is required for extract_colmapPlus(). ",
         "Install it from CRAN.", call. = FALSE)
  }

  cm <- qualtRics::extract_colmap(survey)

  qs <- meta$questions
  if (is.null(qs) || length(qs) == 0L) {
    cm$value_labels <- NA_character_
    return(cm)
  }

  qid_to_labels <- vapply(qs, function(q) {
    choices <- q$choices
    if (is.null(choices) || length(choices) == 0L) return(NA_character_)

    codes <- vapply(seq_along(choices), function(i) {
      ch <- choices[[i]]
      if (!is.null(ch$recode)) as.character(ch$recode) else names(choices)[i]
    }, character(1))

    labels <- vapply(choices, function(ch) {
      if (!is.null(ch$description)) return(as.character(ch$description))
      if (!is.null(ch$choiceText))  return(as.character(ch$choiceText))
      NA_character_
    }, character(1))

    keep <- !is.na(labels) & nzchar(labels)
    if (!any(keep)) return(NA_character_)

    paste(paste0(codes[keep], "=", labels[keep]), collapse = sep)
  }, character(1))

  if ("ImportId" %in% names(cm)) {
    qid_key <- sub("_.*$", "", as.character(cm$ImportId))
  } else {
    qid_key <- rep(NA_character_, nrow(cm))
  }

  cm$value_labels <- unname(qid_to_labels[qid_key])
  cm
}
