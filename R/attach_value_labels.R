#' Attach Value Labels to a Qualtrics Data Frame
#' @description
#' Decorates a numeric Qualtrics data frame with \code{haven::labelled}
#' value-label attributes pulled from the survey definition. Each
#' choice-based column ends up with (a) a \code{labels} attribute mapping
#' numeric response codes to their displayed labels and (b) a \code{label}
#' attribute holding the question text. Columns retain their numeric values,
#' so analyses behave exactly as before; tools that understand
#' \code{haven_labelled} (e.g.\ \code{haven::write_sav()},
#' \code{haven::as_factor()}, \code{sjlabelled::as_label()},
#' \code{labelled::lookfor()}) can read the embedded number/label
#' correspondence.
#' @param survey A data frame fetched with
#'   \code{qualtRics::fetch_survey(label = FALSE, convert = FALSE)} (i.e.,
#'   columns hold numeric response codes, not text labels).
#' @param surveyID Character. Survey ID, used to query
#'   \code{qualtRics::metadata()}.
#' @param meta Optional. The result of \code{qualtRics::metadata(surveyID)}.
#'   Supply this when the metadata has already been fetched (e.g.\ inside
#'   \code{fetch_survey_plus()}) to avoid a redundant API call. Defaults to
#'   querying the Qualtrics API.
#' @return The input data frame with \code{haven_labelled} attributes added to
#'   columns whose questions have a defined choice scale. Columns without a
#'   choice scale (text entry, file upload, descriptive blocks) are returned
#'   unchanged.
#' @details
#' For a matrix question, the same value-label set is applied to every
#' sub-item column; each column also receives its own variable label from the
#' colmap's \code{description} (combining the question stem with the sub-item
#' text). Non-numeric choice codes (rare — e.g., free-text "Other" codes)
#' cause that question's value-labels to be skipped rather than coerced.
#'
#' This function is intended as a companion to \code{extract_colmapPlus()}:
#' the colmap gives you a human-readable codebook, while
#' \code{attach_value_labels()} embeds the same mapping into the data file
#' itself for downstream export or interactive lookup.
#' @examples
#' \dontrun{
#' library(qualtRics)
#' sid <- subset(all_surveys(), name == "ReTurk Survey")$id
#' survey_num <- fetch_survey(surveyID = sid,
#'                            label = FALSE, convert = FALSE,
#'                            breakout_sets = FALSE)
#' survey_lab <- attach_value_labels(survey_num, sid)
#'
#' attr(survey_lab$Q589_1, "labels")   # named numeric: codes -> labels
#' attr(survey_lab$Q589_1, "label")    # variable label (question + sub-item)
#'
#' # Export to SPSS with labels intact:
#' haven::write_sav(survey_lab, "ReTurk.sav")
#' }
#' @export
attach_value_labels <- function(survey, surveyID,
                                meta = qualtRics::metadata(surveyID)) {
  if (!requireNamespace("qualtRics", quietly = TRUE)) {
    stop("Package 'qualtRics' is required for attach_value_labels(). ",
         "Install it from CRAN.", call. = FALSE)
  }
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' is required for attach_value_labels(). ",
         "Install it from CRAN.", call. = FALSE)
  }

  cm <- qualtRics::extract_colmap(survey)
  qs <- meta$questions
  if (is.null(qs) || length(qs) == 0L) return(survey)

  qid_to_labvec <- lapply(qs, function(q) {
    choices <- q$choices
    if (is.null(choices) || length(choices) == 0L) return(NULL)

    codes_chr <- vapply(seq_along(choices), function(i) {
      ch <- choices[[i]]
      if (!is.null(ch$recode)) as.character(ch$recode) else names(choices)[i]
    }, character(1))

    labels <- vapply(choices, function(ch) {
      if (!is.null(ch$description)) return(as.character(ch$description))
      if (!is.null(ch$choiceText))  return(as.character(ch$choiceText))
      NA_character_
    }, character(1))

    codes_num <- suppressWarnings(as.numeric(codes_chr))
    keep <- !is.na(codes_num) & !is.na(labels) & nzchar(labels)
    if (!any(keep)) return(NULL)

    stats::setNames(codes_num[keep], labels[keep])
  })

  if (!"ImportId" %in% names(cm)) return(survey)
  qid_key <- sub("_.*$", "", as.character(cm$ImportId))

  for (i in seq_len(nrow(cm))) {
    col_name <- cm$qname[i]
    if (is.na(col_name) || !col_name %in% names(survey)) next

    lab_vec <- qid_to_labvec[[qid_key[i]]]
    var_label <- if ("description" %in% names(cm)) cm$description[i] else NULL
    if (is.null(lab_vec) && (is.null(var_label) || is.na(var_label))) next

    col <- survey[[col_name]]
    if (!is.numeric(col)) {
      coerced <- suppressWarnings(as.numeric(as.character(col)))
      if (sum(is.na(coerced)) == sum(is.na(col))) {
        col <- coerced
      } else {
        next
      }
    }

    survey[[col_name]] <- haven::labelled(
      col,
      labels = lab_vec,
      label  = if (!is.null(var_label) && !is.na(var_label)) var_label else NULL
    )
  }

  survey
}
