#' Require one row per ID before merging
#'
#' \code{merge()} on a duplicated key produces a Cartesian product rather than
#' an error: if an ID appears twice in one frame and once in the other, the
#' merge emits two rows for that person. Nothing warns, every sample size
#' inflates, and the duplicated cases get extra weight in every estimate --- a
#' wrong answer that looks entirely normal. Callers that merge on an ID should
#' therefore refuse to run until the input is one row per ID.
#'
#' @param df A data frame to check.
#' @param id_col Name of the ID column.
#' @param arg_name Name of the argument \code{df} arrived as, used in messages.
#' @param why One line explaining what duplicates would do to \emph{this}
#'   caller, since the consequence differs by join style: \code{merge()}
#'   multiplies rows, \code{match()} silently keeps only the first.
#' @param max_show Maximum number of offending IDs to name. Defaults to 5.
#' @return Invisibly \code{TRUE}; called for its side effect of stopping.
#' @keywords internal
#' @noRd
checkUniqueIDs <- function(df, id_col, arg_name,
                           why = "Duplicate IDs make row alignment ambiguous.",
                           max_show = 5) {

  if (!is.data.frame(df))
    stop("`", arg_name, "` must be a data frame.", call. = FALSE)
  if (!id_col %in% names(df))
    stop("id_col \"", id_col, "\" not found in `", arg_name, "`.", call. = FALSE)

  ids <- df[[id_col]]
  dup <- unique(ids[duplicated(ids)])
  if (!length(dup)) return(invisible(TRUE))

  shown <- paste(utils::head(dup, max_show), collapse = ", ")
  more  <- if (length(dup) > max_show)
    sprintf(" (and %d more)", length(dup) - max_show) else ""

  stop("`", arg_name, "` has ", length(dup), " duplicated value(s) of \"",
       id_col, "\": ", shown, more, ".\n",
       "  ", why, "\n",
       "  Reduce `", arg_name, "` to one row per \"", id_col,
       "\" before calling.", call. = FALSE)
}
