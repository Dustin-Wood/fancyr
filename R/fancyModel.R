#' Define a Reusable lavaan Model Specification
#' @description
#' Packages a lavaan model as a reusable specification that can be fitted once
#' with \code{\link{fitModel}}, or slid across a whole set of variables with
#' \code{\link{modelOnAllY}}. This separates \emph{what the model is} from
#' \emph{which variables go into it}, so the same model can be run over many
#' outcomes without rewriting the syntax each time.
#'
#' @details
#' The syntax is written in \strong{role names} rather than column names. A role
#' is either:
#'
#' \itemize{
#'   \item a \strong{sliding} role, named in \code{slide}, whose column changes
#'     from fit to fit (e.g. \code{Y1} and \code{Y2} for a two-wave item), or
#'   \item a \strong{fixed} role, named in \code{vars}, bound to one column that
#'     stays the same across all fits (e.g. a control variable).
#' }
#'
#' Writing the syntax in role names is not only tidiness: lavaan cannot parse
#' variable names containing spaces or punctuation, so a column such as
#' \code{"SAT Math"} can never appear in model syntax directly. Binding it to
#' the role \code{C1} solves that, and \code{\link{fitModel}} restores the
#' original names on output.
#'
#' \code{extract} declares which labelled parameters to lift into the tidy
#' output and how to annotate them. Any label in the syntax works, whether it
#' sits on a regression (\code{~}), a covariance (\code{~~}), or a defined
#' parameter (\code{:=}).
#'
#' @param syntax Character string of lavaan model syntax, written in role names.
#' @param slide Character vector of role names whose columns change from fit to
#'   fit. For a two-wave stability model this is \code{c("Y1", "Y2")}; for a
#'   model with a single sliding outcome it might be just \code{"Y"}.
#' @param vars Named character vector mapping fixed role names to column names,
#'   e.g. \code{c(C1 = "SAT Math", C2 = "genderNum")}. Names are the roles used
#'   in \code{syntax}; values are the columns in the data. Defaults to
#'   \code{NULL} (no fixed variables).
#' @param extract Data frame declaring which labelled parameters to return.
#'   Must contain a \code{label} column; any further columns (conventionally
#'   \code{path}, \code{via}, \code{type}) are carried through as annotations
#'   onto each extracted row. Defaults to \code{NULL}, in which case every
#'   defined parameter (\code{:=}) in the syntax is extracted unannotated.
#' @param roles Optional named character vector giving each role a semantic
#'   label to record in \code{fitModel}'s \code{$varmap}, e.g.
#'   \code{c(X1 = "mediator", C1 = "control")}. Downstream consumers such as
#'   \code{\link{plotMedX}} use these labels to decide how to draw each
#'   variable. Roles not listed fall back to \code{"slide"} or \code{"fixed"}.
#' @param sem_args List of extra arguments passed to \code{\link[lavaan]{sem}},
#'   e.g. \code{list(missing = "fiml", fixed.x = FALSE)}. Defaults to those two.
#' @param label Optional short description of the model, shown when printing.
#'
#' @return An object of class \code{fancyModel}: a list with elements
#'   \code{syntax}, \code{slide}, \code{vars}, \code{extract}, \code{sem_args},
#'   \code{label}, and \code{fit_fn} (the fitting backend; currently always the
#'   lavaan fitter).
#'
#' @seealso \code{\link{fitModel}} to fit one, \code{\link{modelOnAllY}} to slide
#'   one across a variable set, and \code{\link{stabilityModel}} for a
#'   ready-made stability-decomposition spec.
#'
#' @examples
#' # Stability of each item, with one fixed control, no mediators
#' spec <- fancyModel(
#'   syntax = "
#'     Y2 ~ b21*Y1 + bG*G
#'     Y1 ~~ G
#'   ",
#'   slide   = c("Y1", "Y2"),
#'   vars    = c(G = "SAT Math"),
#'   extract = data.frame(label = "b21", path = "residual", type = "residual")
#' )
#' spec
#'
#' @export
fancyModel <- function(syntax, slide, vars = NULL, extract = NULL,
                       roles = NULL,
                       sem_args = list(missing = "fiml", fixed.x = FALSE),
                       label = NULL) {

  if (!is.character(syntax) || length(syntax) != 1L)
    stop("`syntax` must be a single character string of lavaan syntax.")
  if (!length(slide))
    stop("`slide` must name at least one sliding role.")
  slide <- as.character(slide)

  if (!is.null(vars)) {
    if (is.null(names(vars)) || any(!nzchar(names(vars))))
      stop("`vars` must be a *named* vector mapping role names to column names, ",
           "e.g. c(C1 = \"SAT Math\").")
    vars <- stats::setNames(as.character(vars), names(vars))
    clash <- intersect(names(vars), slide)
    if (length(clash))
      stop("Role(s) named in both `slide` and `vars`: ",
           paste(clash, collapse = ", "))
    if (anyDuplicated(names(vars)))
      stop("Duplicate role name(s) in `vars`: ",
           paste(unique(names(vars)[duplicated(names(vars))]), collapse = ", "))
  }

  if (is.null(extract)) {
    # default: every defined parameter in the syntax, unannotated
    defined <- regmatches(syntax, gregexpr("(\\w+)\\s*:=", syntax))[[1]]
    defined <- sub("\\s*:=$", "", defined)
    extract <- if (length(defined))
      data.frame(label = defined, stringsAsFactors = FALSE) else
      data.frame(label = character(0), stringsAsFactors = FALSE)
  }
  if (!is.data.frame(extract) || !"label" %in% names(extract))
    stop("`extract` must be a data frame with at least a `label` column.")
  extract$label <- as.character(extract$label)

  if (!is.null(roles)) {
    if (is.null(names(roles)) || any(!nzchar(names(roles))))
      stop("`roles` must be a *named* vector, e.g. c(X1 = \"mediator\").")
    roles <- stats::setNames(as.character(roles), names(roles))
  }

  structure(
    list(syntax   = syntax,
         slide    = slide,
         vars     = vars,
         extract  = extract,
         roles    = roles,
         sem_args = sem_args,
         label    = label,
         fit_fn   = "lavaan"),
    class = "fancyModel"
  )
}

#' @param x A \code{fancyModel} object.
#' @param ... Ignored.
#' @rdname fancyModel
#' @export
print.fancyModel <- function(x, ...) {
  cat("<fancyModel>",
      if (!is.null(x$label)) paste0(" ", x$label) else "", "\n", sep = "")
  cat("  sliding roles: ", paste(x$slide, collapse = ", "), "\n", sep = "")
  if (length(x$vars))
    cat("  fixed roles:   ",
        paste(sprintf("%s = %s", names(x$vars), x$vars), collapse = ", "),
        "\n", sep = "")
  else
    cat("  fixed roles:   (none)\n")
  cat("  extracts:      ",
      if (nrow(x$extract)) paste(x$extract$label, collapse = ", ") else "(none)",
      "\n", sep = "")
  cat("  backend:       ", x$fit_fn, "\n", sep = "")
  cat("  syntax:\n")
  cat(paste0("    ", strsplit(trimws(x$syntax), "\n")[[1]]), sep = "\n")
  invisible(x)
}
