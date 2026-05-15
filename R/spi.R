#' Seconds Per Item (SPI) — response speed for a block of items
#' @description
#' Compute the average seconds the respondent spent per item for a block of
#' items, using Qualtrics' page-timing fields (or any equivalent source of
#' first-click and last-click timestamps within a page).
#'
#' \deqn{spi = (t_{last} - t_{first}) / (n_{items} - 1)}
#'
#' The intuition: \code{t_first} is when the respondent first interacted with
#' the page (typically the first response click) and \code{t_last} is when
#' they made their final response click. The interval between these two clicks
#' brackets \eqn{n_{items} - 1} response transitions, so dividing by
#' \eqn{n_{items} - 1} gives the average seconds-per-item.
#'
#' @param t_first Numeric vector. Time of first click within the page, in
#'   seconds. In Qualtrics page-timing question output this is the
#'   \emph{First Click} column for a given page.
#' @param t_last Numeric vector (same length as \code{t_first}). Time of last
#'   click within the page, in seconds. In Qualtrics this is the
#'   \emph{Last Click} column.
#' @param n_items Integer scalar or vector. Number of items the respondent
#'   was rating on the page.
#' @details
#' Wood, Harms, Lowman, & DeSimone (2017) recommend treating \code{spi < 1}
#' (less than one second per item, on average) as a flag for likely careless
#' responding. This threshold is not enforced by \code{spi()} itself — it
#' returns the raw seconds-per-item — but is the default suggested cut for
#' downstream screening utilities.
#'
#' Note that the formula divides by \code{n_items - 1} rather than
#' \code{n_items}: the two click timestamps bracket the response transitions
#' between items, not the response events themselves.
#'
#' @return Numeric vector the same length as \code{t_first} giving the
#'   estimated seconds-per-item. Returns \code{NA} where either click time
#'   is missing.
#' @references Wood, D., Harms, P., Lowman, G. H., & DeSimone, J. A. (2017).
#'   Response speed and response consistency as mutually validating indicators
#'   of data quality in online samples. \emph{Social Psychological and
#'   Personality Science, 8}(4), 454-464.
#' @export
#' @examples
#' # Three respondents who rated a 10-item block:
#' #   r1 took 25 seconds (2.5 spi -- fine)
#' #   r2 took 6 seconds (0.67 spi -- below the suggested cut)
#' #   r3 has missing click data
#' spi(t_first = c(5,  3, NA),
#'     t_last  = c(30, 9, 12),
#'     n_items = 10)
#'
#' # Flag careless responders using the Wood et al. (2017) threshold
#' rates <- spi(t_first = c(5, 3, 8), t_last = c(30, 9, 40), n_items = 10)
#' careless <- rates < 1
spi <- function(t_first, t_last, n_items) {
  (t_last - t_first) / (n_items - 1)
}
