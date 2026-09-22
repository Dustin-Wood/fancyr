#' Simulated Two-Wave Study of Personality and a Leadership Experience
#'
#' A simulated dataset with a \emph{known} answer, for learning and checking
#' \code{\link{stabilityPaths}}. Six personality items were measured a year
#' apart; in between, about 30\% of people took a leadership role. Items differ
#' in how the experience relates to them:
#'
#' \itemize{
#'   \item \code{dominant}: strongly selects people into leadership \emph{and}
#'     is increased by it -- a corresponsive trait, so a meaningful part of its
#'     stability runs through the experience.
#'   \item \code{sociable}: the same pattern, more weakly.
#'   \item \code{anxious}: reduced by leadership, but plays no part in who
#'     leads. A real experience effect that carries none of the item's
#'     stability, because there is no selection for it to multiply.
#'   \item \code{organized}, \code{curious}, \code{warm}: unrelated to
#'     leadership.
#' }
#'
#' Socioeconomic status (\code{ses}) relates to several traits at both waves
#' and to leadership, so it acts as a confounder. Items were measured with the
#' known reliabilities in \code{reliability}. About 20\% of people did not return
#' at Time 2, and dropout depends on observed Time 1 values (\code{ses} and
#' \code{organized}): the data are missing at random.
#'
#' @format A list with six elements:
#' \describe{
#'   \item{T1}{Data frame, 800 rows: \code{id}, the six items (roughly 1-5
#'     scale), \code{ses} (standardized), and \code{date} of measurement.}
#'   \item{T2}{Data frame, one row per returning participant: \code{id}, the
#'     six items, and \code{date}.}
#'   \item{experience}{Data frame listing only the people who took a leadership
#'     role: \code{id} and \code{leader} (always 1). Everyone else should be
#'     coded 0, e.g. with \code{stabilityData(..., fill = list(leader = 0))}.}
#'   \item{reliability}{Named numeric vector: each item's reliability, the same
#'     at both waves.}
#'   \item{truth}{Data frame of the true standardized decomposition for each
#'     item (\code{item}, \code{path}, \code{type}, \code{est}, \code{share}):
#'     what \code{stabilityPaths(..., X = "leader", controls = "ses")} would
#'     recover from error-free scores in an infinitely large sample.}
#'   \item{generating}{Data frame of the generating parameters for each item's
#'     latent trait: effects of \code{ses} at each wave, selection into
#'     leadership, latent stability, socialization by leadership, and
#'     reliability.}
#' }
#' @source Simulated; see \code{data-raw/stabilitySim.R} in the package source.
#' @examples
#' str(stabilitySim, max.level = 1)
#' stabilitySim$truth[stabilitySim$truth$item == "dominant", ]
"stabilitySim"
