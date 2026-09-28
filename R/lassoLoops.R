#' Repeated Cross-Validated Lasso Regression
#' @description
#' Fits a lasso (or elastic-net) regression of \code{y} on a set of predictors
#' many times, each time on a random training share of the sample, and checks
#' each fit's predictions against the held-out remainder. The result keeps
#' every repetition's coefficients, so it shows both how well the predictors
#' forecast \code{y} in new cases (the \emph{holdout validity}) and how
#' consistently each predictor is chosen (its \emph{selection frequency}).
#'
#' Each repetition calls \code{\link[glmnet]{cv.glmnet}} on the training
#' share, which chooses the penalty by its own internal \code{nfolds}-fold
#' cross-validation. Predictions for new data use the coefficients averaged
#' over repetitions; see \code{\link{predict.lassoLoops}}.
#'
#' @details
#' \strong{Each repetition:}
#' \enumerate{
#'   \item draws a random \code{1 - holdout} share of cases for training;
#'   \item fills missing predictor values with the training-share means;
#'   \item if \code{standardize = TRUE}, z-scores the predictors (and, for a
#'     continuous \code{y}, the outcome) using the training share's means and
#'     standard deviations, applying the same values to the holdout cases, so
#'     nothing about the holdout cases informs the model;
#'   \item fits \code{cv.glmnet} to the training share and keeps the
#'     coefficients at the penalty chosen by \code{s};
#'   \item predicts the holdout cases and records the validity.
#' }
#'
#' With \code{standardize = TRUE}, the coefficients are standardized: each is
#' the expected difference in \code{y} (in SDs, for a continuous outcome; in
#' log-odds, for a binary one) per SD of the predictor, holding the others
#' constant. Predictors the lasso drops in a repetition get a coefficient of
#' 0 there, so averaged coefficients shrink toward 0 for predictors that are
#' chosen only sometimes.
#'
#' For prediction, the averaged coefficients are applied to new data after
#' filling missing values and z-scoring with the means and SDs of the whole
#' sample analysed here (not of the new data), so a new sample, or a single
#' new person, is scored on the same metric as the original sample.
#'
#' Cases with a missing \code{y}, or with no predictor data at all, are
#' dropped.
#'
#' @section Validity:
#' For a continuous \code{y}, \code{r} is the correlation between predicted
#' and actual scores in the holdout cases. For a binary \code{y}
#' (\code{family = "binomial"}), \code{r} is the correlation of the predicted
#' probability with the 0/1 outcome, \code{r_link} the same for the predicted
#' log-odds, and \code{auc} the area under the ROC curve: the probability
#' that a randomly chosen holdout case with \code{y = 1} has a higher
#' prediction than one with \code{y = 0}. Holdout validities vary from split
#' to split, and their spread across repetitions shows how much.
#'
#' @param x A data frame or matrix of numeric predictors.
#' @param y The outcome: a numeric vector with one value per row of \code{x},
#'   or, for \code{family = "binomial"}, a vector with exactly two distinct
#'   values (0/1, logical, or a two-level factor; the second level or the
#'   larger value is coded 1).
#' @param loops Number of repetitions. Default 100.
#' @param family \code{"gaussian"} (default) for a continuous outcome or
#'   \code{"binomial"} for a binary one (logistic lasso).
#' @param holdout Share of cases held out for validation in each repetition.
#'   Default \code{.2}.
#' @param alpha The elastic-net mixing parameter passed to
#'   \code{cv.glmnet}: 1 (default) is the lasso, 0 ridge regression.
#' @param s Which penalty to keep from each \code{cv.glmnet} fit:
#'   \code{"lambda.min"} (default), the penalty with the smallest
#'   cross-validated error, or \code{"lambda.1se"}, the largest penalty within
#'   one standard error of it, which keeps fewer predictors.
#' @param nfolds Number of folds for \code{cv.glmnet}'s internal
#'   cross-validation. Default 10.
#' @param standardize Logical. Standardize the predictors (and a continuous
#'   outcome) within each repetition, so that the coefficients are
#'   standardized? Default \code{TRUE}.
#' @param seed Random seed, so the splits and folds can be reproduced.
#'   Default 1. The user's own random-number stream is restored afterwards.
#'   \code{NULL} leaves the seed alone. Each repetition gets its own seed drawn
#'   from this one, so results are identical whatever \code{cores} is.
#' @param cores Number of CPU cores to spread the repetitions over (default
#'   1, no parallel processing), or a cluster from
#'   \code{\link[parallel]{makeCluster}} to reuse. See "Parallel processing".
#'
#' @section Parallel processing:
#' With \code{cores} above 1, the work is shared among that many extra R
#' sessions running in the background. Each takes a few seconds to start, so
#' this pays off for longer runs. \code{parallel::detectCores()} reports how
#' many cores the machine has; on a shared server, use no more than you were
#' allocated. Each session holds its own copy of the data, so very many cores
#' can use a lot of memory.
#'
#' The sessions talk to yours over a network connection that stays on your
#' computer. On Windows, the first time this happens a firewall prompt may
#' ask whether R may "communicate on networks"; allowing it (private networks
#' are enough) lets parallel processing run. If the sessions can't be started
#' within 30 seconds, for example because a firewall or IT policy blocks the
#' connection, the analysis runs on one core instead, with a warning.
#' Results are the same either way.
#'
#' @return An object of class \code{lassoLoops}: a list with
#' \item{coefficients}{Matrix of coefficients, one row per term (the
#'   intercept first, then each predictor) and one column per repetition.}
#' \item{summary}{Data frame, one row per predictor: \code{predictor},
#'   \code{mean} and \code{sd} of its coefficient across repetitions,
#'   \code{selected} (the share of repetitions in which it had a nonzero
#'   coefficient), and \code{mean_selected} (its mean coefficient in the
#'   repetitions that selected it), sorted by the absolute mean.}
#' \item{validity}{Data frame, one row per repetition: \code{loop},
#'   \code{lambda}, \code{n_predictors} (nonzero coefficients), \code{r}, and
#'   for a binary outcome \code{r_link} and \code{auc}.}
#' \item{center, scale}{Means and SDs of the predictors in the analysed
#'   sample, used by \code{predict()}.}
#' \item{y_center, y_scale}{Mean and SD of a standardized continuous outcome,
#'   used by \code{predict(type = "response")}; \code{NA} otherwise.}
#' \item{y_levels}{For a binary outcome, the original values coded 0 and 1.}
#' \item{n}{Number of cases analysed.}
#' \item{settings}{The arguments used.}
#'
#' \code{print()} shows the holdout validity and the most influential
#' predictors; \code{summary()} returns the full \code{$summary} table;
#' \code{coef()} returns the averaged coefficients; and \code{predict()}
#' scores new data.
#'
#' @seealso \code{\link{predict.lassoLoops}}, \code{\link[glmnet]{cv.glmnet}}
#'
#' @examples
#' \donttest{
#' # How well do people's self-descriptions on 59 adjectives predict the
#' # social power their peers attribute to them?
#' if (requireNamespace("glmnet", quietly = TRUE)) {
#'   T1 <- powerTraits$T1
#'   adj <- powerTraits$items$item
#'   fit <- lassoLoops(T1[adj], T1$power, loops = 20)
#'   fit
#'   head(summary(fit))
#'
#'   # score the same people (or new ones) with the averaged coefficients
#'   head(predict(fit, T1[adj]))
#' }
#' }
#'
#' @export
lassoLoops <- function(x, y, loops = 100, family = c("gaussian", "binomial"),
                       holdout = .2, alpha = 1,
                       s = c("lambda.min", "lambda.1se"), nfolds = 10,
                       standardize = TRUE, seed = 1, cores = 1) {
  if (!requireNamespace("glmnet", quietly = TRUE))
    stop("lassoLoops() needs the glmnet package: install.packages(\"glmnet\").")
  family <- match.arg(family)
  s <- match.arg(s)
  if (!is.data.frame(x) && !is.matrix(x))
    stop("`x` must be a data frame or matrix of predictors.")
  x <- as.data.frame(x, check.names = FALSE)
  if (is.null(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)))
    stop("The predictors in `x` need unique column names.")
  nonnum <- names(x)[!vapply(x, function(v) is.numeric(v) || is.logical(v), logical(1))]
  if (length(nonnum))
    stop("Predictors must be numeric; not numeric: ", paste(nonnum, collapse = ", "),
         ". Dummy-code factors first (e.g. with model.matrix()).")
  x <- as.matrix(data.frame(lapply(x, as.numeric), check.names = FALSE))
  if (length(y) != nrow(x)) stop("`y` must have one value per row of `x`.")
  if (!is.numeric(loops) || loops < 1) stop("`loops` must be a positive number.")
  if (!is.numeric(holdout) || holdout <= 0 || holdout >= 1)
    stop("`holdout` must be a proportion between 0 and 1.")

  ## ---- outcome ------------------------------------------------------------
  y_levels <- NULL
  if (family == "binomial") {
    vals <- if (is.factor(y)) levels(droplevels(y[!is.na(y)])) else sort(unique(y[!is.na(y)]))
    if (length(vals) != 2L)
      stop("A binomial `y` needs exactly two distinct values; found ", length(vals), ".")
    y_levels <- stats::setNames(as.character(vals), c("0", "1"))
    y <- ifelse(is.na(y), NA_real_, as.numeric(as.character(y) == as.character(vals[2])))
  } else {
    if (!is.numeric(y)) stop("`y` must be numeric for family = \"gaussian\".")
    y <- as.numeric(y)
  }

  ## ---- cases --------------------------------------------------------------
  keep <- !is.na(y) & rowSums(!is.na(x)) > 0
  if (any(!keep))
    message("lassoLoops(): dropped ", sum(!keep), " case(s) with a missing outcome ",
            "or no predictor data.")
  x <- x[keep, , drop = FALSE]; y <- y[keep]
  n <- nrow(x)
  n_test <- floor(holdout * n)
  if (n_test < 3 || n - n_test < 3 * nfolds)
    stop("Too few cases (", n, ") for a holdout of ", holdout, " and ", nfolds,
         " folds.")
  allNA <- colnames(x)[colSums(!is.na(x)) == 0]
  if (length(allNA)) stop("Predictor(s) with no data: ", paste(allNA, collapse = ", "))

  ## ---- reproducible, without disturbing the user's random stream -----------
  if (!is.null(seed)) {
    had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(if (had) assign(".Random.seed", old, envir = globalenv())
            else rm(".Random.seed", envir = globalenv()), add = TRUE)
    set.seed(seed)
  }
  # one seed per repetition, drawn up front, so each repetition's split and
  # folds are the same however many cores share the work
  loopSeeds <- sample.int(.Machine$integer.max, loops)

  terms <- c("(Intercept)", colnames(x))
  oneLoop <- function(i) {
    set.seed(loopSeeds[i])
    test  <- sample.int(n, n_test)
    train <- setdiff(seq_len(n), test)
    st <- prepStats(x[train, , drop = FALSE], standardize)
    xtr <- applyStats(x[train, , drop = FALSE], st)
    xte <- applyStats(x[test, , drop = FALSE], st)
    ytr <- y[train]
    if (family == "gaussian" && standardize)
      ytr <- (ytr - mean(ytr)) / stats::sd(ytr)
    m <- glmnet::cv.glmnet(xtr, ytr, family = family, alpha = alpha,
                           nfolds = nfolds)
    b <- as.matrix(stats::coef(m, s = s))[, 1][terms]
    link <- as.numeric(b[1] + xte %*% b[-1])
    yte <- y[test]
    list(b = b, lambda = m[[s]], n_predictors = sum(b[-1] != 0),
         r = if (family == "gaussian") safeCor(link, yte)
             else safeCor(stats::plogis(link), yte),
         r_link = if (family == "binomial") safeCor(link, yte) else NA_real_,
         auc = if (family == "binomial") aucRank(link, yte) else NA_real_)
  }
  # Give the loop (and the helpers it calls) a minimal environment holding
  # just what it needs, so parallel workers receive the data but don't have to
  # load fancyr and all its imports -- only glmnet.
  env <- list2env(list(x = x, y = y, n = n, n_test = n_test, terms = terms,
                       loopSeeds = loopSeeds, standardize = standardize,
                       family = family, alpha = alpha, nfolds = nfolds, s = s),
                  parent = baseenv())
  for (f in c("prepStats", "applyStats", "safeCor", "aucRank")) {
    fn <- get(f); environment(fn) <- env; assign(f, fn, envir = env)
  }
  environment(oneLoop) <- env
  res <- fancyLapply(seq_len(loops), oneLoop, cores = cores)

  coefs <- vapply(res, `[[`, numeric(length(terms)), "b")
  coefs <- matrix(coefs, length(terms), loops,
                  dimnames = list(terms, paste0("loop", seq_len(loops))))
  pick <- function(nm) vapply(res, `[[`, numeric(1), nm)
  val <- data.frame(loop = seq_len(loops), lambda = pick("lambda"),
                    n_predictors = as.integer(pick("n_predictors")), r = pick("r"))
  if (family == "binomial") { val$r_link <- pick("r_link"); val$auc <- pick("auc") }

  ## ---- summaries on the whole analysed sample --------------------------------
  st <- prepStats(x, standardize)
  y_center <- y_scale <- NA_real_
  if (family == "gaussian" && standardize) { y_center <- mean(y); y_scale <- stats::sd(y) }

  B <- coefs[-1, , drop = FALSE]
  sel <- rowMeans(B != 0)
  summ <- data.frame(
    predictor = rownames(B),
    mean = rowMeans(B),
    sd = apply(B, 1, stats::sd),
    selected = sel,
    mean_selected = ifelse(sel > 0, rowSums(B) / rowSums(B != 0), NA_real_),
    stringsAsFactors = FALSE)
  summ <- summ[order(-abs(summ$mean)), ]
  rownames(summ) <- NULL

  structure(
    list(coefficients = coefs, summary = summ, validity = val,
         center = st$center, scale = st$scale,
         y_center = y_center, y_scale = y_scale, y_levels = y_levels, n = n,
         settings = list(loops = loops, family = family, holdout = holdout,
                         alpha = alpha, s = s, nfolds = nfolds,
                         standardize = standardize, seed = seed,
                         cores = if (is.numeric(cores)) cores else 1L)),
    class = "lassoLoops")
}

# Means (for filling missing values) and, if standardizing, SDs of each
# predictor. A predictor with no variance keeps an SD of 1, so it passes
# through unscaled (glmnet then gives it a zero coefficient).
prepStats <- function(x, standardize) {
  center <- colMeans(x, na.rm = TRUE)
  scale <- if (standardize) apply(x, 2, stats::sd, na.rm = TRUE)
           else stats::setNames(rep(1, ncol(x)), colnames(x))
  scale[is.na(scale) | scale == 0] <- 1
  list(center = center, scale = scale, standardize = standardize)
}

# Fill missing values with the stored means; then z-score if standardizing.
applyStats <- function(x, st) {
  for (j in seq_len(ncol(x))) {
    v <- x[, j]
    v[is.na(v)] <- st$center[j]
    x[, j] <- if (st$standardize) (v - st$center[j]) / st$scale[j] else v
  }
  x
}

safeCor <- function(a, b)
  if (stats::sd(a) > 0 && stats::sd(b) > 0) stats::cor(a, b) else NA_real_

# Area under the ROC curve from ranks (the Mann-Whitney statistic).
aucRank <- function(score, y) {
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (!n1 || !n0) return(NA_real_)
  (sum(rank(score)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

#' Predict from a lassoLoops Fit
#' @description
#' Scores new data with the coefficients from \code{\link{lassoLoops}},
#' averaged across repetitions. Predictors are matched by name. Missing values
#' are filled, and predictors z-scored, with the means and SDs of the sample
#' the model was fitted to, so new cases are scored on that sample's metric,
#' whatever the new sample's own means.
#'
#' \code{dvPred()} is a shorthand for \code{predict()}.
#'
#' @param object A \code{lassoLoops} object.
#' @param newdata A data frame or matrix containing (at least) every
#'   predictor used in the fit, by name.
#' @param type \code{"link"} (default) or \code{"response"}. For a
#'   continuous outcome, \code{"link"} gives predictions in the fitted metric
#'   (z-scores of \code{y} when standardized) and \code{"response"} converts
#'   them to \code{y}'s original units. For a binary outcome, \code{"link"}
#'   gives predicted log-odds and \code{"response"} predicted probabilities
#'   that \code{y = 1}.
#' @param ... Not used.
#' @return A numeric vector of predictions, one per row of \code{newdata}.
#' @seealso \code{\link{lassoLoops}}
#' @export
predict.lassoLoops <- function(object, newdata, type = c("link", "response"), ...) {
  type <- match.arg(type)
  vars <- names(object$center)
  if (!is.data.frame(newdata) && !is.matrix(newdata))
    stop("`newdata` must be a data frame or matrix.")
  absent <- setdiff(vars, colnames(newdata))
  if (length(absent))
    stop("`newdata` is missing predictor(s): ", paste(absent, collapse = ", "))
  x <- as.matrix(data.frame(lapply(as.data.frame(newdata, check.names = FALSE)[vars],
                                   as.numeric), check.names = FALSE))
  st <- list(center = object$center, scale = object$scale,
             standardize = object$settings$standardize)
  b <- stats::coef(object)
  link <- as.numeric(b[1] + applyStats(x, st) %*% b[-1])
  if (type == "link") return(link)
  if (object$settings$family == "binomial") return(stats::plogis(link))
  if (is.na(object$y_scale)) link else link * object$y_scale + object$y_center
}

#' @rdname predict.lassoLoops
#' @param data For \code{dvPred()}: the new data (as \code{newdata}).
#' @export
dvPred <- function(object, data, type = c("link", "response")) {
  predict.lassoLoops(object, data, type = match.arg(type))
}

#' Print, Summarize, and Extract Coefficients from a lassoLoops Fit
#' @description
#' \code{print()} shows the holdout validity across repetitions and the
#' predictors with the largest mean coefficients, with how often each was
#' selected. \code{summary()} returns the full table for every predictor, and
#' \code{coef()} the coefficients averaged over repetitions (intercept first),
#' which \code{\link{predict.lassoLoops}} uses.
#' @param x,object A \code{lassoLoops} object from \code{\link{lassoLoops}}.
#' @param digits Number of decimal places to print.
#' @param top Number of predictors to list in \code{print()}.
#' @param ... Not used.
#' @return \code{print()} returns \code{x} invisibly; \code{summary()} a data
#'   frame (see \code{$summary} in \code{\link{lassoLoops}}); \code{coef()} a
#'   named numeric vector.
#' @name lassoLoops-methods
#' @rdname lassoLoops-methods
#' @export
print.lassoLoops <- function(x, digits = 2, top = 10, ...) {
  st <- x$settings
  v <- x$validity
  cat("<lassoLoops>", st$loops, "repetitions of",
      if (st$alpha == 1) "lasso" else sprintf("elastic net (alpha = %s)", st$alpha),
      sprintf("(%s), n = %d, %d predictors\n", st$family, x$n, length(x$center)))
  cat(sprintf("  each: %.0f%% training / %.0f%% holdout; %d-fold CV; penalty %s; %s\n",
              100 * (1 - st$holdout), 100 * st$holdout, st$nfolds, st$s,
              if (st$standardize) "standardized coefficients" else "raw coefficients"))
  q <- function(v) sprintf("mean %s (SD %s; middle 90%%: %s to %s)",
                           fmtNum(mean(v, na.rm = TRUE), digits),
                           fmtNum(stats::sd(v, na.rm = TRUE), digits),
                           fmtNum(stats::quantile(v, .05, na.rm = TRUE), digits),
                           fmtNum(stats::quantile(v, .95, na.rm = TRUE), digits))
  cat("\n  holdout validity\n")
  cat("    r:  ", q(v$r), "\n")
  if (st$family == "binomial") cat("    AUC:", q(v$auc), "\n")
  cat(sprintf("    predictors kept per repetition: median %s (range %d to %d)\n",
              format(stats::median(v$n_predictors)), min(v$n_predictors),
              max(v$n_predictors)))
  s <- utils::head(x$summary, top)
  s <- data.frame(predictor = s$predictor, mean = fmtNum(s$mean, digits + 1),
                  sd = fmtNum(s$sd, digits + 1),
                  selected = paste0(fmtNum(100 * s$selected, 0), "%"),
                  stringsAsFactors = FALSE)
  cat(sprintf("\n  top %d predictors by mean coefficient (all in $summary)\n", nrow(s)))
  print(s, row.names = FALSE, right = FALSE)
  invisible(x)
}

#' @rdname lassoLoops-methods
#' @export
summary.lassoLoops <- function(object, ...) object$summary

#' @rdname lassoLoops-methods
#' @export
coef.lassoLoops <- function(object, ...) rowMeans(object$coefficients)
