# Creates `stabilitySim` in the calling environment: a simulated two-wave
# personality study with a leadership experience between waves, known latent
# paths, known item reliabilities, and dropout that depends on observed
# baseline scores (MAR). Also defines the generators `genTrue()` and
# `addError()`, plus `items` and `pars`.
#
# Development use only (dev/ is excluded from the package build): it gives
# dev/validate_stability.R a dataset whose true decomposition is known. The
# package itself ships no simulated data.
#
# Run from the package root, with the package loaded:
#   devtools::load_all(); source("dev/sim_stability.R")

items <- c("dominant", "sociable", "anxious", "organized", "curious", "warm")

# Latent-trait generating values (true scores have variance ~1 at T1).
pars <- data.frame(
  item      = items,
  ses_T1    = c(.30,  .20, -.20,  .20,  .30,  .00),  # ses -> trait at T1
  select    = c(.80,  .30,  .00,  .00,  .00,  .00),  # trait -> leader propensity
  stability = c(.60,  .60,  .55,  .65,  .60,  .70),  # T1 trait -> T2 trait
  socialize = c(.35,  .20, -.30,  .00,  .00,  .00),  # leader -> T2 trait
  ses_T2    = c(.10,  .10,  .00,  .15,  .10,  .00),  # ses -> T2 trait
  rel       = c(.80,  .70,  .75,  .60,  .65,  .85),  # reliability, both waves
  stringsAsFactors = FALSE
)

genTrue <- function(n) {
  ses <- stats::rnorm(n)
  T1 <- sapply(seq_along(items), function(j)
    pars$ses_T1[j] * ses + stats::rnorm(n, sd = sqrt(1 - pars$ses_T1[j]^2)))
  colnames(T1) <- items
  prop   <- as.vector(T1 %*% pars$select) + .2 * ses + stats::rnorm(n)
  leader <- as.numeric(prop > stats::quantile(prop, .70))   # top 30% lead
  T2 <- sapply(seq_along(items), function(j)
    pars$stability[j] * T1[, j] + pars$socialize[j] * leader +
      pars$ses_T2[j] * ses + stats::rnorm(n, sd = .7))
  colnames(T2) <- items
  list(ses = ses, leader = leader, T1 = T1, T2 = T2)
}

# Add error so each observed item has exactly its reliability in the sample,
# then put scores on a familiar 1-5-ish scale (linear, so reliability holds).
addError <- function(tru, rel) {
  err_sd <- sqrt(stats::var(tru) * (1 - rel) / rel)
  obs <- tru + stats::rnorm(length(tru), sd = err_sd)
  round(3 + 0.8 * (obs - mean(obs)) / stats::sd(obs), 2)
}

## ---- the bundled sample ------------------------------------------------------
set.seed(20170101)
n  <- 800
tr <- genTrue(n)
id <- sprintf("P%03d", seq_len(n))

T1 <- data.frame(id = id, stringsAsFactors = FALSE)
T2 <- data.frame(id = id, stringsAsFactors = FALSE)
for (j in seq_along(items)) {
  T1[[items[j]]] <- addError(tr$T1[, j], pars$rel[j])
  T2[[items[j]]] <- addError(tr$T2[, j], pars$rel[j])
}
T1$ses <- round(tr$ses, 2)
T1$date <- format(as.Date("2024-09-02") + sample(0:20, n, TRUE))
T2$date <- format(as.Date("2025-09-01") + sample(0:40, n, TRUE))

# Dropout (~20%) is more likely for low-ses, low-organized people: it depends
# only on observed T1 values, so it is missing at random (MAR).
p_drop <- stats::plogis(-1.6 - .5 * T1$ses - .8 * (T1$organized - 3))
kept   <- stats::runif(n) > p_drop
T2 <- T2[kept, ]
rownames(T2) <- NULL

# The experience file lists only the people who took a leadership role.
experience <- data.frame(id = id[tr$leader == 1], leader = 1,
                         stringsAsFactors = FALSE)

## ---- the true decomposition --------------------------------------------------
# Fit the stability model to the error-free true scores in a very large sample
# with no dropout: what an analysis would recover with perfect measurement.
set.seed(1)
big <- genTrue(2e5)
truth <- do.call(rbind, lapply(seq_along(items), function(j) {
  dd <- data.frame(Y1 = big$T1[, j], Y2 = big$T2[, j], X1 = big$leader,
                   C1 = big$ses)
  spec <- stabilityModel(X = "X1", controls = "C1")
  fit  <- lavaan::sem(spec$syntax, data = dd, fixed.x = FALSE)
  ss   <- lavaan::standardizedSolution(fit)
  ex   <- spec$extract
  ex$path <- sub("X1$", "leader", sub("C1$", "ses", ex$path))
  ex$via  <- sub("^X1$", "leader", sub("^C1$", "ses", ex$via))
  data.frame(item = items[j], path = ex$path, type = ex$type,
             est = round(ss$est.std[match(ex$label, ss$label)], 3),
             stringsAsFactors = FALSE)
}))
truth$share <- round(truth$est / truth$est[truth$type == "total"][
  match(truth$item, truth$item[truth$type == "total"])], 3)

reliability <- stats::setNames(pars$rel, items)

stabilitySim <- list(T1 = T1, T2 = T2, experience = experience,
                     reliability = reliability, truth = truth,
                     generating = pars)
