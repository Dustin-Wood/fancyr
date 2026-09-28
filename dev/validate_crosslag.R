# Validation checks for crossLagModel() / crossLagPaths() and their methods.
# Simulates a two-wave cross-lagged panel with known selection, change,
# stabilities and residual co-change, then measures each variable with a
# known reliability. Run from the package root:
#   Rscript dev/validate_crosslag.R

suppressMessages(devtools::load_all(quiet = TRUE))
grDevices::pdf(NULL)   # plots are only checked for errors; no Rplots.pdf

fails <- 0
check <- function(desc, ok) {
  cat(if (isTRUE(ok)) "PASS" else { fails <<- fails + 1; "FAIL" }, desc, "\n")
}

## ---- generator ----------------------------------------------------------------
# Latent scores; all T1 variables and C have variance 1.
genCLPM <- function(n, sel = .15, chg = .25, sYY = .5, sXX = .6, bYC = .2,
                    bXC = -.1, rYX = .4, rYC = .3, rXC = .2, rcochg = .3) {
  S <- matrix(c(1, rYX, rYC, rYX, 1, rXC, rYC, rXC, 1), 3)
  T1 <- matrix(stats::rnorm(n * 3), n) %*% chol(S)
  Y1 <- T1[, 1]; X1 <- T1[, 2]; C <- T1[, 3]
  e <- matrix(stats::rnorm(n * 2), n) %*% chol(matrix(c(1, rcochg, rcochg, 1), 2))
  Y2 <- sYY * Y1 + chg * X1 + bYC * C + .7 * e[, 1]
  X2 <- sXX * X1 + sel * Y1 + bXC * C + .7 * e[, 2]
  data.frame(`y[T1]` = Y1, `y[T2]` = Y2, `x[T1]` = X1, `x[T2]` = X2, C = C,
             check.names = FALSE)
}
noisy <- function(v, rel) v + stats::rnorm(length(v), sd = sqrt(stats::var(v) * (1 - rel) / rel))

set.seed(2026)
tru <- genCLPM(1e5)
relY <- .7; relX <- .9
obs <- tru
obs[c("y[T1]", "y[T2]")] <- lapply(tru[c("y[T1]", "y[T2]")], noisy, rel = relY)
obs[c("x[T1]", "x[T2]")] <- lapply(tru[c("x[T1]", "x[T2]")], noisy, rel = relX)

key <- function(x) {
  e <- x$effects
  stats::setNames(e$est, paste(e$effect, e$path))
}

## ---- recovery -------------------------------------------------------------------
fitT <- crossLagPaths(tru, items = "y", X = "x", controls = "C", metric = "raw")
kT <- key(fitT)
check("raw selection recovered (error-free)", abs(kT[["selection x[T2] ~ Y1"]] - .15) < .01)
check("raw change recovered (error-free)", abs(kT[["change Y2 ~ x[T1]"]] - .25) < .01)
check("raw stabilities recovered (error-free)",
      abs(kT[["stability Y2 ~ Y1"]] - .5) < .01 &&
      abs(kT[["stability x[T2] ~ x[T1]"]] - .6) < .01)

truthStd <- key(crossLagPaths(tru, items = "y", X = "x", controls = "C"))
fitO <- crossLagPaths(obs, items = "y", X = "x", controls = "C")
fitL <- crossLagPaths(obs, items = "y", X = "x", controls = "C",
                      reliability = c(y = relY, x = relX))
errO <- abs(key(fitO) - truthStd); errL <- abs(key(fitL) - truthStd)
check("latent fit recovers error-free std estimates (within .02)", max(errL) < .02)
# (error in both Y1 and X1 can bias a cross-lag either way, so no direction
# is asserted here)
check("observed fit is biased (some effect off by > .05)", max(errO) > .05)

## ---- additivity -------------------------------------------------------------------
for (m in c("std", "raw")) for (lat in c(FALSE, TRUE)) {
  f <- crossLagPaths(obs[1:3000, ], items = "y", X = "x", controls = "C", metric = m,
                     reliability = if (lat) c(y = relY, x = relX))
  p <- f$paths
  ok <- all(vapply(c("item", "x"), function(o) {
    q <- p[p$outcome == o, ]
    isTRUE(all.equal(sum(q$est[q$type != "total"]), q$est[q$type == "total"],
                     tolerance = 1e-8))
  }, logical(1)))
  check(sprintf("both decompositions add up (metric=%s, latent=%s)", m, lat), ok)
  check(sprintf("shares of each total sum to 1 (metric=%s, latent=%s)", m, lat),
        all(abs(tapply(p$share[p$type != "total"], p$outcome[p$type != "total"], sum) - 1) < 1e-8))
}
fObs <- crossLagPaths(obs[1:3000, ], items = "y", X = "x", controls = "C")
tot <- fObs$paths$est[fObs$paths$type == "total"]
check("std totals equal observed retest correlations",
      isTRUE(all.equal(tot, c(cor(obs[1:3000, "y[T1]"], obs[1:3000, "y[T2]"]),
                              cor(obs[1:3000, "x[T1]"], obs[1:3000, "x[T2]"])),
                       tolerance = 1e-6)))

## ---- equivalence with stabilityPaths -------------------------------------------------
small <- obs[1:3000, ]
small[sample(3000, 400), "y[T2]"] <- NA
cc <- small[stats::complete.cases(small), ]
cl <- crossLagPaths(small, items = "y", X = "x", controls = "C", missing = "listwise")
spY <- stabilityPaths(cc, items = "y", controls = c("x[T1]", "C"), missing = "listwise")
spX <- stabilityPaths(cc, items = "x", controls = c("y[T1]", "C"), missing = "listwise")
check("item decomposition == stabilityPaths(items = Y, controls = c(X1, C))",
      isTRUE(all.equal(cl$paths$est[cl$paths$outcome == "item"], spY$paths$est,
                       tolerance = 1e-6)))
check("X decomposition == stabilityPaths(items = X, controls = c(Y1, C))",
      isTRUE(all.equal(cl$paths$est[cl$paths$outcome == "x"], spX$paths$est,
                       tolerance = 1e-6)))

## ---- reliability forms ----------------------------------------------------------------
vm <- function(f) { v <- f$fits$y$varmap; stats::setNames(v$reliability, v$internal) }
r1 <- vm(crossLagPaths(small, items = "y", X = "x", reliability = c(y = .7, x = .9)))
check("X base name sets both waves", identical(unname(r1[c("X1", "X2")]), c(.9, .9)))
r2 <- vm(crossLagPaths(small, items = "y", X = "x",
                       reliability = data.frame(item = c("y", "x"), T1 = c(.7, .85),
                                                T2 = c(.75, .95))))
check("data frame row for X sets each wave",
      identical(unname(r2[c("Y1", "Y2", "X1", "X2")]), c(.7, .75, .85, .95)))
r3 <- vm(crossLagPaths(small, items = "y", X = "x", controls = "C",
                       reliability = c("x[T2]" = .9, C = .8)))
check("column name sets one wave of X; controls by name",
      identical(unname(r3[c("X1", "X2", "C1")]), c(NA, .9, .8)))
r4 <- vm(crossLagPaths(small, items = "y", X = "x", reliability = .7))
check("single number applies to items only",
      identical(unname(r4[c("Y1", "Y2", "X1", "X2")]), c(.7, .7, NA, NA)))
check("unknown reliability name errors",
      inherits(try(crossLagPaths(small, items = "y", X = "x", reliability = c(z = .7)),
                   silent = TRUE), "try-error"))

## ---- argument checks ------------------------------------------------------------------
check("X is dropped from items",
      identical(crossLagPaths(small, items = c("y", "x"), X = "x")$settings$items, "y"))
check("missing X column errors",
      inherits(try(crossLagPaths(small, items = "y", X = "w"), silent = TRUE), "try-error"))
check("X in controls errors",
      inherits(try(crossLagPaths(small, items = "y", X = "x", controls = "x[T1]"),
                   silent = TRUE), "try-error"))

## ---- methods and sensitivity ------------------------------------------------------------
small[["y2[T1]"]] <- small[["y[T1]"]] + stats::rnorm(3000)
small[["y2[T2]"]] <- small[["y[T2]"]] + stats::rnorm(3000)
f2 <- crossLagPaths(small, items = c("y", "y2"), X = "x", controls = "C",
                    reliability = c(x = .9))
ok <- function(expr) !inherits(try(expr, silent = TRUE), "try-error")
check("print", ok(capture.output(print(f2))))
check("summary", ok(capture.output(print(summary(f2)))))
check("plot effects", ok(print(plot(f2))))
check("plot bars", ok(print(plot(f2, type = "bars"))))
check("plot bars share, sorted", ok(print(plot(f2, type = "bars", what = "share", sort = TRUE))))
sens <- reliabilitySensitivity(f2, rel = c(.8, 1))
e1 <- sens$effects[sens$effects$reliability == 1, ]
# f2 has observed items and latent X, so this also checks X's reliability is kept
check("sensitivity at rel=1 equals the original fit",
      isTRUE(all.equal(e1$est, f2$effects$est, tolerance = 1e-6)))
check("co-change has one path label across the grid",
      length(unique(sens$effects$path[sens$effects$effect == "co-change"])) == 1)
check("print sensitivity", ok(capture.output(print(sens))))
check("plot sensitivity effects",
      ok(print(plot(sens, effects = c("selection", "change", "co-change")))))
check("plot sensitivity est", ok(print(plot(sens, what = "est", show_controls = TRUE))))

cat("\n", if (fails) paste(fails, "check(s) FAILED") else "all checks passed", "\n")
