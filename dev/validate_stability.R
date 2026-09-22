# Validation checks for the stability-decomposition family:
# stabilityData(), stabilityPaths(), fitModel() latent correction, methods,
# reliabilitySensitivity(). Run from the package root:
#   Rscript dev/validate_stability.R
suppressMessages(devtools::load_all(quiet = TRUE))

pass <- 0; fail <- 0
check <- function(desc, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1; cat("  PASS ", desc, "\n") }
  else            { fail <<- fail + 1; cat("  FAIL ", desc, "\n") }
}

d <- stabilityData(stabilitySim$T1, stabilitySim$T2, stabilitySim$experience,
                   keep = "T1", fill = list(leader = 0), date = "date")
rel <- stabilitySim$reliability

cat("\n1. Components sum exactly to total\n")
sums_ok <- function(sp) {
  p <- sp$paths
  all(vapply(unique(p$item), function(it) {
    q <- p[p$item == it, ]
    abs(sum(q$est[q$type != "total"]) - q$est[q$type == "total"]) < 1e-8
  }, logical(1)))
}
for (m in c("std", "raw")) for (Xs in list(NULL, "leader", c("leader", "ses2")))
  for (Cs in list(NULL, "ses", c("ses", "age"))) {
    dd <- d; set.seed(3); dd$ses2 <- dd$ses + rnorm(nrow(dd)); dd$age <- rnorm(nrow(dd))
    Xs2 <- setdiff(Xs, Cs); if (identical(Xs2, character(0))) Xs2 <- NULL
    for (r in list(NULL, rel)) {
      sp <- stabilityPaths(dd, X = Xs2, controls = Cs, reliability = r, metric = m)
      check(sprintf("metric=%s X=%s C=%s latent=%s", m,
                    paste(Xs2, collapse = "+"), paste(Cs, collapse = "+"),
                    !is.null(r)), sums_ok(sp))
    }
  }

cat("\n2. reliability = 1 reproduces the observed model\n")
a <- stabilityPaths(d, X = "leader", controls = "ses")
b <- stabilityPaths(d, X = "leader", controls = "ses", reliability = 1)
check("identical estimates", isTRUE(all.equal(a$paths$est, b$paths$est)))

cat("\n3. Standardized totals: observed ~ retest r; latent ~ r / rel\n")
dI <- stabilityData(stabilitySim$T1, stabilitySim$T2, join = "inner")
o  <- stabilityPaths(dI)
L  <- stabilityPaths(dI, reliability = rel)
tot_o <- o$paths$est[o$paths$type == "total"]
tot_L <- L$paths$est[L$paths$type == "total"]
check("observed total == r_obs (complete data)",
      max(abs(tot_o - o$summary$r_obs)) < 1e-3)
check("latent total == r_obs / rel (same rel both waves)",
      max(abs(tot_L - o$summary$r_obs / rel[o$summary$item])) < 1e-3)

cat("\n4. Recovery of the true decomposition\n")
tr <- stabilitySim$truth
sp_o <- stabilityPaths(d, X = "leader", controls = "ses")
sp_L <- stabilityPaths(d, X = "leader", controls = "ses", reliability = rel)
key <- function(p) paste(p$item, p$path)
tru <- tr$est[match(key(sp_L$paths), key(tr))]
err_L <- abs(sp_L$paths$est - tru)
err_o <- abs(sp_o$paths$est - tru)
cat(sprintf("    mean |error|: observed %.3f, latent %.3f\n", mean(err_o), mean(err_L)))
check("latent estimates closer to truth than observed", mean(err_L) < mean(err_o))
inCI <- tru >= sp_L$paths$ci.lower & tru <= sp_L$paths$ci.upper
cat(sprintf("    latent 95%% CIs covering truth: %d of %d\n", sum(inCI), length(inCI)))
check("most latent CIs cover the truth", mean(inCI) >= .85)
# Large-N check: at n = 20000, latent estimates within .03 of truth
# Borrow only the generator definitions from data-raw (running the whole script
# would rebuild and re-save the bundled .rda).
source_env <- new.env()
for (e in parse("data-raw/stabilitySim.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c("items", "pars", "genTrue", "addError"))
    eval(e, source_env)
set.seed(99)
big <- source_env$genTrue(20000)
bigT1 <- data.frame(id = seq_len(20000)); bigT2 <- bigT1
for (j in seq_along(source_env$items)) {
  it <- source_env$items[j]
  bigT1[[it]] <- source_env$addError(big$T1[, j], rel[[it]])
  bigT2[[it]] <- source_env$addError(big$T2[, j], rel[[it]])
}
bigT1$ses <- big$ses
bigX <- data.frame(id = seq_len(20000), leader = big$leader)
db <- stabilityData(bigT1, bigT2, bigX, keep = "T1")
bL <- stabilityPaths(db, X = "leader", controls = "ses", reliability = rel)
bO <- stabilityPaths(db, X = "leader", controls = "ses")
tb <- tr$est[match(key(bL$paths), key(tr))]
cat(sprintf("    large-N max |error|: latent %.3f, observed %.3f\n",
            max(abs(bL$paths$est - tb)), max(abs(bO$paths$est - tb))))
check("large-N latent within .03 of truth", max(abs(bL$paths$est - tb)) < .03)
dom <- function(s) s$paths$share[s$paths$item == "dominant" & s$paths$type == "mediated"]
cat(sprintf("    dominant mediated share: truth %.3f, observed %.3f, latent %.3f\n",
            tr$share[tr$item == "dominant" & tr$type == "mediated"], dom(bO), dom(bL)))
check("observed model inflates the mediated share", dom(bO) > dom(bL))

cat("\n5. Too-low reliability is flagged, not an error\n")
bad <- stabilityPaths(d, X = "leader", reliability = .4)
check("admissible = FALSE for some items", any(!bad$summary$admissible))
check("estimates still returned", all(!is.na(bad$paths$est)))

cat("\n6. stabilityData behaviour\n")
T1dup <- rbind(stabilitySim$T1, stabilitySim$T1[1, ])
check("duplicate IDs error",
      inherits(try(stabilityData(T1dup, stabilitySim$T2), silent = TRUE), "try-error"))
check("full join keeps T1-only people", nrow(d) == nrow(stabilitySim$T1))
check("inner join keeps only both-wave people", nrow(dI) == nrow(stabilitySim$T2))
check("fill codes non-leaders 0", all(d$leader %in% c(0, 1)) &&
        sum(d$leader) == nrow(stabilitySim$experience))
check("interval_days computed", all(d$interval_days[!is.na(d[["dominant[T2]"]])] > 300))
check("commonItems attribute set", identical(attr(d, "commonItems"), names(rel)))

# order preserved, not sorted; one-wave columns governed by `keep`
oT1 <- data.frame(id = c(3, 1, 2), zeta = 1:3, age = 4:6, beta = c(2, 5, 1),
                  note = "a")
oT2 <- data.frame(id = c(9, 2, 3), beta = 3:1, onlyT2 = 1:3, zeta = c(1, 3, 2),
                  note = "b")
oc <- suppressMessages(stabilityData(oT1, oT2))
check("rows follow T1, then T2-only people", identical(oc$id, c(3, 1, 2, 9)))
check("keep = 'common' keeps only id + common items",
      identical(names(oc), c("id", "zeta[T1]", "beta[T1]", "zeta[T2]", "beta[T2]")))
check("dropped attribute lists one-wave columns",
      setequal(attr(oc, "dropped"), c("age", "note", "onlyT2")))
check("dropping is announced",
      inherits(tryCatch(stabilityData(oT1, oT2), message = identity), "message"))
check("merged values stay with their IDs",
      identical(oc[["beta[T2]"]], c(1L, NA, 2L, 3L)) &&
        identical(oc[["zeta[T1]"]], c(1L, 2L, 3L, NA)))
o1 <- suppressMessages(stabilityData(oT1, oT2, keep = "T1"))
check("keep = 'T1' keeps T1 layout in place",
      identical(names(o1), c("id", "zeta[T1]", "age", "beta[T1]", "note",
                             "zeta[T2]", "beta[T2]")))
oa <- stabilityData(oT1, oT2, keep = "all", join = "inner")
check("keep = 'all' suffixes a shared non-item column",
      all(c("note[T1]", "note[T2]", "onlyT2") %in% names(oa)) &&
        identical(oa$id, c(3, 2)))
dC <- suppressMessages(stabilityData(stabilitySim$T1, stabilitySim$T2,
                                     stabilitySim$experience, fill = list(leader = 0)))
e <- tryCatch(stabilityPaths(dC, X = "leader", controls = "ses"), error = identity)
check("dropped control gives a keep = hint",
      inherits(e, "error") && grepl("keep = \"T1\"", conditionMessage(e)))

cat("\n7. Reliability argument forms\n")
rdf <- data.frame(item = c(names(rel), "ses"), T1 = c(rel, .9), T2 = c(rel, NA))
f1 <- stabilityPaths(d, X = "leader", controls = "ses", reliability = rdf)
check("data frame form incl. control reliability",
      all(f1$fits$dominant$varmap$reliability[f1$fits$dominant$varmap$original == "ses"] == .9))
f2 <- stabilityPaths(d, X = "leader", reliability = c(dominant = .8))
check("partial named vector leaves others observed",
      is.na(f2$summary$rel_T1[f2$summary$item == "warm"]))
check("unknown name errors",
      inherits(try(stabilityPaths(d, reliability = c(nope = .8)), silent = TRUE), "try-error"))

cat("\n8. Methods run\n")
pdf(NULL)
check("print",   !inherits(try(capture.output(print(sp_L)), silent = TRUE), "try-error"))
check("summary", !inherits(try(capture.output(print(summary(sp_L))), silent = TRUE), "try-error"))
check("plot bars", !inherits(try(plot(sp_L), silent = TRUE), "try-error"))
check("plot share", !inherits(try(plot(sp_L, what = "share", sort = TRUE), silent = TRUE), "try-error"))
check("plot diagram", !inherits(try(plot(sp_L, item = "dominant"), silent = TRUE), "try-error"))
sens <- reliabilitySensitivity(sp_o, rel = c(.6, .8, 1))
check("sensitivity at rel=1 equals observed",
      isTRUE(all.equal(sens$est[sens$reliability == 1], sp_o$paths$est)))
check("plot sensitivity", !inherits(try(plot(sens), silent = TRUE), "try-error"))
dev.off()

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
