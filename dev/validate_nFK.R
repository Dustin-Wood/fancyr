suppressMessages({ library(devtools); load_all(quiet = TRUE) })

mk <- function(off, dg = 1) {
  m <- matrix(off, 6, 6); diag(m) <- dg; m
}

cases <- list(
  list(label = "#1  identity (diag=1, r=0)",       expected = 6.000,
       m = diag(6)),
  list(label = "#2  ones (diag=1, r=1)",            expected = 1.000,
       m = matrix(1, 6, 6)),
  list(label = "#3  diag=0.7 r=0.7 (SB(6,0.7))",    expected = 0.933,
       m = matrix(0.7, 6, 6)),
  list(label = "#4  diag=0.7 r=0   (6x0.7=4.2)",    expected = 4.200,
       m = mk(0, dg = 0.7)),
  list(label = "#5  diag=1 r=0.2",                  expected = 6.000,
       m = mk(0.2)),
  list(label = "#6  diag=1 r=0.7",                  expected = 6.000,
       m = mk(0.7)),
  list(label = "#7  diag=0.9 r=0.2",                expected = 5.325,
       m = mk(0.2, dg = 0.9)),
  list(label = "#8  diag=0.9 r=0.7",                expected = 4.311,
       m = mk(0.7, dg = 0.9)),
  list(label = "#9  two perfect 3-blocks",          expected = 2.000,
       m = { x <- matrix(0, 6, 6); x[1:3,1:3] <- 1; x[4:6,4:6] <- 1; x }),
  list(label = "#10 two 0.7 3-blocks (2*SB(3,0.7))",expected = 1.750,
       m = { x <- matrix(0, 6, 6); x[1:3,1:3] <- 0.7; x[4:6,4:6] <- 0.7; x }),
  list(label = "#11 rank-5 compound (A=.7B+.3C+.1D)", expected = 5.000,
       m = {
         cv <- matrix(c(.59, .7, .3, .1, 0, 0,
                        .7,  1,  0,  0, 0, 0,
                        .3,  0,  1,  0, 0, 0,
                        .1,  0,  0,  1, 0, 0,
                        0,   0,  0,  0, 1, 0,
                        0,   0,  0,  0, 0, 1),
                      nrow = 6, byrow = TRUE)
         cov2cor(cv)
       })
)

cat(sprintf("%-42s %10s %10s %10s\n", "case", "expected", "computed", "diff"))
cat(strrep("-", 76), "\n")
all_ok <- TRUE
for (cs in cases) {
  res <- suppressWarnings(nFK(cs$m))
  diff <- res$nFK - cs$expected
  ok <- abs(diff) < 5e-3
  all_ok <- all_ok && ok
  cat(sprintf("%-42s %10.3f %10.3f %10.4f%s\n",
              cs$label, cs$expected, res$nFK, diff,
              if (ok) "" else "  <-- FAIL"))
}
cat(strrep("-", 76), "\n")
cat(if (all_ok) "ALL PASS\n" else "FAILURES PRESENT\n")

# Spot-check rotation invariance on case #8.
cat("\nRotation invariance check (case #8, diag=0.9 r=0.7):\n")
m8 <- mk(0.7, dg = 0.9)
res_none <- nFK(m8, rotate = "none")
res_vmx  <- nFK(m8, rotate = "varimax")
cat(sprintf("  rotate='none'    : nFK = %.6f, r2 = [%s]\n",
            res_none$nFK, paste(sprintf("%.3f", res_none$r2), collapse = ", ")))
cat(sprintf("  rotate='varimax' : nFK = %.6f, r2 = [%s]\n",
            res_vmx$nFK,  paste(sprintf("%.3f", res_vmx$r2),  collapse = ", ")))
cat(sprintf("  total invariant? %s   (per-PC redistributed? %s)\n",
            isTRUE(all.equal(res_none$nFK, res_vmx$nFK)),
            !isTRUE(all.equal(sort(res_none$r2), sort(res_vmx$r2)))))
