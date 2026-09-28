# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**fancyr** is an R package ("Fancy Statistics for Correlational Analyses") by Dustin Wood. It provides specialized statistical functions for correlational and measurement analysis: reliability-adjusted correlation matrices, scale-centered correlations (Cohen 1969), item clustering, dimensionality assessment, and data screening utilities.

## Common Commands

```r
# Generate documentation from Roxygen2 headers (run after editing @param/@return/@export tags)
devtools::document()

# Install the package locally for testing
devtools::install()

# Load the package without installing (for interactive development)
devtools::load_all()

# Check package for CRAN-standard issues
devtools::check()

# Build a source tarball
devtools::build()
```

There is no testthat infrastructure. Function testing is done interactively.

## Architecture

All functions live in `R/` as individual files (one function per file, named to match). Documentation is Roxygen2 in each file; `NAMESPACE` and `man/*.Rd` are auto-generated — do not edit them directly.

### Functional Groups

**Scale transformations:** `cx()` (scale-center to [-1,1]), `zcx()` (z-scored scale-center), `pomp()` (proportion of maximum possible), `scalebyN()`

**Correlational analyses:** `ccor()` (Cohen/scale-center correlations), `avgLagR()` (reliability-adjusted lag correlations), `avgRep()` (average replication reliability), `longR()` (long-form correlations), `phatd()`

**Clustering & profiles:** `voiClusters()` / `voiClusters2()` (validity-relevant item clusters), `reflectedClusters()` / `reflectedRs()` (clusters with reflected items), `profileAnalysis()` (normative and distinctive profile correlations)

**Dimensionality:** `nFK()` (number of orthogonal factors in a set — active development), `nPCX()` (number of independent PCA dimensions), `qrrSplithalf()` (split-half reliability)

**Stability decomposition (two-wave):** `stabilityData()` (merge waves + experience files) → `stabilityPaths()` (mediated / confounded / residual stability per item, optional latent `reliability` correction; returns a `fancyStability` object with print/summary/plot methods) → `reliabilitySensitivity()`. Engine underneath: `fancyModel()` / `fitModel()` / `modelOnAllY()` / `stabilityModel()`; latent roles are injected in `fitModel()`, so spec syntax never changes. `plotMedX()` is internal (behind `plot(x, item =)`). Checks: `dev/validate_stability.R`, which sources `dev/sim_stability.R` for simulated data with a known decomposition.

**Cross-lagged (two-wave, X measured at both waves):** `crossLagPaths()` (class `fancyCrossLag`; selection `X2 ~ Y1`, change `Y2 ~ X1`, co-change `Y2 ~~ X2` left undecomposed, plus both stability decompositions) on the spec from `crossLagModel()`. Same engine as above; the spec's `extract` has an `outcome` column (`Y`/`X`) so `fitModel()` takes shares per total. `X` is a base name; `reliability` accepts that base name for both waves. Shared display helpers (`decompCells()`, `decompBars()`, `effectsScatter()`, `poolPaths()`) live in `R/stability-methods.R`. Checks: `dev/validate_crosslag.R`.

Data for both: `powerTraits` (real data from Wood & Harms, 2017; built by `data-raw/powerTraits.R` from a cleaned file kept outside the package, whose cleaning script and decisions log live with the raw data in the user's Dropbox `R/workspace/greek study data/`). Vignette: `vignettes/traits-and-power.Rmd`.

The package ships **no simulated data** (the user's rule): examples, vignettes and bundled datasets use real data only. Simulation is fine in `dev/` (build-ignored) for validation.

**Data screening/prep:** `maxMissing()`, `itemOrder()`, `allPairs()`, `invertVarOrder()`, `setDepRDiffs()`, `conScores()`, `expRse()`, `prMaxSD()`, `randomIntModel()`, `nullModellavaan()`

### Key Dependencies

- **psych** — `principal()`, `corr.test()`, `setCor()`, `partial.r()`, `cor.smooth()`
- **lavaan** — `sem()`, `inspect()`
- **Matrix** — `nearPD()` for positive-definite corrections
- **plyr** — `ddply()`

Note: some functions call `library()` internally rather than relying on DESCRIPTION `Imports`. This is a known inconsistency; prefer using `::` namespacing or adding dependencies to DESCRIPTION when editing functions.

### Joining on ID: `merge()` vs `match()`

Both appear in the package. The split is deliberate, not drift:

- **`match()`** when the output must stay aligned to a specific input frame. It preserves that frame's row order and length exactly, and fills `NA` for non-matches. `resChange()` needs this: `$residuals` has one row per row of `T2_data`, in the same order, so it can be `cbind()`-ed straight back.
- **`merge()`** when assembling a new analysis frame where row identity doesn't survive anyway. `stabilityData()` builds a fresh frame (T1∪T2 by default, for FIML) and returns nothing row-aligned to an input; it still uses `match()` for `interval_days`, which must stay aligned to the merged frame, and to restore T1 row order afterwards (users want input order preserved, not sorted by ID).

Two traps worth remembering. `merge()` re-sorts by the `by` column (`sort = TRUE` is the default) and drops non-matching rows unless `all.x = TRUE` — so its result row count tells you nothing on its own. And on a duplicated key it silently produces a Cartesian product rather than erroring, inflating every N.

Because of that, **any function joining on an ID should call `checkUniqueIDs()`** (`R/utils-ids.R`) on each input first. It errors, naming the offending IDs, rather than letting duplicates through. Pass `why =` to describe what duplicates would do to that particular caller, since the consequence differs by join style.

### Parallel `cores`

Functions that repeat independent fits take `cores` (a number, or a cluster to reuse) and run the tasks through `fancyLapply()` in `R/utils-parallel.R`; `openCores()` starts a cluster once when several calls should share it (see `reliabilitySensitivity()`). Two rules:

- **Give the per-task function a minimal environment** (`list2env(..., parent = baseenv())`, with any fancyr helpers it calls copied in and re-environmented, and data trimmed to the needed columns); see `modelOnAllY()` and `lassoLoops()`. Otherwise its enclosing environment is the fancyr namespace, and every Windows worker loads fancyr with all its imports, which cost more than the parallelism saved (lassoLoops: 26 s on 4 cores before, 9 s after). Inside, call other packages with `pkg::`.
- **Random numbers:** draw one seed per task up front and `set.seed()` inside each task, so results don't depend on the number of cores (see `lassoLoops()`).

Workers are always separate R sessions (PSOCK, no forking, on every platform) using the *installed* fancyr: reinstall before testing `cores > 1`. If they can't start within `getOption("fancyr.setup_timeout", 30)` seconds (firewall/IT policy), `openCores()` warns and falls back to 1 core.

### Documentation Pattern

Each function file uses Roxygen2 headers. After any change to `@param`, `@return`, `@export`, `@examples`, or `@importFrom` tags, run `devtools::document()` to regenerate `NAMESPACE` and `man/`.

`itemOrderOLD.R` is a deprecated file kept for reference; do not export or call it.
