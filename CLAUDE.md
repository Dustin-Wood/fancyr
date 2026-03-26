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

**Data screening/prep:** `maxMissing()`, `itemOrder()`, `allPairs()`, `invertVarOrder()`, `setDepRDiffs()`, `conScores()`, `expRse()`, `prMaxSD()`, `randomIntModel()`, `nullModellavaan()`

### Key Dependencies

- **psych** — `principal()`, `corr.test()`, `setCor()`, `partial.r()`, `cor.smooth()`
- **lavaan** — `sem()`, `inspect()`
- **Matrix** — `nearPD()` for positive-definite corrections
- **plyr** — `ddply()`

Note: some functions call `library()` internally rather than relying on DESCRIPTION `Imports`. This is a known inconsistency; prefer using `::` namespacing or adding dependencies to DESCRIPTION when editing functions.

### Documentation Pattern

Each function file uses Roxygen2 headers. After any change to `@param`, `@return`, `@export`, `@examples`, or `@importFrom` tags, run `devtools::document()` to regenerate `NAMESPACE` and `man/`.

`itemOrderOLD.R` is a deprecated file kept for reference; do not export or call it.
