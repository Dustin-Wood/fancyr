# fancyr (development)

## Stability decomposition overhaul

* New two-step workflow: `stabilityData()` merges Time 1, Time 2 and any extra
  files (e.g. an experience record) into one frame; `stabilityPaths()` then
  decomposes each item's stability into mediated, confounded and residual
  pathways, for one item or many.
* Items (and continuous experiences or controls) can be adjusted for their
  retest reliability: `stabilityPaths(..., reliability = )` models each as a
  latent variable with a known reliability. `reliabilitySensitivity()` shows
  how the results change across assumed reliabilities.
* Results are standardized by default (`metric = "std"`, read from lavaan's
  standardized solution), so the total is the retest correlation, or the
  disattenuated retest correlation when items are latent.
* `stabilityPaths()` returns a `fancyStability` object with `print()`,
  `summary()` and `plot()` methods. `plot(x)` compares items; `plot(x, item = )`
  draws the path diagram.
* `stabilityData()` keeps people missing at Time 2 by default
  (`join = "full"`), so full-information maximum likelihood can use them.
* `stabilityData()` preserves input order (rows follow `T1`, columns follow
  `T1` with `[T2]` items appended) instead of sorting by ID. Columns found in
  only one wave are dropped by default, with a message; `keep = "T1"` keeps
  the Time 1 ones (e.g. baseline controls) and `keep = "all"` keeps both.
  The shared items are given by `commonItems` (formerly `items`) and returned
  as `attr(, "commonItems")`, which `stabilityPaths()` reads by default.
* New simulated dataset `stabilitySim`, with the true decomposition, and a
  vignette: `vignette("stability-decomposition")`.
* `fitModel()` and `modelOnAllY()` gain `reliability` and `metric`; they lose
  `standardize`. Output renamed: `propTotal` is now `share`, `totalStability`
  is now `total`.

### Removed

* `xEffects()`: use `stabilityData()` followed by `stabilityPaths()`. The
  `NA_to_0` argument is replaced by `stabilityData(fill = list(var = 0))`;
  retest correlations are in `stabilityPaths()$summary$r_obs`.
* `allYstabilities()` and `medXonAllY()`: use `stabilityPaths()`, which now
  handles any number of items.
* `plotMedX()`: use `plot(stabilityPaths(...), item = "name")`.
* `inCommon()`: use `intersect(names(T1), names(T2))`, or let
  `stabilityData()` find the common items (`attr(, "commonItems")`).
