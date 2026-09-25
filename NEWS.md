# fancyr (development)

## Real data: traits and social power

* New dataset `powerTraits`: two waves of general- and role-identity ratings
  on 59 trait adjectives, plus peer-rated social power and survey dates, from
  seven fraternities and sororities (Wood & Harms, 2017).
* New vignette on these data: `vignette("traits-and-power")`.
* `print()` and `plot()` for `stabilityPaths()` results gain `pool`, which
  shows the pathways of a set of controls (e.g. dummy codes) as one column.
  This affects the display only.
* `reliabilitySensitivity()` now also records the structural effects at each
  modeled reliability: selection (`X ~ Y1`), change (`Y2 ~ X`), residual
  stability and control effects, each with standard errors, p-values and
  confidence intervals. It returns a list (`$effects`, `$paths`). `print()`
  tabulates the selection and change effects, and `plot()` draws them with
  confidence intervals by default. The decomposition is still available with
  `what = "est"` or `"share"`, which leave out control pathways unless
  `show_controls = TRUE`.
* `plot(x, type = "effects")` for `stabilityPaths()` results draws each item's
  selection effect against its change effect, labelled by item (with
  non-overlapping labels when ggrepel is installed).
* The bar chart, effects scatterplot and sensitivity plots are now drawn with
  ggplot2 and return ggplot objects, so they can be modified with `+`.
  ggplot2 is a new import; ggrepel and ragg are suggested.
* Examples use `powerTraits`; the package ships no simulated data.

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
