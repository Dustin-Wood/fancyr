# fancyr (development)

## Parallel processing

* `lassoLoops()`, `crossLagPaths()`, `stabilityPaths()`, `modelOnAllY()` and
  `reliabilitySensitivity()` gain `cores`: the number of CPU cores to spread
  repetitions or items over (default 1), or a cluster from
  `parallel::makeCluster()` to reuse. Results are identical whatever the
  number of cores. On an 8-core Windows machine, 59 cross-lagged fits took
  191 s on 1 core and 53 s on 4; 100 lasso repetitions, 27 s and 9 s.
  `reliabilitySensitivity()` starts its worker processes once for the whole
  grid. parallel (a base R package) is a new import.
* Parallel work always uses separate background R sessions (no forking), so
  it behaves the same on every platform and is safe inside RStudio. If the
  sessions can't be started within 30 seconds (e.g. a firewall or IT policy
  blocks their local connection), the analysis runs on one core with a
  warning instead of failing. On Windows, first use may bring up a firewall
  prompt; see "Parallel processing" in `?lassoLoops`.

## Repeated cross-validated lasso

* New `lassoLoops()`: repeats a lasso (or elastic-net) regression many
  times, each on a random training share of the sample with the rest held
  out, using `glmnet::cv.glmnet()` (glmnet is a new suggested package).
  Reports holdout validity across repetitions (r; for a binary outcome also
  AUC), each predictor's mean coefficient and selection frequency, and keeps
  every repetition's coefficients. Missing predictor values are filled and
  predictors standardized with training-share statistics only, so holdout
  cases never inform the model.
* `predict()` (or `dvPred()`) scores new data with the coefficients averaged
  across repetitions, matching predictors by name and standardizing with the
  fitted sample's means and SDs, so a new sample or a single person is scored
  on the original metric.

## Cross-lagged models

* New `crossLagPaths()`: for each item, fits a two-wave cross-lagged panel
  model with an experience `X` measured at both waves. It reports selection
  (`X2 ~ Y1`, prospective), change (`Y2 ~ X1`), both stabilities, the Time 1
  association and the Time 2 co-change (residual covariance), and decomposes
  the stability of *both* variables into residual, cross-lagged and
  confounded pathways. Standardized by default (`metric = "raw"` also
  available); any variable, including `X` at either wave and the controls,
  can be given a reliability. Returns a `fancyCrossLag` object with
  `print()`, `summary()` and `plot()` (selection-versus-change scatterplot,
  or decomposition bars with `type = "bars"`).
* New `crossLagModel()` builds the underlying model specification.
* `reliabilitySensitivity()` accepts `crossLagPaths()` results.
* `fitModel()` and `modelOnAllY()` support specs that decompose more than one
  total: an `outcome` column in the spec's `extract` table groups the shares.
* The selection-versus-change scatterplot (for both `crossLagPaths()` and
  `stabilityPaths()` results) now gives both axes the same range, centred on
  zero, with equal scaling (`same_range = TRUE`). It shades significance
  bands on each axis: a darker band where an estimate would be significant
  for no item (below the smallest 1.96 × SE), and a lighter band where it
  would be for some items only (up to the largest), with thin grey lines at
  the band edges and solid black zero lines. `bands =` takes a list of
  fits so several plots can share bands. Point fill now shows which effects
  reached p < .05 (both, selection only, change only, neither).
* The saturated stability and cross-lag models skip lavaan's baseline and
  unrestricted (h1) fits, which only feed fit indices; estimates and standard
  errors are unchanged, and fitting is about 10% faster.
* The traits-and-power vignette (now "Cross-lagged effects: traits and social
  power") estimates Paths A and B with `crossLagPaths()`, drops the
  `stabilityPaths()` analyses (power was measured alongside the traits, not
  between the waves), and adds a section on measurement timing.

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
