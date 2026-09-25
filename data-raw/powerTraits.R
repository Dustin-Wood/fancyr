# Builds data/powerTraits.rda: two waves of self-rated trait adjectives and
# peer-rated social power in seven fraternities and sororities (Wood & Harms,
# 2017, PSPB).
#
# Source: the cleaned study data, produced by `clean greek data.R` (kept with
# the raw data outside the package, together with `greek data decisions log.md`,
# which records every cleaning decision). That script handles
# deidentification, screening, rescoring, and building the power composite.
# This one only selects and reshapes.
#
# Run from the package root:
#   source("data-raw/powerTraits.R")

src <- "C:/Users/dusti/Dropbox/R/workspace/greek study data/greek data y1 & y2 cleaned.Rdata"
e <- new.env()
load(src, envir = e)

## The 59 adjectives rated at every wave in both instruction sets
items <- e$colmap$qname[e$colmap$file == "W2B" & e$colmap$section == "role_traits"]
stopifnot(length(items) == 59,
          all(items %in% names(e$W1A)), all(items %in% names(e$W1B)),
          all(items %in% names(e$W2A)), all(items %in% names(e$W2B)))

## Item properties: agency and communion, mean of the 8 retained judges
acr <- e$agencyCommunionRatings[match(items, e$agencyCommunionRatings$item), ]
itemInfo <- data.frame(item = items, agency = round(acr$agency, 3),
                       communion = round(acr$communion, 3), stringsAsFactors = FALSE)

## People: drop anyone flagged by a data-quality screen (7 people)
flagged <- e$dataQuality$idnum[e$dataQuality$anyFlag]

nScreen <- tapply(e$colmap$screen, e$colmap$file, sum)
answered <- function(f) {
  n <- e$dataQuality[[paste0(f, "_nScreen")]]
  e$dataQuality$idnum[!is.na(n) & n >= nScreen[f] / 2]
}

profile <- function(X, prop) round(apply(as.matrix(X), 1, function(x) {
  if (sum(!is.na(x)) < 10 || stats::sd(x, na.rm = TRUE) == 0) return(NA_real_)
  stats::cor(x, prop, use = "pair")
}), 3)

## Participants: anyone who answered a self-report form at either wave.
respondents <- sort(setdiff(Reduce(union, lapply(c("W1A", "W1B", "W2A", "W2B"), answered)), flagged))

## A wave's rows cover participants who answered that wave's survey OR were
## rated for power at that wave. Power comes from other members' ratings, so
## it exists for people who skipped a survey (e.g. Time 1 participants who did
## not return the Time 2 survey but were still members); Path A needs them.
wave <- function(w) {
  A <- as.data.frame(e[[paste0("W", w, "A")]]); B <- as.data.frame(e[[paste0("W", w, "B")]])
  rated <- e$peerComposites$idnum[!is.na(e$peerComposites[[paste0("W", w, "_influence")]])]
  selfW <- union(answered(paste0("W", w, "A")), answered(paste0("W", w, "B")))
  ids <- sort(intersect(respondents, union(selfW, rated)))
  gen  <- A[match(ids, A$idnum), items]
  role <- B[match(ids, B$idnum), items]
  gen[!ids %in% answered(paste0("W", w, "A")), ]  <- NA
  role[!ids %in% answered(paste0("W", w, "B")), ] <- NA
  names(role) <- paste0(items, "_role")
  pc <- e$peerComposites[match(ids, e$peerComposites$idnum), ]
  dm <- as.data.frame(e[[paste0("W", w, "demos")]])
  srv <- dm$date[match(ids, dm$idnum)]
  srv[!ids %in% selfW] <- NA          # rated only: no survey date
  out <- data.frame(
    id    = ids,
    date  = srv,
    power = round(pc[[paste0("W", w, "_influence")]], 3),
    power_n = pc[[paste0("W", w, "_influence_n")]],
    communion      = profile(gen,  itemInfo$communion),
    agency         = profile(gen,  itemInfo$agency),
    communion_role = profile(role, itemInfo$communion),
    agency_role    = profile(role, itemInfo$agency),
    gen, role, check.names = FALSE, row.names = NULL)
  out
}
T1 <- wave(1)
T2 <- wave(2)

## Tenure (years in the organization at each person's first survey)
tenureAt <- function(dm) {
  start <- as.Date(ifelse(dm$jointerm %in% "S", paste0(dm$joinyear, "-01-15"),
                          paste0(dm$joinyear, "-09-01")))
  d <- dm$date
  fillDate <- stats::ave(as.numeric(dm$date), dm$orgID, FUN = function(x) stats::median(x, na.rm = TRUE))
  d[is.na(d)] <- as.Date(fillDate[is.na(d)], origin = "1970-01-01")
  round(as.numeric(d - start) / 365.25, 2)
}
W1d <- as.data.frame(e$W1demos); W2d <- as.data.frame(e$W2demos)
W1d$tenure <- tenureAt(W1d); W2d$tenure <- tenureAt(W2d)

ids <- respondents
stopifnot(setequal(union(T1$id, T2$id), ids))
people <- data.frame(
  id = ids,
  org = e$dataQuality$orgID[match(ids, e$dataQuality$idnum)],
  tenure = ifelse(ids %in% W1d$idnum, W1d$tenure[match(ids, W1d$idnum)],
                  W2d$tenure[match(ids, W2d$idnum)]),
  stringsAsFactors = FALSE)
people$tenure[!is.na(people$tenure) & people$tenure < 0] <- NA
people$linkageIssue <- people$org %in% e$analysisExclusions$orgID
stopifnot(!anyNA(people$org), !anyDuplicated(people$id))

powerTraits <- list(T1 = T1, T2 = T2, people = people, items = itemInfo)

usethis::use_data(powerTraits, overwrite = TRUE, compress = "xz")
