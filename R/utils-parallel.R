# Parallel helpers shared by functions that repeat independent fits
# (lassoLoops(), modelOnAllY() and so stabilityPaths()/crossLagPaths(),
# reliabilitySensitivity()). A user-facing `cores` argument is either a
# number of cores or a cluster from parallel::makeCluster(), which is then
# reused and left open.
#
# Workers are always separate R sessions (a PSOCK cluster), on every
# platform. Forking (mclapply) starts faster on Unix-alikes but is unsafe in
# GUIs such as RStudio and with some multithreaded libraries, so it isn't
# used. Workers load the INSTALLED fancyr, so after editing the package,
# reinstall before testing with cores > 1 (devtools::load_all() alone won't
# reach them). Give the per-task function a minimal environment so workers
# don't have to load fancyr at all; see CLAUDE.md ("Parallel `cores`").
#
# If workers can't be started (e.g. a firewall or IT policy blocks the local
# connection between R sessions), the work runs on one core with a warning.

# Validate a core count, capping it at what the machine has.
checkCores <- function(cores) {
  if (!is.numeric(cores) || length(cores) != 1L || is.na(cores) || cores < 1)
    stop("`cores` must be a whole number of 1 or more, or a cluster from ",
         "parallel::makeCluster().", call. = FALSE)
  cores <- as.integer(cores)
  avail <- parallel::detectCores()
  if (!is.na(avail) && cores > avail) {
    warning("`cores` = ", cores, " is more than this machine has (", avail,
            "); using ", avail, ".", call. = FALSE)
    cores <- avail
  }
  cores
}

# Turn `cores` into something fancyLapply() accepts: 1, or a cluster started
# here (once, so several fancyLapply() calls can share it). Returns
# list(cores = , close = ); call close() when done.
openCores <- function(cores, n_tasks = Inf) {
  none <- function() invisible(NULL)
  if (inherits(cores, "cluster")) return(list(cores = cores, close = none))
  k <- min(checkCores(cores), n_tasks)
  if (k <= 1) return(list(cores = 1L, close = none))
  timeout <- getOption("fancyr.setup_timeout", 30)
  cl <- tryCatch(
    parallel::makePSOCKcluster(k, setup_timeout = timeout),
    error = function(e) {
      warning("Could not start ", k, " parallel R sessions (", conditionMessage(e),
              "), so running on 1 core. On Windows, a firewall or IT policy may ",
              "be blocking the local connection between R sessions; see ",
              "\"Parallel processing\" in ?lassoLoops.", call. = FALSE)
      NULL
    })
  if (is.null(cl)) return(list(cores = 1L, close = none))
  list(cores = cl, close = function() parallel::stopCluster(cl))
}

# lapply() over independent tasks, serially or in parallel. FUN should not
# depend on the random-number stream unless it sets its own seed per task.
fancyLapply <- function(X, FUN, cores = 1) {
  if (inherits(cores, "cluster")) return(parallel::parLapply(cores, X, FUN))
  if (checkCores(cores) <= 1 || length(X) <= 1) return(lapply(X, FUN))
  oc <- openCores(cores, length(X))
  on.exit(oc$close())
  fancyLapply(X, FUN, oc$cores)
}
