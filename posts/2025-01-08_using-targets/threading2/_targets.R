
library(targets)
library(crew)

tar_option_set(controller = crew_controller_local(workers = 3))

sleeper <- function(duration, pipeline_start, name) {
  sleep_start <- Sys.time()
  Sys.sleep(duration)
  sleep_stop <- Sys.time()
  tibble::tibble(
    name           = name,
    pipeline_start = pipeline_start,
    worker_pid     = Sys.getpid(),
    begins_at      = difftime(sleep_start, pipeline_start),
    finishes_at    = difftime(sleep_stop, pipeline_start)
  )
}

startup <- function() {
  tibble::tibble(
    name = "start",
    pipeline_start = Sys.time(),
    worker_pid     = Sys.getpid(),
    begins_at      = as.difftime(0, units = "secs"),
    finishes_at    = difftime(Sys.time(), pipeline_start)
  )
}

collate <- function(...) {
  start <- Sys.time()
  na_difftime <- as.difftime(NA_real_, units = "secs")
  out <- rbind(...)
  pipeline_start <- out$pipeline_start[1]
  out$pipeline_start <- NULL
  out <- rbind(
    out,
    tibble::tibble(
      name         = "trace",
      worker_pid   = Sys.getpid(),
      begins_at    = difftime(start, pipeline_start),
      finishes_at  = difftime(Sys.time(), pipeline_start)
    )
  )
  out$duration    <- out$finishes_at - out$begins_at
  out$begins_at   <- round(as.numeric(out$begins_at), digits = 3)
  out$finishes_at <- round(as.numeric(out$finishes_at), digits = 3)
  out$duration    <- round(as.numeric(out$duration), digits = 3)
  out
}

list(
  tar_target(start, startup(), cue = tar_cue("always")),
  tar_target(wait1, sleeper(1, start$pipeline_start, "wait1")),
  tar_target(wait2, sleeper(2, start$pipeline_start, "wait2")),
  tar_target(wait3, sleeper(3, start$pipeline_start, "wait3")),
  tar_target(wait4, sleeper(4, start$pipeline_start, "wait4")),
  tar_target(trace, collate(start, wait1, wait2, wait3, wait4))
)
