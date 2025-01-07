library(targets)
library(crew)

tar_option_set(controller = crew_controller_local(workers = 3))

list(
  tar_target(wait1, Sys.sleep(1)),
  tar_target(wait2, Sys.sleep(2)),
  tar_target(wait3, Sys.sleep(3)),
  tar_target(wait4, Sys.sleep(4))
)
