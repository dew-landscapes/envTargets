#' Set options for a targets workflow
#'
#' Optionally enable logging and metric
#' [collection](https://wlandau.github.io/crew/articles/logging.html#logging-worker-processes).
#' See `envTargets::find_log_errors()` for using the logs.
#'
#' @param script Character. Which `tars` script is being run?
#' @param tars List, usually from `envTargets::make_tars()`
#' @param logs,metrics Logical. [Log worker processes](https://wlandau.github.io/crew/articles/logging.html#logging-worker-processes)?
#' @param controller_local_args Named list of `crew::controller()` arguments
#' @param tar_options_set_args Named list of `crew::controller()` arguments
#'
#' @returns Sets options for a targets workflow
#' @export
#'
#' @examples
env_tar_option_set <- function(script
                               , tars = yaml::read_yaml("_targets.yaml")
                               , logs = TRUE
                               , metrics = FALSE
                               , controller_local_args = list(workers = envFunc::use_cores(absolute_max = yaml::read_yaml("settings/setup.yaml")$max_cores)
                                                              , name = "main_controller"
                                                              , crashes_max = 1L
                                                              )
                               , tar_option_set_args = list(packages = yaml::read_yaml("settings/packages.yaml")[[script]]
                                                            , controller = get(controller_local_args$name)
                                                            )
                               ) {

  this_run_start <- format(Sys.time(), "%Y%m%d_%H%M")

  # options local  ---------
  controller_local_args$options_local <- if(logs) {

    crew::crew_options_local(log_directory = fs::path(tars[[script]]$store
                                                      , "log"
                                                      , this_run_start
                                                      , controller_local_args$name
                                                      )
                             )

  } else crew::crew_options_local()

  # options metrics --------
  controller_local_args$options_metrics <- if(metrics) {

    crew::crew_options_metrics(path = fs::path(tars[[script]]$store
                                               , "metrics"
                                               , this_run_start
                                               , controller_local_args$name
                                               )
                               , seconds_interval = 1
                               )

  } else crew::crew_options_metrics()

  cont <- do.call(crew::crew_controller_local
                  , controller_local_args
                  )

  assign(controller_local_args$name, cont)

  # tar options --------
  do.call(targets::tar_option_set
          , tar_option_set_args
          )

}
