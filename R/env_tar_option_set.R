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
#' @param add_x_task_controller Numeric. If >0 a secondary controller with
#' the same arguments as `controller_local_args` will be established with the
#' exception that `tasks_max = x` is specified. Particularly useful for a target
#' that will run many terra tasks that can fill temporary space if the workers
#' remain active for the life of the script. To use the x task controller,
#' specify
#' `resources = tar_resources(crew = tar_resources_crew(controller = "controller_x_task")`
#' (where x is literally 'x', not the numeric value you assigned) within the
#' arguments for any target.
#'
#' @returns invisible(NULL). Sets options for a targets workflow
#' @export
#'
#' @examples
env_tar_option_set <- function(script
                               , tars = yaml::read_yaml("_targets.yaml")
                               , logs = TRUE
                               , metrics = FALSE
                               , controller_local_args = list(workers = envFunc::use_cores(absolute_max = yaml::read_yaml("settings/cores.yaml")$max_cores)
                                                              , name = "controller_main"
                                                              )
                               , tar_option_set_args = list(packages = yaml::read_yaml("settings/packages.yaml")[[script]]
                                                            , controller = get(controller_local_args$name)
                                                            )
                               , add_x_task_controller = 0
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

  cont_main <- do.call(crew::crew_controller_local
                       , controller_local_args
                       )

  assign(controller_local_args$name, cont_main)

  if(add_x_task_controller > 0) {

    controller_x_args <- controller_local_args
    controller_x_args$tasks_max <- add_x_task_controller
    controller_x_args$name <- "controller_x_task"

    cont_x <- do.call(crew::crew_controller_local
                      , controller_x_args
                      )

    assign(controller_x_args$name, cont_x)

    tar_option_set_args$controller = crew::crew_controller_group(cont_main, cont_x)

  }

  # tar options --------
  do.call(targets::tar_option_set
          , tar_option_set_args
          )

  return(invisible(NULL))

}
