#' Set options for a targets workflow
#' 
#' Also enables logging and metric
#' [collection](https://wlandau.github.io/crew/articles/logging.html#logging-worker-processes).
#' See `envTargets::find_log_errors()` for using the logs.
#'
#' @param script Character. Which `tars` script is being run?
#' @param tars List, usually from `envTargets::make_tars()`
#' @param use_cores How many cores to use for [distributed computing](https://books.ropensci.org/targets/performance.html#worker-storage)
#' @param pacs Character. Packages required for `script`
#'
#' @returns Sets options for a targets workflow
#' @export
#'
#' @examples
env_tar_option_set <- function(script
                               , tars = yaml::read_yaml("_targets.yaml") 
                               , use_cores = envFunc::use_cores(absolute_max = yaml::read_yaml("settings/setup.yaml")$max_cores)
                               , pacs = yaml::read_yaml("settings/packages.yaml")[[script]]
                               ) {
  
  # controllers  ---------
  this_run_start <- format(Sys.time(), "%Y%m%d_%H%M")
  
  main_controller <- crew::crew_controller_local(workers = use_cores
                                                 , options_local = crew::crew_options_local(log_directory = fs::path(tars[[script]]$store
                                                                                                                     , "log"
                                                                                                                     , this_run_start
                                                                                                                     , "main_controller"
                                                                                                                     )
                                                                                            )
                                                 , options_metrics = crew::crew_options_metrics(path = fs::path(tars[[script]]$store
                                                                                                                , "metrics"
                                                                                                                , this_run_start
                                                                                                                , "main_controller"
                                                                                                                )
                                                                                                , seconds_interval = 1
                                                                                                )
                                                 , name = "main_controller"
                                                 )
  
  # tar options --------
  targets::tar_option_set(packages = pacs
                          , controller = main_controller
                          )
  
}
