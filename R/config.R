#' Set the default targets store and script
#'
#' @param script Character. Name of the script to use as default.
#' @param tars tars list
#'
#' @returns as for `targets::tar_config_set()` called with `store =` and
#' `script =`
#' @export
#'
#' @examples
config <- function(script
                   , tars = yaml::read_yaml("_targets.yaml")
                   ) {

  tars <- yaml::read_yaml("_targets.yaml")

  targets::tar_config_set(store = tars[[script]]$store
                          , script = tars[[script]]$script
                          )

}
