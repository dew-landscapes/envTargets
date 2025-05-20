
#' Generate a path to a storage directory
#'
#' @param set_list Named, nested list. Passed to `set_list` argument of
#' `envFunc::name_env_out()`
#' @param base_dir Character. Path to outputs store.
#' @param project Character. Name of the (usually Rstudio) project.
#'
#' @return Full path to a store directory for a project
#' @export
#'
store_dir <- function(set_list
                      , base_dir = fs::path("..", "..", "out")
                      , project = basename(here::here())
                      ) {

  fs::path(base_dir
           , project
           , envFunc::name_env_out(set_list)$path
           )

}
