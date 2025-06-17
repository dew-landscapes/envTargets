#' Write tars as yaml
#' 
#' Writes a `tars` object to `here::here("_targets.yaml")` (i.e. the script
#' directory) and `fs::path(dirname(tars[[1]]$store), "_targets.yaml")` (i.e.
#' the store directory).
#'
#' @param tars Tars list
#'
#' @return `NULL`. Two `_targets.yaml` files written
#' @export
#'
#' @examples
write_tars <- function(tars) {
  
  yaml::write_yaml(tars
                   , here::here("_targets.yaml")
                   )
  
  store_dir <- dirname(tars[[1]]$store)

  fs::dir_create(store_dir)

  yaml::write_yaml(tars
                   , fs::path(store_dir, "_targets.yaml")
                   )
  
  return(invisible(NULL))
  
}