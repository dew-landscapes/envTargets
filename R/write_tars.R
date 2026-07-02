#' Write tars as yaml
#'
#' Writes a `tars` object to `here::here("_targets.yaml")` (i.e. the script
#' directory) and `fs::path(dirname(tars[[1]]$store), "_targets.yaml")` (i.e.
#' the store directory). Write `scales.yaml` into the store directory.
#' Optionally exports a .csv of package versions based on
#' packages listed in `package_yaml`.
#'
#' @param tars Tars list
#' @param package_dump Logical. Save package information (from
#' `sessioninfo::package_info()`) to the store (as .csv).
#' @param package_yaml Path to yaml file containing packages to be loaded by
#' scripts.
#' @param scales_yaml Path to yaml file containing 'scales' (e.g. extent, grain
#' and possibly area-of-interest (aoi)).
#'
#' @return `NULL`. Two `_targets.yaml` and one `scales_yaml` files written.
#' Optionally, `package_info.csv` written.
#' @export
#'
#' @examples
write_tars <- function(tars
                       , package_dump = TRUE
                       , package_yaml = "settings/packages.yaml"
                       , scales_yaml = "settings/scales.yaml"
                       ) {

  yaml::write_yaml(tars
                   , here::here("_targets.yaml")
                   )

  store_dir <- dirname(tars[[1]]$store)

  if(!dir.exists(store_dir)) {

    fs::dir_create(store_dir
                   , mode = "775"
                   )

  }

  # save _targets.yaml
  store_tars <- fs::path(store_dir, "_targets.yaml")

  if(file.exists(store_tars)) {
    unlink(store_tars) # remove existing file - doesn't care about permissions
  }

  yaml::write_yaml(tars
                   , store_tars
                   )

  fs::file_chmod(store_tars, mode = "775")


  # save scales.yaml
  if(file.exists(scales_yaml)) {

    store_scales <- fs::path(store_dir, "scales.yaml")

    if(file.exists(store_scales)) {
      unlink(store_scales) # remove existing file - doesn't care about permissions
    }

    fs::file_chmod(scales_yaml, mode = "775")

    fs::file_copy(scales_yaml
                  , store_scales
                  , overwrite = TRUE
                  )

  } else {

    warning("scales_yaml file not found, so scales yamls not copied to store directory")

  }

  if(package_dump) {

    store_pkgs <- fs::path(store_dir, "package_info.csv")

    if(file.exists(store_pkgs)) {
      unlink(store_pkgs) # remove existing file - doesn't care about permissions
    }

    sessioninfo::package_info(pkgs = yaml::read_yaml(package_yaml) |> unlist() |> unname() |> unique()
                              , include_base = TRUE
                              ) |>
      readr::write_csv(store_pkgs)

    fs::file_chmod(store_pkgs, mode = "775")

  }

  return(invisible(NULL))

}
