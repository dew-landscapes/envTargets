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

  fs::dir_create(store_dir)

  yaml::write_yaml(tars
                   , fs::path(store_dir, "_targets.yaml")
                   )

  if(file.exists(scales_yaml)) {

    fs::copy(scales_yaml
             , fs::path(store_dir, "scales.yaml")
             )

  } else {

    warning("scales_yaml file not found, so scales yamls not copied to store directory")

  }

  if(package_dump) {

    sessioninfo::package_info(pkgs = yaml::read_yaml("settings/packages.yaml") |> unlist() |> unname() |> unique()
                              , include_base = TRUE
                              ) |>
      readr::write_csv(fs::path(store_dir, "package_info.csv"))

  }

  return(invisible(NULL))

}
