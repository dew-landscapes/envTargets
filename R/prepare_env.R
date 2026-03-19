
#' Prepare a tibble of environmental layers from settings
#'
#' 'Seasons' functionality removed 2025-09-05. If seasons are needed, left join
#' the result of `prepare_env()` to the result of `make_seasons()``$seasons`.
#'
#' @param set_list list. usually from yaml::read_yaml("settings/setup.yaml")
#' @param base_dir Character. Passed to `base_dir` argument of
#' `envFunc::name_env_out()`
#' @param create_short_desc Logical. Create a short description to use as a
#' unique 'name'. Requires columns 'name' and 'func'.
#' @param max_date_only Logical. Filter
#' rows duplicated by `layer` and `func` to the latest `start_date`.
#' @param ... Passed to envRaster::name_env_tif
#'
#' @return tibble of raster paths and meta data parsed into columns
#' @export
#'
#' @examples
prepare_env <- function(set_list
                        , base_dir = if(Sys.info()["sysname"] == "Windows") "I:" else fs::path("/mnt/envcube", "")
                        , create_short_desc = TRUE
                        , max_date_only = TRUE
                        , ...
                        ) {

  result <- envFunc::name_env_out(set_list
                        , base_dir = base_dir
                        , all_files = FALSE
                        ) |>
    dplyr::pull(path) |>
    envRaster::name_env_tif(parse = TRUE
                            , skips = "base|DEW__SDM"
                            , ...
                            ) |>
    dplyr::left_join(envRaster::ras_layers |>
                       dplyr::distinct(layer, description)
                     )

  result <- result |>
    dplyr::mutate(filter_date = as.Date(start_date))

  if(max_date_only) {

    result <- result |>
      dplyr::group_by(source, collection, layer, func) |>
      dplyr::filter(filter_date == max(filter_date) | start_date == "static") |>
      dplyr::ungroup()

  }

  if(create_short_desc) {

    result <- result |>
      dplyr::mutate(desc = purrr::map2_chr(layer, func
                                           , \(x, y) paste0(x
                                                            , if(x != y & y != "index") paste0(" ", y)
                                                            )
                                           )
                    )

  }

  return(result)

}






