
#' Prepare a tibble of environmental layers from settings
#'
#' 'Seasons' functionality removed 2025-09-05. If seasons are needed, left join
#' the result of `prepare_env()` to the result of `make_seasons()``$seasons`.
#'
#' @param set_list list. usually from yaml::read_yaml("settings/setup.yaml")
#' @param reg_exp Character. Used to limit returned files.
#' @param base_dir Character. Passed to `base_dir` argument of
#' `envFunc::name_env_out()`
#' @param create_short_desc Logical. Create a short description to use as a
#' unique 'name'. Requires columns 'name' and 'func'.
#' @param ... Passed to envRaster::name_env_tif
#'
#' @return tibble of raster paths and meta data parsed into columns
#' @export
#'
#' @examples
prepare_env <- function(set_list
                        , reg_exp = "\\.tif"
                        , base_dir = if(Sys.info()["sysname"] == "Windows") "I:" else fs::path("/mnt/envcube", "")
                        , create_short_desc = TRUE
                        , ...
                        ) {

  result <- envFunc::name_env_out(set_list
                        , base_dir = base_dir
                        , reg_exp = reg_exp
                        , all_files = FALSE
                        ) |>
    dplyr::pull(path) |>
    envRaster::name_env_tif(parse = TRUE
                            , skips = "base|DEW__SDM"
                            , ...
                            ) |>
    dplyr::mutate(start_date = as.Date(start_date))

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






