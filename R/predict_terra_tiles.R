#' Predict a model across tiles
#'
#' @param extent_df Tibble from `make_tile_extents()`.
#' @param predict_stack SpatRaster, possibly from `make_env_stack()`.
#' @param model Model object with a predict method.
#' @param terra_options List of options to be passed to `terra::terraOptions()`.
#' e.g. `list(memfrac = 0.5)`.
#' @param out_dir Character. Name of directory into which predicted tiles will
#' be saved.
#' @param force_new Logical. If TRUE, predict tile even if it already exists.
#' Leave as `TRUE` to let targets decide if the tile needs to be rewritten.
#' @param load_packages Character. Any packages to load before predict.
#' @param check_tifs Logical. If `TRUE` and `force_new` is `FALSE`, an attempt
#' will be made to open any existing tile. If that fails, the tail will be
#' deleted. While expensive in time, this ensures that any crashes that result
#' in incomplete tiles are fixed.
#' @param ... Passed to `terra::predict()`. Object, model and filename arguments
#' are already passed so are not needed here. e.g. `na.rm = TRUE`;
#' `overwrite = TRUE`; or `wopt = list(datatype = "INT1U")`
#'
#' @returns Path to the tile predicted tile. Writes tile into `out_dir`.
#' @export
#'
#' @examples
predict_terra_tiles <- function(extent_df
                                , predict_stack
                                , model
                                , terra_options = NULL
                                , out_dir
                                , force_new = TRUE
                                , load_packages = NULL
                                , check_tifs = TRUE
                                , ...
                                ) {

  if(!is.null(load_packages)) {

    envFunc::check_packages(load_packages
                            , lib = TRUE
                            )

  }

  extent_df <- extent_df |>
    dplyr::mutate(out_file = fs::path(out_dir
                                      , paste0(tile_name, ".tif")
                                      )
                  )

  # check if tifs are openable (and delete them if not) before making 'to_do'
  if(all(check_tifs, isFALSE(force_new))) {

    if(any(purrr::map_lgl(extent_df$out_file, file.exists))) {

      safe_rast <- purrr::safely(terra::rast)

      fails <- extent_df |>
        dplyr::filter(purrr::map_lgl(extent_df$out_file, file.exists)) |>
        dplyr::mutate(fails = purrr::map(out_file, safe_rast)
                      , fails = purrr::map_lgl(fails, \(x) !is.null(x$error))
                      ) |>
        dplyr::filter(fails)

      if(nrow(fails)) {

        fs::file_delete(fails$out_file)

      }

    }

  }

  extent_df <- extent_df |>
    # the file is 'to_do' if: it doesn't exist; or 'force_new' is TRUE
    dplyr::mutate(to_do = any(!file.exists(out_file)
                              , force_new
                              )
                  )

  if(sum(extent_df$to_do) > 0) {

    fs::dir_create(dirname(extent_df$out_file[[1]]))

    ## terra options -------
    if(!is.null(terra_options)) {

      do.call(terra::terraOptions
              , args = terra_options
              )

    }

    purrr::walk(1:nrow(extent_df)
                , \(x) {

                  terra::window(predict_stack) <- terra::ext(as.numeric(extent_df[x, 1:4]))

                  terra::predict(object = predict_stack
                                 , model = model
                                 , filename = extent_df$out_file[[x]]
                                 , ...
                                 )

                }
                )

  }

  return(extent_df$out_file)

}
