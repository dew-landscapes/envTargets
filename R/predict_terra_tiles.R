#' Predict a model across tiles
#'
#' @param extent Tibble from `make_tile_extents()`.
#' @param predict_stack SpatRaster, possibly from `make_env_stack()`.
#' @param model Model object with a predict method.
#' @param terra_options List of options to be passed to `terra::terraOptions()`.
#' e.g. `list(memfrac = 0.5)`.
#' @param out_dir Character. Name of directory into which predicted tiles will
#' be saved.
#' @param force_new Logical. If TRUE, predict tile even if it already exists.
#' @param load_packages Character. Any packages to load before predict.
#' @param tile_name Character. How to name the output files? Default is `tile_`
#' which leads to `tile_0001.tif` etc.
#' @param tile_power Numeric. `10 ^ tile_power` tiles are allowed for, which
#' flows into the leading zeroes in tile names. Default is `4`, allowing for
#' 9999 tiles.
#' @param ... Passed to `terra::predict()`. Object, model and filename arguments
#' are already passed so are not needed here. e.g. `na.rm = TRUE`;
#' `overwrite = TRUE`; or `wopt = list(datatype = "INT1U")`
#'
#' @returns Path to the tile predicted tile. Writes tile into `out_dir`.
#' @export
#'
#' @examples
predict_terra_tiles <- function(extent
                                , predict_stack
                                , model
                                , terra_options = NULL
                                , out_dir
                                , force_new = TRUE
                                , load_packages = NULL
                                , tile_name = "tile_"
                                , tile_power = 4
                                , ...
                                ) {

  if(!is.null(load_packages)) {

    envFunc::check_packages(load_packages
                            , lib = TRUE
                            )

  }

  out_file <- fs::path(out_dir
                       , paste0(tile_name
                                , stringr::str_pad(extent$tile_name[[1]], width = tile_power, pad = "0")
                                , ".tif"
                                )
                       )

  if(any(!file.exists(out_file), force_new)) {

    fs::dir_create(dirname(out_file))

    ## terra options -------
    if(!is.null(terra_options)) {

      do.call(terra::terraOptions
              , args = terra_options
              )

    }

    terra::window(predict_stack) <- terra::ext(as.numeric(extent[1, 1:4]))

    terra::predict(object = predict_stack
                   , model = model
                   , filename = out_file
                   , ...
                   )

  }

  return(out_file)

}
