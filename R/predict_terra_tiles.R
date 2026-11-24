#' Predict a model across tiles
#'
#' @param extent_df Tibble from `make_tile_extents()`.
#' @param predictors Character. Paths to predictor rasters.
#' @param model Either model object, or path to model, with a predict method.
#' @param model_read_fun If `model` is a path, what function to use to read the
#' model. Defaults to `readRDS()`.
#' @param terra_options List of options to be passed to `terra::terraOptions()`.
#' e.g. `list(memfrac = 0.5)`.
#' @param terra_gdal_options Character. Vector of name value pairs to pass to
#' `terra::setGDALconfig()`.
#' @param out_dir Character. Name of directory into which predicted tiles will
#' be saved.
#' @param force_new Logical. If TRUE, predict tile even if it already exists.
#' Leave as `TRUE` to let targets decide if the tile needs to be rewritten.
#' @param load_packages Character. Any packages to load before predict.
#' @param check_tifs Logical. If `TRUE` and `force_new` is `FALSE`, an attempt
#' will be made to open any existing tile. If that fails, the tail will be
#' deleted. While expensive in time, this ensures that any crashes that result
#' in incomplete tiles are fixed.
#' @param predict_retries Numeric. The number of attempts to make at predicting.
#' Can be useful to overcome intermittent `[extract] cannot read values` errors.
#' @param ... Passed to `terra::predict()`. Object, model and filename arguments
#' are already passed so are not needed here. e.g. `na.rm = TRUE`;
#' `overwrite = TRUE`; or `wopt = list(datatype = "INT1U")`
#'
#' @returns Path to the predicted tile(s). Writes tile into `out_dir`.
#' @export
#'
#' @examples
predict_terra_tiles <- function(extent_df
                                , predictors
                                , model
                                , model_read_fun = readRDS
                                , terra_options = NULL
                                , terra_gdal_options = NULL
                                , out_dir
                                , force_new = TRUE
                                , load_packages = NULL
                                , check_tifs = TRUE
                                , predict_retries = 5
                                , ...
                                ) {

  # catch any NULL extent_df and pass through without predicting
  if("tile_name" %in% names(extent_df)) {

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
    if(check_tifs) {

      if(any(purrr::map_lgl(extent_df$out_file, file.exists))) {

        safe_rast <- purrr::safely(terra::rast)

        fails <- extent_df |>
          dplyr::filter(purrr::map_lgl(out_file, file.exists)) |>
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

      ## terra gdal options -------
      if(!is.null(terra_gdal_options)) {

        terra::setGDALconfig(option = terra_gdal_options)

      }

      if(is.character(model)) model <- model_read_fun(model)

      predict_retry <- function(predictors
                                , extent_df
                                , row
                                , model
                                , ...
                                , tries
                                , delay = 3
                                , bad_warning = "GDAL Error|I/O error|error reading|cannot read"
                                ) {

        last_err <- NULL

        for (i in seq_len(tries)) {

          bad <- character()

          res <- tryCatch({

            # fresh SpatRaster (and so fresh file handles) on every attempt
            predict_stack <- envRaster::make_env_stack(predictors = predictors)

            terra::window(predict_stack) <- terra::ext(as.numeric(extent_df[row, 1:4]))

            use_temp_file <- tempfile(fileext = ".tif")

            on.exit(unlink(use_temp_file)
                    , add = TRUE
                    )

            out <- withCallingHandlers(

              terra::predict(object = predict_stack
                             , model = model
                             , filename = use_temp_file
                             , ...
                             )

              , warning = function(w) {

                # only intercept the I/O-type warnings; others pass through as normal
                if (grepl(bad_warning, conditionMessage(w))) {

                  bad <<- c(bad, conditionMessage(w))

                  invokeRestart("muffleWarning")

                }
              }
            )

            if (length(bad)) stop(paste(unique(bad), collapse = " | "))

            # verify the output: it exists, opens, and its last row can be read
            stopifnot(file.exists(use_temp_file), file.size(use_temp_file) > 0)
            chk <- terra::rast(use_temp_file)
            invisible(terra::values(chk, row = terra::nrow(chk), nrows = 1))

            fs::file_copy(use_temp_file
                          , extent_df$out_file[[row]]
                          , overwrite = TRUE
                          )

            return(extent_df$out_file[[row]])

          }

          , error = function(e) e
          )

          if (!inherits(res, "error")) return(res)

          last_err <- res

          if (i < tries) Sys.sleep(delay * i)

        }

        stop("predict failed after "
             , tries
             , " tries: "
             , extent_df$out_file[[row]]
             , "\n"
             , "Last error: "
             , conditionMessage(last_err)
             , call. = FALSE
             )

      }

      purrr::walk(1:nrow(extent_df)
                  , \(x) {

                    predict_retry(predictors = predictors
                                  , extent_df = extent_df
                                  , model = model
                                  , row = x
                                  , ...
                                  , tries = predict_retries
                                  , delay = 3
                                  , bad_warning = "GDAL Error|I/O error|error reading|cannot read"
                                  )
                    }
                  )

    }

    result <- extent_df$out_file

  } else {

    message("no tiles supplied so no predict returned")

    result <- character(0)

  }

  return(result)

}
