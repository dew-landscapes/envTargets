#' Combine predicted tiles into a single raster
#'
#' @param predicted_tiles Paths to predicted tiles, usually from
#' `predict_terra_tiles()`.
#' @param out_file Character. Name of the saved file.
#' @param ... Passed to `terra::merge()`. e.g. `overwrite = TRUE`; or
#' `wopt = list(datatype = "INT1U")`.
#'
#' @returns `out_file`
#' @export
#'
#' @examples
combine_tiles <- function(predicted_tiles
                          , out_file
                          , ...
                          ) {

  terra::merge(terra::sprc(predicted_tiles)
              , filename = out_file
              , ...
              )

  return(out_file)

}
