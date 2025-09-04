#' Combine tiles into a single raster
#'
#' @param tiles Paths to tiles, possibly from
#' `predict_terra_tiles()`, or list of `SpatRaster` tiles.
#' @param out_file Character. Name of the saved file.
#' @param ... Passed to `terra::merge()`. e.g. `overwrite = TRUE`; or
#' `wopt = list(datatype = "INT1U")`.
#'
#' @returns `out_file`
#' @export
#'
#' @examples
combine_tiles <- function(tiles
                          , out_file
                          , ...
                          ) {

  terra::merge(terra::sprc(tiles)
               , filename = out_file
               , ...
               )

  return(out_file)

}
