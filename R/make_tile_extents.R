#' Make tile (extents) for raster split, apply, combine
#'
#' @param base_grid_path Character. Path to a template raster.
#' @param aoi Optional sf object. Used to define the outer extent of tiles
#' @param tile_size Numeric. Tile size in units of the raster at `base_grid_path`
#' @param min_tiles Numeric. Minimum number of tiles to return. If `tile_size`
#' returns less than `min_tiles`, then `min_tiles` will be used instead of
#' `tile_size`
#' @param add_hectares Logical. Append the hectares of each tile? Ignored if
#' the raster at `base_grid_path` is not projected.
#'
#' @returns tibble of extents
#' @export
#'
#' @examples
make_tile_extents <- function(base_grid_path
                              , aoi = NULL
                              , tile_size = 100000
                              , min_tiles = 4
                              , add_hectares = TRUE
                              ) {

  r <- terra::rast(base_grid_path)

  if(!is.null(aoi)) {

    aoi <- aoi |>
      sf::st_transform(crs = sf::st_crs(r)) |>
      sf::st_make_valid()

    terra::window(r) <- terra::vect(aoi)

  }

  rs <- ceiling(terra::nrow(r) / (terra::nrow(r) * terra::yres(r) / tile_size))
  cs <- ceiling(terra::ncol(r) / (terra::ncol(r) * terra::xres(r) / tile_size))

  tiles_y <- c(rs, cs)

  extents <- terra::getTileExtents(r
                                   , tiles_y
                                   )

  if(nrow(extents) < min_tiles) {

    # recalculate ignoring tile_size and just using min_tiles
    rs <- ceiling(terra::nrow(r) / sqrt(min_tiles))
    cs <- ceiling(terra::ncol(r) / sqrt(min_tiles))

    tiles_y <- c(rs, cs)

    extents <- terra::getTileExtents(r
                                     , tiles_y
                                     )

  } else {

    # recalculate to get tiles of (roughly) equal size and (roughly) of size tile_size
    rs <- ceiling(terra::nrow(r) / sqrt(nrow(extents)))
    cs <- ceiling(terra::ncol(r) / sqrt(nrow(extents)))

    tiles_y <- c(rs, cs)

    extents <- terra::getTileExtents(r
                                     , tiles_y
                                     )

  }

  tiles <- extents |>
    tibble::as_tibble() %>%
    dplyr::mutate(tile_name = dplyr::row_number())

  if(all(! terra::is.lonlat(r), add_hectares)) {

    tiles <- tiles |>
      dplyr::mutate(ha = purrr::map_dbl(1:nrow(extents)
                                        , \(x) sf::st_area(sf::st_as_sfc(sf::st_bbox(extents[x,]))) / 10000
                                        )
                    )

  }

  return(tiles)

}
