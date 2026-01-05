#' Make tile (extents) for raster split, apply, combine
#'
#' @param base_grid_path Character. Path to a template raster.
#' @param aoi Optional sf object. Used to define the outer extent of tiles'
#' @param tile_name Character. Prefix for the (file) name of each tile.
#' @param tile_length Numeric. Number of rows and columns in each tile.
#' @param tile_size Numeric. Ignored if tile_length is not `NULL`. Tile size in
#' units of the raster at `base_grid_path`. e.g. with `tile_size` of `10000`
#' a projected raster with units of metres will result in tiles of size 10 km by
#' 10 km.
#' @param min_tiles Numeric. Ignored if tile_length is not `NULL`. Minimum
#' number of tiles to return. If `tile_size` returns less than `min_tiles`, then
#'  `min_tiles` will be used instead of `tile_size`.
#' @param add_hectares Logical. Append the hectares of each tile? Ignored if
#' the raster at `base_grid_path` is not projected.
#'
#' @returns tibble of extents
#' @export
#'
#' @examples
make_tile_extents <- function(base_grid_path
                              , aoi = NULL
                              , tile_name = "tile__"
                              , tile_length = 512
                              , tile_size = 100000
                              , min_tiles = 4
                              , add_hectares = TRUE
                              ) {

  r <- terra::rast(base_grid_path)

  # window -------
  if(!is.null(aoi)) {

    aoi <- aoi |>
      sf::st_transform(crs = sf::st_crs(r)) |>
      sf::st_make_valid()

    terra::window(r) <- terra::vect(aoi)

  }

  # using tile_length ------
  if(!is.null(tile_length)) {

    tiles_y <- c(tile_length, tile_length)

    extents <- terra::getTileExtents(r
                                     , tiles_y
                                     )

  }

  # not using tile_length --------
  if(is.null(tile_length)) {

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

  }

  tile_power <- floor(log10(nrow(extents))) + 1

  # tiles tibble --------
  tiles <- extents |>
    tibble::as_tibble() %>%
    dplyr::mutate(tile_name = paste0(tile_name
                                     , stringr::str_pad(dplyr::row_number()
                                                        , width = tile_power
                                                        , pad = "0"
                                                        )
                                     )
                  )

  # add area? --------
  if(all(! terra::is.lonlat(r), add_hectares)) {

    tiles <- tiles |>
      dplyr::mutate(ha = purrr::map_dbl(1:nrow(extents)
                                        , \(x) sf::st_area(sf::st_as_sfc(sf::st_bbox(extents[x,]))) / 10000
                                        )
                    )

  }

  return(tiles)

}
