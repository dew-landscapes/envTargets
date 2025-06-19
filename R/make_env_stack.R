#' Make a spatRaster from env_df
#'
#' @param env_df tibble. Probably a result of `prepare_env()`.
#' @param aoi sf defining an area of interest
#'
#' @return spatRaster
#' @export
#'
#' @examples
make_env_stack <- function(env_df
                           , aoi = NULL
                           ) {

  r <- terra::rast(env_df$path)
  names(r) <- env_df$name

  if(! is.null(aoi)) {

    terra::window(r) <- terra::ext(aoi_sf |> sf::st_transform(crs = sf::st_crs(r)))

  }

  return(r)

}
