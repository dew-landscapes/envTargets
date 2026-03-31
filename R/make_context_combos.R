#' Make context combinations data frame based on settings
#'
#' @param project Character. One or more project names. e.g. basename(here::here()) from
#' inside, say, an RStudio project.
#' @param scales_yaml Character. Name of the `scales.yaml` file containing the `project` settings.
#' @param vars List of settings and values to vary, e.g. list(extent_time = c("P10Y", "P50Y", "P100Y")
#' , taxonomic = c("species", "subspecies")).
#' @param all_vec_lev Logical. Use all levels in the spatial vector specified by the aoi_vector setting within the
#' column specified by the aoi_filt_col setting in the `scales_yaml`? Used instead of specifying a vector in `vars`
#' for the aoi_filt_level setting. Useful where there are large numbers of aoi_filt_levels.
#' @param vec_dir Vector directory containing the vector file that corresponds to the aoi_vector setting for the
#' `project` in the `scales_yaml`. Only used if `all_vec_lev` is TRUE, for retreiving all the levels in the filt_col
#' in the specified vector. Currently has to be a geoparquet with ".parquet" extension accessible by
#' sfarrow::st_read_parquet.
#'
#' @return Data frame with all possible combinations of the variables provided, along with
#' fields with constant values for the other non-varied settings contexts.
#'
#' @export
#'
#' @examples
#'
make_context_combos <- function(project = basename(here::here())
                                , scales_yaml = "scales.yaml"
                                , vars
                                , all_vec_lev = FALSE
                                , vec_dir = fs::path(yaml::read_yaml("settings/setup.yaml")$data_dir, "vector")

) {

  scales_file <- find_file(path = here::here(), find = scales_yaml, recurse_depth = 1)

  if(all_vec_lev) {

    settings <- envFunc::extract_scale(element = project
                                       , scales = scales_file
    )

    vec_lev <- sfarrow::st_read_parquet(fs::path(vec_dir, paste0(settings$aoi$aoi_vector, ".parquet"))) |>
      sf::st_drop_geometry() |>
      dplyr::distinct(across(settings$aoi$aoi_filt_col)) |>
      dplyr::pull()

    vars <- c(list(aoi_filt_level = vec_lev), vars)

  }

  res <- do.call(tidyr::expand_grid, vars)

  return(res)

}
