#' Run all targets stores for multiple settings contexts
#'
#' @param settings List. usually from yaml::read_yaml("settings/setup.yaml").
#' Must contain extent, grain & optionally aoi as first list elements, with secondary lists of
#' vector, filt_col, filt_level, buffer, ext_time, region_taxa under extent,
#' res_x, res_y, res_time, taxonomic under grain, and
#' vector, filt_col, filt_level, buffer under aoi.
#' @param run_all List of specific settings vectors to run all combinations of stores for.
#' These are currently limited to ext_time, taxonomic grain & filt_level.
#' @param current_proj,current_store Current project & store names in which running this function and
#' running multiple outputs, e.g. 'envRegCont' & 'reg_cont'. Used for checking if existing `track_file`s exist.
#' @param upstream_proj,upstream_store Upstream project & store names required for the current project,
#' i.e. the precursor project that contains the combos of settings context directories & the store within them,
#' e.g. 'envRange' & 'grd'.
#' @param current_ext,upstream_ext Character vectors of current & upstream temporal extents corresponding to
#' the 'ext_time' setting in `settings$extent`, e.g. c("P10Y", "P20Y", "P30Y", "P50Y", "P100Y").
#' Often the same for current and upstream. Numbers must be preceded by 'P' and followed by 'D', 'M', or 'Y'.
#' @param current_tax_grains,upstream_tax_grains Character vectors of taxonomic grains for the current & upstream
#' projects, e.g. c("species", "subspecies"). Often the same for current and upstream.
#' @param current_lev,upstream_lev Vectors of current & upstream temporal extents corresponding to
#' the 'filt_level' setting in `settings$grain`. Use NULL for upstream where the upstream project does not have
#' an aoi setting.
#' @param current_track_file,upstream_track_file Names of files to use for tracking if a store relating to a context combo has been run.
#' Usually one of the last files created in the project/store, and or one used downstream.
#' @param lev_all_type Type of method used to determine 'all' values for `lev`.
#' Either, 'all_in_vec' which retrieves all levels in the column in the vector corresponding to those specified by
#' `settings$aoi$filt_col` and `settings$aoi$vector` respectively,
#' or 'already_run', which retrieves levels from existing store directories that have already been run,
#' i.e. those that are available.
#' @param current_aoi_setting,upstream_aoi_setting Names of the 'aoi' type settings for the current and
#' upstream projects containing the list of aoi related measures (vector, filt_col, filt_level, buffer),
#' e.g. sometimes referred to as 'region' instead of 'aoi'. Set to NULL if there is no aoi setting.
#' @param force_new Logical. Force new runs of stores corresponding to the context combos if they exist?
#' @param run_file Name of file that contains the targets stores and code to create them corresponding to
#' the current project.
#'
#' @return Executes the run file for all the combinations of settings specified.
#'
#' @export
#'
#' @examples
#'
run_all <- function(settings = yaml::read_yaml("settings/setup.yaml")
                    , run_all = yaml::read_yaml("settings/run_all.yaml")
                    , current_proj = "envRegCont"
                    , current_store = "reg_cont"
                    , upstream_proj = "envRange"
                    , upstream_store = "grd"
                    , current_ext = run_all$ext_time
                    , upstream_ext = current_ext
                    , current_lev = run_all$ext_time
                    , upstream_lev = NULL
                    , current_tax_grains = run_all$taxonomic
                    , upstream_tax_grains = current_tax_grains
                    , upstream_track_file = "grd_files"
                    , current_track_file = "reg_cont_tbl_tidy"
                    , current_aoi_setting = "aoi"
                    , upstream_aoi_setting = NULL
                    , lev_all_type = "all_in_vec"
                    , force_new = TRUE
                    , run_file = "run.R"

) {

  # check stores exist ----
  # check if relevant upstream project stores exist & stops if they don't
  find_context_combos(proj = upstream_proj
                      , store = upstream_store
                      , settings
                      , ext = upstream_ext # specific extents e.g. c(10, 20, 30, 50, 100) or 'all'
                      , lev = NULL
                      , tax_grains = run_all$taxonomic # specific taxonomic grains or 'all'
                      , lev_all_type = lev_all_type
                      , stop_if_not_run = TRUE
                      , aoi_setting = upstream_aoi_setting
                      , track_file = upstream_track_file
  )

  # find contexts to run ----
  ext_rank_lev <- find_context_combos(proj = current_proj
                                      , store = current_store
                                      , settings
                                      , ext = run_all$ext_time # specific extents e.g. c(10, 20, 30, 50, 100) or 'all'
                                      , lev = run_all$filt_lev
                                      , tax_grains = run_all$taxonomic # specific taxonomic grains or 'all'
                                      , lev_all_type = lev_all_type
                                      , stop_if_not_run = FALSE
                                      , aoi_setting = current_aoi_setting
                                      , track_file = current_track_file
  ) %>%
    {if(!force_new) dplyr::filter(., !exists) else .}

  # run all contexts ----

  if(force_new|nrow(ext_rank_lev)) {

    purrr::pwalk(list(ext_rank_lev$ext_time
                      , ext_rank_lev$rank
                      , ext_rank_lev$filt_level
    )
    , \(x, y, z) {

      settings$extent$ext_time <- x

      settings$grain$taxonomic <- y

      settings$region$filt_level <- z

      yaml::write_yaml(settings, fs::path("settings/setup.yaml"))

      source(run_file)

    }
    )

  } else {

    warning("No new contexts and force_new = FALSE, so no contexts were run.")

  }

}
