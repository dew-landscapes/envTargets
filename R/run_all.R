#' Run all targets stores for multiple settings contexts
#'
#' @param settings_file File path of yaml file containing a list of default contexts (or 'scales') usually from
#' "settings/scales.yaml".
#' The contexts list must contain extent, grain & optionally aoi as first list elements, with secondary lists of
#' vector, filt_col, filt_level, buffer, ext_time, region_taxa under extent,
#' res_x, res_y, res_time, taxonomic under grain, and
#' vector, filt_col, filt_level, buffer under aoi.
#' @param list_name Name of the list in the settings file that contains the list of relevant contexts if it is
#' contained within another list. Use NULL if the extent, grain and aoi elements are primary and
#' not contained in another list.
#' @param run_all_combos List of specific settings vectors to vary the default settings with and
#' run all combinations of stores for. These are currently limited to ext_time, taxonomic grain & filt_level settings.
#' @param current_proj,current_store Current project & store names in which running this function and
#' running multiple outputs, e.g. 'envRegCont' & 'reg_cont'. Used for checking if existing `track_file`s exist.
#' @param upstream_proj,upstream_store Upstream project & store names required for the current project,
#' i.e. the precursor project that contains the combos of settings context directories & the store within them,
#' e.g. 'envRange' & 'grd'.
#' @param upstream_ext Character vector of upstream temporal extents corresponding to
#' the 'ext_time' setting in `settings$extent`, e.g. c("P10Y", "P20Y", "P30Y", "P50Y", "P100Y").
#' Often the same for current and upstream, and usually sourced from run_all_combos.
#' Only needed if different to current project ext_time specified in `run_all_combos`.
#' Numbers must be preceded by 'P' and followed by 'D', 'M', or 'Y'.
#' @param upstream_tax_grains Character vector of taxonomic grains for the current & upstream
#' projects, e.g. c("species", "subspecies"). Often the same for current and upstream, and
#' usually sourced from run_all_combos. Only needed if different to current project taxonomic grain/s
#' specified in `run_all_combos`.
#' @param current_lev,upstream_lev Vectors of current & upstream filter levels corresponding to the 'filt_level' setting
#' in `settings$grain`. Use NULL for upstream where the upstream project does not have an aoi setting.
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
#' @param data_dir Path to data directory for sourcing the vector with the filter levels if `lev_all_type` = 'all_in_vec'.
#' @param current_store_base,upstream_store_base Base directory for the current and upstream targets stores, e.g. "../../out" or "projects/data".
#' @param current_region_taxa,upstream_region_taxa Logical. Is there a region taxa setting for the current and
#' upstream projects? Used to enable searching for track files in stores that have a region taxa setting and
#' those that don't.
#'
#' @return Executes the run file for all the combinations of settings specified.
#'
#' @export
#'
#' @examples
#'
run_all <- function(settings_file = "settings/scales.yaml"
                    , list_name = "default"
                    , run_all_combos = yaml::read_yaml("settings/run_all.yaml")
                    , current_proj = "envRegCont"
                    , current_store = "reg_cont"
                    , upstream_proj = "envRange"
                    , upstream_store = "grd"
                    , upstream_ext = run_all_combos$ext_time
                    , upstream_tax_grains = run_all_combos$taxonomic
                    , current_lev = run_all_combos$filt_lev
                    , upstream_lev = NULL
                    , upstream_track_file = "grd_files"
                    , current_track_file = "reg_cont_tbl_tidy"
                    , current_aoi_setting = "aoi"
                    , upstream_aoi_setting = NULL
                    , lev_all_type = "all_in_vec"
                    , force_new = TRUE
                    , run_file = "run.R"
                    , data_dir = yaml::read_yaml("settings/setup.yaml")$data_dir
                    , current_store_base = fs::path("..", "..", "out")
                    , upstream_store_base = fs::path("..", "..", "out")
                    , current_region_taxa = TRUE
                    , upstream_region_taxa = TRUE

) {

  # load settings ----
  settings <- yaml::read_yaml(settings_file)

  # check stores exist ----
  # check if relevant upstream project stores exist & stops if they don't
  find_context_combos(proj = upstream_proj
                      , store = upstream_store
                      , store_base = upstream_store_base
                      , settings = if(!is.null(list_name)) settings[[list_name]] else settings
                      , ext = upstream_ext
                      , lev = upstream_lev
                      , tax_grains = upstream_tax_grains
                      , lev_all_type = lev_all_type
                      , stop_if_not_run = TRUE
                      , aoi_setting = upstream_aoi_setting
                      , track_file = upstream_track_file
                      , data_dir = data_dir
                      , region_taxa_setting = upstream_region_taxa
  )

  # find contexts to run ----
  ext_rank_lev <- find_context_combos(proj = current_proj
                                      , store = current_store
                                      , store_base = current_store_base
                                      , settings = if(!is.null(list_name)) settings[[list_name]] else settings
                                      , ext = run_all_combos$ext_time
                                      , lev = current_lev
                                      , tax_grains = run_all_combos$taxonomic
                                      , lev_all_type = lev_all_type
                                      , stop_if_not_run = FALSE
                                      , aoi_setting = current_aoi_setting
                                      , track_file = current_track_file
                                      , data_dir = data_dir
                                      , region_taxa_setting = current_region_taxa
  ) %>%
    {if(!force_new) dplyr::filter(., !exists) else .} %>%
    {if(!"filt_level" %in% names(.)) dplyr::mutate(., filt_level = NA) else .}

  # run all contexts ----

  if(force_new|nrow(ext_rank_lev)) {

    purrr::pwalk(list(ext_rank_lev$ext_time
                      , ext_rank_lev$rank
                      , ext_rank_lev$filt_level
    )
    , \(x, y, z) {

      settings[[list_name]]$extent$ext_time <- x

      settings[[list_name]]$grain$taxonomic <- y

      if(!all(is.na(z))) settings[[list_name]]$region$filt_level <- z

      yaml::write_yaml(settings, settings_file)

      source(run_file)

    }
    )

  } else {

    warning("No new contexts and force_new = FALSE, so no contexts were run.")

  }

}
