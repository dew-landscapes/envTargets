#' Find context combinations based on settings
#'
#' Finds all context combinations for mulitple ext_time, taxonmic grain, and filt_level values
#' specified as vectors in a yaml file. Also returns the associated store paths for each combo via envTargets::name_env_out
#' corresponding to a file in the store, and an indication if it exists.
#'
#' @param proj,store Project and store names used to specify the directory path to look for existing stores & files
#' relating to the context combos.
#' @param settings list. usually from yaml::read_yaml("settings/setup.yaml").
#' Must contain extent, grain & optionally aoi as first list elements, with secondary lists of
#' vector, filt_col, filt_level, buffer, ext_time, region_taxa under extent,
#' res_x, res_y, res_time, taxonomic under grain, and
#' vector, filt_col, filt_level, buffer under aoi.
#' @param ext Character vector of temporal extents to vary the `ext_time` setting in `settings$extent`,
#' e.g. c(P10Y, P20Y, P30Y, P50Y, P100Y). Numbers must be preceded with 'P' and followed by 'Y'.
#' @param tax_grains Character vector of filter levels to vary the `taxonomic` setting in `settings$grain`,
#' e.g. c("species", "subspecies").
#' @param lev Character vector of filter levels to vary the `filt_level` setting in `settings$aoi`.
#' As with `ext` & `lev`, can be specific levels or 'all' to run all available (see `lev_all_type` below for 'all' options).
#' @param lev_all_type Type of method used to determine 'all' values for `lev`.
#' Either, 'all_in_vec' which retrieves all levels in the column in the vector corresponding to those specified by
#' `settings$aoi$filt_col` and `settings$aoi$vector` respectively,
#' or 'already_run', which retrieves levels from existing store directories that have already been run,
#' i.e. those that are available.
#' @param stop_if_not_run Logical. Stop if the any of the specified context cominations haven't been run
#' and don't have a `track_file` (see below)?
#' @param aoi_setting Character. Name of the 'aoi' type setting containing the list of aoi related measures
#' (vector, filt_col, filt_level, buffer), e.g. sometimes referred to as 'region' instead of 'aoi'.
#' @param track_file Character name of file to use for tracking if a store relating to a context combo has been run.
#' Usually one of the last files created in the project/store, and or one used downstream.
#'
#' @return Data frame with ext_time, rank, filt_level, path, exists and combo columns,
#' with each row representing a combination of the ext_time, taxonomic & filt_level inputs.
#'
#' @export
#'
#' @examples
#'
find_context_combos <- function(proj = "envRegCont"
                                , store = "reg_cont"
                                , settings
                                , ext = settings$extent$ext_time # specific extents e.g. c(10, 20, 30, 50, 100) or 'all'
                                , lev = settings$aoi$filt_lev # specific filt levels or 'all'
                                , tax_grains = settings$grain$taxonomic # specific taxonomic grains or 'all'
                                , lev_all_type = "already_run" # or "all_in_vec"
                                , stop_if_not_run = TRUE
                                , aoi_setting = "aoi"
                                , track_file = "reg_cont_tbl_tidy"

) {

  # proj directory ----
  # overarching directory
  proj_dir <- fs::path("..", "..", "out", proj)

  # find all ----
  ## ext_time ----
  if(any(ext == "all")|any(is.null(ext))) {

    ext_pattern <- paste0(settings$extent$vector, "__"
                          , settings$extent$filt_col, "__"
                          , settings$extent$filt_level, "__"
                          , settings$extent$buffer, "__"
                          , ".*"
                          , settings$extent$region_taxa
    )

    dirs <- fs::dir_info(path = proj_dir, regexp = ext_pattern
                         , type = "directory", recurse = 0) |>
      dplyr::mutate(path = basename(path)) |>
      dplyr::distinct(path) |>
      dplyr::pull(path)

    ext <- dirs |>
      stringr::str_extract(paste0("(?<=", settings$extent$vector, "__"
                                  , settings$extent$filt_col, "__"
                                  , settings$extent$filt_level, "__"
                                  , settings$extent$buffer, "__"
                                  , ").*?(?=__"
                                  , settings$extent$region_taxa
                                  , ")"
      )
      )

  } else { # if not all or null then find pattern for finding relevant directories for other parameters below

    ext_pattern <- paste0(settings$extent$vector, "__"
                          , settings$extent$filt_col, "__"
                          , settings$extent$filt_level, "__"
                          , settings$extent$buffer, "__"
                          , ".+("
                          , paste(ext, collapse = "|")
                          , ").+"
                          , settings$extent$region_taxa
    )

  }

  ## taxonomic ----
  if(any(tax_grains == "all")|any(is.null(tax_grains))) {

    grain_pattern <- paste0(settings$grain$res_x, "__"
                            , settings$grain$res_y, "__"
                            , settings$grain$res_time, "__"
                            , ".*"
    )

    dirs <- fs::dir_info(path = proj_dir, regexp = paste(ext_pattern, grain_pattern, sep = "/")
                         , type = "directory", recurse = 1) |>
      dplyr::mutate(path = basename(path)) |>
      dplyr::distinct(path) |>
      dplyr::pull(path)

    tax_grains <- dirs |>
      stringr::str_extract(paste0("(?<=", settings$grain$res_x, "__"
                                  , settings$grain$res_y, "__"
                                  , settings$grain$res_time, "__)", ".*?$"
      )
      )

  } else {

    grain_pattern <- paste0(settings$grain$res_x, "__"
                            , settings$grain$res_y, "__"
                            , settings$grain$res_time, "__"
                            , ".+("
                            , paste(tax_grains, collapse = "|")
                            , ").+"
    )

  }

  ## filt_lev ----
  if(any(lev == "all")) {

    if(lev_all_type == "already_run") {

      aoi_pattern <- paste0(settings[[aoi_setting]]$vector, "__"
                               , settings[[aoi_setting]]$filt_col, "__", ".*", "__"
                               , settings[[aoi_setting]]$buffer
      )

      dirs <- fs::dir_info(path = proj_dir, regexp = paste(ext_pattern, grain_pattern, aoi_pattern, sep = "/")
                           , type = "directory", recurse = 2) |>
        dplyr::mutate(path = basename(path)) |>
        dplyr::distinct(path) |>
        dplyr::pull(path)

      lev <- dirs |>
        stringr::str_extract(paste0("(?<=", settings[[aoi_setting]]$vector, "__"
                                    , settings[[aoi_setting]]$filt_col, "__"
                                    , ").*?(?=__"
                                    , settings[[aoi_setting]]$buffer
                                    , ")"
        )
        )

    } else if(lev_all_type == "all_in_vec") {

      aoi_sf_file <- fs::path(settings$data_dir, "vector", paste0(settings[[aoi_setting]]$vector, ".parquet"))

      aoi_sf <- sfarrow::st_read_parquet(aoi_sf_file)

      filt_col <- settings[[aoi_setting]]$filt_col

      lev <- aoi_sf |>
        dplyr::distinct(!!rlang::ensym(filt_col)) |>
        dplyr::filter(!is.na(!!rlang::ensym(filt_col))) |>
        dplyr::pull()

    }

  }

  # settings temp ----
  # for updating settings & creating relevant directory paths with envFunc::name_env_out
  settings_temp <- list(extent = c(yaml::read_yaml("settings/setup.yaml")$extent[1:4]
                                   , ext_time = yaml::read_yaml("settings/setup.yaml")$extent$ext_time[1]
                                   , yaml::read_yaml("settings/setup.yaml")$extent[6]
  )
  , grain = c(yaml::read_yaml("settings/setup.yaml")$grain[1:3]
              , list(taxonomic = "species")
  )
  )

  if(!is.null(aoi_setting) & !is.null(lev)) {

    settings_aoi <- list(aoi = c(yaml::read_yaml("settings/setup.yaml")[[aoi_setting]][1:2]
                                       , filt_lev = yaml::read_yaml("settings/setup.yaml")[[aoi_setting]]$filt_lev[1]
                                       , yaml::read_yaml("settings/setup.yaml")[[aoi_setting]][4]
    )
    ) |>
      purrr::set_names(aoi_setting)

    settings_temp <- c(settings_temp, settings_aoi)

  }

  # combos ----
  # all possible combos of ext_time, filt_lev & taxonomic values
  if(!is.null(lev)) {

    combos <- expand.grid(ext_time = ext, filt_lev = lev, taxonomic = tax_grains
                          , stringsAsFactors = FALSE
    )

  } else {

    combos <- expand.grid(ext_time = ext, taxonomic = tax_grains
                          , stringsAsFactors = FALSE
    ) |>
      dplyr::mutate(filt_lev = NA)

  }

  # paths df ----
  paths_df <- combos |>
    purrr::pmap(\(ext_time, filt_lev, taxonomic) {

      settings_temp$extent$ext_time <- ext_time

      settings_temp$grain$taxonomic <- taxonomic

      if(!is.na(filt_lev) & !is.null(aoi_setting)) settings_temp[[aoi_setting]]$filt_lev <- filt_lev

      settings_temp$out_dir <- envFunc::name_env_out(set_list = settings_temp
                                                     # , base_dir = fs::path(envFunc::get_env_dir(fs::path("..", "..", "envRegCont")
                                                     #                                            , linux_default = "/projects")
                                                     #                       )
                                                     , base_dir = fs::path(envFunc::get_env_dir(fs::path("dev", "out", proj)
                                                                                                , linux_default = "/projects")
                                                     )
                                                     , dir_with_context = TRUE
      )$path %>%
        gsub("\\-\\-", "_", .)

      fs::path(settings_temp$out_dir, store, "objects", track_file) |>
        tibble::as_tibble_col(column_name = "path") |>
        dplyr::mutate(ext_time = ext_time
                      , rank = taxonomic
        ) %>%
        {if(!is.na(filt_lev) & !is.null(aoi_setting)) dplyr::mutate(., filt_level = filt_lev
                                                                       , aoi = settings[[aoi_setting]]$vector
                                                                       , filt_col = settings[[aoi_setting]]$filt_col
        ) else .}

    }
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(exists = purrr::map_lgl(path, \(x) fs::file_exists(x))) |>
    dplyr::distinct(across(tidyr::any_of(c("ext_time", "rank", "aoi", "filt_col", "filt_level", "path", "exists")))) %>%
    {if("filt_level" %in% names(.)) dplyr::mutate(., combo = paste(ext_time, rank, filt_level, sep = ", ")) else dplyr::mutate(., combo = paste(ext_time, rank, sep = ", "))} %>%
    {if("aoi" %in% names(.)) dplyr::arrange(., ext_time, rank, aoi, filt_col, filt_level) else dplyr::arrange(., ext_time, rank)} %>%
    {if(!is.null(aoi_setting) & !is.null(lev)) dplyr::rename(., aoi_setting = aoi) else .}

  # not run ----
  not_run <- paths_df |>
    dplyr::filter(!exists) |>
    dplyr::pull(combo)

  completed_run <- paths_df |>
    dplyr::filter(exists) |>
    dplyr::pull(combo)

  if(length(not_run) & stop_if_not_run) stop(paste0("These context combos have not been run in "
                                                    , proj, ": \n")
                                             , stringr::str_flatten(not_run, collapse = "\n")
                                             , paste0("\nOnly these context combos have been run in "
                                                      , proj, ": \n")
                                             , stringr::str_flatten(completed_run, collapse = "\n")
  )

  return(paths_df)

}
