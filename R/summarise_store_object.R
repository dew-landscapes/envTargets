
#' Summarise an object from a cleaning workflow from a targets store
#'
#' Returns a single row tibble that is equivalent to the multiple row tibble
#' returned by `envTargets::summarise_store_data()`. `summarise_store_object`
#' enables mapping over the objects in a targets workflow (potentially making
#' use of parallel processing to acheive the same result as
#' `summarise_store_data()`). Also see `envTargets::collect_clean_objects()` for
#' creating a data frame to map over (and ensure upstream dependencies are
#' tracked).
#'
#' @param store Path to store containing `object`
#' @param object Name of `tars` element to summarise
#' @param prefix Character. e.g. `bio_` or `flor_`
#' @param site_cols,visit_cols,taxa_cols Columns within objects identifying
#' sites, visits and taxa respectively
#' @param rmd_dir What directory is the Rmd associated with each object found?
#' If left as `NULL`, will use `tars_name`.
#' @param use_arrow Logical. Use `arrow::open_dataset()` instead of
#' `tar_read_raw` on store objects. Saves memory, but requires targets to be
#' saved as parquets (with no file extension).
#' @param prep_filter_sf Logical. Prepare site-only (lat, long) data for
#' use in 'filter_sf' summary maps?
#' @param filter_sf_round Numeric. `digits` argument of `base::round()` for
#' use in rounding `site_cols`. Ignored if `prep_filter_sf` is not
#' `TRUE`.
#'
#' @return tibble
#' @export
#'
#' @examples
summarise_store_object <- function(store
                                   , object
                                   , prefix = "bio_"
                                   , site_cols = c("lat", "long", "cell_lat", "cell_long")
                                   , visit_cols = c("lat", "long", "cell_lat", "cell_long", "year")
                                   , taxa_cols = c("original_name", "taxa")
                                   , rmd_dir = NULL
                                   , use_arrow = TRUE
                                   , prep_filter_sf = TRUE
                                   , filter_sf_round = 3
                                   ) {

  if(is.null(rmd_dir)) rmd_dir <- basename(store)

  result <- targets::tar_meta(store = store
                              , targets_only = TRUE
                              ) |>
    dplyr::filter(grepl(paste0("^", object, "$"), name)) |>
    dplyr::filter(type == "stem") |>
    dplyr::select(name, warnings, time, path) |>
    tidyr::unnest(cols = c(path)) |>
    dplyr::mutate(obj = purrr::map(name,
                                   \(x) {
                                     if(use_arrow) {
                                       arrow::open_dataset(fs::path(store, "objects", x)) |>
                                         dplyr::select(tidyselect::any_of(c(visit_cols, site_cols, taxa_cols))) |>
                                         dplyr::collect()
                                     } else tar_read_raw(x, store = store)
                                   } )
                  ) |>
    dplyr::mutate(summary = purrr::map(obj
                                       , \(x) envClean::rec_vis_sit_tax(x
                                                                        , site_cols = site_cols
                                                                        , visit_cols = visit_cols
                                                                        , taxa_cols = taxa_cols
                                                                        )
                                       )
                  , path = fs::path(store, "objects", name)
                  , rmd = here::here("report", "child", rmd_dir, paste0(gsub(prefix, "", name), ".Rmd"))
                  ) |>
    tidyr::unnest(cols = c(summary)) |>
    dplyr::select(! dplyr::where(is.list))

  if(prep_filter_sf) {

    rename_df <- tibble::tibble(old_name = c(site_cols)) |>
      dplyr::mutate(new_name = dplyr::if_else(grepl("lon|east", old_name), "x", NA_character_)
                    , new_name = dplyr::if_else(grepl("lat|north", old_name), "y", new_name)
                    ) |>
      dplyr::distinct() |>
      dplyr::select(new_name, old_name)

    rename_vec <- tibble::deframe(rename_df)

    result <- result |>
      dplyr::mutate(filter_sf_data = purrr::map(name
                                                , \(x) arrow::open_dataset(fs::path(store, "objects", x)) |>
                                                  dplyr::distinct(dplyr::across(tidyselect::any_of(site_cols))) |>
                                                  dplyr::collect() |>
                                                  dplyr::mutate(dplyr::across(tidyselect::any_of(site_cols)
                                                                              , \(x) base::round(x, digits = filter_sf_round)
                                                                              )
                                                                ) |>
                                                  dplyr::distinct() |>
                                                  dplyr::rename(tidyselect::any_of(rename_vec))
                                                )
                    )

  }

  return(result)

}
