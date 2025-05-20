
#' Summarise a cleaning workflow from a targets store
#'
#' @param store path to a [targets](https://books.ropensci.org/targets/) store
#' (i.e. the `store` argument to `targets::tar_meta()`)
#' @param prefix Object name prefixes. Objects within the `store` that match
#' this will be summarised
#' @param excludes Any object that matches any regex provided in `excludes` will
#' not be summarised
#' @param extras Any object that matches any regex provided in `extras` will be
#' summarised
#' @param site_cols,visit_cols,taxa_cols Columns within objects identifying
#' sites, visits and taxa respectively
#'
#' @return
#' @export
#'
#' @examples
summarise_store_clean <- function(store = "."
                                  , prefix = "bio_"
                                  , excludes = c("path", "file", "env", "rich")
                                  , extras = c("clean_end")
                                  , site_cols = c("cell_lat", "cell_long")
                                  , visit_cols = c("cell_lat", "cell_long", "year")
                                  , taxa_cols = "taxa"
                                  ) {

  keeps <- paste0(paste0("^", prefix)
                  , if(!is.null(extras)) paste0("|", paste0(unique(extras), collapse = "|")) else NULL
                  )

  store |>
    tar_meta(store = _) |>
    dplyr::filter(grepl(keeps, name)) |>
    dplyr::filter(! grepl(paste0(excludes, collapse = "|"), name)) |>
    dplyr::select(name, warnings, path) |>
    tidyr::unnest(cols = c(path)) |>
    dplyr::mutate(obj = purrr::map(name, \(x) tar_read_raw(x, store = store))
                  , class = purrr::map(obj, \(x) class(x))
                  ) |>
    dplyr::filter(purrr::map_lgl(class, \(x) "data.frame" %in% x)) |>
    dplyr::mutate(summary = purrr::map(obj
                                       , \(x) envClean::rec_vis_sit_tax(x
                                                                        , site_cols = site_cols
                                                                        , visit_cols = visit_cols
                                                                        , taxa_cols = taxa_cols
                                                                        )
                                       )
                  , path = fs::path(store, "objects", name)
                  , rmd = here::here("report", "child", "clean", paste0(gsub(prefix, "", name), ".Rmd"))
                  ) |>
    tidyr::unnest(cols = c(summary)) |>
    dplyr::arrange(desc(taxa), desc(visits), desc(sites)) |>
    dplyr::select(! dplyr::where(is.list))

}
