
#' Collect the objects of a cleaning workflow from a targets store
#'
#' @param store Path to a targets store
#' @param prefix Object name prefixes. Objects within `store`
#' that match this will be collected
#' @param excludes Any object that matches any regex provided in `excludes` will
#' not be collected
#' @param extras Any object that matches any regex provided in `extras` will be
#' collected
#' @param deps Dependencies that, if updated, should invalidate the output.
#'
#' @return tibble
#' @export
#'
#' @examples
collect_clean_objects <- function(store
                                  , prefix = "bio_"
                                  , excludes = c("path", "dir", "file", "env", "rich", "names")
                                  , extras = c("clean_end")
                                  , deps = NULL
                                  ) {

  keeps <- paste0(paste0("^", prefix)
                  , if(!is.null(extras)) paste0("|", paste0(unique(extras), collapse = "|")) else NULL
                  )

  result <- targets::tar_meta(store = store
                              , targets_only = TRUE
                              ) |>
    dplyr::filter(grepl(keeps, name)) |>
    dplyr::filter(! grepl(paste0(excludes, collapse = "|"), name)) |>
    dplyr::filter(type == "stem") |>
    dplyr::select(name, warnings, time, path) |>
    tidyr::unnest(cols = c(path))

  return(result)

}
