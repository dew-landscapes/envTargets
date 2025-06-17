#' Create a vector of taxa of interest (toi) from yaml files
#'
#' Usually used to create a vector over which tarchetypes::tar_map is run
#'
#' @param dir Directory in which `toi_regex` files will be found
#' @param toi_regex Regular expression that will pick up any yaml files
#' containing the taxa of interest.
#' @param exclude Character. Vector of taxa used to limit the resulting
#' vector. i.e. only taxa that do not appear in `exlude` will make it into
#' the resulting vector.
#' @param include Character. Vector of taxa used to limit the resulting
#' vector. i.e. only taxa that appear in `include` will make it into the
#' resulting vector.
#' @param collect_previous Logical. Should any previous toi for this extent and
#' grain be collected into the output toi?
#' @param store_path Character. Path to targets store directory used to find any
#' previous toi.
#' @param obj_prefix Character. A prefix that can identify taxa to include in
#' toi from within `store_path`.
#'
#' @return Character vector
#' @export
#'
#' @examples
collect_toi <- function(dir = "settings"
                        , toi_regex = "\\/toi"
                        , exclude = NULL
                        , include = NULL
                        , collect_previous = TRUE
                        , store_path
                        , obj_prefix = "tune_"
                        ) {

  previous <- if(collect_previous) {

    if(file.exists(store_path)) {

      targets::tar_meta(tidyselect::starts_with(obj_prefix)
                        , store = store_path
                        , targets_only = TRUE
                        ) |>
        dplyr::pull(name) |>
        gsub(obj_prefix, "", x = _) |>
        gsub("_", " ", x = _) |>
        unique() |>
        sort()

    } else NULL

  } else NULL

  keep <- c(previous
            , fs::dir_ls(dir, regexp = toi_regex) |>
              purrr::map(yaml::read_yaml) |>
              unlist() |>
              unname()
            ) |>
    unique() |>
    sort()

  if(!is.null(include)) {

    keep <- include |>
      intersect(keep)

  }

  if(!is.null(exclude)) {

    keep <- setdiff(keep, exclude)

  }

  return(keep)

}
