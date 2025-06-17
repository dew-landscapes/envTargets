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
#'
#' @return Character vector
#' @export
#'
#' @examples
collect_toi <- function(dir = "settings"
                        , toi_regex = "\\/toi"
                        , exclude = NULL
                        , include = NULL
                        , keep_previous = TRUE
                        , store_path
                        ) {

  previous <- if(keep_previous) {

    tar_read(toi, store = store_path)

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
