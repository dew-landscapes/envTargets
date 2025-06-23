#' Create a vector of values from a targets store object
#'
#' Usually used to create a vector for dynamic or static branching
#'
#' @param dir Directory containing targets store(s).
#' @param object Name of object within store(s).
#' @param collect_col Character name of column within objects found in store(s)
#' with the values to collect.
#' @param exclude Character. Vector of values used to limit the resulting
#' vector. i.e. only values that do not appear in `exclude` will make it into
#' the resulting vector.
#' @param include Character. Vector of values used to limit the resulting
#' vector. i.e. only taxa that appear in `include` will make it into the
#' resulting vector.
#'
#' @return Character vector
#' @export
#'
#' @examples
collect_values <- function(dir
                           , object
                           , collect_col
                           , exclude = NULL
                           , include = NULL
                           ) {

  results <- fs::dir_ls(dir
                        , regexp = paste0(object, collapse = "|")
                        , recurse = TRUE
                        , type = "file"
                        ) |>
    grep("\\/objects\\/", x = _, value = TRUE)

  keep <- purrr::map2(dirname(dirname(results))
                       , basename(results)
                       , \(x, y) targets::tar_read_raw(y, store = x)
                       ) |>
    dplyr::bind_rows() |>
    dplyr::pull(!!rlang::ensym(collect_col)) |>
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
