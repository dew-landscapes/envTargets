#' Create a vector of taxa of interest (toi) from yaml files
#'
#' Usually used to create a vector over which tarchetypes::tar_map is run
#'
#' @param dir Directory in which `toi_regex` files will be found
#' @param toi_regex Regular expression that will pick up any yaml files
#' containing the taxa of interest.
#' @param environ_df Dataframe of environments (e.g. terrestrial, marine).
#' @param environ_remove Character. Vector of environments to remove.
#' @param other_taxa Character. Vector of taxa used to limit the resulting
#' vector. i.e. only taxa that appear in `other_taxa` will make it into the
#' resulting vector.
#'
#' @return Character vector
#' @export
#'
#' @examples
collect_toi <- function(dir = "settings"
                        , toi_regex = "\\/toi"
                        , environ_df = NULL
                        , environ_remove = "marine"
                        , other_taxa = NULL
                        ) {

  keep_taxa <- fs::dir_ls(dir, regexp = toi_regex) |>
    purrr::map(yaml::read_yaml) |>
    unlist() |>
    unname() |>
    unique() |>
    sort()

  if(!is.null(other_taxa)) {

    keep_taxa <- other_taxa |>
      intersect(keep_taxa)

  }

  if(!is.null(environ_df)) {

    keep_taxa <- environs |>
      dplyr::filter(! grepl(paste0(environ_remove, collapse = "|")
                            , environ
                            )
                    ) |>
      dplyr::distinct(taxa) |>
      dplyr::pull(taxa) |>
      intersect(keep_taxa)

  }

  return(keep_taxa)

}
