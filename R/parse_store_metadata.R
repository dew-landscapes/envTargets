#' Parse metadata in the file path of a (env) targets store
#'
#' @param project Character. One or more project names. e.g. basename(here::here()) from
#' inside, say, an RStudio project.
#' @param store_base Character. Path to the level above the project store (e.g. an "out" folder).
#' @param targets_yaml Character. Name of the `_targets.yaml` file(s) to search
#' for within the `store_base`
#' @param scales_yaml Character. Name of the `scales.yaml` file(s) to search
#' for within the `store_base`
#' @param recurse_depth Numeric. Passed to the recurse argument of `fs::dir_ls()`
#'
#' @returns Tibble of parsed metadata
#' @export
#'
#' @examples
parse_store_metadata <- function(project = basename(here::here())
                                 , store_base = "../../out"
                                 , targets_yaml = "_targets.yaml"
                                 , scales_yaml = "scales.yaml"
                                 , recurse_depth = 3
                                 ) {

  stores <- purrr::set_names(project) |>
    purrr::map(\(x)
               find_file(path = fs::path(store_base, x)
                         , recurse_depth = recurse_depth
                         ) |>
                 tibble::enframe(name = NULL, value = "targets_path")
               ) |>
    tibble::enframe(name = "project") |>
    tidyr::unnest(value) |>
    dplyr::mutate(scales_path = fs::path(dirname(targets_path), scales_yaml)
                  , scales_exists = file.exists(scales_path)
                  , stores = dirname(targets_path)
                  )

  if(sum(!stores$scales_exists)) {

      warning("No ", scales_yaml, " files found in:\n "
              , paste0(stores$stores[!stores$scales_exists], collapse = "\n ")
              , "\nscales_yaml is needed to parse the metadata"
      )
  }

  if(sum(stores$scales_exists)) {

    stores <- stores |>
      dplyr::filter(scales_exists) |>
      dplyr::select(- scales_exists) |>
      dplyr::mutate(data = purrr::map(scales_path
                                      , \(x) envFunc::extract_scale(element = project
                                                                    , scales = x
                                                                    ) |>
                                        envFunc::name_env_out()
                                      )
                    ) |>
      tidyr::unnest(cols = c(data)) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.character)
                                  , \(x) gsub("^$", "NULL", x)
                                  )
                    )

  }

  return(stores)

}
