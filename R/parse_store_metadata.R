#' Parse metadata in the file path of a (env) targets store
#'
#' @param project Character. Project name. e.g. basename(here::here()) from
#' inside, say, an RStudio project.
#' @param store_base Character. Path to the level above the project store.
#' @param targets_yaml Character. Name of the `_targets.yaml` file(s) to search
#' for within the `store_base`
#' @param scales_yaml Character. Name of the `scales.yaml` file(s) to search
#' for within the `store_base`
#'
#' @returns Tibble of parsed metadata
#' @export
#'
#' @examples
parse_store_metadata <- function(project = basename(here::here())
                                 , store_base = "../../out"
                                 , targets_yaml = "_targets.yaml"
                                 , scales_yaml = "scales.yaml"
                                 ) {

  stores <- fs::dir_ls(fs::path(store_base, project)
                      , recurse = TRUE
                      , regexp = targets_yaml
                      ) |>
    tibble::enframe(name = NULL, value = "store") |>
    dplyr::mutate(scales_path = fs::path(dirname(store), scales_yaml)
                  , scales_exists = file.exists(scales_path)
                  , store = dirname(store)
                  )

  if(sum(stores$scales_exists)) {

    stores <- stores |>
      dplyr::select(- scales_exists) |>
      dplyr::mutate(data = purrr::map(store
                                      , \(x) envFunc::extract_scale(element = project, scales = fs::path(x, "scales.yaml")) |>
                                        envFunc::name_env_out()
                                      )
                    ) |>
      tidyr::unnest(cols = c(data)) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.character)
                                  , \(x) gsub("^$", "NULL", x)
                                  )
                    )

  } else {

    warning("No ", scales_yaml, " files found in:\n "
            , paste0(stores$store, collapse = "\n ")
            , "\nscales_yaml is needed to parse the metadata"
            )

  }

  return(stores)

}
