#' Find context files based on settings combos in a data frame
#'
#' @param project Character. One or more project names. e.g. basename(here::here()) from
#' inside, say, an RStudio project.
#' @param scales_yaml Character. Name of the `scales.yaml` file to obtain context values for
#' non-varied context settings.
#' @param combos_df Data frame of settings context combinations for all the settings contexts, where the columns
#' are the settings and the rows are each combination of different values for each setting, e.g. as produced by
#' envTargets::make_context_combos.
#' @param track_file Name of file with extension to use for tracking if a store relating to a context combo has been
#' run. Usually one of the last files created in the project/store, and or one used downstream.
#' @param stop_if_not_run Logical. Stop if the any of the specified context files haven't been run
#' and don't have a `track_file`?
#' @param store_base Path to base directory of project store, e.g. "../../out"  or "/projects/data".
#'
#' @return Data frame with extent_time, rank, filt_level, path, exists and combo columns,
#' with each row representing a combination of the extent_time, taxonomic & filt_level inputs.
#'
#' @export
#'
#' @examples
#'
find_context_files <- function(project = basename(here::here())
                               , scales_yaml = "scales.yaml"
                               , combos_df
                               , track_file = "_targets.yaml"
                               , stop_if_not_run = TRUE
                               , store_base = "../../out"

) {

  # settings ----
  scales_file <- find_file(path = here::here(), find = scales_yaml, recurse_depth = 1)

  settings <- envFunc::extract_scale(element = project
                                     , scales = scales_file
  )

  # all settings df----
  all_set_df <- settings |>
    purrr::list_flatten(name_spec = "{inner}") |>
    purrr::map(\(x) ifelse(is.null(x), NA, x)) |> # convert NULL to NA, otherwise dplyr::bind_cols drops the NULL elements and lose those columns below
    dplyr::bind_cols()

  # add non-varied settings ----
  combos_df <- combos_df |>
    dplyr::bind_cols(all_set_df |>
                       dplyr::select(-names(combos_df))
    )

  # find all paths ----
  paths_df <- purrr::map(1:nrow(combos_df), \(a) {

    names(settings) |>
      purrr::set_names() |>
      purrr::map(\(x) {

        cols <- names(envFunc::extract_scale()[[x]])

        elements <- combos_df |>
          dplyr::slice(a) |>
          dplyr::select(tidyr::any_of(cols)) |>
          as.list() |>
          purrr::map(\(l) if(is.na(l)) NULL else l)

        set_list <- list()

        set_list[[x]] <- list() |>
          c(elements)

      }
      ) |>
      envFunc::name_env_out(base_dir = fs::path(store_base, project))

  }
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(file = fs::dir_ls(path, regexp = track_file, recurse = TRUE, type = "file")
                  , exists = ifelse(length(file) > 0, TRUE, FALSE)
    )

  # not run ----
  not_run <- paths_df |>
    dplyr::filter(!exists) |>
    dplyr::pull(file)

  # completed run ----
  completed_run <- paths_df |>
    dplyr::filter(exists) |>
    dplyr::pull(file)

  #options(warning.length = 5000L)
  # stop ----
  if(length(not_run) & stop_if_not_run) stop(paste0("These context files have not been run in "
                                                    , project, ": \n")
                                             , stringr::str_flatten(not_run, collapse = "\n")
  )

  return(paths_df)

}
