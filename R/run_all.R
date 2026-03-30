#' Run all targets stores for multiple settings contexts
#'
#' @param scales_yaml Character. Name of the `scales.yaml` file to obtain settings contexts.
#' Must be the same in both the current and upstream projects.
#' @param current_combos_df,upstream_combos_df Data frames of settings context combinations
#' for all the settings contexts, where the columns are the settings and the rows are each
#' combination of different values for each setting, e.g. as produced by
#' envTargets::make_context_combos.
#' @param current_proj Current project name in which running this function and running multiple outputs,
#' e.g. 'envRegCont'. Used for checking if existing `track_file`s exist.
#' @param upstream_proj Upstream project name required for the current project,
#' i.e. the precursor project that contains the combos of settings context directories within them,
#' e.g. 'envRange'.
#' @param current_track_file,upstream_track_file Names of files to use for tracking if a store relating to a context
#' combo has been run. Usually one of the last files created in the project/store, and or one used downstream.
#' @param force_new Logical. Force new runs of stores corresponding to the context combos if they exist?
#' @param run_file Name of file that contains the targets stores and code to create them corresponding to
#' the current project.
#' @param current_store_base,upstream_store_base Base directory for the current and upstream targets stores,
#' e.g. "../../out" or "projects/data".
#'
#' @return Executes the run file for all the combinations of settings specified.
#'
#' @export
#'
#' @examples
#'
run_all <- function(scales_yaml = "scales.yaml"
                    , current_combos_df
                    , upstream_combos_df
                    , current_proj = basename(here::here())
                    , upstream_proj = "envRange"
                    , upstream_track_file = "grd_files"
                    , current_track_file = "reg_cont_tbl_tidy"
                    , force_new = TRUE
                    , run_file = "run.R"
                    , current_store_base = fs::path("..", "..", "out")
                    , upstream_store_base = fs::path("..", "..", "out")

) {

  # load settings ----
  scales_file <- find_file(path = here::here(), find = scales_yaml, recurse_depth = 1)

  settings <- envFunc::extract_scale(element = project
                                     , scales = scales_file
  )

  # check upstream ----
  # check if relevant upstream project files exist & stop if they don't
  find_context_files(project = upstream_proj
                     , scales_yaml = "scales.yaml"
                     , combos_df = upstream_combos_df
                     , track_file = upstream_track_file
                     , stop_if_not_run = TRUE
                     , store_base = upstream_store_base
  )

  # find contexts to run ----
  contexts_to_run <- find_context_files(project = current_proj
                                        , scales_yaml = "scales.yaml"
                                        , combos_df = current_combos_df
                                        , track_file = current_track_file
                                        , stop_if_not_run = FALSE
                                        , store_base = current_store_base
  ) %>%
    {if(!force_new) dplyr::filter(., !exists) else .}

  # run all contexts ----
  if(force_new|nrow(contexts_to_run)) {

    purrr::walk(1:nrow(current_combos_df), \(a) {

      new_set <- names(settings) |>
        purrr::set_names() |>
        purrr::map(\(x) {

          cols <- names(envFunc::extract_scale()[[x]])

          elements <- combos_df |>
            dplyr::slice(a) |>
            dplyr::select(tidyr::all_of(cols)) |>
            as.list() |>
            purrr::map(\(l) if(is.na(l)) NULL else l)

          set_list <- list()

          set_list[[x]] <- list() |>
            c(elements)

        }
        )

      yaml::write_yaml(new_set, scales_file)

      source(run_file)

    }
    )

  } else {

    warning("No new contexts and force_new = FALSE, so no contexts were run.")

  }

}
