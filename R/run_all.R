#' Run all targets stores for multiple settings contexts
#'
#' @param scales_yaml Character. Name of the `scales.yaml` file to obtain settings contexts.
#' Must be the same in both the current and upstream projects.
#' @param combos_df Data frame of combinations of the settings to be varied, where the columns are
#' the setting names and the rows are each combination of different values for each setting,
#' e.g. as produced by envTargets::make_context_combos. The setting names must correspond to those in
#' the settings for the `current_proj` element in the `scales_yaml`.
#' @param current_proj Current project name in which running this function and running multiple outputs,
#' e.g. 'envRegCont'.
#' @param upstream_proj Upstream project name required for the current project,
#' i.e. the precursor project that contains the combos of settings context directories within them,
#' e.g. 'envRange'.
#' @param current_track_file,upstream_track_file Names of files to use for tracking if a store
#' relating to a context combo has been run. Usually one of the last files created in the project/store,
#' and or one used downstream, e.g. 'grd_files' in the envRange project as the upstream file, and
#' 'reg_cont_tbl_tidy' in envRegCont if that project was the current. Otherwise, defaults to the
#' '_targets.yaml', as an indication the store has been run (although possibly not fully).
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
                    , combos_df
                    , current_proj = basename(here::here())
                    , upstream_proj
                    , upstream_track_file = "_targets.yaml"
                    , current_track_file = "_targets.yaml"
                    , force_new = TRUE
                    , run_file = "run.R"
                    , current_store_base = fs::path("..", "..", "out")
                    , upstream_store_base = fs::path("..", "..", "out")

) {

  # settings ----
  ## scales file ----
  scales_file <- find_file(path = here::here(), find = scales_yaml, recurse_depth = 1)

  ## all projects ----
  # all project settings in scales yaml
  settings_all <- yaml::read_yaml(scales_file)

  ## current project ----
  settings_cur_proj <- envFunc::extract_scale(element = current_proj
                                              , scales = scales_file
  )

  ## df ----
  # current & upstream projects
  # for adding non-varied settings below
  c(current_proj, upstream_proj) |>
    purrr::set_names(c("all_cur_set", "all_up_set")) |>
    purrr::map(\(p) {

      envFunc::extract_scale(element = p
                             , scales = scales_file
      ) |>
        purrr::list_flatten(name_spec = "{inner}") |>
        purrr::map(\(x) ifelse(is.null(x), NA, x)) |> # convert NULL to NA, otherwise dplyr::bind_cols drops the NULL elements and lose those columns below
        dplyr::bind_cols()

    }
    ) |>
    list2env(envir = .GlobalEnv)

  # check upstream ----
  ## upstream combos ----
  upstream_combos_df <- combos_df |>
    dplyr::select(tidyr::any_of(names(all_up_set))) |>
    dplyr::distinct()

  # check if relevant upstream project files exist & stop if they don't
  find_context_files(project = upstream_proj
                     , scales_yaml = "scales.yaml"
                     , combos_df = upstream_combos_df
                     , track_file = upstream_track_file
                     , stop_if_not_run = TRUE
                     , store_base = upstream_store_base
  )

  # find contexts to run ----
  # only relevant if force_new = FALSE
  contexts_to_run <- find_context_files(project = current_proj
                                        , scales_yaml = "scales.yaml"
                                        , combos_df = combos_df
                                        , track_file = current_track_file
                                        , stop_if_not_run = FALSE
                                        , store_base = current_store_base
  ) %>%
    {if(!force_new) dplyr::filter(., !exists) else .}

  # run all contexts ----
  ## add non-varied settings ----
  current_combos_df <- combos_df |>
    dplyr::bind_cols(all_cur_set |>
                       dplyr::select(-names(combos_df))
    )

  ## run all ----
  if(force_new|nrow(contexts_to_run)) {

    purrr::walk(1:nrow(current_combos_df), \(a) {

      new_set <- names(settings_cur_proj) |>
        purrr::set_names() |>
        purrr::map(\(x) {

          cols <- names(envFunc::extract_scale()[[x]])

          elements <- current_combos_df |>
            dplyr::slice(a) |>
            dplyr::select(tidyr::all_of(cols)) |>
            as.list() |>
            purrr::map(\(l) if(is.na(l)) NULL else l)

          set_list <- list()

          set_list[[x]] <- list() |>
            c(elements)

        }
        )

      settings_all[[current_proj]] <- new_set

      yaml::write_yaml(settings_all, scales_file)

      source(run_file)

    }
    )

  } else {

    warning("No new contexts and force_new = FALSE, so no contexts were run.")

  }

}
