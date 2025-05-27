
#' Make tars
#'
#' Runs `targets::tar_source()`, creates and then reads _targets.yaml from any
#' files matching the pattern ``.
#'
#' @param settings Character path to yaml settings file or named, nested list.
#' Passed to `set_list` argument of `envFunc::name_env_out()`. If character, the
#' named, nested list will be `yaml::read_yaml(settings)`. Needs named elements
#' `extent` and `grain` somewhere in the list.
#' @param store_base Character. Path at which to create the store.
#' @param target_pattern Character. Regexp to find targets ['project files']
#' (https://books.ropensci.org/targets/projects.html#project-files).
#' @param save_yaml Logical. Save `_targets.yaml` from within `make_tars()`?
#' @param local Logical. If `local`, the project names is not included within
#' each tars element.
#' @param list_names Character string of names used to extract elements of
#' `settings` to include in the store structure. Defaults to `extent` and
#' `grain`.
#'
#'
#' @return list of 'projects' each with elements 'script' and 'store'. Saves
#' _targets.yaml
#' @export
#'
#' @examples
make_tars <- function(settings = "settings/setup.yaml"
                      , store_base = fs::path("..", "..", "out")
                      , project_base = here::here()
                      , target_pattern = "\\d{3}_.*\\.R$"
                      , local = TRUE
                      , save_yaml = local
                      , list_names = c("extent", "grain")
                      ) {

  if(is.character(settings)) settings <- yaml::read_yaml(settings)

  set_list <- list_names |>
    purrr::map(\(x) envFunc::find_name(settings, x)) |>
    purrr::set_names(list_names)

  # tars --------
  ## tars df ------
  tars_df <- tibble::tibble(script = fs::dir_ls(project_base
                                                , regexp = target_pattern
                                                )
                            ) |>
    dplyr::mutate(project = purrr::map_chr(script, \(x) gsub("\\d{3}_|\\.R", "", basename(x)))
                  , order = readr::parse_number(basename(script))
                  , store = envTargets::store_dir(set_list = set_list
                                                  , base_dir = store_base
                                                  , project = basename(project_base)
                                                  )
                  , store = fs::path(store, project)
                  ) |>
    dplyr::arrange(order) |>
    dplyr::select(project, script, store)

  ## _targets.yaml -------
  if(! save_yaml) {

    out_yml <- tempfile(fileext = ".yaml")

    Sys.setenv(TAR_CONFIG = out_yml)

  } else {

    out_yml <- "_targets.yaml"

  }

  if(file.exists(out_yml)) fs::file_delete(out_yml)

  purrr::pmap(tars_df
              , targets::tar_config_set
              , config = out_yml
              )

  ## tars list ------
  tars <- yaml::read_yaml(out_yml)

  if(!local) tars <- list(tars) |>
    purrr::set_names(basename(project_base))

  return(tars)

}

