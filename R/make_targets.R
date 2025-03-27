
#' Make targets
#'
#' Runs `targets::tar_source()`, creates and then reads _targets.yaml from any
#' files matching the pattern ``.
#'
#' @param settings Character path to yaml settings file or named, nested list.
#' Passed to `set_list` argument of `envFunc::name_env_out()`. If character, the
#' named, nested list will be `yaml::read_yaml(settings)$context`
#' @param base_dir Character. Path at which to create the store.
#' @param target_pattern Character. Regexp to find targets ['project files']
#' (https://books.ropensci.org/targets/projects.html#project-files).
#'
#'
#' @return list of 'projects' each with elements 'script' and 'store'. Saves
#' _targets.yaml
#' @export
#'
#' @examples
make_targets <- function(settings = "settings/setup.yaml"
                         , base_dir = fs::path("..", "..", "out")
                         , target_pattern = "^\\d{3}_.*\\.R$"
                         ) {

  set_list <- if(is.character(settings)) yaml::read_yaml(settings)$context else settings

  # tars --------
  ## tars df ------
  tars_df <- tibble::tibble(script = fs::dir_ls(regexp = target_pattern)) |>
    dplyr::mutate(project = purrr::map_chr(script, \(x) gsub("\\d{3}_|\\.R", "", x))
                  , order = readr::parse_number(script)
                  , store = envFunc::store_dir(set_list = set_list
                                               , base_dir = base_dir
                                               )
                  , store = fs::path(store, project)
                  ) |>
    dplyr::arrange(order) |>
    dplyr::select(project, script, store)

  ## _targets.yaml -------
  if(file.exists("_targets.yaml")) fs::file_delete("_targets.yaml")

  purrr::pmap(tars_df
              , targets::tar_config_set
              )

  ## tars list ------
  tars <- yaml::read_yaml("_targets.yaml")

  return(tars)

}

