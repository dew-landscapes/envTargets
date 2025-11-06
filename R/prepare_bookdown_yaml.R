#' Prepare _bookdown.yaml within a targets workflow
#'
#' @param report_dir Character. Where should the `_bookdown.yaml` file be saved?
#' @param store Character. Path to the targets store into which the report
#' should be saved
#' @param output_dir Character. Name to give the folder in `store` where the
#' report will be saved.
#'
#' @returns Path to saved `_bookdown.yaml`.
#' @export
#'
#' @examples
prepare_bookdown_yaml <- function(report_dir = "report"
                                  , store = tars$report$store
                                  , output_dir = "compiled_report"
                                  , repo = usethis::git_remotes()$origin) {

  out_file <- fs::path(report_dir, "_bookdown.yaml")

  proj <- basename(here::here())

  ymlthis::yml_empty() |>
    ymlthis::yml_bookdown_opts(delete_merged_file = TRUE
                               , edit = paste0("https =//github.com/dew-landscapes/"
                                               , proj
                                               , "/edit/master/%s"
                                               )
                               , rmd_files = c("index.Rmd"
                                               , fs::dir_ls(path = "report"
                                                            , regexp = "/\\d{4}.*Rmd$"
                                                            ) |>
                                                 gsub("report/", "", x = _) |>
                                                 unname()
                                               )
                               , repo = repo
                               , output_dir = fs::path(store |>
                                                         gsub("\\.\\.\\/\\.\\."
                                                              , "../../.."
                                                              , x = _
                                                              )
                                                       , output_dir
                                                       )
                               ) |>
    yaml::write_yaml(out_file)

  return(out_file)

}
