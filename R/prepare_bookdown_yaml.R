#' Prepare _bookdown.yaml within a targets workflow
#'
#' @param report_dir Character. Where should the `_bookdown.yaml` file be saved?
#' @param store Character. Path to the targets store into which the report
#' should be saved
#' @param repo Path to github repository
#'
#' @returns Path to saved `_bookdown.yaml`.
#' @export
#'
#' @examples
prepare_bookdown_yaml <- function(report_dir = "report"
                                  , store = tars$report$store
                                  , repo = usethis::git_remotes()$origin
                                  , output_dir = lifecycle::deprecated()
                                  ) {

  if (lifecycle::is_present(output_dir)) {
    lifecycle::deprecate_warn(
      when = "2026-04-02",
      what = "prepare_bookdown_yaml(output_dir)",
      with = "render_with_deps(output_directory)"
    )
  }

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
                               ) |>
    yaml::write_yaml(out_file)

  return(out_file)

}
