#' Prepare _bookdown.yaml within a targets workflow
#'
#' @param report_dir Character. Where should the `_bookdown.yaml` file be saved?
#' @param store Character. Path to the targets store into which the report
#' should be saved
#' @param repo Path to github repository
#' @param rmd_files Character vector of .Rmd files to include in the bookdown.yaml file. If NULL (default), searches in report/ for any standard-named files (eg. 0010_intro.Rmd). index.Rmd is always included.
#'
#' @returns Path to saved `_bookdown.yaml`.
#' @export
#'
#' @examples
prepare_bookdown_yaml <- function(report_dir = "report"
                                  , store = tars$report$store
                                  , repo = usethis::git_remotes()$origin
                                  , output_dir = lifecycle::deprecated()
                                  , rmd_files = NULL) {

  if (lifecycle::is_present(output_dir)) {
    lifecycle::deprecate_warn(
      when = "2026-04-02",
      what = "prepare_bookdown_yaml(output_dir)",
      with = "render_with_deps(output_dir)",
      details = "when used with render_with_deps, the output_dir argument is passed via ... to bookdown::render_book()"
    )
  }

  out_file <- fs::path(report_dir, "_bookdown.yaml")

  proj <- basename(here::here())

  if(is.null(rmd_files)) {

    rmd_files <- fs::dir_ls(path = "report"
                            , regexp = "/\\d{4}.*Rmd$|index\\.Rmd"
                            ) |>
      basename() |>
      unname()

  } else {

    rmd_files <- rmd_files |>
      basename() |>
      unname()

  }

  ymlthis::yml_empty() |>
    ymlthis::yml_bookdown_opts(delete_merged_file = TRUE
                               , edit = paste0("https =//github.com/dew-landscapes/"
                                               , proj
                                               , "/edit/master/%s"
                                               )
                               , rmd_files = c(rmd_files[grepl("^index\\.Rmd$", rmd_files)]
                                               , rmd_files[!grepl("^index\\.Rmd$", rmd_files)]
                                               )
                               , repo = repo
                               ) |>
    yaml::write_yaml(out_file)

  return(out_file)

}
