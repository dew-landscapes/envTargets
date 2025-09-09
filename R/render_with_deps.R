
#' Render bookdown and force Rmd file dependencies
#'
#' Moving the report to the targets store relies on output_dir being specified
#' in the _bookdown.yaml file, usually written within report_prep.R
#'
#' Enables a bookdown 'target' within a [targets workflow](https://books.ropensci.org/targets/).
#' I was unable to get [bookdowntargets](https://mps9506.r-universe.dev/bookdowntargets/doc/manual.html)
#' to work without error. That may be a better way to achieve this. Currently
#' (20250520) `deps` still need to be provided inside bookdown scripts (i.e. providing
#' `deps` to the function simply tells targets when the book needs to be remade,
#' it does not make those `deps` available inside the knit.)
#'
#' Based on code found here: https://github.com/shirdekel/phd_thesis/commit/cc66e1b9d9305a4b21e8489836545ecc4475f9ee
#'
#' @param input_directory Path in which `index.Rmd` can be found.
#' @param deps Any dependencies for the report. Note that these are not made
#' available within the knit, they just trigger the target to run.
#' @param remove_main Logical. Delete _main.Rmd froom the input directory before
#' knit?
#' @param clean_out_dir Logical. Delete the book before knit? Useful if chapter
#' names are changed, leaving orphaned `.html` files in the output directory.
#' @param clean_up Logical. Delete intermediary files after knit?
#'
#' @return Return value is just the `output_dir` specified in `_bookdown.yaml`,
#' or, if that is not specified, the value of `input_directory`. Files needed
#' for the book are written into the output directory.
#'
#' @author Shir Dekel (modified by nw)
#' @export
render_with_deps <- function(input_directory = "."
                             , deps
                             , remove_main = TRUE
                             , clean_out_dir = TRUE
                             , clean_up = TRUE
                             ) {

  output_dir <- yaml::read_yaml(fs::path(input_directory, "_bookdown.yaml"))$output_dir

  if(is.null(output_dir)) output_dir <- fs::path(input_directory, "_book")

  if(remove_main) {

    unlink(fs::path(input_directory, "_main.Rmd"))

  }

  if(clean_out_dir) {

    if(dir.exists(output_dir)) {

      fs::dir_delete(output_dir)

    }

  }

  xfun::in_dir(input_directory
               , bookdown::render_book(input = "index.Rmd"
                                       , config_file = "_bookdown.yaml"
                                       )
               )

  if(clean_up) {

    del_files <- fs::dir_info(input_directory
                              , regexp = "_main"
                              ) |>
      dplyr::filter(type == "file") |>
      dplyr::pull(path)

    if(length(del_files)) {

      fs::file_delete(del_file)

    }

    del_dirs <- fs::dir_info(input_directory
                             , regexp = "_book.*|_main.*"
                             ) |>
      dplyr::filter(type == "directory") |>
      dplyr::pull(path)

    if(length(del_dirs)) {

      fs::dir_delete(del_dirs)

    }

  }

  return(output_dir)

}
