
#' Render bookdown and force Rmd file dependencies
#'
#' Moving the report to the targets store relies on output_directory being
#' specified, usually via the _bookdown.yaml file, itself usually written within
#' report_prep.R using `envTargets::prepare_bookdown_yaml()`.
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
#' @param input_directory Path in which `index.Rmd` can be found from `here::here()`.
#' Will almost certainly fail if `input_directory` is not found within `here::here()`.
#' @param output_directory Path (often a tars store) in which final output(s)
#' will be found. If `NULL` (default) `output_directory` will be
#' `yaml::read_yaml(here::here(input_directory, "_bookdown.yaml"))$output_directory` if that
#' `bookdown.yaml` exists, otherwise `here::here(input_directory, "_book")`.
#' @param deps Any dependencies for the report. Note that these are not made
#' available within the knit, they just trigger the target to run.
#' @param ... Arguments to boookdown::render_book, especially for output_format.
#'
#' @return Return value is just the `output_directory`. Files needed
#' for the book are written to the output directory.
#'
#' @author Shir Dekel (modified by nw)
#' @export
render_with_deps <- function(input_directory = "."
                             , output_directory = NULL
                             , deps = NULL
                             , ...
                             ) {

  input_directory <- here::here(input_directory)

  if(is.null(output_directory)) {

    if(file.exists(fs::path(input_directory, "_bookdown.yaml"))) {

      output_directory <- yaml::read_yaml(fs::path(input_directory, "_bookdown.yaml"))$output_dir

    } else {

      output_directory <- fs::path(input_directory, "_book")

    }

  }

  xfun::in_dir(input_directory
               , bookdown::render_book(input = "index.Rmd"
                                       , config_file = "_bookdown.yaml"
                                       , ...
                                       )
               )

  return(gsub("^\\.\\.\\/", "", output_directory))

}
