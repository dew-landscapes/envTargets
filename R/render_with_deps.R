
#' Render bookdown and force Rmd file dependencies
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
#' @param input_directory Path in which `index.Rmd` can be found
#' @param output_directory Directory path. Where should the final results be
#' saved (this is usually not a targets store).
#' @param clean_up Logical. Delete intermediary _and_ `output_directory` _before_
#' knit and delete intermediary files _after_ knit.
#' @return The target is just the path `output_directory`. Files needed for the
#' book are written into `output_directory`.
#' @author Shir Dekel (modified by nw)
#' @export
render_with_deps <- function(input_directory = "."
                             , deps
                             , output_directory = NULL
                             , clean_up = TRUE
                             ) {

  if(clean_up) {

    unlink(c(fs::path(input_directory, "_main.Rmd")
             , output_directory
             )
           )

  }

  xfun::in_dir(input_directory
               , bookdown::render_book(input = fs::path(input_directory, "index.Rmd")
                                       , config_file = fs::path(input_directory, "_bookdown.yaml")
                                       )
               )

  fs::dir_copy(fs::path(input_directory, "_book")
               , output_directory
               , overwrite = TRUE
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

  return(output_directory)

}
