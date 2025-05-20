
# from https://github.com/shirdekel/phd_thesis/commit/cc66e1b9d9305a4b21e8489836545ecc4475f9ee

##' @title Get dependencies
##'
##' For use in `render_with_deps()`

##' @return
##' @author Shir Dekel (modified by nw)
##' @export
get_deps <- function() {
  
  rmd <- fs::dir_ls(path = here::here("report")
                    , regexp = "Rmd$|rmd$|RMD$"
                    , recurse = TRUE
                    )
  
  lst(rmd)
  
}
