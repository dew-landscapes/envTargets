#' Search down through a directory until file is found
#'
#' @param path path to search
#' @param find file to find
#' @param recurse_depth maximum value passed to the recurse argument of
#' `fs::dir_ls()`
#'
#' @keywords internal
#' @returns Character
#' @export
#'
#' @examples
find_file <- function(path = "../../out"
                      , find = "_targets.yaml"
                      , recurse_depth = 4
                      ) {

  items <- fs::dir_ls(path
                      , recurse = recurse_depth
                      , regex = paste0(find, "$")
                      )

  if(length(items) == 0) warning("No matches to ", find, " in ", path, " with recurse_depth of ", recurse_depth)

  return(items)

}
