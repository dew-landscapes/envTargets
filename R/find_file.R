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

  match <- character(0)
  depth <- 0

  while(all(length(match) == 0, depth <= recurse_depth)) {

    depth <- depth + 1

    items <- fs::dir_ls(path
                        , recurse = depth
                        )

    match <- items[basename(items) == find]

  }

  if(length(match) == 0) warning("No matches to ", find, " in ", path, " with recurse_depth of ", recurse_depth)

  return(match)

}
