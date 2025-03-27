#' Make a suitable targets 'id'
#'
#' @param char Character
#' @param replacement Character. What to use to replace punctuation and spaces?
#'
#' @return Character with punctuation or blanks replaced with `replacement`
#' @export
#'
#' @examples
make_tar_id <- function(char
                        , replacement = "_"
                        ) {

  gsub("[[:punct:][:blank:]]+", replacement, char)

}
