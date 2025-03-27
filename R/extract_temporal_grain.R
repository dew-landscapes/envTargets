#' Get the temporal grain from settings
#'
#' @param settings list. usually from yaml::read_yaml("settings/setup.yaml")
#'
#' @return Character. "year", "month" or "date"
#' @export
#'
#' @examples
extract_temporal_grain <- function(settings) {

  if(grepl("Y", settings$context$grain$res_time)) {

    "year"

  } else if(grepl("M", settings$context$grain$res_time)) {

    "month"

  } else if(grepl("D", settings$context$grain$res_time)) {

    "date"

  }

}
