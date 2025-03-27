#' Extract a minimum date to use in the project, based on the settings
#'
#' @param settings list. usually from yaml::read_yaml("settings/setup.yaml")
#' @param temporal_extent Character. Name of the temporal extent within
#' `settings`
#' @param max_year Numeric. Year. e.g. 2024. Default is the last full year via
#' `as.numeric(format(Sys.Date() - 365, "%Y"))`
#'
#' @return Character in date format. e.g. 1970-01-01
#' @export
#'
#' @examples
extract_min_date <- function(settings
                             , temporal_extent = "ext_time"
                             , max_year = as.numeric(format(Sys.Date() - 365, "%Y"))
                             ) {

  t_ext <- settings$context$extent[[temporal_extent]]
  min_year <- max_year - readr::parse_number(t_ext)
  paste0(min_year, "-01-01")

}
