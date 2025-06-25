#' Get the temporal grain from settings
#'
#' @param settings list. usually from
#' yaml::read_yaml("settings/setup.yaml")$grain
#'
#' @return Character. "year", "month" or "date"
#' @export
#'
#' @examples
extract_temporal_grain <- function(settings) {

  index <- which(grepl("Y|M|D", settings))

  if(index) {

    if(grepl("Y", settings[index])) {

      "year"

    } else if(grepl("M", settings[index])) {

      "month"

    } else if(grepl("D", settings[index])) {

      "date"

    }

  } else {

    stop("Need a simple time period within 'settings', e.g. 'P1Y' or 'P1M'")

  }

}
