#' Get the temporal grain from settings
#'
#' @param settings list. usually from
#' yaml::read_yaml("settings/setup.yaml")$grain
#'
#' @return Character. "year", "month" or "date"
#' @export
#'
#' @examples
extract_temporal_grain <- function(...) {

  lifecycle::deprecate_warn("2026-07-03"
                            , "envTargets::extract_temporal_grain()"
                            , "envFunc::extract_temporal_grain()"
                            )

  envFunc::extract_temporal_grain(...)

}
