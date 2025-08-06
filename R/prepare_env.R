
#' Prepare a tibble of environmental layers from settings
#'
#' @param set_list list. usually from yaml::read_yaml("settings/setup.yaml")
#' @param reg_exp Character. Used to limit returned files.
#' @param base_year Numeric. Default is
#' `as.numeric(format(Sys.Date(), "%Y")) - 10`
#'
#' @return tibble
#' @export
#'
#' @examples
prepare_env <- function(set_list
                        , reg_exp = "\\.tif"
                        , base_year = as.numeric(format(Sys.Date(), "%Y")) - 10
                        ) {

  envFunc::name_env_out(set_list
                        , base_dir = "I:/"
                        , reg_exp = reg_exp
                        , all_files = FALSE
                        ) %>%
    dplyr::pull(path) %>%
    envRaster::name_env_tif(parse = TRUE, skips = "base|DEW__SDM") %>%
    dplyr::mutate(start_date = as.Date(start_date)) %>%
    dplyr::left_join(envFunc::make_seasons(base_year, base_year
                                           , include_all = TRUE
                                           )$seasons
                     )

}






