#' Prepare a target for batched branching
#'
#' Helps to minimise long tails while processing batched data frames by taking
#' into account one of the columns (`col`) within the dataframe (usually `n`)
#' that is likely to be correlated with processing time. Remember to set
#' `iteration = "group"` within the target.
#'
#' @param df dataframe
#' @param col column to use when assigning rows to groups
#' @param batches How many groups to create?
#'
#' @returns `targets::tar_group()`ed dataframe
#' @export
#'
#' @examples
env_tar_group <- function(df
                          , col = "n"
                          , batches = envFunc::use_cores(absolute_max = yaml::read_yaml("settings/setup.yaml")$max_cores)
                          ) {

  arr <- df |>
    dplyr::arrange(desc(n))

  n_rows <- nrow(arr)

  res <- if(n_rows > use_cores) {

    arr |>
      dplyr::mutate(group = rep(1:use_cores, length.out = n_rows))

  } else {

    arr |>
      dplyr::mutate(group = dplyr::row_number())

  }

  res <- res |>
    dplyr::group_by(group) |>
    targets::tar_group()

  return(res)

}
