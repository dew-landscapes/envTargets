#' View targets metadata with progress
#'
#' `targets::tar_progress` only returns target names and status (skipped, completed, etc); `targets::tar_meta` returns
#' more metadata but no progress. This function combines the two and formats some columns to be more useful
#' (e.g. more human-readable sizes, with sensible units; separate date and time)
#'
#' @param store Path to the store to check (usually tars$blah$store)
#' @param incl_branches Include targets with "_branches" in the name (ie dynamic branches)? Default FALSE can greatly reduce table length.
#' @param keep_cols Extra columns to include from tar_meta.
#'
#' @returns A data frame with names and metadata of targets. Always returns name, date, time, seconds (runtime), size (with useful units), format, and warnings; add more with `keep_cols`.
#'
#' @seealso [`envShiny::tar_meta_opts`] in the options argument of `DT::datatable()` is useful for easy consistent output formatting in Shiny metadata tabs.
#'
#' @note Previously in envShiny.
#'
#' @export

get_tar_meta <- function(store,
                         incl_branches = FALSE,
                         keep_cols = NULL
                         ){

  prog <- targets::tar_progress(store = store)
  meta <- targets::tar_meta(store = store,
                            targets_only = TRUE) |>
    dplyr::arrange(time)

  prog_meta <- dplyr::left_join(meta, prog)

  if(!incl_branches) prog_meta <- prog_meta |>
    dplyr::filter(!grepl("_branches", name))

  res <- prog_meta |>
    dplyr::mutate(date = format(time, "%d-%b-%Y"), time = format(time, "%H:%M"),
                  seconds = round(seconds, 2),
                  size = paste0(fs::fs_bytes(bytes), "B")) |>
    dplyr::mutate(size = gsub("(?<=\\d)(?=[A-Za-z])", " ", size, perl = TRUE)) |>
    dplyr::select(name, progress, date, time, seconds, size, format, warnings,
                  dplyr::any_of(keep_cols))

  res
}
