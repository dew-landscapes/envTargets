#' Check completion of an envSDMs run
#'
#' Checks for the existence of files in relevant taxa's output directories (or reads the 'boundary' target).
#'
#' @param sdm_store File path to sdm store
#' @param grain "fine" or "coarse" - which output directory to search
#' @param find_progress Which files to check for
#' @param expected Logical; Count the number of records for each taxa to see if they are expected to produce an SDM (e.g. n records >6). Uses bio_geo_range for data.
#' @param return_log Logical; Return the pred log file as a list-column
#' @param taxa Optionally check for only specified taxa. If NULL (default), will `tar_read` toi from the setup store.
#'
#' @return Tibble with columns 'toi', and logical columns for each of `find_progress`, 'finished' (which checks for a pred .log file), and 'expected' (if TRUE).
#' @export
#'

sdm_progress <- function(sdm_store = tars$sdm$store,
                         grain = "fine",
                         find_progress = c("boundary", "prep", "tune", "full_run", "pred", "thresh"),
                         expected = FALSE,
                         return_log = FALSE,
                         taxa = NULL) {

  if(length(grain) > 1 || !grain %in% c("coarse", "fine")) stop("'grain' must be *one* of 'coarse' or 'fine'")

  toi <- tibble::tibble(
    toi = if(is.null(taxa)) {
      targets::tar_read(toi, store = gsub("sdm", "setup", sdm_store))
    } else taxa
  )

  pred_dir <- fs::path(sdm_store, paste0("sdm_", grain))
  if(!is.null(taxa)) {
    pred_dir <- fs::dir_ls(pred_dir, regexp = paste(taxa, collapse = "|"))
  }

  files <- purrr::map(find_progress,
             \(x) {

               if(x == "boundary") {
                 targets::tar_read(boundary, store = sdm_store) |>
                   dplyr::select('toi' = taxa, 'boundary' = exists) |>
                   dplyr::inner_join(toi)

               } else if(x %in% c("prep", "tune", "full_run")) {
                 tibble::tibble(toi = list.files(pred_dir, recursive = TRUE,
                                                 pattern = paste0(x, "\\.rds$"),
                                                 full.names = TRUE) |>
                                  dirname() |> basename(),
                                !!rlang::ensym(x) := TRUE)


               } else if(x %in% c("pred", "thresh")) {
                 tibble::tibble(toi = list.files(pred_dir, recursive = TRUE,
                                                 pattern = paste0(".*__", x, ".*\\.tif$"),
                                                 full.names = TRUE) |>
                                  gsub(paste0("__", x, ".*"), "\\1", x=_) |>
                                  basename(),
                                !!rlang::ensym(x) := TRUE)
               }
             }) |>
    purrr::compact()


  files[["finished"]] <- tibble::tibble(logfile = list.files(pred_dir, recursive = TRUE,
                                                  pattern = paste0(".*__pred.*\\.log$"),
                                                  full.names = TRUE)) |>
    dplyr::rowwise() |>
    dplyr::mutate(toi = gsub(paste0("__pred.*"), "\\1", logfile) |>
                    basename(),
                  log = list(readr::read_lines(logfile)),
                  finished = TRUE,
                  abandoned = grepl("abandoned", paste(log, collapse="\n")),
                  errored = grepl("Error", paste(log, collapse="\n"))) |>
    dplyr::select(-logfile)

  if(!return_log) {
    files$finished <- files$finished |>
      dplyr::select(-log)
  }


  if(expected == TRUE) {

    min_n <- targets::tar_read_raw(paste0("settings_", grain), store = sdm_store)$min_fold_n
    min_rel <- if(grain == "fine") 100 else 10000

    files[["expected"]] <- arrow::open_dataset(fs::path(gsub("sdm", "setup", sdm_store), "objects", "bio_geo_range")) |>
      dplyr::select(taxa, rel_metres_adj, pa, cell_lat, cell_long, year) |>
      dplyr::distinct() |>
      dplyr::right_join(toi, by = c("taxa" = "toi")) |>
      dplyr::filter(rel_metres_adj <= min_rel) |>
      dplyr::group_by(taxa) |>
      dplyr::add_count() |>
      dplyr::distinct(taxa, n) |>
      dplyr::collect() |>
      dplyr::mutate(expected = dplyr::case_when(n >= min_n ~TRUE,
                                                .default = FALSE)) |>
      dplyr::select('toi' = taxa, expected)

  }

  res <- purrr::reduce(files, dplyr::left_join, .init = toi) |>
    dplyr::arrange(toi)

  res

}
