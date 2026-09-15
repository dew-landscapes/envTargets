#' Check completion of an envSDMs run
#'
#' Checks for the existence of files in relevant taxa's output directories (or reads the 'boundary' target).
#'
#' @param sdm_store File path to sdm store. Used to read `toi`, if `taxa` argument is null.
#' @param pred_dir File path to where outputs (not targets) are stored. Used to check for the existence of relevant files.
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
                         pred_dir,
                         find_progress = c("boundary", "prep", "tune", "full_run", "pred", "thresh", "reproject"),
                         expected = FALSE,
                         return_log = FALSE,
                         taxa = NULL) {

  if(length(grain) > 1 || !grain %in% c("coarse", "fine")) stop("'grain' must be *one* of 'coarse' or 'fine'")

  if(any(!grepl(grain, pred_dir))) stop("pred_dir (.../", basename(pred_dir), ") does not match grain ('", grain, "')")

  toi <- tibble::tibble(
    toi = if(is.null(taxa)) {
      targets::tar_read(toi, store = gsub("sdm", "setup", sdm_store))
    } else taxa
  ) |>
    dplyr::mutate(tar_id = make_tar_id(toi))

  if(!is.null(taxa)) {
    pred_dir <- fs::dir_ls(pred_dir, regexp = paste(stringr::str_escape(taxa), collapse = "|"))
  }

  files <- purrr::set_names(find_progress) |>
    purrr::map(\(x) {

      df <- if(x == "boundary") {
        tibble::tibble(toi = list.files(pred_dir, recursive = TRUE,
                                        pattern = "boundary.parquet",
                                        full.names = TRUE) |>
                         dirname() |> basename(),
                       boundary = TRUE) |>
          dplyr::mutate(tar_id = make_tar_id(toi))

      } else if(x %in% c("prep", "tune", "full_run")) {
        tibble::tibble(toi = list.files(pred_dir, recursive = TRUE,
                                        pattern = paste0(x, "\\.rds$"),
                                        full.names = TRUE) |>
                         dirname() |> basename(),
                       !!rlang::ensym(x) := TRUE) |>
          dplyr::mutate(tar_id = make_tar_id(toi))


      } else if(x %in% c("pred", "thresh")) {
        tibble::tibble(toi = list.files(pred_dir, recursive = TRUE,
                                        pattern = paste0(".*__", x, "__.*\\.tif$"),
                                        full.names = TRUE) |>
                         basename() |>
                         gsub(paste0("__", x, ".*"), "\\1", x=_),
                       !!rlang::ensym(x) := TRUE) |>
          dplyr::mutate(tar_id = make_tar_id(toi))

      } else if(x == "reproject") {
        tibble::tibble(toi = list.files(pred_dir, recursive = TRUE,
                                        pattern = ".*__thresh_[0-9]{4}.*\\.tif$",
                                        full.names = TRUE) |>
                         basename() |>
                         gsub("__thresh.*", "\\1", x=_),
                       reproject = TRUE) |>
          dplyr::mutate(tar_id = make_tar_id(toi))
      }

      if(all(df$toi == df$tar_id)) {
        df |>
          dplyr::select(-toi) |>
          dplyr::left_join(toi)
      } else df

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
    dplyr::select(-logfile) |>
    dplyr::mutate(tar_id = make_tar_id(toi))

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
