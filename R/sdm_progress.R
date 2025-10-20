#' Check completion of an envSDMs run
#'
#' Checks for the existence of files in relevant taxa's output directories (or reads the 'boundary' target).
#'
#' @param sdm_store
#' @param grain "fine" or "coarse" - which output directory to search
#' @param find_progress Which files to check for (skipping boundary can )
#' @param taxa Optionally check for only specified taxa. If NULL (default), will `tar_read` toi from the setup store.
#'
#' @return Tibble with columns 'toi', each of `find_progress`, and 'finished' (which checks for a pred .log file)
#' @export
#'

sdm_progress <- function(sdm_store = tars$sdm$store,
                         grain = "fine",
                         find_progress = c("boundary", "prep", "tune", "full_run", "pred", "thresh"),
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

  files[["finished"]] <- tibble::tibble(toi = list.files(pred_dir, recursive = TRUE,
                                                         pattern = paste0(".*__pred.*\\.log$"),
                                                         full.names = TRUE) |>
                                          gsub(paste0("__pred.*"), "\\1", x=_) |>
                                          basename(),
                                        finished = TRUE)

  res <- purrr::reduce(files, dplyr::left_join, .init = toi) |>
    dplyr::arrange(toi)

  res

}
