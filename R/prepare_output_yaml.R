#' Write _output.yaml for html and docx reports
#'
#' To select output format, use `output_format = "bookdown::bs4_book"` / `"bookdown::word_document2"` in [`envTargets::render_with_deps()`].
#'
#' @returns File paths (relative) to _output.yaml and style file (for bs4_book output).
#' @export
#'
#' @examples
prepare_output_yaml <- function() {

  if(grepl("\\/dev\\/", here::here())) {

    output_yaml_file <- fs::path("report", "_output.yaml")

    style_file <- fs::path("report", "style.css")

    writeLines('body::before {
      content: "DRAFT";
      position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%) rotate(-45deg);
      font-size: 10rem;
      color: rgba(0, 0, 0, 0.1); /* Adjust opacity here */
        z-index: 9999;
      pointer-events: none; /* Allows clicking and selecting text "underneath" */
    }'
               , style_file
    )


    yaml::write_yaml(
      list(
        `bookdown::bs4_book` = list(css = "style.css"),
        `bookdown::word_document2` = list(toc = FALSE,
                                          reference_docx = system.file("Styles.dotx", package = "envReport"),
                                          fig_caption = TRUE
        )
      ),
      output_yaml_file
    )


    return(c(style_file, output_yaml_file))

  } else {

    return(NULL)
  }

}
