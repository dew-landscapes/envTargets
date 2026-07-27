#' Write _output.yaml for html and docx reports
#'
#' Use the resulting file paths within [`envTargets::render_with_deps()`] `output_yaml` argument, which is passed through to `[rmarkdown::render()]`
#'
#' @param output_html_file,output_docx_file Paths where yaml files will be saved. Set to NULL to not create a format's file.
#'
#' @returns File paths to where output yamls and style file are written.
#' @export
#'
#' @examples
prepare_output_yamls <- function(output_html_file = fs::path("report", "_output_html.yaml"),
                                 output_docx_file = fs::path("report", "_output_docx.yaml")
) {

  if(grepl("\\/dev\\/", here::here())) {

    # HTML
    style_file <- gsub(basename(output_html_file), "style.css", output_html_file)

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

    if(!is.null(output_html_file)) {
      writeLines('bookdown::bs4_book:
      css: style.css
              '
                 , output_html_file
      )
    }

    # Word
    docx_settings <- list(
      `bookdown::word_document2` = list(
        toc = FALSE,
        reference_docx = system.file("Styles.dotx", package = "envReport"),
        fig_caption = TRUE
      )
    )

    if(!is.null(output_docx_file)) {
      yaml::write_yaml(docx_settings, output_docx_file)
    }



    return(c(style_file, output_html_file, output_docx_file))

  } else {

    return(NULL)
  }

}
