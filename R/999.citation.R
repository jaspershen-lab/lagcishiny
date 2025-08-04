bibtex_entry <- "@ARTICLE{Shen2019-xv,
  title     = \"Metabolic reaction network-based recursive metabolite annotation
               for untargeted metabolomics\",
  author    = \"Shen, Xiaotao and Wang, Ruohong and Xiong, Xin and Yin, Yandong
               and Cai, Yuping and Ma, Zaijun and Liu, Nan and Zhu, Zheng-Jiang\",
  journal   = \"Nat. Commun.\",
  publisher = \"Springer Science and Business Media LLC\",
  volume    =  10,
  number    =  1,
  pages     = \"1516\",
  month     =  apr,
  year      =  2019,
  copyright = \"https://creativecommons.org/licenses/by/4.0\",
  language  = \"en\"
}"

#' Citation Display and Copy UI
#'
#' UI module for displaying a BibTeX citation and allowing users to copy it
#' to the clipboard with a single click.
#'
#' @param id Module ID used to namespace the UI elements.
#'
#' @return A `tagList` with a "Copy BibTeX" button and formatted BibTeX output.
#'
#' @details This module injects a small JavaScript function using `navigator.clipboard`
#' to support clipboard copying in modern browsers.
#'
#' @importFrom shinyjs useShinyjs runjs
#' @importFrom shiny tags
#'
#' @keywords internal
#' @noRd
citation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$script(shiny::HTML(sprintf("
      function copyBibtex_%s() {
        var text = document.getElementById('%s').innerText;
        navigator.clipboard.writeText(text).then(function() {
          alert('Copied!');
        });
      }
    ", id, paste0(id, "-bibtex_output")))),
    
    shiny::actionButton(ns("copy_btn"), "Copy BibTeX"),
    shiny::verbatimTextOutput(ns("bibtex_output"))
  )
}

#' Citation Display and Copy Server
#'
#' Server module for rendering a BibTeX entry and enabling copy-to-clipboard
#' functionality via JavaScript.
#'
#' @param id Module ID used to namespace the server logic.
#' @param bibtex_entry A character string containing a BibTeX-formatted citation.
#'
#' @return No return value. This function registers outputs and handlers in the Shiny server.
#'
#' @importFrom shiny moduleServer renderPrint observeEvent
#' @importFrom shinyjs runjs
#'
#' @keywords internal
#' @noRd
citation_server <- function(id, bibtex_entry) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$bibtex_output <- shiny::renderPrint({
      cat(bibtex_entry)
    })
    
    shiny::observeEvent(input$copy_btn, {
      shinyjs::runjs(sprintf("copyBibtex_%s();", id))
    })
  })
}
