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


citation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::useShinyjs(),
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
