#' Data Preview UI Module
#'
#' This UI module displays a short preview of a dataset using a DataTable,
#' along with a dynamic title indicating the source file.
#'
#' @param id The module ID used to namespace the UI elements.
#'
#' @return A `tagList` containing a file title and a 5-row data preview table.
#'
#' @importFrom shiny NS tagList div uiOutput
#' @importFrom DT dataTableOutput
#'
#' @keywords internal
#' @noRd
data_preview_ui <- function(id){
  ns <- shiny::NS(id)
  
  tagList(
    div(uiOutput(ns("file_title")), style = "text-align:center;"),
    DT::dataTableOutput(ns("head_data"))
  )
}

#' Data Preview Server Module
#'
#' This server logic renders a short preview (first 5 rows) of a dataset
#' and displays the file name dynamically above the table.
#'
#' @param id The module ID used to namespace the server logic.
#' @param data A reactive expression returning the data frame to preview.
#' @param file_title A reactive expression returning the title (e.g., file name) to display above the table.
#'
#' @return This function is called for its side effects. It registers table and title outputs in the UI.
#'
#' @importFrom shiny moduleServer renderUI req
#' @importFrom DT renderDataTable
#'
#' @keywords internal
#' @noRd
data_preview_server <- function(id, data, file_title) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$file_title <- renderUI({
      req(file_title())
      h4(paste0(file_title(), " Preview"))
    })
    
    output$head_data <- DT::renderDataTable({
      req(data())
      head(data(), 5)
    }, options = list(dom = 't', pageLength = 5))
  })
}

