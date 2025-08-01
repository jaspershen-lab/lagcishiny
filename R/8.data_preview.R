data_preview_ui <- function(id){
  ns <- shiny::NS(id)
  
  tagList(
    div(uiOutput(ns("file_title")), style = "text-align:center;"),
    DT::dataTableOutput(ns("head_data"))
  )
}

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

