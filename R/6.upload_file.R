upload_file_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(ns("file1"), "Upload First File (.csv, .xlsx, .xls, .tsv, .rda)", 
                     accept = c(".csv", ".xlsx", ".xls", ".tsv", ".rda")),
    shiny::fileInput(ns("file2"), "Upload Second File (.csv, .xlsx, .xls, .tsv, .rda)", 
                     accept = c(".csv", ".xlsx", ".xls", ".tsv", ".rda"))
  )
}



upload_file_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    file_list <- shiny::reactive({
      req(input$file1, input$file2)
      
      file_list <- read_two_files(input$file1, input$file2)
      if (is.null(file_list)) return(NULL)
      
      if (check_upload_file(file_list)) {
        return(list(data_1 = file_list[[1]], data_2 = file_list[[2]]))
      } else {
        return(NULL)
      }
    })
    
    return(file_list)
  })
}
