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
      
      file_list_raw <- read_two_files(input$file1, input$file2)
      if (is.null(file_list_raw)) return(NULL)
      
      if (check_upload_file(file_list_raw)) {
        return(list(data_1 = file_list_raw[[1]], data_2 = file_list_raw[[2]]))
      } else {
        return(NULL)
      }
    })
    
    return(list(
      file1 = list(
        data = reactive({ req(file_list()); file_list()[["data_1"]] }),
        name = reactive({ req(input$file1); input$file1$name })
      ),
      file2 = list(
        data = reactive({ req(file_list()); file_list()[["data_2"]] }),
        name = reactive({ req(input$file2); input$file2$name })
      )
    ))
    
  })
}
