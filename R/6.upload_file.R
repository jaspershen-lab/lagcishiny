upload_file_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(ns("file1"), "Upload First File (.csv, .xlsx, .xls, .tsv, .rda)", 
                     accept = c(".csv", ".xlsx", ".xls", ".tsv", ".rda")),
    shiny::fileInput(ns("file2"), "Upload Second File (.csv, .xlsx, .xls, .tsv, .rda)", 
                     accept = c(".csv", ".xlsx", ".xls", ".tsv", ".rda")),
    actionButton(ns("load_example"), "Click to Use Example Data", icon = icon("flask"))
    
  )
}



upload_file_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 用 reactiveVal 来存储上传或示例数据
    file_list <- reactiveVal(NULL)
    
    # 处理用户上传的文件
    observeEvent({
      input$file1
      input$file2
    }, {
      req(input$file1, input$file2)
      
      file_list_raw <- read_two_files(input$file1, input$file2)
      if (is.null(file_list_raw)) return()
      
      if (check_upload_file(file_list_raw)) {
        file_list(list(data_1 = file_list_raw[[1]], data_2 = file_list_raw[[2]]))
      }
    }, ignoreInit = TRUE)
    
    
    # 加载示例数据
    observeEvent(input$load_example, {
      message("📁 Example data button clicked")
      
      tryCatch({
        e <- new.env()
        data("heart_data", package = "laggedcor", envir = e)
        data("step_data", package = "laggedcor", envir = e)
        
        df1 <- e$heart_data
        df2 <- e$step_data
        
        if (!check_upload_file(list(df1, df2))) return()
        
        file_list(list(data_1 = df1, data_2 = df2))
        notify_error_shiny("Now you can adjust the parameters for preview.")
      }, error = function(e) {
        notify_error_shiny(paste("❌ Failed to load example data:", e$message))
      })
    })
    
    # 返回数据与文件名（示例数据名也给默认值）
    return(list(
      file1 = list(
        data = reactive({ req(file_list()); file_list()[["data_1"]] }),
        name = reactive({
          if (!is.null(input$file1)) {
            input$file1$name
          } else {
            "heart_data.rda"
          }
        })
      ),
      file2 = list(
        data = reactive({ req(file_list()); file_list()[["data_2"]] }),
        name = reactive({
          if (!is.null(input$file2)) {
            input$file2$name
          } else {
            "step_data.rda"
          }
        })
      )
    ))
  })
}
