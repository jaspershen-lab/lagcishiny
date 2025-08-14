#' File Upload UI Module
#'
#' UI component that provides two file input selectors for uploading time-series data
#' and a button to load example datasets.
#'
#' @param id The module ID to create a namespace for the UI elements.
#'
#' @return A Shiny `tagList` containing two `fileInput` components and an example data button.
#'
#' @importFrom shiny NS tagList fileInput actionButton icon
#'
#' @keywords internal
#' @noRd
upload_file_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(ns("file1"), "Upload First File (.csv, .xlsx, .xls, .tsv, .rda)", 
                     accept = c(".csv", ".xlsx", ".xls", ".tsv", ".rda")),
    shiny::fileInput(ns("file2"), "Upload Second File (.csv, .xlsx, .xls, .tsv, .rda)", 
                     accept = c(".csv", ".xlsx", ".xls", ".tsv", ".rda")),
    actionButton(ns("load_example"), "Click to Use Example Data", icon = icon("flask")),
    downloadButton(ns("download_example"), "Download Example Data", icon = icon("download"))
    
  )
}


#' File Upload Server Module
#'
#' This server logic handles file uploads and example data loading. It validates file structure,
#' reads the content of uploaded files, and makes the data available for downstream processing.
#'
#' @param id The module ID to namespace the server logic.
#'
#' @return A list with two named elements `file1` and `file2`, each containing:
#' \describe{
#'   \item{data}{A reactive expression returning the data frame.}
#'   \item{name}{A reactive expression returning the file name (user-provided or default).}
#' }
#'
#' @details
#' - Uploaded files are validated using \code{\link{read_two_files}} and \code{\link{check_upload_file}}.
#' - Supported file types include `.csv`, `.tsv`, `.xlsx`, `.xls`, and `.rda`.
#' - Example datasets `heart_data` and `step_data` from the `laggedcor` package can be loaded using the example button.
#' - Errors are reported using \code{\link{notify_error_shiny}}.
#'
#' @seealso \code{\link{upload_file_ui}}, \code{\link{read_uploaded_file}}, \code{\link{check_upload_file}}, \code{\link{notify_error_shiny}}
#'
#' @importFrom shiny moduleServer reactiveVal observeEvent reactive req
#' @importFrom zip zipr
#' @importFrom readr write_csv

#'
#' @keywords internal
#' @noRd
upload_file_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 用 reactiveVal 来存储上传或示例数据
    file_list <- reactiveVal(NULL)
    
    # ------- 新增：下载示例数据（heart_data & step_data）-------
    output$download_example <- shiny::downloadHandler(
      filename = function() {
        paste0("laggedcor_example_data_", format(Sys.Date()), ".zip")
      },
      content = function(file) {
        tmpdir <- tempdir()
        f1 <- file.path(tmpdir, "heart_data.csv")
        f2 <- file.path(tmpdir, "step_data.csv")
        
        # 读取包内数据
        e <- new.env()
        tryCatch({
          data("heart_data", package = "laggedcor", envir = e)
          data("step_data",  package = "laggedcor", envir = e)
        }, error = function(err) {
          notify_error_shiny(paste0("❌ Failed to load example data from {laggedcor}: ", err$message))
          stop(err)
        })
        
        # 写出为 CSV
        tryCatch({
          readr::write_csv(e$heart_data, f1)
          readr::write_csv(e$step_data,  f2)
        }, error = function(err) {
          notify_error_shiny(paste0("❌ Failed to write CSV files: ", err$message))
          stop(err)
        })
        
        tryCatch({
          zip::zipr(zipfile = file, files = c(f1, f2))
        }, error = function(err) {
          notify_error_shiny(paste0("❌ Failed to create ZIP: ", err$message))
          stop(err)
        })
      }
    )
    
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
