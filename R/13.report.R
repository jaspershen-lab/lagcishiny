report_preview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(textOutput(ns("report_title")), align = "center"),
    textOutput(ns("report_time")),
    uiOutput(ns("preview_content"))
  )
}


report_preview_server <- function(id, data1, data2, plot_time, result) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$report_title <- renderText({
      "Report Preview"
    })
    
    output$report_time <- renderText({
      paste("Generated at:", Sys.time())
    })
    
    output$preview_content <- renderUI({
      req(data1(), data2(), result())
      tagList(
        h5("Data Preview"),
        DT::dataTableOutput(ns("preview_data1")),
        DT::dataTableOutput(ns("preview_data2")),
        h5("Time Plot"),
        plotOutput(ns("plot_time")),
        h5("Correlation Summary"),
        verbatimTextOutput(ns("summary_text"))
      )
    })
    
    output$preview_data1 <- DT::renderDataTable(head(data1(), 5), options = list(dom = 't'))
    output$preview_data2 <- DT::renderDataTable(head(data2(), 5), options = list(dom = 't'))
    output$plot_time <- renderPlot(plot_time())
    output$summary_text <- renderPrint({
      list(
        max_cor = laggedcor::extract_max_cor(result()),
        global_cor = laggedcor::extract_global_cor(result()),
        all_cor = laggedcor::extract_all_cor(result()),
        all_cor_p = laggedcor::extract_all_cor_p(result()),
        shift_time_numeric = laggedcor::extract_shift_time(result(), numeric = TRUE),
        shift_time_no_numeric = laggedcor::extract_shift_time(result(), numeric = FALSE),
        evaluated_lagged_cor = laggedcor::evaluate_lagged_cor(object = result, plot = FALSE)
      )
    })
  })
}



report_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("report_name"), "Report Name", value = "my_report"),
    selectInput(ns("format"), "Format", choices = c("PDF" = "pdf", "HTML" = "html", "PowerPoint" = "pptx")),
    downloadButton(ns("download_report"), "Download Report")
  )
}


report_download_server <- function(id, data1, data2, plot_time, result) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0(input$report_name, ".", input$format)
      },
      content = function(file) {
        temp_dir <- tempdir()
        rmd_template <- switch(input$format,
                               pdf = "pdf_template.Rmd",
                               html = "html_template.Rmd",
                               pptx = "slide_template.Rmd")
        
        file.copy(
          from = system.file("reports", rmd_template, package = "laggedcorAPP"),
          to = file.path(temp_dir, "report.Rmd"),
          overwrite = TRUE
        )
        
        rmarkdown::render(
          input = file.path(temp_dir, "report.Rmd"),
          output_format = switch(input$format,
                                 pdf = "pdf_document",
                                 html = "html_document",
                                 pptx = "powerpoint_presentation"),
          output_file = file,
          params = list(
            title = input$report_name,
            time = Sys.time(),
            data1 = data1(),
            data2 = data2(),
            result = result(),
            plot_time = plot_time()
          ),
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}
