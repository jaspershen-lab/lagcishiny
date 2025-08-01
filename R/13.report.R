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
