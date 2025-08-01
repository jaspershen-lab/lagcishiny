#' Report Download UI Module
#'
#' This function creates the user interface for downloading analysis reports
#' in different formats (PDF, HTML, or PowerPoint).
#'
#' @param id The module ID used to namespace the UI elements.
#'
#' @return A `tagList` containing a text input for the report name, a
#' format selector, and a download button.
#'
#' @importFrom shiny NS textInput selectInput downloadButton tagList
#'
#' @keywords internal
#' @noRd
report_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("report_name"), "Report Name", value = "my_report"),
    selectInput(ns("format"), "Format", choices = c("PDF" = "pdf", "HTML" = "html", "PowerPoint" = "pptx")),
    downloadButton(ns("download_report"), "Download Report")
  )
}

#' Report Download Server Module
#'
#' This server module handles the generation and download of dynamic reports
#' using a parameterized R Markdown template. It supports PDF, HTML, and PowerPoint output.
#'
#' @param id The module ID used to namespace the server elements.
#' @param data1 A reactive expression returning the first data frame.
#' @param data2 A reactive expression returning the second data frame.
#' @param plot_time A reactive expression returning a list of two ggplot objects for time plots.
#' @param result A reactive expression returning the result table or list to be included in the report.
#' @param scatter_max A reactive expression returning a ggplot object for the max scatter plot.
#' @param scatter_global A reactive expression returning a ggplot object for the global scatter plot.
#' @param align_max A reactive expression returning a ggplot object for the max alignment plot.
#' @param align_global A reactive expression returning a ggplot object for the global alignment plot.
#' @param eva_plot A reactive expression returning a ggplot object for the evaluation plot.
#'
#' @return This function is called for its side effects. It registers a download handler within the module.
#'
#' @importFrom shiny moduleServer downloadHandler
#' @importFrom rmarkdown render
#'
#' @keywords internal
#' @noRd
report_download_server <- function(id, 
                                   data1, data2, 
                                   plot_time, result,
                                   scatter_max, scatter_global,
                                   align_max, align_global,
                                   eva_plot) {
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
            plot_time1 = plot_time()[[1]],
            plot_time2 = plot_time()[[2]],
            scatter_max = scatter_max(),
            scatter_global = scatter_global(),
            align_max = align_max(),
            align_global = align_global(),
            eva_plot = eva_plot()
          ),
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}
