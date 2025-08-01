export_plot_ui <- function(id, label = "Export Plot") {
  ns <- shiny::NS(id)
  
  tagList(
    actionButton(ns("open_export_modal"), label),
    downloadButton(ns("download_plot"), "Download", style = "display:none;")
  )
}


export_plot_server <- function(id, plot_expr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    export_settings <- reactiveValues(
      filename = NULL,
      format = NULL,
      width = NULL,
      height = NULL,
      dpi = NULL
    )
    
    # 弹出导出设置窗口
    observeEvent(input$open_export_modal, {
      showModal(modalDialog(
        title = "Export Plot",
        textInput(ns("file_name"), "File name (no extension)", value = "plot"),
        selectInput(ns("format"), "Format",
                    choices = c("pdf", "png", "jpeg", "jpg", "tiff", "svg", "eps", "bmp"),
                    selected = "png"
        ),
        numericInput(ns("width"), "Width (inches)", value = 6, min = 1),
        numericInput(ns("height"), "Height (inches)", value = 4, min = 1),
        numericInput(ns("dpi"), "DPI (only applies to raster formats)", value = 300, min = 72),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_export"), "Export", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })
    
    # 保存设置，触发下载
    observeEvent(input$confirm_export, {
      req(input$file_name, input$format, input$width, input$height, input$dpi)
      export_settings$filename <- input$file_name
      export_settings$format <- input$format
      export_settings$width <- input$width
      export_settings$height <- input$height
      export_settings$dpi <- input$dpi
      removeModal()
      shinyjs::click(ns("download_plot"))
    })
    
    # 下载 handler
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0(export_settings$filename, ".", export_settings$format)
      },
      content = function(file) {
        req(plot_expr())
        plot_obj <- plot_expr()
        
        # 使用 ggplot2::ggsave 进行保存
        ggsave(
          filename = file,
          plot = plot_obj,
          device = export_settings$format,
          width = export_settings$width,
          height = export_settings$height,
          dpi = export_settings$dpi
        )
      }
    )
  })
}
