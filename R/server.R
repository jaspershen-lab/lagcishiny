laggedcor_server <- function(input,output,session){
  
  uploaded_file_list <- upload_file_server("upload_file")
  
  data1 <- uploaded_file_list$file1$data
  data2 <- uploaded_file_list$file2$data
  name1 <- uploaded_file_list$file1$name
  name2 <- uploaded_file_list$file2$name
  
  
  start_preview <- reactiveVal(FALSE)
  
  observeEvent(input$preview, {
    message("✅ Preview button clicked")
    start_preview(TRUE)
  })
  
  time_plot_para <- time_plot_parameter_server("time_plot_parameters")
  
  plot_params <- reactive({
    req(start_preview())
    time_plot_para()
  })
  
  
  output$preview_outputs <- renderUI({
    req(start_preview())
    
    tagList(
      div(h3("Data Preview"), style = "text-align:center;"),
      fluidRow(
        column(6, data_preview_ui("preview_file_1")),
        column(6, data_preview_ui("preview_file_2"))
      ),
      div(h3("Time Plot"), style = "text-align:center;"),
      fluidRow(
        time_plot_ui("preview_file_1"),
        export_plot_ui("export_time_plot_1", label = "Export the Above Time Plot"),
        time_plot_ui("preview_file_2"),
        export_plot_ui("export_time_plot_2", label = "Export the Above Time Plot")
      )
    )
  })
  
  output$preview_outputs_report <- renderUI({
    req(start_preview())
    
    tagList(
      div(h3("Data Preview"), style = "text-align:center;"),
      fluidRow(
        column(6, data_preview_ui("preview_file_1")),
        column(6, data_preview_ui("preview_file_2"))
      ),
      div(h3("Time Plot"), style = "text-align:center;"),
      fluidRow(
        time_plot_ui("preview_file_1"),
        export_plot_ui("export_time_plot_1", label = "Export the Above Time Plot"),
        time_plot_ui("preview_file_2"),
        export_plot_ui("export_time_plot_2", label = "Export the Above Time Plot")
      )
    )
  })
  
  time_plot_obj_1 <- reactive({
    df <- data1()
    params <- plot_params()
    req(df, params)
    
    time_col <- "time"
    value_col <- setdiff(names(df), time_col)
    if (length(value_col) != 1) return(NULL)
    
    plot_time_series(
      df = df,
      time_col = time_col,
      value_col = value_col[[1]],
      params = params,
      y_axis_param = "y_axis_name_1"
    )
  })
  
  time_plot_obj_2 <- reactive({
    df <- data2()
    params <- plot_params()
    req(df, params)
    
    time_col <- "time"
    value_col <- setdiff(names(df), time_col)
    if (length(value_col) != 1) return(NULL)
    
    plot_time_series(
      df = df,
      time_col = time_col,
      value_col = value_col[[1]],
      params = params,
      y_axis_param = "y_axis_name_2"
    )
  })
  
  
  observe({
    req(start_preview())
    
    export_plot_server("export_time_plot_1", plot_expr = time_plot_obj_1)
    export_plot_server("export_time_plot_2", plot_expr = time_plot_obj_2)
  })
  
  
  observe({
    req(start_preview())
    
    data_preview_server("preview_file_1", data = data1, file_title = name1)
    data_preview_server("preview_file_2", data = data2, file_title = name2)
    
    time_plot_server("preview_file_1", 
                     y_axis_param = "y_axis_name_1",
                     data_reactive = data1, 
                     plot_params = plot_params,
                     file_name = name1)
    
    time_plot_server("preview_file_2", 
                     y_axis_param = "y_axis_name_2",
                     data_reactive = data2,
                     plot_params = plot_params,
                     file_name = name2)
  })
  
  calculation_params <- calculation_parameter_server("calculation")
  
  cor_result <- reactiveVal(NULL)
  
  # 监听点击 Run 按钮时执行计算
  observeEvent(input$run, {
    req(data1(), data2(), calculation_params)
    
    if (nrow(data1()) == 0 || nrow(data2()) == 0) {
      notify_error_shiny("❌ One of the uploaded data files has no rows.")
      return(NULL)
    }
    
    result <- calculate_laggedcor(
      df1 = data1(),
      df2 = data2(),
      params = calculation_params
    )
    
    cor_result(result)
    
    display_cor_summary_server("cor_summary", result = cor_result)
    
    output$cor_summary <- renderUI({
      display_cor_summary_ui("cor_summary")
    })
    
    output$cor_summary_report <- renderUI({
      
      display_cor_summary_ui("cor_summary")
    })
    
  })
  

  
  
  alignment_params <- alignment_plot_parameter_server("alignment_plot", result = cor_result)
  scatter_params <- scatter_plot_parameter_server("scatter_plot", result = cor_result)
  
  observeEvent(input$get_plot, {
    req(cor_result(), alignment_params(), scatter_params())
    
    output$plot_result_ui <- renderUI({
      tagList(
        div(h4("Evaluated Lagged Correlation Plot"), style = "text-align:center;"),
        plotOutput("eva_plot"),
        export_plot_ui("export_eva_plot", label = "Export Lagged Correlation Plot"),
        
        div(h4("Max Alignment Plot"), style = "text-align:center;"),
        plotOutput("max_align_plot"),
        export_plot_ui("export_max_align", label = "Export Max Align Plot"),
        
        div(h4("Max Scatter Plot"), style = "text-align:center;"),
        plotOutput("max_scatter_plot"),
        export_plot_ui("export_max_scatter", label = "Export Max Scatter Plot"),
        
        div(h4("Global Alignment Plot"), style = "text-align:center;"),
        plotOutput("global_align_plot"),
        export_plot_ui("export_global_align", label = "Export Global Align Plot"),
        
        div(h4("Global Scatter Plot"), style = "text-align:center;"),
        plotOutput("global_scatter_plot"),
        export_plot_ui("export_global_scatter", label = "Export Global Scatter Plot")
      )
    })
    
    output$plot_result_ui_report <- renderUI({
      tagList(
        div(h4("Evaluated Lagged Correlation Plot"), style = "text-align:center;"),
        plotOutput("eva_plot"),
        export_plot_ui("export_eva_plot", label = "Export Lagged Correlation Plot"),
        
        div(h4("Max Alignment Plot"), style = "text-align:center;"),
        plotOutput("max_align_plot"),
        export_plot_ui("export_max_align", label = "Export Max Align Plot"),
        
        div(h4("Max Scatter Plot"), style = "text-align:center;"),
        plotOutput("max_scatter_plot"),
        export_plot_ui("export_max_scatter", label = "Export Max Scatter Plot"),
        
        div(h4("Global Alignment Plot"), style = "text-align:center;"),
        plotOutput("global_align_plot"),
        export_plot_ui("export_global_align", label = "Export Global Align Plot"),
        
        div(h4("Global Scatter Plot"), style = "text-align:center;"),
        plotOutput("global_scatter_plot"),
        export_plot_ui("export_global_scatter", label = "Export Global Scatter Plot")
      )
    })
    
    # Max Alignment Plot
    max_align_plot_obj <- reactive({
      alignment_plot(result = cor_result(), params = alignment_params(), which = "max")
    })
    
    # Max Scatter Plot
    max_scatter_plot_obj <- reactive({
      scatter_plot(result = cor_result(), plot_params = scatter_params(), which = "max")
    })
    
    # Global Alignment Plot
    global_align_plot_obj <- reactive({
      alignment_plot(result = cor_result(), params = alignment_params(), which = "global")
    })
    
    # Global Scatter Plot
    global_scatter_plot_obj <- reactive({
      scatter_plot(result = cor_result(), plot_params = scatter_params(), which = "global")
    })
    
    # Evaluated Lagged Correlation Plot
    eva_plot_obj <- reactive({
      laggedcor::evaluate_lagged_cor(object = cor_result(), plot = TRUE)$plot
    })
    
    # Server bindings for export
    export_plot_server("export_max_align", plot_expr = max_align_plot_obj)
    export_plot_server("export_max_scatter", plot_expr = max_scatter_plot_obj)
    export_plot_server("export_global_align", plot_expr = global_align_plot_obj)
    export_plot_server("export_global_scatter", plot_expr = global_scatter_plot_obj)
    export_plot_server("export_eva_plot", plot_expr = eva_plot_obj)
    
    output$max_align_plot <- renderPlot({ max_align_plot_obj() })
    output$max_scatter_plot <- renderPlot({ max_scatter_plot_obj() })
    output$global_align_plot <- renderPlot({ global_align_plot_obj() })
    output$global_scatter_plot <- renderPlot({ global_scatter_plot_obj() })
    output$eva_plot <- renderPlot({ eva_plot_obj() })
    
  })
  
  report_download_server("report_download")
  
}