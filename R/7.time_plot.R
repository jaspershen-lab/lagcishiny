time_plot_parameter_ui <- function(id){
  ns <- shiny::NS(id)
  
  tagList(
    h4("Time Plot Parameters"),
    colourpicker::colourInput(ns("color"), "Line color (Hex or color name)", value = "blue"),
    textInput(ns("y_axis_name_1"), "Y-axis label of file 1", value = "Value_1"),
    textInput(ns("y_axis_name_2"), "Y-axis label of file 2", value = "Value_2"),
    textInput(ns("sun_rise_time"), "Sunrise time (hh:mm:ss)", value = "6:00:00"),
    textInput(ns("sun_set_time"), "Sunset time (hh:mm:ss)", value = "18:00:00"),
    numericInput(ns("time_gap"), "Time gap (hour)", value = 12, min = 1),
    checkboxInput(ns("add_point"), "Add points", value = FALSE),
    checkboxInput(ns("facet"), "Facet by day", value = FALSE)
  )
}

time_plot_parameter_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        color = input$color,
        y_axis_name_1 = input$y_axis_name_1,
        y_axis_name_2 = input$y_axis_name_2,
        sun_rise_time = input$sun_rise_time,
        sun_set_time = input$sun_set_time,
        time_gap = input$time_gap,
        add_point = input$add_point,
        facet = input$facet
      )
    })
  })
}


time_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("dynamic_title")),
    plotOutput(ns("time_plot"))
  )
}


#laggedcor需要能够检查数据的结构！！！！！！！！！！！！
time_plot_server <- function(id, data_reactive, file_name, y_axis_param, plot_params) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$dynamic_title <- renderUI({
      req(file_name())
      h4(paste0(file_name(), " Time Plot"), style = "text-align:center;")
    })
    
    output$time_plot <- renderPlot({
      df <- data_reactive()
      params <- plot_params()
      req(df, params)
      
      time_col <- "time"
      value_col <- setdiff(names(df), time_col)
      
      if (length(value_col) != 1) {
        notify_error_shiny("Data must contain exactly one column besides 'time'")
        return(NULL)
      }
      
      plot_time_series(
        df = df,
        time_col = time_col,
        value_col = value_col[[1]],
        params = params,
        y_axis_param = y_axis_param
      )
    })
  })
}

plot_time_series <- function(df, time_col, value_col, params, y_axis_param) {
  laggedcor::time_plot(
    x = df[[value_col]],
    time = df[[time_col]],
    color = params$color,
    y_axis_name = params[[y_axis_param]],
    sun_rise_time = params$sun_rise_time,
    sun_set_time = params$sun_set_time,
    time_gap = params$time_gap,
    add_point = params$add_point,
    facet = params$facet
  )
}


