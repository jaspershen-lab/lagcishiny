scatter_plot_parameter_ui <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    h4("Scatter Plot Parameters"),
    checkboxInput(ns("hex"), "Use hex binning", value = TRUE),
    textInput(ns("x_name"), "X-axis name", value = "x"),
    textInput(ns("y_name"), "Y-axis name", value = "y")
  )
}

scatter_plot_parameter_server <- function(id,result) {
  moduleServer(id, function(input, output, session) {
    reactive({
      req(result())
      list(
        hex = input$hex,
        x_name = input$x_name,
        y_name = input$y_name
      )
    })
  })
}

alignment_plot_parameter_ui <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    h4("Alignment Plot Parameters"),
    textInput(ns("x_name"), label = "x name of alignment plot", value = "x"),
    textInput(ns("y_name"), label = "y name of alignment plot", value = "y"),
    colourpicker::colourInput(ns("x_color"), "Color for x series", value = "blue"),
    colourpicker::colourInput(ns("y_color"), "Color for y series", value = "red"),
    numericInput(ns("time_gap"), "Time gap (hour)", value = 4, min = 1),
    checkboxInput(ns("integrated"), "Integrated plot (x and y together)", value = FALSE),
    checkboxInput(ns("add_connect_line"), "Add connect line", value = FALSE),
    checkboxInput(ns("add_point"), "Add point", value = FALSE),
    uiOutput(ns("x_limit_ui")),
    numericInput(ns("x_point_size"), "Point size for x", value = 3, min = 0.1),
    numericInput(ns("y_point_size"), "Point size for y", value = 1, min = 0.1),
    numericInput(ns("non_matched_point_size"), "Point size for unmatched", value = 0.1, min = 0.01)
  )
}



alignment_plot_parameter_server <- function(id, result) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # 动态生成 x_limit 滑块（根据 result 计算时间点数量）
    output$x_limit_ui <- renderUI({
      req(result())
      
      total_points <- length(unique(c(result()@time1, result()@time2)))
      
      sliderInput(ns("x_limit"),
                  "x-axis limit (index range)",
                  min = 1,
                  max = total_points,
                  value = c(1, total_points))
    })
    
    # 返回 reactive 参数列表
    reactive({
      req(input$x_limit)
      list(
        x_name = input$x_name,
        y_name = input$y_name,
        x_color = input$x_color,
        y_color = input$y_color,
        time_gap = input$time_gap,
        integrated = input$integrated,
        add_connect_line = input$add_connect_line,
        add_point = input$add_point,
        x_limit = input$x_limit,
        x_point_size = input$x_point_size,
        y_point_size = input$y_point_size,
        non_matched_point_size = input$non_matched_point_size
      )
    })
  })
}


scatter_plot <- function(result, plot_params, which = c("max", "global")) {
  which <- match.arg(which)
  
  # 安全性判断
  if (is.null(plot_params$x_name) || is.null(plot_params$y_name)) {
    notify_error_shiny("Scatter plot parameters are not ready.")
    return(ggplot2::ggplot() + ggplot2::theme_void())  # 空图替代报错
  }
  
  laggedcor::lagged_scatter_plot(
    object = result,
    x_name = plot_params$x_name,
    y_name = plot_params$y_name,
    which = which,
    hex = plot_params$hex
  )
}



alignment_plot <- function(result, params, which = c("max", "global")) {
  which <- match.arg(which)
  
  if (is.null(params$y_name)) {
    notify_error_shiny("❌ y_name is NULL in alignment_plot params")
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  
  if (is.null(params$x_name)) {
    notify_error_shiny("❌ x_name is NULL in alignment_plot params")
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  
  laggedcor::lagged_alignment_plot(
    object = result,
    which = which,
    x_color = params$x_color,
    y_color = params$y_color,
    x_name = params$x_name,
    y_name = params$y_name,
    x_limit = params$x_limit,
    non_matched_point_size = params$non_matched_point_size,
    y_point_size = params$y_point_size,
    x_point_size = params$x_point_size,
    integrated = params$integrated,
    add_connect_line = params$add_connect_line,
    add_point = params$add_point,
    time_gap = params$time_gap
  )
}


#' General factory to create reactive plot objects
#' 
#' @param result A reactive expression that returns result object
#' @param params A reactive expression that returns plot parameters
#' @param plot_func A function that returns a ggplot object
#' @param which Either "max" or "global"
#' 
#' @return A reactive expression returning a ggplot object
#' @keywords internal
#' @noRd
make_plot_obj <- function(result, params, plot_func, which = c("max", "global")) {
  which <- match.arg(which)
  reactive({
    req(result(), params())
    tryCatch({
      plot_func(result(), params(), which = which)
    }, error = function(e) {
      notify_error_shiny(paste("Plot rendering failed:", e$message))
      ggplot2::ggplot() + ggplot2::theme_void()
    })
  })
}
