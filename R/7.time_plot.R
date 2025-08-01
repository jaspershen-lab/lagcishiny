#' Time Plot Parameter Input UI
#'
#' Creates UI components to collect user-defined parameters for plotting time-series data.
#'
#' @param id The module ID for namespacing the UI elements.
#'
#' @return A `tagList` of UI elements including color picker, axis labels, and time settings.
#'
#' @importFrom shiny NS tagList h4 textInput numericInput checkboxInput
#' @importFrom colourpicker colourInput
#'
#' @keywords internal
#' @noRd
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


#' Time Plot Parameter Server
#'
#' A Shiny server module that collects and returns user-defined plotting parameters
#' for time-series visualization.
#'
#' @param id The module ID to namespace the server logic.
#'
#' @return A reactive list containing the plotting parameters:
#' \code{color}, \code{y_axis_name_1}, \code{y_axis_name_2},
#' \code{sun_rise_time}, \code{sun_set_time}, \code{time_gap},
#' \code{add_point}, and \code{facet}.
#'
#' @importFrom shiny moduleServer reactive
#'
#' @keywords internal
#' @noRd
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

#' Time Plot UI Module
#'
#' UI elements to display a dynamic plot title and a time-series plot.
#'
#' @param id The module ID for namespacing.
#'
#' @return A `tagList` containing a dynamic title (`uiOutput`) and a `plotOutput`.
#'
#' @importFrom shiny NS tagList uiOutput plotOutput
#'
#' @keywords internal
#' @noRd
time_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("dynamic_title")),
    plotOutput(ns("time_plot"))
  )
}


#' Time Plot Server Module
#'
#' Generates a time-series plot using user-specified parameters and uploaded data.
#'
#' @param id The module ID for namespacing the server logic.
#' @param data_reactive A reactive expression returning a data frame with a "time" column and one value column.
#' @param file_name A reactive expression returning the name of the uploaded file (used in title).
#' @param y_axis_param A string indicating which y-axis label to use (e.g., `"y_axis_name_1"`).
#' @param plot_params A reactive expression returning a list of plotting parameters from `time_plot_parameter_server()`.
#'
#' @return No return value. This module registers plot rendering in the UI.
#'
#' @importFrom shiny moduleServer renderPlot renderUI req
#'
#' @seealso \code{\link{plot_time_series}}, \code{\link{laggedcor::time_plot}}
#'
#' @keywords internal
#' @noRd
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

#' Render Time-Series Plot with Custom Parameters
#'
#' A helper function to generate a time-series plot using the `laggedcor::time_plot()` function.
#'
#' @param df A data frame containing at least two columns: one named `time`, and another numeric value column.
#' @param time_col A string specifying the name of the time column (typically "time").
#' @param value_col A string specifying the name of the value column to be plotted.
#' @param params A list of plot parameters returned from `time_plot_parameter_server()`.
#' @param y_axis_param A string name of the y-axis label field to use (e.g., `"y_axis_name_1"`).
#'
#' @return A `ggplot` object representing the time-series plot.
#'
#' @seealso \code{\link[laggedcor]{time_plot}}
#'
#' @keywords internal
#' @noRd
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


