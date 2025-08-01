#' Correlation Calculation Parameter UI
#'
#' UI module for collecting user-defined parameters for lagged correlation analysis.
#'
#' @param id The module ID for namespacing the UI elements.
#'
#' @return A `tagList` containing numeric and select input controls for correlation settings.
#'
#' @importFrom shiny NS tagList numericInput selectInput
#'
#' @keywords internal
#' @noRd
calculation_parameter_ui <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    numericInput(ns("time_tol"),
                 "Time tolerance (hour)",
                 value = 0.2, min = 0),
    numericInput(ns("step"), 
                 "Step size (hour)", 
                 value = 2/60, min = 0),
    numericInput(ns("min_match"), 
                 "Min matched samples", 
                 value = 10, min = 0),
    selectInput(ns("cor_method"), 
                "Correlation method", 
                choices = c("spearman", "pearson"),
                selected = "spearman"),
    selectInput(ns("align_method"),
                label = "Select interpolation method for alignment:",
                choices = c("linear", "constant"),
                selected = "constant")
  )
}

#' Correlation Calculation Parameter Server
#'
#' Server-side module that collects user-defined numeric and categorical inputs
#' required for lagged correlation calculation.
#'
#' @param id The module ID to namespace the server logic.
#'
#' @return A named list of reactive expressions for:
#' \describe{
#'   \item{time_tol}{Numeric input for time tolerance (hours).}
#'   \item{step}{Numeric input for step size (hours).}
#'   \item{min_match}{Minimum number of matched samples required.}
#'   \item{cor_method}{Correlation method: "spearman" or "pearson".}
#'   \item{align_method}{Interpolation method for alignment: "linear" or "constant".}
#' }
#'
#' @importFrom shiny moduleServer reactive
#'
#' @keywords internal
#' @noRd
calculation_parameter_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    list(
      time_tol = reactive(input$time_tol),
      step = reactive(input$step),
      min_match = reactive(input$min_match),
      cor_method = reactive(input$cor_method),
      align_method = reactive(input$align_method)
    )
  })
}

#' Run Lagged Correlation Calculation
#'
#' This function runs lagged correlation analysis between two time-series data frames,
#' using the parameters collected from a Shiny input module.
#'
#' @param df1 A data frame containing a "time" column and one value column.
#' @param df2 A second data frame with the same structure as `df1`.
#' @param params A list of reactive expressions returned by `calculation_parameter_server()`.
#'
#' @return A data frame or list as returned by \code{laggedcor::calculate_lagged_correlation()}.
#' If the input data is invalid or empty, `NULL` is returned with an error notification.
#'
#' @details
#' - The function auto-detects which dataset is shorter and aligns accordingly.
#' - It performs checks to ensure structure validity and non-empty data before calculation.
#'
#' @importFrom laggedcor calculate_lagged_correlation
#' @importFrom shiny req
#'
#' @seealso \code{\link{calculation_parameter_ui}}, \code{\link{notify_error_shiny}}
#'
#' @keywords internal
#' @noRd
calculate_laggedcor <- function(df1, df2, params) {
  stopifnot(is.data.frame(df1), is.data.frame(df2))
  stopifnot(ncol(df1) == 2, ncol(df2) == 2)
  
  time_col <- "time"
  value_col_1 <- setdiff(names(df1), time_col)
  value_col_2 <- setdiff(names(df2), time_col)
  
  x <- if (nrow(df1) <= nrow(df2)) df1[[value_col_1]] else df2[[value_col_2]]
  y <- if (nrow(df1) <= nrow(df2)) df2[[value_col_2]] else df1[[value_col_1]]
  
  if (length(x) == 0 || length(y) == 0) {
    notify_error_shiny("❌ Either x or y column is empty, check your data.")
    return(NULL)
  }
  
  time1 <- if (nrow(df1) <= nrow(df2)) df1[[time_col]] else df2[[time_col]]
  time2 <- if (nrow(df1) <= nrow(df2)) df2[[time_col]] else df1[[time_col]]
  
  laggedcor::calculate_lagged_correlation(
    x = x,
    y = y,
    time1 = time1,
    time2 = time2,
    time_tol = params$time_tol(),
    step = params$step(),
    min_matched_sample = params$min_match(),
    align_method = params$align_method(),
    cor_method = params$cor_method()
  )
}

