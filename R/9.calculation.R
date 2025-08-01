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

