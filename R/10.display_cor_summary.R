display_cor_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::h4("Correlation Summary"),
    shiny::verbatimTextOutput(ns("cor_summary"))
  )
}

display_cor_summary_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    output$cor_summary <- renderPrint({
      req(result())
      
      list(
        summary = result(),
        max_cor = laggedcor::extract_max_cor(result()),
        global_cor = laggedcor::extract_global_cor(result()),
        all_cor = laggedcor::extract_all_cor(result()),
        all_cor_p = laggedcor::extract_all_cor_p(result()),
        shift_time_numeric = laggedcor::extract_shift_time(result(), numeric = TRUE),
        shift_time_no_numeric = laggedcor::extract_shift_time(result(), numeric = FALSE),
        evaluated_lagged_cor = laggedcor::evaluate_lagged_cor(object = result(), plot = FALSE)$score
      )
    })
  })
}
