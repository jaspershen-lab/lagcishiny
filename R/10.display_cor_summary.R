#' Correlation Summary Display UI
#'
#' A UI module to display a textual summary of lagged correlation results.
#'
#' @param id The module ID used to namespace the UI elements.
#'
#' @return A `tagList` containing a section title and a `verbatimTextOutput` element.
#'
#' @importFrom shiny NS tagList h4 verbatimTextOutput
#'
#' @keywords internal
#' @noRd
display_cor_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::h4("Correlation Summary"),
    shiny::verbatimTextOutput(ns("cor_summary"))
  )
}

#' Correlation Summary Display Server
#'
#' This server module renders a comprehensive summary of lagged correlation analysis results,
#' including extracted metrics and evaluation scores.
#'
#' @param id The module ID used to namespace the server logic.
#' @param result A reactive expression returning the lagged correlation result object (from `calculate_lagci()`).
#'
#' @return No return value. This function is called for its side effects—registering a print output in the UI.
#'
#' @details The summary includes:
#' \itemize{
#'   \item Raw correlation result object.
#'   \item Maximum correlation and global correlation.
#'   \item All correlation values and corresponding p-values.
#'   \item Time shifts (numeric and string).
#'   \item Evaluation score from \code{lagci::evaluate_lagged_cor()}.
#' }
#'
#' @importFrom shiny moduleServer renderPrint req
#' @importFrom lagci extract_max_cor extract_global_cor extract_all_cor extract_all_cor_p extract_shift_time evaluate_lagged_cor
#'
#' @seealso \code{\link{calculate_lagci}}, \code{\link{evaluate_lagged_cor}}
#'
#' @keywords internal
#' @noRd
display_cor_summary_server <- function(id, result) {
  shiny::moduleServer(id, function(input, output, session) {
    output$cor_summary <- renderPrint({
      req(result())
      
      list(
        summary = result(),
        max_cor = lagci::extract_max_cor(result()),
        global_cor = lagci::extract_global_cor(result()),
        all_cor = lagci::extract_all_cor(result()),
        all_cor_p = lagci::extract_all_cor_p(result()),
        shift_time_numeric = lagci::extract_shift_time(result(), numeric = TRUE),
        shift_time_no_numeric = lagci::extract_shift_time(result(), numeric = FALSE),
        evaluated_lagged_cor = lagci::evaluate_lagged_cor(object = result(), plot = FALSE)$score
      )
    })
  })
}
