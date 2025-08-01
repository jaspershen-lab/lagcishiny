#' Launch LaggedCor Shiny App
#'
#' @return A Shiny app object.
#' @export
#' @importFrom shiny shinyApp
laggedcor_shiny <- function() {
  shiny::shinyApp(ui = laggedcor_ui, server = laggedcor_server)
}

