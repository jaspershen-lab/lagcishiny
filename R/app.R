#' Launch lagci Shiny App
#'
#' @return A Shiny app object.
#' @export
#' @importFrom shiny shinyApp
lagci_shiny <- function() {
  shiny::shinyApp(ui = lagci_ui, server = lagci_server)
}

