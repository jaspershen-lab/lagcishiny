#' Check Whether the Code Is Running in a Shiny Session
#'
#' This internal utility function determines whether the current R context is inside
#' a Shiny session. It checks for the availability of the \pkg{shiny} package and
#' whether a default reactive domain is active.
#'
#' @return A logical value: \code{TRUE} if running in a Shiny session, otherwise \code{FALSE}.
#'
#' @details This function is typically used to control behavior such as notification or UI
#' rendering depending on whether the code is running inside a Shiny app.
#'
#' @examples
#' is_shiny_session()
#'
#' @keywords internal
#' @noRd

is_shiny_session <- function() {
  requireNamespace("shiny", quietly = TRUE) &&
    !is.null(shiny::getDefaultReactiveDomain())
}
