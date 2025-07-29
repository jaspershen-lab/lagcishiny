#' Unified Error Notification Handler for Shiny and Console
#'
#' This internal utility function displays an error message appropriately depending
#' on whether the code is running inside a Shiny session or in a regular R console.
#'
#' @param msg A character string. The error message to display.
#'
#' @return None. This function is called for its side effect—displaying the error message.
#'
#' @details
#' - In a Shiny session, the function uses \code{shiny::showNotification()} to display the message.
#' - Outside of Shiny, it prints the message using \code{message()} with a ❌ prefix.
#'
#' This function is often used to report validation errors or data issues in a unified way.
#'
#' @seealso [shiny::showNotification()], [message()], [is_shiny_session()]
#'
#' @examples
#' notify_error_shiny("Something went wrong.")
#'
#' @keywords internal
#' @noRd

notify_error_shiny <- function(msg) {
  if (is_shiny_session()) {
    shiny::showNotification(msg, type = "error")
  } else {
    message("❌ ", msg)
  }
}
