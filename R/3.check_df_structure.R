#' Validate Structure of Uploaded Data Frame
#'
#' This internal function checks whether a given data frame conforms to the expected
#' structure for time-series analysis. It ensures the input is a data frame with exactly
#' two columns, one of which must be named `"time"`. If the validation fails, an error
#' notification is triggered (in a Shiny app).
#'
#' @param df A data frame to be checked.
#' @param i An integer or string label used to identify the file (e.g., "1" or "2") in error messages.
#'
#' @return A logical value: \code{TRUE} if the data frame passes all checks, \code{FALSE} otherwise.
#'
#' @details This function is typically used after file upload to validate user-provided time-series data.
#' It relies on \code{notify_error_shiny()} to report errors if running inside a Shiny session.
#'
#' @seealso \code{\link{notify_error_shiny}} for error handling behavior.
#'
#' @keywords internal
#' @noRd
check_df_structure <- function(df, i) {
  if (!is.data.frame(df)) {
    notify_error_shiny(paste0("File No. ", i, " is invalid"))
    return(FALSE)
  }
  
  if (ncol(df) != 2) {
    notify_error_shiny(paste0("File No. ", i, " should have two columns, but it has ", ncol(df), " column(s)"))
    return(FALSE)
  }
  
  time_col_match <- names(df) == "time"
  if (sum(time_col_match) != 1) {
    notify_error_shiny(
      paste0("File No. ", i, " must contain exactly one column named 'time', but found ",
             sum(time_col_match), " column(s)")
    )
    return(FALSE)
  }
  
  return(TRUE)
}
