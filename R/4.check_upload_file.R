
#' Validate Uploaded File List for Time-Series Analysis
#'
#' This function checks whether the uploaded file list contains exactly two data frames
#' and whether each of them passes structural validation (i.e., has two columns with
#' one named "time").
#'
#' @param file_list A list of data frames representing uploaded files.
#'
#' @return A logical value: \code{TRUE} if both files are valid and structurally correct, \code{FALSE} otherwise.
#'
#' @details This function is intended for validating the input at the upload stage in a Shiny app.
#' It first checks the number of uploaded files, and then validates the structure of each data frame
#' using \code{\link{check_df_structure}}. Errors are reported via \code{\link{notify_error_shiny}}.
#'
#' @seealso \code{\link{check_df_structure}}, \code{\link{notify_error_shiny}}
#'
#' @keywords internal
#' @noRd
check_upload_file <- function(file_list) {
  if (length(file_list) != 2) {
    notify_error_shiny(
      paste0("Please upload two files, now it has been uploaded ", length(file_list), " files")
    )
    return(FALSE)
  }
  
  for (i in seq_along(file_list)) {
    if (!check_df_structure(file_list[[i]], i)) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}
