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
