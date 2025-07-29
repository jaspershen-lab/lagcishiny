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
