#' Read a Single Uploaded File into a Data Frame
#'
#' This function reads a single uploaded file and attempts to parse it into a data frame.
#' It supports `.csv`, `.tsv`, `.xlsx`, `.xls`, and `.rda` formats.
#'
#' @param file A file input object as returned by `shiny::fileInput()`, typically `input$<id>`.
#'
#' @return A `data.frame` if the file is successfully read and valid; otherwise `NULL`.
#'
#' @details
#' - For `.csv` and `.tsv` files, `readr` is used.
#' - For Excel files (`.xlsx`, `.xls`), `readxl` is used.
#' - For `.rda` files, the function expects the first object in the file to be a `data.frame`.
#'
#' If the file cannot be read or is of an unsupported format, an error is reported using
#' \code{\link{notify_error_shiny}} and `NULL` is returned.
#'
#' @seealso \code{\link{read_two_files}}, \code{\link{notify_error_shiny}}
#'
#' @importFrom readr read_csv read_tsv
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#'
#' @keywords internal
#' @noRd
read_uploaded_file <- function(file) {
  ext <- tolower(tools::file_ext(file$datapath))
  filename <- file$name  # 获取上传文件原始名称
  
  result <- tryCatch({
    switch(ext,
           csv = readr::read_csv(file$datapath, show_col_types = FALSE),
           tsv = readr::read_tsv(file$datapath, show_col_types = FALSE),
           xlsx = readxl::read_excel(file$datapath),
           xls = readxl::read_excel(file$datapath),
           rda = {
             e <- new.env()
             load(file$datapath, envir = e)
             obj <- mget(ls(e), envir = e)
             df <- obj[[1]]
             if (!is.data.frame(df)) stop("RDA file must contain a data.frame.")
             df
           },
           {
             notify_error_shiny(paste0("❌ File \"", filename, "\" is of unsupported type: .", ext))
             return(NULL)
           }
    )
  }, error = function(e) {
    notify_error_shiny(paste0("❌ Failed to read file \"", filename, "\": ", e$message))
    return(NULL)
  })
  
  return(result)
}



#' Read and Validate Two Uploaded Files
#'
#' This function reads two uploaded files and returns their contents as a list of two data frames.
#' It uses \code{\link{read_uploaded_file}} internally for parsing.
#'
#' @param file1 The first uploaded file (from `input$file1`).
#' @param file2 The second uploaded file (from `input$file2`).
#'
#' @return A list of two data frames (`list(df1, df2)`) if both files are successfully read;
#' otherwise, `NULL` if either file fails.
#'
#' @details This function is typically used after validating that exactly two files have been uploaded.
#' It provides user-facing error messages if reading fails via \code{\link{notify_error_shiny}}.
#'
#' @seealso \code{\link{read_uploaded_file}}, \code{\link{notify_error_shiny}}
#'
#' @keywords internal
#' @noRd
read_two_files <- function(file1, file2) {
  df1 <- read_uploaded_file(file1)
  if (is.null(df1)) {
    notify_error_shiny(paste0("Failed to load File 1: ", file1$name))
    return(NULL)
  }
  
  df2 <- read_uploaded_file(file2)
  if (is.null(df2)) {
    notify_error_shiny(paste0("Failed to load File 2: ", file2$name))
    return(NULL)
  }
  
  return(list(df1, df2))
}

