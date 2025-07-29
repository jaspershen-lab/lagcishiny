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

