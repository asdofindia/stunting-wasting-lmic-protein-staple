library(readxl)

load_excel_with_check <- function(file_path) {
  # Load Data with empty row check
  tryCatch({
    # First, try to read without specifying header
    temp_data <- readxl::read_excel(file_path, col_names = FALSE)
    
    # Check if the first row is empty
    first_row_empty <- all(is.na(temp_data[1,5]))
    
    # Now read the data properly based on the check
    if (first_row_empty) {
      # Skip first row, use second row as header
      df <- readxl::read_excel(file_path, skip = 1, col_names = TRUE)
      cat("Using second row as headers (first row was empty)\n")
    } else {
      # Use first row as header
      df <- readxl::read_excel(file_path, col_names = TRUE)
      cat("Using first row as headers\n")
    }
    
    return(df)
  }, error = function(e) {
    stop(paste("Error reading Excel file:", e$message))
  })
}