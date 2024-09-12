validate_numeric_input <- function(input_value, min_value = -Inf, max_value = Inf, positive_only = FALSE) {
  if (is.na(as.numeric(input_value))) {
    return("Error: Input must be a numeric value.")
  }
  if (as.numeric(input_value) < min_value || as.numeric(input_value) > max_value) {
    return(paste0("Error: Input must be between ", min_value, " and ", max_value, "."))
  }
  if (positive_only && as.numeric(input_value) <= 0) {
    return("Error: Input must be a positive number.")
  }
  return(NULL)
}

validate_text_input <- function(input_value, min_length = 1, max_length = Inf) {
  input_value <- trimws(input_value)
  if (nchar(input_value) < min_length) {
    return(paste0("Error: Input must be at least ", min_length, " characters long."))
  }
  if (nchar(input_value) > max_length) {
    return(paste0("Error: Input must be no more than ", max_length, " characters long."))
  }
  return(NULL)
}