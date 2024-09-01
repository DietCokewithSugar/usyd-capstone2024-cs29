library(testthat)

test_that("validate_numeric_input returns correct errors and NULL", {
  
  # Test case 1: Non-numeric input
  # Expectation: The function should return an error message stating that the input must be numeric.
  expect_equal(validate_numeric_input("abc"), "Error: Input must be a numeric value.")
  
  # Test case 2: Input with special characters
  # Expectation: The function should return an error message stating that the input must be numeric.
  expect_equal(validate_numeric_input("@#$%"), "Error: Input must be a numeric value.")
  
  # Test case 3: Input with numeric characters mixed with special characters
  # Expectation: The function should return an error message stating that the input must be numeric.
  expect_equal(validate_numeric_input("123@#"), "Error: Input must be a numeric value.")
  
  # Test case 4: Input with special characters in a string of numbers
  # Expectation: The function should return an error message stating that the input must be numeric.
  expect_equal(validate_numeric_input("1.23e2@"), "Error: Input must be a numeric value.")
  
  # Test case 5: Input less than the minimum value
  # Expectation: The function should return an error message indicating that the input is below the allowed minimum.
  expect_equal(validate_numeric_input(5, min_value = 10), "Error: Input must be between 10 and Inf.")
  
  # Test case 6: Input greater than the maximum value
  # Expectation: The function should return an error message indicating that the input exceeds the allowed maximum.
  expect_equal(validate_numeric_input(20, max_value = 15), "Error: Input must be between -Inf and 15.")
  
  # Test case 7: Negative input when only positive numbers are allowed
  # Expectation: The function should return an error message indicating that only positive numbers are accepted.
  expect_equal(validate_numeric_input(-5, positive_only = TRUE), "Error: Input must be a positive number.")
  
  # Test case 8: Input is zero when only positive numbers are allowed
  # Expectation: The function should return an error message indicating that the input must be positive.
  expect_equal(validate_numeric_input(0, positive_only = TRUE), "Error: Input must be a positive number.")
  
  # Test case 9: Input is exactly zero, with positive_only = FALSE
  # Expectation: The function should return NULL, indicating that zero is valid input when positive_only is FALSE.
  expect_equal(validate_numeric_input(0, positive_only = FALSE), NULL)
  
  # Test case 10: Input is within the specified range
  # Expectation: The function should return NULL, indicating that the input is valid.
  expect_equal(validate_numeric_input(5, min_value = 1, max_value = 10), NULL)
  
  # Test case 11: Positive input when positive numbers are required
  # Expectation: The function should return NULL, indicating that the input is valid.
  expect_equal(validate_numeric_input(5, positive_only = TRUE), NULL)
  
  # Test case 12: Input equal to the minimum value
  # Expectation: The function should return NULL, indicating that the input is valid.
  expect_equal(validate_numeric_input(10, min_value = 10), NULL)
  
  # Test case 13: Input equal to the maximum value
  # Expectation: The function should return NULL, indicating that the input is valid.
  expect_equal(validate_numeric_input(15, max_value = 15), NULL)
  
  # Test case 14: Very large input value
  # Expectation: The function should return NULL if within range, otherwise an appropriate error message.
  expect_equal(validate_numeric_input(1e+100, max_value = 1e+150), NULL)
  expect_equal(validate_numeric_input(1e+151, max_value = 1e+150), "Error: Input must be between -Inf and 1e+150.")
  
  # Test case 15: Positive infinity as input
  # Expectation: The function should return an error message indicating that the input exceeds the allowed maximum.
  expect_equal(validate_numeric_input(Inf, max_value = 100), "Error: Input must be between -Inf and 100.")
  
  # Test case 16: Negative infinity as input
  # Expectation: The function should return an error message indicating that the input is below the allowed minimum.
  expect_equal(validate_numeric_input(-Inf, min_value = -100), "Error: Input must be between -100 and Inf.")
  
  # Test case 17: Input is whitespace string
  # Expectation: The function should return an error message stating that the input must be numeric.
  expect_equal(validate_numeric_input("   "), "Error: Input must be a numeric value.")
  
})



test_that("validate_text_input returns correct errors and NULL", {
  
  # Test case 1: Input shorter than the minimum length
  # Expectation: The function should return an error message stating that the input is too short.
  expect_equal(validate_text_input("12", min_length = 3), "Error: Input must be at least 3 characters long.")
  
  # Test case 2: Input longer than the maximum length
  # Expectation: The function should return an error message stating that the input is too long.
  expect_equal(validate_text_input("12345", max_length = 5), "Error: Input must be no more than 5 characters long.")
  
  # Test case 3: Input length within the allowed range
  # Expectation: The function should return NULL, indicating that the input is valid.
  expect_equal(validate_text_input("123", min_length = 1, max_length = 5), NULL)
  
  # Test case 4: Input with leading and trailing spaces
  # Expectation: The function should trim the spaces and return NULL, indicating that the input is valid.
  expect_equal(validate_text_input("  123  ", min_length = 1, max_length = 5), NULL)
 
  # Test case 5: Input is an empty string
  # Expectation: The function should return NULL or an appropriate error message, indicating that the input is invalid.
  expect_equal(validate_text_input("   ", min_length = 1), "Error: Input must be at least 1 characters long.")
  
  # Test case 6: Input is equal to min_length
  # Expectation: The function should return NULL, indicating that the input is valid.
  expect_equal(validate_text_input("123", min_length = 3), NULL)
  
  # Test case 7: Input is equal to max_length
  # Expectation: The function should return NULL, indicating that the input is valid.
  expect_equal(validate_text_input("12345", max_length = 5), NULL)
  
  # Test case 8: Input with leading and trailing spaces
  # Expectation: The function should trim the spaces and return NULL, indicating that the input is valid.
  expect_equal(validate_text_input(" 123 ", min_length = 1, max_length = 5), NULL)
  
  # Test case 9: Very long input string
  # Expectation: The function should return an error message, indicating that the input exceeds the maximum length.
  long_string <- paste(rep("123", 1000), collapse = "")
  expect_equal(validate_text_input(long_string, max_length = 100), "Error: Input must be no more than 100 characters long.")
  
  # Test case 10: Input with special characters
  # Expectation: The function should return NULL, indicating that the input is valid as long as it meets length requirements.
  expect_equal(validate_text_input("@#$%", min_length = 1), NULL)
  
})
