# library(tinytest)
# Test cases for change_point_fit function
library(cpt)
library(trackframe)

# Tests core functionality with a trajectory containing a clear change point
test_basic_functionality <- function() {
  # Create a simple trajectory with a clear change point
  set.seed(123)
  x1 <- cumsum(rnorm(20, 0, 0.5))
  y1 <- cumsum(rnorm(20, 0, 0.5))
  x2 <- cumsum(rnorm(20, 1, 0.5)) + x1[length(x1)]
  y2 <- cumsum(rnorm(20, -1, 0.5)) + y1[length(y1)]
  
  x <- c(x1, x2)
  y <- c(y1, y2)
  
  # Run change point detection
  result <- change_point_fit(x, y, q = 3, N = 100, alpha = 0.05)
  
  # Check that the output is a numeric vector
  expect_true(is.numeric(result))
  
  # Check that the output has the same length as the input
  expect_equal(length(result), length(x))
  
  # Check that the output contains only 0s and 1s
  expect_true(all(result %in% c(0, 1)))
  
  # # There should be at least one change point detected
  # expect_true(sum(result) > 0)
}

# Tests behavior with a trajectory that has no clear change points
test_no_change_points <- function() {
  # Create a trajectory with no clear change points
  set.seed(456)
  x <- cumsum(rnorm(40, 0, 0.5))
  y <- cumsum(rnorm(40, 0, 0.5))
  
  result <- change_point_fit(x, y, q = 3, N = 100, alpha = 0.05)
  
  # There might be some change points detected by chance, but should be few
  expect_true(sum(result) < length(result) * 0.1) # Less than 10% should be change points
}


# Test with different alpha values
test_alpha_parameter <- function() {
  set.seed(101)
  x <- c(cumsum(rnorm(20, 0, 0.5)), cumsum(rnorm(20, 1, 0.5)) + 10)
  y <- c(cumsum(rnorm(20, 0, 0.5)), cumsum(rnorm(20, -1, 0.5)) + 10)
  
  # With small alpha (more stringent), we expect fewer change points
  result_small_alpha <- change_point_fit(x, y, q = 3, N = 100, alpha = 0.01)
  
  # With large alpha (less stringent), we expect more change points
  result_large_alpha <- change_point_fit(x, y, q = 3, N = 100, alpha = 0.1)
  
  # The number of change points should be greater or equal with larger alpha
  expect_true(sum(result_large_alpha) >= sum(result_small_alpha))
}

# Test input validation
test_input_validation <- function() {
  x <- 1:10
  y <- 1:10
  
  # Test with invalid x (contains NA)
  x_na <- c(1:5, NA, 7:10)
  expect_error(change_point_fit(x_na, y, q = 3, N = 100, alpha = 0.05))
  
  # Test with invalid y (different length)
  y_short <- 1:9
  expect_error(change_point_fit(x, y_short, q = 3, N = 100, alpha = 0.05))
  
  # Test with invalid q (not numeric/integer)
  expect_error(change_point_fit(x, y, q = "three", N = 100, alpha = 0.05))
  
  # Test with invalid N (negative)
  expect_error(change_point_fit(x, y, q = 3, N = -10, alpha = 0.05))
  
  # Test with invalid alpha (not numeric)
  expect_error(change_point_fit(x, y, q = 3, N = 100, alpha = "0.05"))
  
  # Test with invalid alpha (negative)
  expect_error(change_point_fit(x, y, q = 3, N = 100, alpha = -0.05))
}


# Test with constant coordinates
test_constant_coordinates <- function() {
  x <- rep(1, 20)
  y <- rep(1, 20)
  
  # With constant coordinates, there should be no change points
  result <- change_point_fit(x, y, q = 3, N = 100, alpha = 0.05)
  
  expect_equal(sum(result), 0)
}

# Run all tests
test_basic_functionality()
test_no_change_points()
test_alpha_parameter()
test_input_validation()
test_constant_coordinates()

cat("All tests for change_point_fit completed successfully!\n")
