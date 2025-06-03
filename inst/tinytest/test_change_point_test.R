# library(tinytest)
# Test Suite for change_point_test Function
library(cpt)
library(trackframe)

# cpttestdata

test_cpttestdata <- function() {
  data("cpttestdata", package = "cpt")
  xyt <- cpttestdata
  cpt <- change_point_test(xyt, alpha = 0.05, q = 4, N = 1000, tol = 0)
  # cat(deparse(cpt$sig))
  results_orig <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
                    1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0)
  expect_equal(cpt$sig, results_orig)
  expect_equal(summary(cpt)[, "last"], c(39, 65, 74, 88, 132))
  
  tf <- as.track_frame(data.frame(t=as.POSIXct(seq_along(cpttestdata[,3])), x = cpttestdata[,1], y=cpttestdata[,2]), 't', 'x', 'y')
  cpt_tf <- change_point_test(tf, alpha = 0.05, q = 4, N = 1000, tol = 0)
  expect_equal(cpt_tf$sig, results_orig)
  expect_equal(summary(cpt_tf)[, "north"], c(688443.1545, 687898.0517, 687666.5149, 687345.6599, 687095.3092))
}

# Basic functionality test with simple trajectory
test_basic_functionality <- function() {
  # Create a simple trajectory with a clear change point
  set.seed(123)
  x1 <- cumsum(rnorm(20, 0, 0.5))
  y1 <- cumsum(rnorm(20, 0, 0.5))
  x2 <- cumsum(rnorm(20, 1, 0.5)) + x1[length(x1)]
  y2 <- cumsum(rnorm(20, -1, 0.5)) + y1[length(y1)]
  
  x <- c(x1, x2)
  y <- c(y1, y2)
  t <- 1:40
  
  # Create data frame with the required columns
  data <- data.frame(x = x, y = y, t = t)
  
  # Run change point detection
  result <- change_point_test(data, alpha = 0.05, q = 3, N = 100, tol = 0)
  
  # Check that the result is a data frame
  expect_true(is.data.frame(result))
  
  # Check that the output has the same number of rows as the input
  expect_equal(nrow(result), nrow(data))
  
  # Check that the output contains the required columns
  expect_true(all(c("x", "y", "t", "sig", "cp_no") %in% colnames(result)))
  
  # Check that sig column contains only 0s and 1s
  expect_true(all(result$sig %in% c(0, 1)))
}



# Test with different input formats
test_input_formats <- function() {
  # Create a simple dataset
  set.seed(202)
  x <- 1:10
  y <- 1:10
  t <- 1:10
  
  # Test with data frame
  data_df <- data.frame(x = x, y = y, t = t)
  result_df <- change_point_test(data_df, alpha = 0.05, q = 2, N = 50, tol = 0)
  expect_true(is.data.frame(result_df))
  
  # Test with matrix
  data_matrix <- cbind(x, y, t)
  colnames(data_matrix) <- c("x", "y", "t")
  result_matrix <- change_point_test(data_matrix, alpha = 0.05, q = 2, N = 50, tol = 0)
  expect_true(is.data.frame(result_matrix))
  
  # Both should have the same structure
  expect_equal(ncol(result_df), ncol(result_matrix))
  expect_true(all(colnames(result_df) %in% colnames(result_matrix)))
  
  # Test with track_frame
  
  data_tf <- as.track_frame(data.frame(t=as.POSIXct(t), x = x, y = y), 't', 'x', 'y')
  result_tf <- change_point_test(data_tf, alpha = 0.05, q = 2, N = 50, tol = 0)
  expect_true(is.data.frame(result_tf))
  expect_equal(result_df[, c("sig", "cp_no")], result_matrix[, c("sig", "cp_no")])
}



# Test consistency of cp_no column
test_cp_no_consistency <- function() {
  # Create data with multiple potential change points
  set.seed(303)
  x <- c(1:10, 20:30, 40:50)
  y <- c(1:10, 20:30, 40:50)
  t <- 1:length(x)
  data <- data.frame(x = x, y = y, t = t)
  
  # Run change point detection
  result <- change_point_test(data, alpha = 0.05, q = 3, N = 100, tol = 0)
  
  # Check that cp_no is sequential and matches sig column
  if(sum(result$sig) > 0) {
    # Get non-zero cp_no values
    cp_nums <- unique(result$cp_no[result$cp_no > 0])
    
    # Should be sequential integers
    expect_equal(cp_nums, 1:length(cp_nums))
    
    # Each cp_no should correspond to a change point (sig=1)
    for(cp in cp_nums) {
      # At least one row with this cp_no should have sig=1
      expect_true(any(result$sig[result$cp_no == cp] == 1))
    }
  }
}


test_colnames <- function() {
  data("cpttestdata", package = "cpt")
  xyt <- cpttestdata
  cn <- c("xnew", "ynew", "time2")
  colnames(xyt) <- cn
  cpt <- change_point_test(xyt, N = 100)
  expect_equal(colnames(cpt[1:3]), cn)

  tf <- as.track_frame(data.frame(tnew = as.POSIXct(seq_along(cpttestdata[,3])),
                                  x2 = cpttestdata[,1],
                                  y2 = cpttestdata[,2]),
                       'tnew', 'x2', 'y2')
  cpt_tf <- change_point_test(tf, N = 100)
  expect_equal(colnames(cpt_tf[1:3]), c('x2', 'y2', 'tnew'))
}

# Run all tests
test_cpttestdata()
test_basic_functionality()
test_input_formats()
test_cp_no_consistency()
test_colnames()

cat("All tests for change_point_test completed successfully!\n")
