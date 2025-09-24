library(tinytest)

# Test Suite for change_point_test Function
library(cpt)
library(trackframe)

# cpttestdata

test_xytdata <- function() {
  data("path_trackframe", package = "trackframe")
  xyt <- path_trackframe[1:50, ]
  xyt$time <- 1:50
  # data("cpttestdata", package = "cpt")
  # xyt <- cpttestdata
  set.seed(2025L)
  cpt_xyt <- change_point_test_pvalue_xyt(
    x = xyt[, "easting"],
    y = xyt[, "northing"],
    time = xyt[, "time"],
    q_max = 3,
    N = 100,
    tol = 0
  )
  expect_inherits(cpt_xyt, "matrix")
  expect_inherits(cpt_xyt, "change_point_test_pvalue")
  set.seed(2025L)
  cpt_tf <- change_point_test_pvalue(xyt, q_max = 3, N = 100, tol = 0)
  expect_inherits(cpt_tf, "change_point_test_pvalue")
  expect_equal(cpt_xyt, cpt_tf)
  # cat(deparse(cpt_xyt))
  results_orig <- structure(c(
    1, 0.67, NA, NA, 0.51, 0.41, 0.2, NA, 0.64, 0.94, 0.68, 0.91, 0.59, NA, 0.92, 0.17, 0.16,
    0.12, NA, 0.25, 0.93,  0.44, 0.04, NA, 0.42, 0.07, 0.89, 0.33, NA, 0.19, 0.29, 0.77,
    0.46, 0.54, 0.89, 0.2, NA, NA, 0.94, 0.33, NA, 0.26, 0.59, 0.42,  1, 0.61, 0.45, 0.94,
    NA, NA, 1, 0.34, NA, NA, 0.07, 0.08, 0.21,  NA, 0.86, 0.41, 0.56, 0.69, 0.59, NA, 0.17,
    0.03, 0.04, 0.07, NA, 0.29, 0.94, 0.15, 0.05, NA, 0.09, 0.2, 0.67, 0.08, NA, 0.09, 
    0.28, 0.97, 0.25, 0.43, 0.53, 0.67, NA, NA, 0.58, 0.23, NA, 0.16,  0.37, 0.51, 0.85,
    0.92, 0.57, NA, NA, NA, 1, 0.12, NA, NA, 0.09,  0.13, 0.4, NA, 0.9, 0.43, 0.27, 0.6,
    0.17, NA, 0.05, 0.02, 0.01,  0.27, NA, 0.29, 0.38, 0.07, 0.02, NA, 0.18, 0.13, 0.24, 0.05,
    NA, 0.04, 0.65, 0.77, 0.39, 0.64, 0.84, 0.7, NA, NA, 0.52, 0.22,  NA, 0.17, 0.5, 0.49,
    0.93, 0.89, NA, NA, NA, NA),
    dim = c(50L,  3L),
    class = c("change_point_test_pvalue", "matrix", "array"),
    dimnames = list(NULL, c("q=1", "q=2", "q=3"))
  ) 
  expect_equal(cpt_xyt, results_orig, check.attributes = FALSE)
  expect_equal(colnames(cpt_xyt), c("q=1", "q=2", "q=3"))
  expect_true(inherits(cpt_xyt, c("matrix", "data.frame")))
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
  t <- as.POSIXct(1:40)

  # Create data frame with the required columns
  data <- data.frame(x = x, y = y, t = t)

  # Run change point detection
  set.seed(2025L)
  result <- change_point_test_pvalue(data, q_max = 3, N = 100, tol = 0)

  # Check that the result is a data frame
  expect_true(is.matrix(result))

  # Check that the output has the same number of rows as the input
  expect_equal(nrow(result), nrow(data))

  # Check that the output contains the required columns
  expect_true(all(c("q=1", "q=2", "q=3") %in% colnames(result)))

  # Check that sig column contains only 0s and 1s
  expect_true(all(result <= 1, na.rm = TRUE))
}


# Test with different input formats
test_input_formats <- function() {
  # Create a simple dataset
  set.seed(2025)
  x <- 1:10
  y <- 1:10
  t <- 1:10

  # Test with data frame
  data_df <- data.frame(x = x, y = y, t = t)
  result_df <- change_point_test_pvalue(data_df, q_max = 3, N = 100, tol = 0)
  expect_equal(NCOL(result_df), 3)
  t <- as.POSIXct(1:40)
  data_df <- data.frame(x = x, y = y, t = t)
  set.seed(2025L)
  result_df <- change_point_test_pvalue(data_df, q_max = 2, N = 50, tol = 0)
  expect_equal(NCOL(result_df), 2)

  # Test with trackframe
  data_tf <- as.trackframe(
    data.frame(t = as.POSIXct(t), x = x, y = y),
    't',
    'x',
    'y'
  )
  set.seed(2025L)
  result_tf <- change_point_test_pvalue(data_tf, q_max = 2, N = 50, tol = 0)
  # FIXME: test

  # move2
  library(move2)
  data_move2 <- mt_as_move2(
    data.frame(t = as.POSIXct(t), x = x, y = y, id = 1),
    coords = c("x", "y"),
    time_column = "t",
    track_id_column = "id",
    crs = 32631
  )
  set.seed(2025L)
  result_move2 <- change_point_test_pvalue(
    data_move2,
    q_max = 2,
    N = 50,
    tol = 0
  )
  # FIXME: test

  # sftrack
  library(sftrack)
  data_sftrack <- as_sftrack(
    data.frame(t = as.POSIXct(t), x = x, y = y, id = 1),
    coords = c("x", "y"),
    time = "t",
    crs = 32632
  )
  set.seed(2025L)
  result_sftrack <- change_point_test_pvalue(
    data_move2,
    q_max = 2,
    N = 50,
    tol = 0
  )
  # FIXME: test
}


# Test consistency of cp_no column
test_cp_no_consistency <- function() {
  # Create data with multiple potential change points
  set.seed(303)
  x <- c(1:10, 20:30, 40:50)
  y <- c(1:10, 20:30, 40:50)
  t <- as.POSIXct(1:length(x) * 5)
  data <- data.frame(x = x, y = y, t = t)

  # Run change point detection
  set.seed(2025L)
  result <- change_point_test_pvalue(data, q_max = 3, N = 100, tol = 0)
}


test_colnames <- function() {
  data("cpttestdata", package = "cpt")
  xyt <- cpttestdata
  cn <- c("xnew", "ynew", "time2")
  colnames(xyt) <- cn
  set.seed(2025L)

  expect_error(change_point_test_pvalue(xyt, N = 100))

  cn <- c("x", "y", "t")
  colnames(xyt) <- cn
  cpt <- change_point_test(xyt, N = 100)
  colnames(cpt)
  expect_equal(colnames(cpt)[1:3], cn)

  tf <- as.trackframe(
    data.frame(
      tnew = as.POSIXct(seq_along(cpttestdata[, 3])),
      x2 = cpttestdata[, 1],
      y2 = cpttestdata[, 2]
    ),
    'tnew',
    'x2',
    'y2'
  )
  set.seed(2025L)
  cpt_tf <- change_point_test_pvalue(tf, N = 100)
  # FIXME: test
}

test_multiple_paths <- function() {
  # multiple paths
  data("paths_trackframe", package = "trackframe")

  expect_inherits(paths_trackframe, "trackframe")
  set.seed(2025L)
  cpt_mpaths <- change_point_test_pvalue(
    data = paths_trackframe,
    q_max = 3,
    N = 10,
    tol = 0
  )

  expect_equal(dim(paths_trackframe)[1], sum(sapply(cpt_mpaths, NROW)))
  track1 <- split_by_id(paths_trackframe)[[1]]
  set.seed(2025L)
  cpt_path1 <- change_point_test_pvalue(
    data = track1,
    q_max = 3,
    N = 10,
    tol = 0
  )

  expect_equal(cpt_path1, cpt_mpaths[[1]])

  # sftrack
  data("paths_sftrack", package = "trackframe")

  expect_inherits(paths_sftrack, "sftrack")
  set.seed(2025L)
  cpt_mpaths_sftrack <- change_point_test_pvalue(
    data = paths_sftrack,
    q_max = 3,
    N = 10,
    tol = 0
  )

  expect_equal(cpt_mpaths_sftrack, cpt_mpaths)
}


# Run all tests
test_xytdata()
test_basic_functionality()
test_input_formats()
test_cp_no_consistency()
test_colnames()
test_multiple_paths()
