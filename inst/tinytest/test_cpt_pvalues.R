library(tinytest)

# Test Suite for change_point_test Function
library(cpt)
library(trackframe)


data("path_trackframe", package = "trackframe")
data("paths_trackframe", package = "trackframe")
data("paths_sftrack", package = "trackframe")
data("cpttestdata", package = "cpt")
projected_crs <- "EPSG:32632"
small_n <- 3

# To please the lintr.
path_trackframe <- path_trackframe # nolint: object_usage_linter
paths_trackframe <- paths_trackframe # nolint: object_usage_linter
paths_sftrack <- paths_sftrack # nolint: object_usage_linter
cpttestdata <- cpttestdata # nolint: object_usage_linter


test_xytdata <- function() {
  xyt <- path_trackframe[1:50, ]
  xyt$time <- 1:50
  set.seed(2025L)
  cpt_xyt <- change_point_test_pvalue_xyt(
    x = xyt[, "easting"],
    y = xyt[, "northing"],
    time = xyt[, "time"],
    q_max = 3,
    # changing n will cause this to fail unless the standard is updated
    n = 100,
    min_move_dist = 0
  )
  expect_inherits(cpt_xyt, "matrix")
  expect_inherits(cpt_xyt, "change_point_test_pvalue")
  set.seed(2025L)
  cpt_tf <- change_point_test_pvalue(
    xyt,
    q_max = 3,
    # changing n will cause this to fail unless the standard is updated
    n = 100,
    min_move_dist = 0
  )[[1]]
  expect_inherits(cpt_tf, "change_point_test_pvalue")
  expect_equal(cpt_xyt, cpt_tf)
  # cat(deparse(cpt_xyt))
  results_orig <- structure(
    c(
      0.54,
      0.67,
      NA,
      NA,
      0.51,
      0.41,
      0.2,
      NA,
      0.79,
      0.94,
      0.68,
      0.91,
      0.59,
      NA,
      0.92,
      0.17,
      0.16,
      0.1,
      NA,
      0.25,
      0.93,
      0.41,
      0.03,
      NA,
      0.54,
      0.06,
      0.89,
      0.36,
      NA,
      0.1,
      0.33,
      0.76,
      0.49,
      0.5,
      0.88,
      0.25,
      NA,
      NA,
      0.94,
      0.18,
      NA,
      0.16,
      0.59,
      0.38,
      1,
      0.64,
      0.44,
      0.94,
      NA,
      NA,
      1,
      0.34,
      NA,
      NA,
      0.07,
      0.13,
      0.14,
      NA,
      0.86,
      0.41,
      0.56,
      0.68,
      0.64,
      NA,
      0.18,
      0.06,
      0.04,
      0.11,
      NA,
      0.4,
      0.94,
      0.15,
      0.09,
      NA,
      0.14,
      0.23,
      0.69,
      0.09,
      NA,
      0.12,
      0.29,
      0.97,
      0.29,
      0.44,
      0.62,
      0.73,
      NA,
      NA,
      0.6,
      0.12,
      NA,
      0.12,
      0.31,
      0.42,
      0.83,
      0.92,
      0.67,
      NA,
      NA,
      NA,
      1,
      0.12,
      NA,
      NA,
      0.19,
      0.22,
      0.4,
      NA,
      0.9,
      0.43,
      0.26,
      0.59,
      0.2,
      NA,
      0.08,
      0.04,
      0.02,
      0.31,
      NA,
      0.32,
      0.45,
      0.11,
      0.02,
      NA,
      0.3,
      0.21,
      0.28,
      0.09,
      NA,
      0.07,
      0.66,
      0.79,
      0.41,
      0.56,
      0.85,
      0.63,
      NA,
      NA,
      0.45,
      0.08,
      NA,
      0.07,
      0.42,
      0.42,
      0.93,
      0.89,
      NA,
      NA,
      NA,
      NA
    ),
    dim = c(50L, 3L),
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
  data <- data.frame(x = x, y = y, t = t, id = "id_1")

  # Run change point detection
  set.seed(2025L)
  result <- change_point_test_pvalue(
    data,
    q_max = 3,
    n = 100,
    min_move_dist = 0
  )[[1]]

  # Check that the result is a data frame
  expect_true(is.matrix(result))

  # Check that the output has the same number of rows as the input
  expect_equal(nrow(result), nrow(data))

  # Check that the output contains the required columns
  expect_true(all(c("q=1", "q=2", "q=3") %in% colnames(result)))

  # Check that pvalues are smaller equal 1
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
  data_df <- data.frame(x = x, y = y, t = t, id = "id_1")
  result_df <- change_point_test_pvalue(
    data_df,
    q_max = 3,
    n = small_n,
    min_move_dist = 0
  )[[1]]
  expect_equal(NCOL(result_df), 3)
  t <- as.POSIXct(1:10)
  data_df <- data.frame(x = x, y = y, t = t, id = "id_1")

  set.seed(2025L)
  result_df <- change_point_test_pvalue(
    data_df,
    q_max = 2,
    n = small_n,
    min_move_dist = 0
  )[[1]]
  expect_equal(NCOL(result_df), 2)

  # Test with trackframe
  data_tf <- as.trackframe(
    data.frame(t = as.POSIXct(t), x = x, y = y, id = "id_1"),
    "t",
    "x",
    "y",
    crs = NA
  )
  set.seed(2025L)
  result_tf <- change_point_test_pvalue(
    data_tf,
    q_max = 2,
    n = small_n,
    min_move_dist = 0
  )[[1]]
  expect_inherits(result_tf, "change_point_test_pvalue")
  expect_equal(result_df, result_tf)

  # move2
  library(move2)
  data_move2 <- mt_as_move2(
    as.data.frame(data_tf),
    coords = c("x", "y"),
    time_column = "t",
    track_id_column = "id",
    crs = projected_crs
  )
  set.seed(2025L)
  result_move2 <- change_point_test_pvalue(
    data_move2,
    q_max = 2,
    n = small_n,
    min_move_dist = 0
  )[[1]]
  expect_inherits(result_move2, "change_point_test_pvalue")

  # sftrack
  library(sftrack)
  data_sftrack <- as_sftrack(
    as.data.frame(data_tf),
    coords = c("x", "y"),
    time = "t",
    crs = projected_crs
  )
  set.seed(2025L)
  result_sftrack <- change_point_test_pvalue(
    data_sftrack,
    q_max = 2,
    n = small_n,
    min_move_dist = 0
  )[[1]]
  expect_inherits(result_sftrack, "change_point_test_pvalue")
  expect_equal(result_move2, result_sftrack)
}


test_colnames <- function() {
  xyt <- cpttestdata
  cn <- c("xnew", "ynew", "time2")
  colnames(xyt) <- cn
  set.seed(2025L)

  expect_error(change_point_test_pvalue(xyt, n = 100))

  cn <- c("x", "y", "t")
  colnames(xyt) <- cn
  cpt <- change_point_test(xyt, n = 100)
  expect_equal(colnames(cpt)[1:3], cn)

  tf <- as.trackframe(
    data.frame(
      tnew = as.POSIXct(seq_along(cpttestdata[, 3])),
      x2 = cpttestdata[, 1],
      y2 = cpttestdata[, 2]
    ),
    "tnew",
    "x2",
    "y2",
    crs = NA
  )
  set.seed(2025L)
  cpt_tf <- change_point_test_pvalue(tf, n = small_n)[[1]]
  expect_inherits(cpt_tf, "change_point_test_pvalue")
  expect_equal(colnames(cpt_tf), c("q=1", "q=2", "q=3", "q=4"))
}

test_multiple_paths <- function() {
  # multiple paths
  expect_inherits(paths_trackframe, "trackframe")
  set.seed(2025L)
  cpt_mpaths <- change_point_test_pvalue(
    data = paths_trackframe,
    q_max = 3,
    n = small_n,
    min_move_dist = 0
  )

  expect_equal(dim(paths_trackframe)[1], sum(sapply(cpt_mpaths, NROW)))
  track1 <- split_by_id(paths_trackframe)[[1]]
  set.seed(2025L)
  cpt_path1 <- change_point_test_pvalue(
    data = track1,
    q_max = 3,
    n = small_n,
    min_move_dist = 0
  )

  expect_equal(cpt_path1[[1]], cpt_mpaths[[1]])

  # sftrack
  expect_inherits(paths_sftrack, "sftrack")
  set.seed(2025L)
  cpt_mpaths_sftrack <- change_point_test_pvalue(
    data = sf::st_transform(paths_sftrack, projected_crs),
    q_max = 3,
    n = small_n,
    min_move_dist = 0
  )

  expect_equal(cpt_mpaths_sftrack, cpt_mpaths)
}


# Run all tests
test_xytdata()
test_basic_functionality()
test_input_formats()
test_colnames()
test_multiple_paths()
