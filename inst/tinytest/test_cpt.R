library(tinytest)
# Test Suite for change_point_test Function
library(changepointpermtest2009)
library(trackframe)

data("path_trackframe", package = "trackframe")
data("cpttestdata", package = "cpt")

# To please the lintr.
path_trackframe <- path_trackframe # nolint: object_usage_linter
cpttestdata <- cpttestdata # nolint: object_usage_linter
projected_crs <- "EPSG:32632"


# cpttestdata

test_xytdata <- function() {
  xyt <- path_trackframe[1:50, ] #nolint
  xyt$time <- 1:50
  set.seed(2025L)
  cpt_xyt <- change_point_test_xyt(
    easting = xyt[, "easting"],
    northing = xyt[, "northing"],
    time = xyt[, "time"],
    alpha = 0.05,
    q = 4,
    n = 1000,
    min_move_dist = 0
  )
  expect_inherits(cpt_xyt, "change_point_test")
  set.seed(2025L)
  cpt_tf <- change_point_test(
    xyt,
    alpha = 0.05,
    q = 4,
    n = 1000,
    min_move_dist = 0
  )
  expect_inherits(cpt_tf, "change_point_test")
  expect_equal(cpt_xyt$cp_id, cpt_tf$cp_id)
  results_orig <- c(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
  )
  expect_equal(as.integer(cpt_xyt[, "cp_id"] != 0), results_orig)
  expect_true(inherits(cpt_xyt, c("matrix", "data.frame")))
  expect_equal(summary(cpt_xyt)[, "last"], c(9, 17, 34))
  expect_equal(
    summary(cpt_tf)[, "east"],
    c(-0.00328936898894608, -0.00207882302487269, -0.00162844973383471),
    tolerance = 1e-03
  )
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
  result <- change_point_test(
    data,
    alpha = 0.05,
    q = 3,
    n = 100,
    min_move_dist = 0
  )

  # Check that the result is a data frame
  expect_true(is.data.frame(result))

  # Check that the output has the same number of rows as the input
  expect_equal(nrow(result), nrow(data))

  # Check that the output contains the required columns
  expect_true(all(c("x", "y", "t", "cp_id") %in% colnames(result)))
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
  result_df <- change_point_test(
    data_df,
    alpha = 0.05,
    q = 3,
    n = 100,
    min_move_dist = 0
  )
  t <- as.POSIXct(1:10)
  data_df <- data.frame(x = x, y = y, t = t, id = "id_1")
  set.seed(2025L)
  result_df <- change_point_test(
    data_df,
    alpha = 0.05,
    q = 2,
    n = 50,
    min_move_dist = 0
  )
  expect_true(is.data.frame(result_df))

  # Test with trackframe
  data_tf <- as.trackframe(
    data.frame(t = as.POSIXct(t), x = x, y = y),
    "t",
    "x",
    "y",
    crs = NA
  )
  set.seed(2025L)
  result_tf <- change_point_test(
    data_tf,
    alpha = 0.05,
    q = 2,
    n = 50,
    min_move_dist = 0
  )
  expect_inherits(result_tf, class(data_tf))
  expect_equal(result_df[, c("cp_id")], result_tf[, c("cp_id")])

  # move2
  library(move2)
  data_move2 <- mt_as_move2(
    data.frame(t = as.POSIXct(t), x = x, y = y, id = 1),
    coords = c("x", "y"),
    time_column = "t",
    track_id_column = "id",
    crs = projected_crs
  )
  set.seed(2025L)
  result_move2 <- change_point_test(
    data_move2,
    alpha = 0.05,
    q = 2,
    n = 50,
    min_move_dist = 0
  )
  expect_inherits(result_move2, class(data_move2))
  expect_equal(result_move2[["cp_id"]], result_tf[["cp_id"]])

  # sftrack
  library(sftrack)
  data_sftrack <- as_sftrack(
    data.frame(t = as.POSIXct(t), x = x, y = y, id = 1),
    coords = c("x", "y"),
    time = "t",
    crs = projected_crs
  )
  set.seed(2025L)
  result_sftrack <- change_point_test(
    data_sftrack,
    alpha = 0.05,
    q = 2,
    n = 50,
    min_move_dist = 0
  )
  expect_inherits(result_sftrack, class(data_sftrack))
  expect_equal(result_sftrack[["cp_id"]], result_tf[["cp_id"]])
}


# Test consistency of cp_id column
test_cp_id_consistency <- function() {
  # Create data with multiple potential change points
  set.seed(303)
  x <- c(1:10, 20:30, 40:50)
  y <- c(1:10, 20:30, 40:50)
  t <- as.POSIXct(seq_along(x) * 5)
  data <- data.frame(x = x, y = y, t = t, id = "id_1")

  # Run change point detection
  set.seed(2025L)
  result <- change_point_test(
    data,
    alpha = 0.05,
    q = 3,
    n = 100,
    min_move_dist = 0
  )

  # Check that cp_id is sequential and matches sig column
  if (sum(result$cp_id) > 0) {
    # Get non-zero cp_id values
    cp_nums <- unique(result$cp_id[result$cp_id > 0])

    # Should be sequential integers
    expect_equal(cp_nums, seq_along(cp_nums))
  }
}


test_colnames <- function() {
  xyt <- cpttestdata
  cn <- c("xnew", "ynew", "time2")
  colnames(xyt) <- cn
  set.seed(2025L)

  expect_error(change_point_test(xyt, n = 100))

  cn <- c("x", "y", "t")
  colnames(xyt) <- cn
  cpt <- change_point_test(xyt, n = 100)
  colnames(cpt)
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
  cpt_tf <- change_point_test(tf, n = 100)
  expect_equal(colnames(cpt_tf)[1:3], c("tnew", "x2", "y2"))
}

# Run all tests
test_xytdata()
test_basic_functionality()
test_input_formats()
test_cp_id_consistency()
test_colnames()
