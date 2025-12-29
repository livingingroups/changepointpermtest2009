library(tinytest)
library(trackframe)

is.trackframe <- function(x) {
  inherits(x, "trackframe")
}

# Create a simple dataset
set.seed(2025)
x <- cpttestdata$x
y <- cpttestdata$y
t <- cpttestdata$t

# Test with data frame
data_df <- data.frame(x = x, y = y, t = t)
result_df <- change_point_test(
  data_df,
  alpha = 0.05,
  q = 3,
  n = 100,
  min_move_dist = 0
)
# t <- as.POSIXct(1:10)
data_df <- data.frame(x = x, y = y, t = t)
set.seed(2025L)
result_df <- change_point_test(
  data_df,
  alpha = 0.05,
  q = 2,
  n = 50,
  min_move_dist = 0
)
expect_true(is.data.frame(result_df))
expect_equal(
  data_df,
  result_df[, 1:(NCOL(result_df) - 2)],
  check.attributes = FALSE
)
# attributes(data_df)
# attributes(result_df)

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
expect_true(is.trackframe(result_tf))
expect_equal(
  data_tf,
  result_tf[, 1:(NCOL(result_tf) - 2)],
  check.attributes = FALSE
)

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
result_move2 <- change_point_test(
  data_move2,
  alpha = 0.05,
  q = 2,
  n = 50,
  min_move_dist = 0
)
class(result_move2)
expect_inherits(result_move2, "move2")
expect_equal(result_move2[["sig"]], result_tf[["sig"]])
expect_equal(result_move2[["cp_no"]], result_tf[["cp_no"]])

# sftrack
library(sftrack)
data_sftrack <- as_sftrack(
  data.frame(t = as.POSIXct(t), x = x, y = y, id = 1),
  coords = c("x", "y"),
  time = "t",
  crs = 32632
)
set.seed(2025L)
result_sftrack <- change_point_test(
  data_sftrack,
  alpha = 0.05,
  q = 2,
  n = 50,
  min_move_dist = 0
)
expect_inherits(result_sftrack, "sftrack")
expect_equal(result_sftrack[["cp_no"]], result_tf[["cp_no"]])
