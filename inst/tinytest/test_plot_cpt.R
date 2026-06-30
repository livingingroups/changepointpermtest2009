source("plot_testing_helpers.R")
using("tinysnapshot")

# run to create plots (delete plots if new plots should be created)

library(trackframe)
library(tinytest)
library(changepointpermtest2009)

data("path_trackframe", package = "trackframe")
xyt <- path_trackframe[1:50, ] #nolint
xyt$time <- 1:50
set.seed(2025L)
cpt_xyt <- change_point_test_xyt(
  easting = xyt[, "easting"],
  northing = xyt[, "northing"],
  time = xyt[, "time"],
  alpha = 0.05,
  q = 4,
  N = 1000,
  tol = 0
)

expect_inherits(cpt_xyt, "change_point_test")

f <- function() plot(cpt_xyt)
expect_snapshot_plot(f, label = "plot_cpt")


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
f <- function() plot(result_df)
expect_snapshot_plot(f, label = "plot_df")

# Test with trackframe
data_tf <- as.trackframe(
  data.frame(t = as.POSIXct(t), x = x, y = y, id = "id_1"),
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
f <- function() plot(result_tf)
expect_snapshot_plot(f, label = "plot_tf")

# move2
library(move2)
data_move2 <- mt_as_move2(
  data.frame(t = as.POSIXct(t), x = x, y = y, id = 1),
  coords = c("x", "y"),
  time_column = "t",
  track_id_column = "id",
  crs = "EPSG:32632"
)
set.seed(2025L)
result_move2 <- change_point_test(
  data_move2,
  alpha = 0.05,
  q = 2,
  n = 50,
  min_move_dist = 0
)
f <- function() plot(result_move2)
expect_snapshot_plot(f, label = "plot_move2")

# sftrack
library(sftrack)
data_sftrack <- as_sftrack(
  data.frame(t = as.POSIXct(t), x = x, y = y, id = 1),
  coords = c("x", "y"),
  time = "t",
  crs = "EPSG:32632"
)
set.seed(2025L)
result_sftrack <- change_point_test(
  data_sftrack,
  alpha = 0.05,
  q = 2,
  n = 50,
  min_move_dist = 0
)
f <- function() plot(result_sftrack)
expect_snapshot_plot(f, label = "plot_sftrack")
