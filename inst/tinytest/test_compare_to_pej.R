source("helper_pej.R")
library(tinytest)
library(trackframe)
library(cpt)

compare_to_pej <- function(tf, alpha, q, N, tol) {
  tf <- as.trackframe(tf)
  pej <- pej_implementation(easting(tf), northing(tf), alpha, q, N, tol) #nolint

  # filter only when there is movement
  tf_move <- tf[c(TRUE, sqrt(diff(easting(tf))^2 + diff(northing(tf))^2) > tol), ]
  # reverse it
  tf_move <- tf_move[rev(seq_len(nrow(tf_move))), ]

  # For some reason, doesn't work with time in df
  # TODO: MRE and document why this is
  old_time <- time(tf_move)
  tf_move[, attr(tf_move, "time")] <- seq_len(nrow(tf_move))

  rcpp <- change_point_test(tf_move, q = q, N = N, alpha = alpha, seed = 2025)
  rcpp$sig[1] <- 0

  tf_move[, attr(tf_move, "time")] <- old_time
  # combine results back with the data
  # no longer trackframe
  tf_move <- cbind(tf_move, rcpp)

  # combine back with the non-moving data
  tf <- merge(tf, tf_move[, c("time", "sig", "cp_no")], by = "time", all.x = TRUE, sort = FALSE)

  # fill missing cp_no and reverse the numbers
  tf[is.na(tf$cp_no), "cp_no"] <- 0
  tf[tf$cp_no > 0, "cp_no"] <- rev(tf[tf$cp_no > 0, "cp_no"])

  # convert cps into format pej outputs
  rownames(tf) <- NULL
  xyt_cp <- tf[tf$sig != 0, ]
  xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)
  cps_rcpp <- do.call("rbind", lapply(xyt_cp_split, function(x) {
    setNames(c(min(x$time), max(x$time), x$x1[1], x$x2[1]), c("first", "last", "north", "east"))
  }))

  rownames(cps_rcpp) <- NULL
  expect_equal(pej$bz1, tf_move[, "x1"])
  expect_equal(pej$bz2, tf_move[, "x2"])
  expect_equal(pej$sig, rcpp$sig)
  expect_equal(pej$cps, cps_rcpp)
}

data("cpttestdata", package = "cpt")
compare_to_pej(
  as.trackframe(cpttestdata, easting_col = "x1", northing_col = "x2"),
  alpha = 0.05,
  q = 4,
  N = 1000,
  tol = 0
)