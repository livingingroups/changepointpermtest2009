source("helper_pej.R")
library(tinytest)
library(trackframe)
library(cpt)

pej_tf_filter <- \(tf, tol) tf[c(
  TRUE,
  abs(diff(easting(tf))) > tol &
    abs(diff(northing(tf))) > tol
), ]


# not used
distance_filter  <- \(tf, tol) tf[c(TRUE, sqrt(diff(easting(tf))^2 + diff(northing(tf))^2) > tol), ]

compare_to_pej <- function(tf, alpha, q, N, tol) {
  tf <- as.trackframe(tf)
  tf_colnames <- list(
    time_col = attr(tf, "time"),
    easting_col = attr(tf, "easting"),
    northing_col = attr(tf, "northing"),
    id_col = attr(tf, "id")
  )
  pej <- pej_implementation(easting(tf), northing(tf), alpha, q, N, tol) #nolint

  # reverse it
  tf <- tf[rev(seq_len(nrow(tf))), ]

  # filter only when there is movement
  tf_move <- pej_tf_filter(tf, tol)

  # For some reason, doesn't work with time in df
  # TODO: MRE and document why this is
  old_time <- time(tf_move)
  tf_move[, tf_colnames$time_col] <- seq_len(nrow(tf_move))

  rcpp <- change_point_test(tf_move, q = q, N = N, alpha = alpha, seed = 2025)
  rcpp$sig[1] <- 0

  # put back original times
  rcpp[, tf_colnames$time_col] <- old_time

  # combine back with the non-moving data
  tf <- merge(tf, rcpp[, c(tf_colnames$time_col, "sig", "cp_no")], by = tf_colnames$time_col, all.x = TRUE, sort = FALSE)
  tf <- do.call(as.trackframe, c(list(tf), tf_colnames))

  # Reverse the numbers and apply them to the rows that were within tolerance
  tf[!is.na(tf$cp_no) & tf$cp_no > 0, "cp_no"] <- rev(tf[!is.na(tf$cp_no) & tf$cp_no > 0, "cp_no"])
  tf[, "cp_no"] <- na.locf(tf[, "cp_no"], fromLast = TRUE, na.rm = FALSE)

  # convert cps into format pej outputs
  rownames(tf) <- NULL
  xyt_cp <- tf[tf$cp_no != 0, ]
  xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)
  cps_rcpp <- do.call("rbind", lapply(xyt_cp_split, function(x) setNames(
    # pej returns indices, not times.
    # because of tol and how it's used, there may be more than one easting, northing
    # pej takes the last one
    as.numeric(c(min(rownames(x)), max(rownames(x)), tail(easting(x), 1), tail(northing(x), 1))),
    c("first", "last", "north", "east")
  )))

  # Default in case no change points
  if (is.null(cps_rcpp)) cps_rcpp <- structure(-Inf, dim = c(1L, 1L), dimnames = list(NULL, "first"))

  rownames(cps_rcpp) <- NULL

  expect_equal(pej$bz1, easting(tf_move), info = unique_ids(tf))
  expect_equal(pej$bz2, northing(tf_move), info = unique_ids(tf))
  expect_equal(pej$sig, rcpp$sig, info = unique_ids(tf))
  expect_equal(pej$cps, cps_rcpp, info = unique_ids(tf))
}

data("cpttestdata", package = "cpt")
compare_to_pej(
  as.trackframe(cpttestdata, easting_col = "x1", northing_col = "x2"),
  alpha = 0.05,
  q = 4,
  N = 1000,
  tol = 0
)

data("cptfiguredata_tf")

lapply(
  split_by_id(cptfiguredata_tf),
  compare_to_pej,
  alpha = 0.05,
  q = 4,
  N = 1000,
  tol = 0
)