source("helper_pej.R")
library(tinytest)
library(trackframe)
library(cpt)

pej_tf_filter <- \(tf, tol) {
  tf <- tf[c(
    abs(diff(easting(tf))) > tol &
      abs(diff(northing(tf))) > tol,
    TRUE
  ), ]
  tf <- tf[rev(seq_len(nrow(tf))),]
  tf
}

pej_style_cps <- \(tf) {
  xyt_cp <- tf[tf$cp_no != 0, ]
  xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)
  cps_rcpp <- do.call("rbind", lapply(xyt_cp_split, function(x) setNames(
    # pej returns indices, not times.
    # because of tol and how it's used, there may be more than one easting, northing
    # pej takes the last one
    as.numeric(c(min(rownames(x)), max(rownames(x)), tail(easting(x), 1), tail(northing(x), 1))),
    c("first", "last", "north", "east")
  )))
  rownames(cps_rcpp) <- NULL

  # Default in case no change points
  if (is.null(cps_rcpp)) cps_rcpp <- structure(
    -Inf,
    dim = c(1L, 1L), dimnames = list(NULL, "first")
  )
  cps_rcpp
}

compare_to_pej <- function(tf, alpha, q, N, tol) {
  tf <- as.trackframe(tf)

  cp_tf <- change_point_test(
    tf, q = q, N = N, alpha = alpha, seed = 2025, legacy_movement_criteria = TRUE
  )

  if (sum(cp_tf$sig) == 0) expect_warning(
    pej <- pej_implementation(easting(tf), northing(tf), alpha, q, N, tol) #nolint
  ) else pej <- pej_implementation(easting(tf), northing(tf), alpha, q, N, tol) #nolint

  # convert cps into format pej outputs
  cps_rcpp <- pej_style_cps(cp_tf)

  # filter only when there is movement and reverse
  bztf <- pej_tf_filter(cp_tf, tol)

  expect_equal(pej$bz1, easting(bztf), info = unique_ids(cp_tf))
  expect_equal(pej$bz2, northing(bztf), info = unique_ids(cp_tf))
  expect_equal(pej$sig, bztf$sig, info = unique_ids(cp_tf))
  expect_equal(pej$cps, cps_rcpp, info = unique_ids(cp_tf))
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