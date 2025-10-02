source("helper_pej.R")
library("tinytest")
library("trackframe")
library("cpt")


pej_tf_filter <- function(tf, min_move_dist) {
  tf <- tf[
    c(
      abs(diff(easting(tf))) > min_move_dist &
        abs(diff(northing(tf))) > min_move_dist,
      TRUE
    ),
  ]
  tf <- tf[rev(seq_len(nrow(tf))), ]
  tf
}


pej_style_cps <- function(tf) {
  xyt_cp <- tf[tf$cp_no != 0, ]
  xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)
  cps_rcpp <- do.call(
    "rbind",
    lapply(xyt_cp_split, function(x) {
      setNames(
        # pej returns indices, not times.
        # because of min_move_dist and how it's used, there may be more than one easting, northing
        # pej takes the last one
        as.numeric(c(
          min(rownames(x)),
          max(rownames(x)),
          tail(easting(x), 1),
          tail(northing(x), 1)
        )),
        c("first", "last", "north", "east")
      )
    })
  )
  rownames(cps_rcpp) <- NULL

  # Default in case no change points
  if (is.null(cps_rcpp)) {
    cps_rcpp <- structure(
      -Inf,
      dim = c(1L, 1L),
      dimnames = list(NULL, "first")
    )
  }
  cps_rcpp
}


compare_to_pej <- function(tf, alpha, q, n, min_move_dist) {
  tf <- as.trackframe(tf)
  cp_tf <- change_point_test(
    tf,
    q = q,
    n = n,
    alpha = alpha,
    seed = 2025,
    legacy_movement_criteria = TRUE
  )

  #nolint start
  pej <- pej_implementation(easting(tf), northing(tf), alpha, q, n = n, min_move_dist)

  # convert cps into format pej outputs
  cps_rcpp <- pej_style_cps(cp_tf) # FIXME:
  #nolint end

  # filter only when there is movement and reverse
  bztf <- pej_tf_filter(cp_tf, min_move_dist)

  expect_equal(pej$bz1, easting(bztf))
  expect_equal(pej$bz2, northing(bztf))
  expect_equal(pej$sig, bztf$sig)
  expect_equal(pej$cps, cps_rcpp)
}


data("cpttestdata", package = "cpt")
compare_to_pej(
  tf = as.trackframe(cpttestdata, easting_col = "x", northing_col = "y"),
  alpha = 0.05,
  q = 4,
  n = 1000,
  min_move_dist = 0
)


data("cptfiguredata_tf")

# FIXME: not working
# lapply(
#   split_by_id(cptfiguredata_tf),
#   compare_to_pej,
#   alpha = 0.05,
#   q = 4,
#   n = 1000,
#   min_move_dist = 0
# )

# i=1
# tf <- split_by_id(cptfiguredata_tf)[[i]]
# alpha = 0.05
# q = 4
# n = 1000
# min_move_dist = 0
