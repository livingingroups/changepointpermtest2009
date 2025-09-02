source('helper_pej.R')
library(trackframe)
library(cpt)

#' @importFrom tinytest expect_equal
compare_to_pej <- function(tf, alpha, q, N, tol) {
  tf <- as.trackframe(tf)
  pej <- pej_implementation(easting(tf), northing(tf), alpha, q, N, tol)
  bz1 <- pej$bz1
  bz2 <- pej$bz2
  
  
  # tf <- cbind("x" = easting(tf), "y" = northing(tf), t = 1:length(inp[[1]]))
  # tf[nrow(tf):1,] <- tf[NROW(tf):1,]
  # # calcuate diff of coordinates
  # b_xy_diff <- structure(apply(tf[nrow(tf):1,][, 1:2], 2, FUN = diff), dimnames = list(NULL,c("x_diff", "y_diff")))
  # # remove points at which animal stays still
  # ind_new <- c(TRUE, sqrt(b_xy_diff[, "x_diff"]^2 + b_xy_diff[, "y_diff"]^2) > tol)
  # tf_move <- tf[nrow(tf):1,][ind_new,]


  tf_move <- tf[c(TRUE, sqrt(diff(easting(tf))^2 + diff(northing(tf))^2) > tol),]
  # reverse it
  tf_move <- tf_move[nrow(tf_move):1, ]
  # TODO: document change time as changes outcome
  old_time <- time(tf_move)
  tf_move[, attr(tf_move, "time")] <- seq_len(nrow(tf_move))
  sig_rcpp <- change_point_test(tf_move, q = q, N = N, alpha = alpha, seed = 2025 )$sig



  sig_rcpp[1] <- 0
  tf_move[, attr(tf_move, "time")] <- old_time
  tf_move <- cbind(tf_move,
                  "sig" = sig_rcpp,
                  "cp_no" = ifelse(sig_rcpp == 0, 0, cumsum(sig_rcpp)))

  # re-index - merge with b_xyt
  tf <- merge(tf, tf_move[, c("time", "sig")], by = "time", all.x = TRUE, sort = FALSE)
  # b_xyt <- b_xyt[rev(order(b_xyt[,"t"])), ]
  tf <- tf[order(tf[ ,"time"]), ]
  tf[, "cp_no"] <- ifelse(is.na(tf[, "sig"]), NA,
                             ifelse(tf[, "sig"] == 0, 0, cumsum(na.fill(tf[, "sig"], fill = 0))))
  #na.locf
  tf[, "sig"] <- na.locf(tf[, "sig"], fromLast = TRUE, na.rm = FALSE)
  tf[, "cp_no"] <- na.locf(tf[, "cp_no"], fromLast = TRUE, na.rm = FALSE)
  rownames(tf) <- NULL


  tf
  tf[which(tf$sig == 1),]

  xyt_cp <- tf[tf$sig != 0,]
  xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)

  cps_rcpp <- do.call("rbind", lapply(xyt_cp_split, function(x) {
    setNames(c(min(x$time), max(x$time), x$x1[1], x$x2[1]), c("first", "last", "north", "east"))
  }))

  rownames(cps_rcpp) <- NULL
  expect_equal(pej$bz1, tf_move[, "x1"])
  expect_equal(pej$bz2, tf_move[, "x2"])
  expect_equal(pej$sig, sig_rcpp)
  expect_equal(pej$cps, cps_rcpp)


}

data("cpttestdata", package = "cpt")
compare_to_pej(
  as.trackframe(cpttestdata, easting_col = 'x1', northing_col = 'x2'),
  alpha = 0.05,
  q = 4,
  N = 1000,
  tol = 0
)