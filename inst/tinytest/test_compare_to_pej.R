source('helper_pej.R')
library(trackframe)
library(cpt)

#' @importFrom tinytest expect_equal
compare_to_pej <- function(tf, alpha, q, N, tol) {
  tf <- as.trackframe(tf)
  pej <- pej_implementation(easting(tf), northing(tf), alpha, q, N, tol)
  bz1 <- pej$bz1
  bz2 <- pej$bz2
  
  
  xyt <- xyt2 <- cbind("x" = easting(tf), "y" = northing(tf), t = 1:length(inp[[1]]))
  b_xyt <- xyt[NROW(xyt):1,]
  # calcuate diff of coordinates
  b_xy_diff <- structure(apply(b_xyt[, 1:2], 2, FUN = diff), dimnames = list(NULL,c("x_diff", "y_diff")))
  # remove points at which animal stays still
  ind_new <- c(TRUE, sqrt(b_xy_diff[, "x_diff"]^2 + b_xy_diff[, "y_diff"]^2) > tol)
  b_xyt2 <- b_xyt[ind_new,]


  tf_move <- tf[c(TRUE, sqrt(diff(easting(tf))^2 + diff(northing(tf))^2) > tol),]
  # reverse it
  tf_move <- tf_move[nrow(tf_move):1, ]
  # TODO: document change time as chai
  tf_move[, attr(tf_move, "time")] <- seq_len(nrow(tf_move))
  set.seed(2025)
  sig_rcpp <- change_point_test(tf_move, q = q, N = N, alpha = alpha, seed = 2025 )$sig #FIXME bz1 and bz2
  # 89785563 0.94423821 0.93400825 0.97844700 0.06036534 0.04507917 0.83709193 0.48538282 0.85336106 0.59090350 0.82804696 0.79522473 0.47586021
  # all.equal(u, sig_rcpp) #last runif is equal
  


  sig_rcpp[1] <- 0
  b_xyt2 <- cbind(b_xyt2,
                  "sig" = sig_rcpp,
                  "cp_no" = ifelse(sig_rcpp == 0, 0, cumsum(sig_rcpp)))

  # re-index - merge with b_xyt
  b_xyt <- merge(b_xyt, b_xyt2[, c("t", "sig")], by = "t", all.x = TRUE, sort = FALSE)
  # b_xyt <- b_xyt[rev(order(b_xyt[,"t"])), ]
  xyt <- b_xyt[order(b_xyt[,"t"]), ]
  xyt[, "cp_no"] <- ifelse(is.na(xyt[, "sig"]), NA,
                             ifelse(xyt[, "sig"] == 0, 0, cumsum(na.fill(xyt[, "sig"], fill = 0))))
  #na.locf
  xyt[, "sig"] <- na.locf(xyt[, "sig"], fromLast = TRUE, na.rm = FALSE)
  xyt[, "cp_no"] <- na.locf(xyt[, "cp_no"], fromLast = TRUE, na.rm = FALSE)
  rownames(xyt) <- NULL


  xyt
  xyt[which(xyt$sig == 1),]

  xyt_cp <- xyt[xyt$sig != 0,]
  xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)

  cps_rcpp <- do.call("rbind", lapply(xyt_cp_split, function(x) {
    setNames(c(min(x$t), max(x$t), x$x[1], x$y[1]), c("first", "last", "north", "east"))
  }))

  rownames(cps_rcpp) <- NULL
  #browser()
  expect_equal(pej$bz1, b_xyt2[, "x"])
  expect_equal(pej$bz2, b_xyt2[, "y"])
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