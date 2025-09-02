source('helper_pej.R')
library(trackframe)
library(cpt)

compare_to_pej <- function(xy, alpha, q, N, tol) {
  #tf <- as.trackframe(tf)
  #pej_result <- pej_implementation(easting(tf), northing(tf), alpha, q, N, tol)
  pej_result <- pej_implementation(xy[,1], xy[,2], alpha, q, N, tol)
  xyt <- xyt2 <- cbind("x" = inp[[1]], "y" = inp[[2]], t = 1:length(inp[[1]]))
  b_xyt <- xyt[NROW(xyt):1,]
  # calcuate diff of coordinates
  b_xy_diff <- structure(apply(b_xyt[, 1:2], 2, FUN = diff), dimnames = list(NULL,c("x_diff", "y_diff")))
  # remove points at which animal stays still
  ind_new <- c(TRUE, sqrt(b_xy_diff[, "x_diff"]^2 + b_xy_diff[, "y_diff"]^2) > tol)
  b_xyt2 <- b_xyt[ind_new,]

  set.seed(2025)
  sig_rcpp <- change_point_fit(bx = b_xyt2[, 1], by = b_xyt2[, 2], q = q, N = N, alpha = alpha) #FIXME bz1 and bz2
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


  cps
  xyt
  xyt[which(xyt$sig == 1),]

  xyt_cp <- xyt[xyt$sig != 0,]
  xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)

  cps_rcpp <- do.call("rbind", lapply(xyt_cp_split, function(x) {
    setNames(c(min(x$t), max(x$t), x$x[1], x$y[1]), c("first", "last", "north", "east"))
}))

rownames(cps_rcpp) <- NULL

expect_equal(b_xyt2[, "x"], bz1)
expect_equal(b_xyt2[, "y"], bz2)
expect_equal(sig, sig_rcpp)
expect_equal(cps, cps_rcpp)


}

# Read in data file
# (e.g. "7_6august11.txt" in CPT2012 on Desktop)
# inp<-scan("~/Desktop/CPT2012/7_6august11.txt",list(x1=0,x2=0))

# setwd("~/travelpaths-devel")
# file <- "data/8_7august11.txt"
# inp <- scan(file, list(x1 = 0, x2 = 0))
data("cpttestdata", package = "cpt")
inp <- list(cpttestdata[,1], cpttestdata[,2])

x1<-inp[[1]]
x2<-inp[[2]]
# Inspect first few rows of the data
xy <- cbind(x1,x2)
head(xy)
alpha <- 0.05
q <- 4
N <- 1000
tol <- 0

compare_to_pej(xy, alpha, q, N, tol)