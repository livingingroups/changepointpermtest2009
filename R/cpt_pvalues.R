#FIXME: work in progress 
# write S3 methods for trackframe, sftrack and move2
# check consistency with paper output and decide how to proceed


change_point_test_pvalue_xyt <- function(x, y, t, q_max = 6, N = 1000, tol = 0, ...) {
  checkmate::assert_numeric(x, min.len = 3L, any.missing = FALSE)
  checkmate::assert_numeric(y, len = length(x), any.missing = FALSE)
  checkmate::assert_numeric(t, len = length(x), any.missing = FALSE)
  checkmate::assert_integerish(q_max, len = 1, any.missing = FALSE, lower = 1)
  checkmate::assert_integerish(N, len = 1, any.missing = FALSE, lower = 1)
  checkmate::assert_numeric(tol, len = 1, any.missing = FALSE, lower = 0)
  
  # Reverse the time-ordering so that (bx[1], by[1]) refers to (final) 
  idx <- order(t, decreasing = TRUE)
  bx <- x[idx]
  by <- y[idx]
  bt <- t[idx]
  # calcuate diff of coordinates
  bxdiff <- diff(bx)
  bydiff <- diff(by)
  # remove points at which animal stays still
  is_moving <- c(TRUE, sqrt(bxdiff^2 + bydiff^2) > tol)
  bxm <- bx[is_moving]
  bym <- by[is_moving]
  btm <- bt[is_moving]
  pvalues <- change_point_fit_pvalue_new(bx = bxm, by = bym, q_max = q_max, N = N)
  
  return(pvalues)
}



#' Change Point Detection for Animal Movement Data
#'
#' Detects significant change points in animal movement trajectory data using a permutation-based approach.
#' This function identifies locations where the movement pattern significantly changes, which can represent
#' behavioral transitions or responses to environmental stimuli.
#'
#' @param data a matrix or data frame with columns for x-coordinates, y-coordinates, and time.
#'   For the default method, this should be a matrix or data frame with at least 3 columns.
#' @param q_max an integer specifying the maximum value of q.
#' @param N an integer specifying the number of random permutations for the permutation test
#'   Higher values provide more accurate p-values but increase computation time.
#' @param tol a numeric value specifying the maximum distance between indistinguishable positions.
#'   Points with movements smaller than this threshold will be considered stationary.
#' @param ... additional arguments passed to methods.
#'
#' @return An augmented data frame containing the original data with additional columns:
#'   \item{sig}{Binary indicator (1 or 0) of whether a point is a significant change point}
#'   \item{cp_no}{Sequential numbering of detected change points}
#'   
#' @details This function implements a sequential change point detection algorithm that uses
#'          a permutation test to identify significant changes in movement patterns. It compares
#'          the sum of distances between consecutive points against randomly permuted sequences
#'          to determine if a change point exists.
#'          
#'          The first point in the trajectory is never considered a change point, as it represents
#'          the starting location.
#'
#' @export
#'
#' @examples
#' library(cpt)
#' data("cpttestdata", package = "cpt")
#' 
#' pvalues <- change_point_test_pvalue(cpttestdata, q_max = 6, N = 100)
#' pvalues
change_point_test_pvalue <- function(data, alpha = 0.05, q = 4, N = 10000, tol = 0, clu = NULL, seed = NULL, ...) {
  UseMethod("change_point_test_pvalue")
}


change_point_test_pvalue <- function(data, q_max = 6, N = 1000, tol = 0, ...) {
  #FIXME: with S3 classes
  x_col <- colnames(data)[1]
  y_col <- colnames(data)[2]
  t_col <- colnames(data)[3]
  
  # reverse order
  b_xyt <- data[NROW(data):1,]
  # # calcuate diff of coordinates
  # b_xy_diff <- structure(apply(b_xyt[, c(x_col, y_col)], 2, FUN = diff), dimnames = list(NULL,c("x_diff", "y_diff")))
  # # remove points at which animal stays still #FIXME do we need it here?
  # ind_new <- c(TRUE, sqrt(b_xy_diff[, "x_diff"]^2 + b_xy_diff[, "y_diff"]^2) > tol)
  # b_xyt2 <- b_xyt[ind_new,]
  b_xyt2 <- b_xyt
  dim(b_xyt2)
  pvalues <- change_point_fit_pvalue_new(bx = b_xyt2[, x_col], by = b_xyt2[, y_col], q_max = q_max, N = N)
  # pvalues <- change_point_fit_pvalue_cpp(bx = b_xyt2[, x_col], by = b_xyt2[, y_col], q_max = q_max, N = N)
  return(pvalues)
}


# change_point_fit_pvalue_cpp <- function(bx, by, q_max, N, alpha) {
#   checkmate::assert_numeric(bx, any.missing = FALSE)
#   checkmate::assert_numeric(by, len = length(bx), any.missing = FALSE)
#   checkmate::assert_integerish(q_max, len = 1, any.missing = FALSE, lower = 1)
#   checkmate::assert_integerish(N, len = 1, any.missing = FALSE, lower = 1)
#   cpf <- rcpparma_change_point_test_pvalue(bx, by, as.integer(q_max), as.integer(N))
#   drop(cpf)
# }

#implemented in c++
change_point_fit_pvalue_new <- function(bx, by, q_max, N) {
  bxdiff <- diff(bx)
  bydiff <- diff(by)
  # q_max = maximum value of q
  # q_max =6 is a convenient default

  # last.t = "time" (backwards in time) of last position of interest
  # (and includes all the positions)
  n_obs <- length(bx)
  last.t <- length(bx) - q_max - 1

  no.of.t <- last.t + 1

  # Set up matrix P in which to store P-values
  # P <- matrix(rep(exp(2), q_max * no.of.t), nrow = no.of.t, ncol = q_max)
  P <- matrix(rep(NA, q_max * n_obs), nrow = n_obs, ncol = q_max)

  # N = total number of permutations (1 observed and N-1 simulated)
  # N = 1000 is a convenient number

  Rsumrand <- 0 * c(1:N)

  for (q in 1:q_max) { # start of q loop
    k_max <- n_obs - q - 1L
    for (k in 1:k_max) { # start of k loop

      R1 <- sqrt((bx[k + 1] - bx[1])^2 +
                 (by[k + 1] - by[1])^2)
      R2 <- sqrt((bx[k + q + 1] - bx[k + 1])^2 +
                 (by[k + q + 1] - by[k + 1])^2)

      Rsum <- R1 + R2
      # writeLines(paste(q, k, R1, R2, sep = ";"))

      # Rsumrand[1] = observed value of statistic R1 + R2
      Rsumrand[1] <- Rsum

      # Now calculate statistic R1 + R2 for a further N-1 random permutations
      # and store in Rsumrand
      for (it in 2:N) { # start of it loop
        u <- runif(k + q, 0, 1)  # FIXME: Why here "+ q" is needed?
        perm <- order(u)
        bxr <- bx[1]
        byr <- by[1]
        for (j in 1:k) { # start of j loop
          bxr <- bxr + bxdiff[perm[j]]
          byr <- byr + bydiff[perm[j]]
        } # end of j loop


        R1rand <- sqrt((bxr - bx[1])^2 + (byr - by[1])^2)
        R2rand <- sqrt((bx[k + q + 1] - bxr)^2 + (by[k + q + 1] - byr)^2)
        # writeLines(paste(R1rand, R2rand, sep = ";"))
        Rsumrand[it] <- R1rand + R2rand
      } # end of it loop

      # calculate P-values
      P[k, q] <- sum(Rsumrand >= Rsum) / N
    } # end of k loop
  } # end of q loop
  return(P)
}
