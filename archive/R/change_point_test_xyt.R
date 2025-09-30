


#' Change Point Test
#' 
#' Detecting change points in animal ranging data
#'
#' @param xyt a matrix with columns x, y (in cartesian coordinates) and t
#' @param alpha nominal significance level
#' @param q q value
#' @param N total number of permutations
#' @param min_move_dist maximum distance between indistinguishable positions
#'
#' @return TODO
#' @export
change_point_test_new <- function(xyt, alpha = 0.05, q = 4, N = 1000, min_move_dist = 0, nbatches = 10) {
  # # xyt
  # alpha = 0.05
  # q = 4
  # N = 1000
  # min_move_dist = 0
  # min_move_dist = 0.0001
  
  # reverse order
  b_xyt <- xyt[NROW(xyt):1,]
  # calcuate diff of coordinates
  b_xy_diff <- structure(apply(b_xyt[, 1:2], 2, FUN = diff), dimnames = list(NULL,c("x_diff", "y_diff")))
  # remove points at which animal stays still
  ind_new <- c(TRUE, sqrt(b_xy_diff[, "x_diff"]^2 + b_xy_diff[, "y_diff"]^2) > min_move_dist)
  b_xyt2 <- b_xyt[ind_new,]
  
  sig <- change_point_test_fit_new(bx = b_xyt2[, "x"], by = b_xyt2[, "y"], q = q, N = N, alpha = alpha, nbatches = nbatches) #FIXME bz1 and bz2
  
  # if alpha is null, that means we want return the probabilities accross k
  if(is.null(alpha)) return(sig)
  
  #  Remove putative goal from list of CP’s
  sig[1] <- 0
  b_xyt2 <- cbind(b_xyt2,
                  "sig" = sig,
                  "cp_no" = ifelse(sig == 0, 0, cumsum(sig)))
  # re-index - merge with b_xyt
  b_xyt <- merge(b_xyt, b_xyt2[, c("t", "sig")], by = "t", all.x = TRUE, sort = FALSE)
  # b_xyt <- b_xyt[rev(order(b_xyt[,"t"])), ]
  xyt <- b_xyt[order(b_xyt[,"t"]), ]

  xyt[, "cp_no"] <- ifelse(is.na(xyt[, "sig"]), NA,
                             ifelse(xyt[, "sig"] == 0, 0, cumsum(na.fill(xyt[, "sig"], fill = 0))))
  #na.locf
  xyt[, "sig"] <- na.locf(xyt[, "sig"], fromLast = TRUE, na.rm = FALSE)
  xyt[, "cp_no"] <- na.locf(xyt[, "cp_no"], fromLast = TRUE, na.rm = FALSE)
  
  return(xyt)
}


change_point_test_fit_new <- function (bx, by, q, N, alpha, nbatches = 10) {
  dqset.seed(2025)
  
  # Calculate the steps (bzdiff1, bzdiff2)
  bx_diff <- diff(bx)
  by_diff <- diff(bx)
  
  n_obs <- length(bx) #FIXME rename
  goal_no <- 1
  
  # pre-allocate
  Rsum_perm  <- rep(0, len=N)
  # Pr will store observed p-values in a run of r
  Pr <- rep(NA, len = n_obs)
  sig <- rep(0,n_obs)
  
  batch_size <- nbatches
  
  ###
  while(goal_no < n_obs - q){
    
    # k is number of steps in “k-leg”
    f <- NA
    l <- NA
    # INCREASE k UNTIL NEXT POSSIBLE CHANGE POINT IS FOUND
    
    # Re-initialise Pr 
    Pr <- NA*Pr
    
    # P <- 1
    k_abs_max <- n_obs - goal_no - q - 1
    all_ks  <- seq_len(k_abs_max)
    
    # ks <- split(all_ks, rep(seq_along(all_ks), each = batch_size)[seq_along(all_ks)])[[1]]
    for(ks in split(all_ks, rep(seq_len(ceiling(n_obs/batch_size)), each = batch_size)[seq_along(all_ks)])) {
      # for(ks in split(all_ks, rep(seq_along(all_ks), each = batch_size)[seq_along(all_ks)])) {
      #if(len(all_ks) > 0) { ks <- all_ks
      k_max <- max(ks)
      k_len <- length(ks)
      Rsum_perm <- matrix(rep(NA, len=N*k_len), c(N, k_len))

      R1 <- sqrt((bx[goal_no+ks] - bx[goal_no])^2 + (by[goal_no+ks] - by[goal_no])^2)
      R2 <- sqrt((bx[goal_no+ks+q] - bx[goal_no+ks])^2 + (by[goal_no+ks+q] - by[goal_no+ks])^2)
      Rsum <- R1 + R2
      
      # Rsum_perm[1,] = observed value of statistic R1 + R2
      Rsum_perm[1, ] <- Rsum
      
      perm <- sapply(ks,
                     function(k) t(sapply(
                       2:N,
                       function(it) c(dqrng::dqsample(k + q, k), rep(NA, k_max - k)),
                       simplify = 'array'
                     )),
                     simplify = 'array'
      )
      perm <- aperm(perm, c(1, 3, 2))

      bx_perm <- rowSums(array(bx_diff[goal_no-1+perm], c(N-1, k_len, k_max)), na.rm = TRUE, dims = 2) + bx[goal_no]
      by_perm <- rowSums(array(by_diff[goal_no-1+perm], c(N-1, k_len, k_max)), na.rm = TRUE, dims = 2) + by[goal_no]
      k_arr <- aperm(array(ks, c(k_len, N-1)), c(2,1)) #FIXME name?

      R1_perm <- sqrt((bx_perm - bx[goal_no])^2 + (by_perm  - by[goal_no])^2)
      R2_perm <- sqrt((bx[goal_no+k_arr+q] - bx_perm)^2 + (by[goal_no+k_arr+q] - by_perm)^2)
      Rsum_perm[2:N, seq_len(k_len)] <- R1_perm + R2_perm
      
      Rsum_arr <- aperm(array(Rsum, c(k_len, N)), c(2,1)) #FIXME name?
      Pr[ks] <- colSums(Rsum_perm >= Rsum_arr)/N
      
      if(!is.null(alpha)){
        f <- match(TRUE, Pr < alpha)
        l <- if(is.na(f)){NA} else {f + match(TRUE, Pr[f:k_max] >= alpha) - 2}
        if (!is.na(l)) break
      }
    }
    
    # If alpha is null, we want all the probabilities for one iteration
    if(is.null(alpha)) return(Pr)
    
    # apply “peak rule”
    rmin <- if(is.na(l)) 0 else if (l >= f) min(which(Pr[f:l] == min(Pr[f:l]))) else 0
    
    goal_no <- ifelse(rmin > 0, goal_no + f + rmin - 1, n_obs)
    
    sig[goal_no] <- ifelse(goal_no == n_obs,0,1)
    batch_size <- l
    
  }  # end of ‘while’ on (goal_no < n_obs - q) # A
  return(sig)
}


# #' Change Point Test Fit
# #'
# #' This function applies a change point test to points with coordinates (bz1, bz2).
# #'
# #' @param bz1 Numeric vector of x-coordinates.
# #' @param bz2 Numeric vector of y-coordinates.
# #' @param q Integer, the number of steps to look ahead.
# #' @param N Integer, the number of random permutations.
# #' @param alpha Numeric, the significance level.
# #' @return Numeric vector indicating change points.
# #' @export
# change_point_test_fit_rcpp <- function(bz1, bz2, q, N, alpha) {
#   .Call(.NAME = 'change_point_test_fit', bz1, bz2, q, N, alpha, PACKAGE = 'cpt')
# }

