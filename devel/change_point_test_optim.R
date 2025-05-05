

#' Change Point Test
#' 
#' Detecting change points in animal ranging data
#'
#' @param tf an object of class "track_frame"
#' @param alpha nominal significance level
#' @param q q value
#' @param N total number of permutations
#' @param tol maximum distance between indistinguishable positions
#'
#' @return TODO
#' @export
change_point_test <- function(tf, alpha = 0.05, q = 4, N = 1000, tol = 0) {
  # tf
  # alpha = 0.05
  # q = 4
  # N = 1000
  # tol = 0
  # tol = 0.0001
  
  checkmate::assert_class(tf, "track_frame")
  # tf <- tf[seq(1, NROW(tf), by = 10),]
  x1 <- rev(longitude(tf))
  x2 <- rev(latitude(tf))

  # PRELIMINARIES
  
  # Reverse the time-ordering,
  # so that (bx1[1], bx2[1]) refers to (final) 
  #  putative goal
  bx1 <- rev(x1)
  bx2 <- rev(x2)
  xts_all <- xts(cbind(x1,x2), order.by = index(tf))
  
  # Calculate the steps (bxdiff1, bxdiff2)
  bx1diff <- diff(bx1)
  bx2diff <- diff(bx2)
  
  
  # REMOVE POINTS AT WHICH ANIMAL STAYS STILL
  # ind is (reverse) time ordering of points
  # newp  > 0 if point differs from previous point
  ind_new <- c(TRUE, abs(bx1diff) > tol & abs(bx2diff) > tol)
  bz1 <- bx1[ind_new]
  bz2 <- bx2[ind_new]
  #backwards index
  bindex <- rev(index(tf))[ind_new]
  
  sig <- change_point_test_fit(bz1 = bz1, bz2 = bz2, q = q, N = N, alpha = alpha)
  # sig <- change_point_test_fit_rcpp(bz1 = bz1, bz2 = bz2, q = q, N = N, alpha = alpha)
  
  
  #  Remove putative goal from list of CP’s
  sig[1] <- 0
  
  sig_xts <- xts(cbind(sig, bz1, bz2), order.by = bindex)
  sig_xts$cp_no <- 0
  sig_xts$cp_no[sig_xts$sig == 1] <- cumsum(sig_xts$sig[sig_xts$sig == 1]) #TODO check if cp is the same
  # replaces first in old code (when point before is within tolerance then it is assumed to be the cp already)
  sig_xts <- merge(xts_all, sig_xts[, c("sig", "cp_no")])
  # sig_xts$no[!ind_new] <- NA
  sig_xts$cp_no <- na.locf(sig_xts$cp_no, fromLast = TRUE)
  # sig_xts$sig[!ind_new] <- NA
  sig_xts$sig <- na.locf(sig_xts$sig, fromLast = TRUE)
  
  
  # or return tf?
  return(sig_xts)
  # cpt <- sig_xts[sig_xts$sig == 1, ] #order like in bsig
  # return(cpt)
  
}

change_point_test_fit <- function (bz1, bz2, q, N, alpha) {
  set.seed(2025)
  nz <- length(bz1)
  # We shall apply the CPT to points with coordinates (bz1,bz2)
  
  # Calculate the steps (bzdiff1, bzdiff2)
  bz1diff <- diff(bz1)
  bz2diff <- diff(bz2)
  
  # SOME DECLARATIONS
  
  # goal.no = number (from end) of current putative goal (with goal.no = 1 for end position 
  # Thus goal.no = goal.t + 1 
  # where goal.t was used in original code to refer to 
  # time of current putative goal (with goal.t = 0 for # end position 
  # start with goal.no = 1 
  goal.no <- 1
  
  # last.no = number (backwards in time) of last position of interest
  # last.no = length(bz1) is a convenient default
  # (and includes all the positions)
  last.no <- length(bz1) 
  
  no.of.nos <- length(bz1)
  
  Rsumrand  <- rep(0, len=N)
  Rsumrandr <- rep(0, len=N)
  
  # Pr will store observed p-values in a run of r
  Pr <- rep(NA, len = no.of.nos)
  
  # sig is a vector indicating whether or not 
  # waypoint k is detected as a possible change point:
  # sig[k] = 1  if change point detected at waypoint k
  # sig[k] = 0  otherwise
  sig <- rep(0,nz)
  
  
  # LOOK (SEQUENTIALLY) FOR NEXT POSSIBLE CHANGE POINT
  
  ## Set rmin to control outer loop on goal.no
  ## rmin <- 1
  
  ## while((rmin > 0) && (goal.no < last.no – q)){ 
  ## A
  
  while(goal.no < last.no - q){
    
    # k is number of steps in “k-leg”
    k <- 0 
    f <- NA
    l <- NA
    # INCREASE k UNTIL NEXT POSSIBLE CHANGE POINT IS FOUND
    
    # Re-initialise Pr 
    Pr <- NA*Pr
    
    P <- 1
    found_P_leq_alpha <- FALSE
    found_P_back_up <- FALSE

    for(k in seq_len(last.no - goal.no - q - 1)) {
      # B
      
      R1 <- sqrt((bz1[goal.no+k] - bz1[goal.no])^2 + (bz2[goal.no+k] - bz2[goal.no])^2)
      R2 <- sqrt((bz1[goal.no+k+q] - bz1[goal.no+k])^2 + (bz2[goal.no+k+q] - bz2[goal.no+k])^2)
      
      Rsum <- R1 + R2
      
      # Rsumrand[1] = observed value of statistic R1 + R2
      Rsumrand[1] <- Rsum

      perm <- matrix(sapply(2:N, function(it) sample(k + q, k)), c(N-1, k), byrow = TRUE)
      bz1r <- rowSums(matrix(bz1diff[goal.no-1+perm], c(N-1, k))) + bz1[goal.no]
      bz2r <- rowSums(matrix(bz2diff[goal.no-1+perm], c(N-1, k))) + bz2[goal.no]
      R1rand <- sqrt((bz1r - bz1[goal.no])^2 + (bz2r  - bz2[goal.no])^2)
      R2rand <- sqrt((bz1[goal.no+k+q] - bz1r)^2 + (bz2[goal.no+k+q] - bz2r)^2)
      Rsumrand[2:N] <- R1rand + R2rand

      # for: goal.no=5
      # P = .11, Pr[0] is NA, f is 6, Pr[20] .0414

      # while: goal.no 5
      # P: .1105, Pr[0] is 6639, f is 6, Pr[20] is .0399
      P <- sum(Rsumrand >= Rsum)/N

      Pr[k] <- P

      # first look for P < alpha
      if(!found_P_leq_alpha && P <= alpha) {
        # f is the first value of k in the current run that is significant
        f <- k
        found_P_leq_alpha <- TRUE
      # once p <= alpha is found, look for P >= alpha
      } else if (found_P_leq_alpha && P > alpha) {
        # l is the last value of k in the current run that is significant
        l <- k - 1
        # found_P_back_up <- TRUE
        break
      }
    }  # end of ‘while’ on 
    # 13/for -> P .178
    # 12/while -> P .2108
    # 13/for -> goal.no 13
    # 12/while -> goal.no 14
    print(k)

    # apply “peak rule”
    rmin <- if(is.na(l)) 0 else if (l >= f) min(which(Pr[f:l] == min(Pr[f:l]))) else 0
    # 21/for -> rmin = 3, goal.no = 13
    # 21/while -> rmin = 4, goal.no = 14
    
    goal.no <- ifelse(rmin > 0, goal.no + f + rmin - 1, last.no)
    
    sig[goal.no] <- ifelse(goal.no == last.no,0,1)
    
  }  # end of ‘while’ on (goal.no < last.no - q) # A
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
