

#' Change Point Test
#' 
#' Detecting change points in animal ranging data
#'
#' @param tf an object of class "trackframe"
#' @param alpha nominal significance level
#' @param q q value
#' @param N total number of permutations
#' @param tol maximum distance between indistinguishable positions
#'
#' @return TODO
#' @export
change_point_test_vectorized <- function(tf, alpha = 0.05, q = 4, N = 1000, tol = 0) {
  # tf
  # alpha = 0.05
  # q = 4
  # N = 1000
  # tol = 0
  # tol = 0.0001
  
  # # Ugly implementation of multiple tracks in the frame
  # tf_ids <- unique_ids(tf)
  # if(nrow(tf_ids) > 1) return(
  #   do.call(rbind,
  #     lapply(
  #       seq_len(nrow(tf_ids)),
  #       function(id_row_idx) {
  #         id <- tf_ids[id_row_idx,]
  #         # calling itself, unnecessary, but it works
  #         cp <- change_point_test(
  #           select_id(tf, id),
  #           alpha = alpha, q = q, N = N, tol = tol
  #         )
  #         cp[, names(tf_ids)] <- id
  #         cp
  #       }
  #     )
  #   )
  # )
  
  checkmate::assert_class(tf, "trackframe")
  # tf <- tf[seq(1, NROW(tf), by = 10),]
  x1 <- easting(tf)
  x2 <- northing(tf)

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
  ind_new <- c(TRUE, sqrt(bx1diff^2 + bx2diff^2) > tol)
  bz1 <- bx1[ind_new]
  bz2 <- bx2[ind_new]
  #backwards index
  bindex <- rev(index(tf))[ind_new]
  
  sig <- change_point_test_fit_vectorized(bz1 = bz1, bz2 = bz2, q = q, N = N, alpha = alpha)
  # sig <- change_point_test_fit_rcpp(bz1 = bz1, bz2 = bz2, q = q, N = N, alpha = alpha)
  
  # if alpha is null, that means we want return the probabilities accross k
  if(is.null(alpha)) return(sig)
  
  
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
  return(as.data.frame(sig_xts))
  # cpt <- sig_xts[sig_xts$sig == 1, ] #order like in bsig
  # return(cpt)
  
}


change_point_test_fit_vectorized <- function (bz1, bz2, q, N, alpha, start_batch_size = 10) {

  # set.seed(2025)
  # dqrng::dqset.seed(2025)
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

  # batch size is how many candidate ks to calculate for 
  # in each iteration
  batch_size <- start_batch_size
  
  # LOOK (SEQUENTIALLY) FOR NEXT POSSIBLE CHANGE POINT
  
  ## Set rmin to control outer loop on goal.no
  ## rmin <- 1
  
  ## while((rmin > 0) && (goal.no < last.no – q)){ 
  ## A
  
  while(goal.no < last.no - q){
    
    # k is number of steps in “k-leg”
    f <- NA
    l <- NA
    # INCREASE k UNTIL NEXT POSSIBLE CHANGE POINT IS FOUND
    
    # Re-initialise Pr 
    Pr <- NA*Pr
    
    P <- 1
    found_P_leq_alpha <- FALSE
    found_P_back_up <- FALSE

    k_abs_max <- last.no - goal.no - q - 1
    all_ks  <- seq_len(k_abs_max)
    for(ks in split(all_ks, rep(seq_along(all_ks), each = batch_size)[seq_along(all_ks)])) {
    #if(len(all_ks) > 0) { ks <- all_ks
      k_max <- max(ks)
      k_len <- length(ks)
      Rsumrand <- matrix(rep(NA, len=N*k_len), c(N, k_len))
      Rsumrandr <- matrix(rep(NA, len=N*k_len), c(N, k_len))
      
      # B
      
      R1 <- sqrt((bz1[goal.no+ks] - bz1[goal.no])^2 + (bz2[goal.no+ks] - bz2[goal.no])^2)
      R2 <- sqrt((bz1[goal.no+ks+q] - bz1[goal.no+ks])^2 + (bz2[goal.no+ks+q] - bz2[goal.no+ks])^2)
      
      Rsum <- R1 + R2
      
      # Rsumrand[1,] = observed value of statistic R1 + R2
      Rsumrand[1, ] <- Rsum
      

      perm <- sapply(ks,
        function(k) t(sapply(
          2:N,
          # function(it) c(dqrng::dqsample(k + q, k), rep(NA, k_max - k)),
          function(it) c(sample(k + q, k), rep(NA, k_max - k)),
          simplify = 'array'
        )),
        simplify = 'array'
      )

      # # check dim
      # stopifnot(all.equal(dim(perm), c(N-1, k_max, k_len)))
      
      perm <- aperm(perm, c(1, 3, 2))

      # stopifnot(all.equal(dim(perm), c(N-1, k_len, k_max)))

      #
      # # Check that the sample without replacement is
      # # along the correct axis
      # for(ts in 1:(N-1)){
      #   for(k_idx in seq_along(ks)){
      #     stopifnot(length(unique(na.omit(perm[ts,k_idx,]))) == ks[k_idx])
      #   }
      # }

      bz1r <- rowSums(array(bz1diff[goal.no-1+perm], c(N-1, k_len, k_max)), na.rm = TRUE, dims = 2) + bz1[goal.no]
      bz2r <- rowSums(array(bz2diff[goal.no-1+perm], c(N-1, k_len, k_max)), na.rm = TRUE, dims = 2) + bz2[goal.no]
      k_arr <- aperm(array(ks, c(k_len, N-1)), c(2,1)) 
      # # check that orientation is correct
      #stopifnot(all.equal(dim(k_arr), c(N-1, k_len)))
      #stopifnot(all.equal(k_arr[1,], ks))

      R1rand <- sqrt((bz1r - bz1[goal.no])^2 + (bz2r  - bz2[goal.no])^2)
      R2rand <- sqrt((bz1[goal.no+k_arr+q] - bz1r)^2 + (bz2[goal.no+k_arr+q] - bz2r)^2)
      Rsumrand[2:N, seq_len(k_len)] <- R1rand + R2rand

      Rsum_arr <- aperm(array(Rsum, c(k_len, N)), c(2,1)) 
      Pr[ks] <- colSums(Rsumrand >= Rsum_arr)/N

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
    
    goal.no <- ifelse(rmin > 0, goal.no + f + rmin - 1, last.no)
    
    sig[goal.no] <- ifelse(goal.no == last.no,0,1)
    batch_size <- l
    
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
