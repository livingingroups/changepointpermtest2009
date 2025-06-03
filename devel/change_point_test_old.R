

#' Title
#'
#' @param tf 
#' @param alpha 
#' @param q 
#' @param N 
#' @param tol 
#'
#' @return
#' @export
#'
#' @examples
change_point_test_old <- function(tf, alpha = 0.05, q = 4, N = 1000, tol = 0) {
  # tf
  # alpha = 0.05
  # q = 4
  # N = 1000
  # tol = 0
  
  assert_class(tf, "track_frame")
  # tf <- tf[seq(1, NROW(tf), by = 10),]
  x1 <- rev(longitude(tf))
  x2 <- rev(latitude(tf))
  # rev(zoo(x1, order.by = index(tf)))
  
  # x1 <- x1[seq(1, length(x1), by = 10)]
  # x2 <- x2[seq(1, length(x2), by = 10)]
  
  # PRELIMINARIES
  
  # Reverse the time-ordering,
  # so that (bx1[1], bx2[1]) refers to (final) 
  #  putative goal
  
  bx1 <- 0*x1
  bx2 <- 0*x2
  
  n <- length(x1)
  for (j in 1:n){
    bx1[j] <- x1[n-j+1]
    bx2[j] <- x2[n-j+1]
  }
  
  
  # Calculate the steps (bxdiff1, bxdiff2)
  bx1diff <- diff(bx1)
  bx2diff <- diff(bx2)
  
  
  # REMOVE POINTS AT WHICH ANIMAL STAYS STILL
  # ind is (reverse) time ordering of points
  # newp  > 0 if point differs from previous point
  ind <- c(1:n)
  newp <- c(1:n)
  for (j in 2:n){
    newp[j] <- ind[j]*(abs(bx1diff[j-1]) > tol && abs(bx2diff[j-1]) > tol)
  }
  # bz1, bz2 are coordinates of points(in reverse time order) at which there is movement 
  bz1 <- bx1[newp >0]
  bz2 <- bx2[newp >0]
  
  #backwards index
  bindex <- rev(index(tf))[newp >0]
  
  
  
  
  sig <- change_point_test_fit(bz1 = bz1, bz2 = bz2, q = q, N = N, alpha = alpha)
  
  #  Remove putative goal from list of CP’s
  sig[1] <- 0
  
  length(sig)
  length(bx1)
  length(bz1)
  length(index(tf))
  length(bindex)
  sig_xts <- xts(cbind(sig, bz1, bz2), order.by = bindex)
  sig_xts$no <- cumsum(sig_xts$sig) #TODO check if cp is the same
  
  # or return tf?
  return(sig_xts)
  # cpt <- sig_xts[sig_xts==1, ] #order like in bsig
  # return(cpt)
  
  # # OUTPUTS
  # 
  # # OUTPUT OF 
  # # (first, last) (row nos. of first and last times at “significant” change points) 
  # # (east, north) (their coordinates) 
  # 
  # # cp lists indices of waypoints identified as change points
  # cp <- which(sig==1)
  # 
  # 
  # # bsig is sig in reverse time-order
  # bsig <- rep(0,nz)
  # for (j in 1:nz){
  #   bsig[j] <- sig[nz-j+1]
  # }
  # 
  # # “times” (row nos.) and coordinates of change points
  # newpp <- newp[newp > tol]
  # cp.time <- newpp[sig==1]
  # cp.bx1 <- bz1[sig==1]
  # cp.bx2 <- bz2[sig==1]
  # cp.no <- n + 1 - cp.time
  # # last is cp.no in reverse order
  # last <- 0*cp.no
  # cp.leng <- length(cp.no)
  # for (j in 1:cp.leng){
  #   last[j] <- cp.no[cp.leng-j+1]
  # }
  # # last contains row nos. of (last times at) change points 
  # # (east, north) are their coordinates 
  # east <- x2[last]
  # north <- x1[last]
  # 
  # # first contains row nos. of first times at change points 
  # first <- 0*last
  # for (j in 1:length(last)){
  #   first[j] <- n+2 - min(which(newp > (n+1-last[j])))
  # }
  
}

change_point_test_fit <- function (bz1, bz2, q, N, alpha) {

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
  Pr <- rep(1, len= no.of.nos)

  # sig is a vector indicating whether or not
  # waypoint k is detected as a possible change point:
  # sig[k] = 1  if change point detected at waypoint k
  # sig[k] = 0  otherwise
  sig <- rep(0,nz)


  # k is number of steps in “k-leg”
  k <- 0

  # k is number of steps in “k-leg”
  k <- 0

  # LOOK (SEQUENTIALLY) FOR NEXT POSSIBLE CHANGE POINT

  ## Set rmin to control outer loop on goal.no
  ## rmin <- 1

  ## while((rmin > 0) && (goal.no < last.no – q)){
  ## A

  while(goal.no < last.no - q){

    k <- 0
    print(k)
    # INCREASE k UNTIL NEXT POSSIBLE CHANGE POINT IS FOUND

    # Re-initialise Pr
    Pr <- 0*Pr + 1

    P <- 1

    while ((P > alpha) && (goal.no + q + k < last.no)){
      # B

      k <- k + 1

      R1 <- sqrt((bz1[goal.no+k] - bz1[goal.no])^2 + (bz2[goal.no+k] - bz2[goal.no])^2)
      R2 <- sqrt((bz1[goal.no+k+q] - bz1[goal.no+k])^2 + (bz2[goal.no+k+q] - bz2[goal.no+k])^2)

      Rsum <- R1 + R2

      # Rsumrand[1] = observed value of statistic R1 + R2
      Rsumrand[1] <- Rsum

      # Now calculate statistic R1 + R2 for a further N-1 random permutations
      # and store in Rsumrand
      for (it in 2:N){
        # start of it loop C
        #FIXME RH: use seed here
        u <- runif(k+q,0,1)
        perm <- order(u)
        bz1r <- bz1[goal.no]
        bz2r <- bz2[goal.no]
        for (j in 1:k){ # start of j loop D
          bz1r <- bz1r + bz1diff[goal.no-1+perm[j]]
          bz2r <- bz2r + bz2diff[goal.no-1+perm[j]]
        } # end of j loop D

        R1rand <- sqrt((bz1r - bz1[goal.no])^2 + (bz2r  - bz2[goal.no])^2)
        R2rand <- sqrt((bz1[goal.no+k+q] - bz1r)^2 + (bz2[goal.no+k+q] - bz2r)^2)
        Rsumrand[it] <- R1rand + R2rand
      } # end of it loop C

      P <- sum(Rsumrand >= Rsum)/N

      Pr[k] <- P

    }  # end of ‘while’ on
    #  ((P > alpha) && (goal.no + q + k < last.no)) B

    # f is the first value of k in the current run that is significant
    f <- k

    while ((P <= alpha) && (goal.no + q + k < last.no)){
      # E

      k <- k+1
      R1 <- sqrt((bz1[goal.no+k] - bz1[goal.no])^2 + (bz2[goal.no+k] - bz2[goal.no])^2)
      R2 <- sqrt((bz1[goal.no+k+q] - bz1[goal.no+k])^2 + (bz2[goal.no+k+q] - bz2[goal.no+k])^2)


      Rsum <- R1 + R2

      # Rsumrand[1] = observed value of statistic R1 + R2
      Rsumrand[1] <- Rsum

      # Now calculate statistic R1 + R2 for a further N-1 random permutations
      # and store in Rsumrand
      for (it in 2:N){ # start of it loop F
        u <- runif(k+q,0,1)
        perm <- order(u)
        bz1r <- bz1[goal.no]
        bz2r <- bz2[goal.no]
        for (j in 1:k){ # start of j loop G
          bz1r <- bz1r + bz1diff[goal.no-1+perm[j]]
          bz2r <- bz2r + bz2diff[goal.no-1+perm[j]]
        } # end of j loop G

        R1rand <- sqrt((bz1r - bz1[goal.no])^2 + (bz2r  - bz2[goal.no])^2)
        R2rand <- sqrt((bz1[goal.no+k+q] - bz1r)^2 + (bz2[goal.no+k+q] - bz2r)^2)
        Rsumrand[it] <- R1rand + R2rand
      } # end of it loop F

      P <- sum(Rsumrand >= Rsum)/N

      Pr[k] <- P

    }  # end of ‘while’ on
    #  ((P < alpha) && (goal.no + q + k < last.no)) E

    # l is the last value of k in the current run that is significant
    l <- k - 1


    # apply “peak rule”
    Pr[f:l]
    rmin <- min(which(Pr[f:l] == min(Pr[f:l])))
    rmin <- if(l >=f) rmin else 0

    goal.no <- ifelse(rmin > 0, goal.no + f + rmin - 1, last.no)

    sig[goal.no] <- ifelse(goal.no == last.no,0,1)

  }  # end of ‘while’ on (goal.no < last.no - q) # A
  return(sig)
}
