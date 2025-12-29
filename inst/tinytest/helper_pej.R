# Read in data file
# (e.g. "7_6august11.txt" in CPT2012 on Desktop)
# inp<-scan("~/Desktop/CPT2012/7_6august11.txt",list(x1=0,x2=0))

# setwd("~/travelpaths-devel")
# file <- "data/8_7august11.txt"
# inp <- scan(file, list(x1 = 0, x2 = 0))

pej_implementation <- function(
  x1,
  x2,
  alpha,
  q,
  n,
  min_move_dist,
  seed = 2025
) {
  # INPUTS

  # # input alpha
  # # alpha = significance level
  # # alpha = 0.05 is a convenient default
  # alpha <- 0.05

  # # input q
  # # Enter value of q
  # # q = 4 is used here just as a convenient default
  # # The user may like to replace this
  # # by the value of q obtained by running CPT_Rcode
  # # on part of the data
  # q <- 4

  # # input n
  # # n = total number of permutations (1 observed and n-1 simulated)
  # # n = 10000 is a convenient number
  # n <- 1000

  # # input min_move_dist
  # # min_move_dist = tolerance
  # # = maximum distance between indistinguishable positions
  # # min_move_dist = 0 is a convenient default
  # # The user may like to replace this by
  # # the average GPS error in research area
  # min_move_dist <- 0

  # PRELIMINARIES

  # Reverse the time-ordering,
  # so that (bx1[1], bx2[1]) refers to (final)
  #  putative goal

  bx1 <- 0 * x1
  bx2 <- 0 * x2

  x1_len <- length(x1)
  for (j in 1:x1_len) {
    bx1[j] <- x1[x1_len - j + 1]
    bx2[j] <- x2[x1_len - j + 1]
  }

  # Calculate the steps (bxdiff1, bxdiff2)
  bx1diff <- diff(bx1)
  bx2diff <- diff(bx2)

  # REMOVE POINTS AT WHICH ANIMAL STAYS STILL
  # ind is (reverse) time ordering of points
  # newp  > 0 if point differs from previous point
  ind <- c(1:x1_len)
  newp <- c(1:x1_len)
  for (j in 2:x1_len) {
    newp[j] <- ind[j] *
      (abs(bx1diff[j - 1]) > min_move_dist &&
        abs(bx2diff[j - 1]) > min_move_dist)
  }
  # bz1, bz2 are coordinates of points(in reverse time order) at which there is movement
  bz1 <- bx1[newp > 0]
  bz2 <- bx2[newp > 0]
  nz <- length(bz1)
  # We shall apply the CPT to points with coordinates (bz1,bz2)

  # Calculate the steps (bzdiff1, bzdiff2)
  bz1diff <- diff(bz1)
  bz2diff <- diff(bz2)

  # SOME DECLARATIONS

  # goal_no = number (from end) of current putative goal (with goal_no = 1 for end position
  # Thus goal_no = goal.t + 1
  # where goal.t was used in original code to refer to
  # time of current putative goal (with goal.t = 0 for # end position
  # start with goal_no = 1
  goal_no <- 1

  # last_no = number (backwards in time) of last position of interest
  # last_no = length(bz1) is a convenient default
  # (and includes all the positions)
  last_no <- length(bz1)

  no_of_nos <- length(bz1)

  r_sum_rand <- rep(0, len = n)
  r_sum_rand_r <- rep(0, len = n) #nolint

  # pr will store observed p-values in a run of r
  pr <- rep(1, len = no_of_nos)

  # sig is a vector indicating whether or not
  # waypoint k is detected as a possible change point:
  # sig[k] = 1  if change point detected at waypoint k
  # sig[k] = 0  otherwise
  sig <- rep(0, nz)

  # k is number of steps in “k-leg”
  k <- 0

  # LOOK (SEQUENTIALLY) FOR NEXT POSSIBLE CHANGE POINT

  ## Set rmin to control outer loop on goal_no
  ## rmin <- 1

  ## while((rmin > 0) && (goal_no < last_no – q)){
  ## A
  set.seed(seed)
  while (goal_no < last_no - q) {
    k <- 0

    # INCREASE k UNTIL NEXT POSSIBLE CHANGE POINT IS FOUND

    # Re-initialise pr
    pr <- 0 * pr + 1

    p <- 1

    while ((p > alpha) && (goal_no + q + k < last_no)) {
      # B

      k <- k + 1

      r1 <- sqrt(
        (bz1[goal_no + k] - bz1[goal_no])^2 +
          (bz2[goal_no + k] - bz2[goal_no])^2
      )
      r2 <- sqrt(
        (bz1[goal_no + k + q] - bz1[goal_no + k])^2 +
          (bz2[goal_no + k + q] - bz2[goal_no + k])^2
      )

      r_sum <- r1 + r2

      # r_sum_rand[1] = observed value of statistic r1 + r2
      r_sum_rand[1] <- r_sum

      # Now calculate statistic r1 + r2 for a further n-1 random permutations
      # and store in r_sum_rand
      for (it in 2:n) {
        # start of it loop C
        u <- runif(k + q, 0, 1)
        perm <- order(u)
        bz1r <- bz1[goal_no]
        bz2r <- bz2[goal_no]
        for (j in 1:k) {
          # start of j loop D
          bz1r <- bz1r + bz1diff[goal_no - 1 + perm[j]]
          bz2r <- bz2r + bz2diff[goal_no - 1 + perm[j]]
        } # end of j loop D

        r1_rand <- sqrt((bz1r - bz1[goal_no])^2 + (bz2r - bz2[goal_no])^2)
        r2_rand <- sqrt(
          (bz1[goal_no + k + q] - bz1r)^2 + (bz2[goal_no + k + q] - bz2r)^2
        )
        r_sum_rand[it] <- r1_rand + r2_rand
      } # end of it loop C

      p <- sum(r_sum_rand >= r_sum) / n

      pr[k] <- p
    } # end of ‘while’ on
    #  ((p > alpha) && (goal_no + q + k < last_no)) B

    # f is the first value of k in the current run that is significant
    f <- k

    while ((p <= alpha) && (goal_no + q + k < last_no)) {
      # E

      k <- k + 1
      r1 <- sqrt(
        (bz1[goal_no + k] - bz1[goal_no])^2 +
          (bz2[goal_no + k] - bz2[goal_no])^2
      )
      r2 <- sqrt(
        (bz1[goal_no + k + q] - bz1[goal_no + k])^2 +
          (bz2[goal_no + k + q] - bz2[goal_no + k])^2
      )

      r_sum <- r1 + r2

      # r_sum_rand[1] = observed value of statistic r1 + r2
      r_sum_rand[1] <- r_sum

      # Now calculate statistic r1 + r2 for a further n-1 random permutations
      # and store in r_sum_rand
      for (it in 2:n) {
        # start of it loop F
        u <- runif(k + q, 0, 1)
        perm <- order(u)
        bz1r <- bz1[goal_no]
        bz2r <- bz2[goal_no]
        for (j in 1:k) {
          # start of j loop G
          bz1r <- bz1r + bz1diff[goal_no - 1 + perm[j]]
          bz2r <- bz2r + bz2diff[goal_no - 1 + perm[j]]
        } # end of j loop G

        r1_rand <- sqrt((bz1r - bz1[goal_no])^2 + (bz2r - bz2[goal_no])^2)
        r2_rand <- sqrt(
          (bz1[goal_no + k + q] - bz1r)^2 + (bz2[goal_no + k + q] - bz2r)^2
        )
        r_sum_rand[it] <- r1_rand + r2_rand
      } # end of it loop F

      p <- sum(r_sum_rand >= r_sum) / n

      pr[k] <- p
    } # end of ‘while’ on
    #  ((p < alpha) && (goal_no + q + k < last_no)) E

    # l is the last value of k in the current run that is significant
    l <- k - 1

    # apply “peak rule”
    pr[f:l]
    rmin <- min(which(pr[f:l] == min(pr[f:l])))
    rmin <- if (l >= f) rmin else 0

    goal_no <- ifelse(rmin > 0, goal_no + f + rmin - 1, last_no)

    sig[goal_no] <- ifelse(goal_no == last_no, 0, 1)
  } # end of ‘while’ on (goal_no < last_no - q) # A

  #  Remove putative goal from list of CP’s
  sig[1] <- 0

  ###################################################
  # OUTPUT OF
  # (first, last) (row nos. of first and last times at “significant” change points)
  # (east, north) (their coordinates)

  # cp lists indices of waypoints identified as change points
  cp <- which(sig == 1) #nolint

  # bsig is sig in reverse time-order
  bsig <- rep(0, nz)
  for (j in 1:nz) {
    bsig[j] <- sig[nz - j + 1]
  }

  # “times” (row nos.) and coordinates of change points
  newpp <- newp[newp > min_move_dist]
  cp_time <- newpp[sig == 1]
  cp_bx1 <- bz1[sig == 1] #nolint
  cp_bx2 <- bz2[sig == 1] #nolint
  cp_no <- x1_len + 1 - cp_time
  # last is cp_no in reverse order
  last <- 0 * cp_no
  cp_leng <- length(cp_no)
  for (j in 1:cp_leng) {
    last[j] <- cp_no[cp_leng - j + 1]
  }
  # last contains row nos. of (last times at) change points
  # (east, north) are their coordinates
  east <- x2[last]
  north <- x1[last]

  # first contains row nos. of first times at change points
  first <- 0 * last
  for (j in seq_len(length(last))) {
    first[j] <- x1_len + 2 - min(which(newp > (x1_len + 1 - last[j])))
  }

  # PLOT WAYPOINTS AND MARK CHANGE POINTS

  # plot data
  # and overlay with change points in colour
  # For plotting purposes, reverse the order of
  # bz1, bz2 to get z1, z2
  z1 <- 0 * bz1
  z2 <- 0 * bz2
  nz <- length(z1)
  for (j in 1:nz) {
    z1[j] <- bz1[nz - j + 1]
    z2[j] <- bz2[nz - j + 1]
  }
  # s is vector to index the points
  s <- c(1:nz) #nolint

  # NOTE: If the data have come from a GPS then
  # it is likely that the first column of the
  # data file contains northings and the second
  # column contains eastings.
  # The portion of code below assumes this ordering
  # and produces a plot with the conventional
  # orientation (north at the top)

  # # set limits of plot
  # cxlim <- c(min(bz2),max(bz2))
  # cylim <- c(min(bz1) - sd(bz1diff),max(bz1))
  # plot(bz2,bz1, pch=18, xlim=cxlim, ylim=cylim, xlab="East", ylab="North")
  # title(main=paste("q = ", q, ", " , "alpha = ", alpha, ", ", "n = ", n , ", ",
  #  "min_move_dist = ", min_move_dist ,
  #   sep=""), sub ="Blue triangle = putative goal, red star = change pt.,
  #   red no. = row of data file")
  # segments(bz2[s], bz1[s], bz2[s+1], bz1[s+1])
  # par(new ="T")
  # plot(bz2[1],bz1[1],pch=17,col="blue", xlim=cxlim, ylim=cylim, xlab="", ylab="")
  # par(new ="T")
  # z1bsig <- z1[bsig==1]
  # z2bsig <- z2[bsig==1]
  # plot(z2bsig,z1bsig,pch=8,col="red", xlim=cxlim, ylim=cylim, xlab="", ylab="")
  # cpsame <- which(first == last)
  # cpdiff <- which(first != last)
  # lastsame <- last[cpsame]
  # firstdiff <- first[cpdiff]
  # lastdiff <- last[cpdiff]
  # z1bsame <- z1bsig[cpsame]
  # z2bsame <- z2bsig[cpsame]
  # z1bdiff <- z1bsig[cpdiff]
  # z2bdiff <- z2bsig[cpdiff]
  # par(new ="T")
  # text(z2bsame, z1bsame - 0.6*sd(bz1diff), lastsame, col="red", cex = 0.7)
  # par(new ="T")
  # text(z2bdiff, z1bdiff - 0.6*sd(bz1diff), paste(firstdiff,lastdiff,sep="-"),col="red", cex = 0.7)

  # “first” and “last” are the row nos. of the first and last times at `significant’ change points
  # “north” and “east” are the coordinates of these change points
  # “cps” contains “first, “last”, “north” and “east”.
  cps <- cbind(first, last, north, east)
  list(sig = sig, cps = cps, bz1 = bz1, bz2 = bz2)
}
