rm(list=ls())
library(cpt)

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


# INPUTS

# input alpha
# alpha = significance level
# alpha = 0.05 is a convenient default
alpha <- 0.05

# input q
# Enter value of q
# q = 4 is used here just as a convenient default
# The user may like to replace this 
# by the value of q obtained by running CPT_Rcode 
# on part of the data 
q <- 4

# input N 
# N = total number of permutations (1 observed and N-1 simulated)
# N = 10000 is a convenient number
N <- 1000

# input tol 
# tol = tolerance 
# = maximum distance between indistinguishable positions
# tol = 0 is a convenient default
# The user may like to replace this by 
# the average GPS error in research area
tol <- 0


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

# LOOK (SEQUENTIALLY) FOR NEXT POSSIBLE CHANGE POINT

## Set rmin to control outer loop on goal.no
## rmin <- 1

## while((rmin > 0) && (goal.no < last.no – q)){ 
## A
set.seed(2025)
while(goal.no < last.no - q){
  
  k <- 0 
  
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

#  Remove putative goal from list of CP’s
sig[1] <- 0


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

sig
sig_rcpp


all.equal(b_xyt2[, "x"], bz1)
all.equal(b_xyt2[, "y"], bz2)
all.equal(sig, sig_rcpp)


###################################################
# OUTPUT OF 
# (first, last) (row nos. of first and last times at “significant” change points) 
# (east, north) (their coordinates) 

# cp lists indices of waypoints identified as change points
cp <- which(sig==1)

# bsig is sig in reverse time-order
bsig <- rep(0,nz)
for (j in 1:nz){
  bsig[j] <- sig[nz-j+1]
}

# “times” (row nos.) and coordinates of change points
newpp <- newp[newp > tol]
cp.time <- newpp[sig==1]
cp.bx1 <- bz1[sig==1]
cp.bx2 <- bz2[sig==1]
cp.no <- n + 1 - cp.time
# last is cp.no in reverse order
last <- 0*cp.no
cp.leng <- length(cp.no)
for (j in 1:cp.leng){
  last[j] <- cp.no[cp.leng-j+1]
}
# last contains row nos. of (last times at) change points 
# (east, north) are their coordinates 
east <- x2[last]
north <- x1[last]

# first contains row nos. of first times at change points 
first <- 0*last
for (j in 1:length(last)){
  first[j] <- n+2 - min(which(newp > (n+1-last[j])))
}

# PLOT WAYPOINTS AND MARK CHANGE POINTS

# plot data
# and overlay with change points in colour 
# For plotting purposes, reverse the order of 
# bz1, bz2 to get z1, z2
z1 <- 0*bz1
z2 <- 0*bz2
nz <- length(z1)
for (j in 1:nz){
  z1[j] <- bz1[nz-j+1]
  z2[j] <- bz2[nz-j+1]
}
# s is vector to index the points
s <- c(1:nz)

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
# title(main=paste("q = ", q, ", " , "alpha = ", alpha, ", ", "N = ", N , ", ", "tol = ", tol , sep=""), 
#       sub ="Blue triangle = putative goal, red star = change pt., red no. = row of data file")
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
cps

###
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
all.equal(cps, cps_rcpp)

