### Name: change_point_test
### Title: Change Point Detection for Animal Movement Data
### Aliases: change_point_test

### ** Examples

library("cpt")
data("cpttestdata", package = "cpt")
# First detect change points
set.seed(2025L)
cpt <- change_point_test(cpttestdata, alpha = 0.05, q = 3, N = 500)

# Second show the change points in a summarized format
summary(cpt)

# with trackframe
library("trackframe")
df <- data.frame(x = cpttestdata[, 1],
                 y = cpttestdata[, 2],
                 t = as.POSIXct(seq_along(cpttestdata[, 3])))
tf <- as.track_frame(df, time_col = 't', easting_col = 'x', northing_col = 'y')
set.seed(2025L)
cpt_tf <- change_point_test(tf, alpha = 0.05, q = 3, N = 500, tol = 0)
summary(cpt_tf)

# Get probability values instead of binary indicators
pvalues <- change_point_test_pvalue(cpttestdata, q_max = 3, N = 500)
pvalues



library("trackframe")
library("cpt")
data("cpttestdata", package = "cpt")
str(cpttestdata)

df <- as.data.frame(cpttestdata)
head(df)
cpttestdata <- as.track_frame(df, time_col = "t", easting_col = "x", northing_col = "y")
dir("data")
file <- normalizePath("data/cpttestdata.rda")
save(cpttestdata, file = file)


str(as.track_frame(df))


q("no")
R

