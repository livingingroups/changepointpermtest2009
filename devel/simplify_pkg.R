library("trackframe")
library("cpt")
data("cpttestdata", package = "cpt")


str(cpttestdata)

data <- cpttestdata
alpha = 0.05
q = 3
N = 500L
tol = 0


set.seed(2025L)
z1 <- change_point_test(cpttestdata, alpha = alpha, q = q, N = N, tol = tol)
str(z1)

easting <- data[[attr(data, "easting")]]
northing <- data[[attr(data, "northing")]]
time <- data[[attr(data, "time")]]

set.seed(2025L)
z2 <- change_point_test_xyt(data[[attr(data, "easting")]],
                      data[[attr(data, "northing")]],
                      data[[attr(data, "time")]],
                      alpha = alpha, q = q, N = N, tol = tol)

rownames(z2) <- NULL

set.seed(2025L)
xyt <- tf_as_xyt(data)[, 1:3]
z3 <- cpt:::change_point_test_internal(xyt, alpha = alpha, q = q, N = N, tol = tol)

summary(z2 - z1)
summary(z3 - z1)
summary(z3 - z2)


z1$cp_no
z2$cp_no


#
#
#
do_rbind <- cpt:::do_rbind
change_point_test_trackframe_single_id <- cpt:::change_point_test_trackframe_single_id

data <- rbind(cbind(cpttestdata, id = 1L), cbind(cpttestdata, id = 2L), cbind(cpttestdata, id = 3L))
data <- as.track_frame(data, id = "id")
cpt <- split(data, data[[attr(data, "id")]])
str(cpt[[1]])
x <- lapply(cpt, change_point_test_trackframe_single_id, alpha = alpha, q = q, N = N, tol = tol, verify = TRUE)

set.seed(0)
z1 <- change_point_test_trackframe_single_id(cpt[[1]], alpha = alpha, q = q, N = N, tol = tol)
set.seed(1)
z2 <- change_point_test_trackframe_single_id(cpt[[1]], alpha = alpha, q = q, N = N, tol = tol)
all(z1[["sig"]] == z2[["sig"]])

z <- do_rbind(x)
cols <- setdiff(colnames(z), c("easting", "northing", "time"))
data


x[[1]][["sig"]]
x[[2]][["sig"]]
x[[3]][["sig"]]

apply(z[z$id == 1, ] == z[z$id == 2, ], 2, all)


data <- rbind(cbind(cpttestdata, id = 1L), cbind(cpttestdata, id = 2L), cbind(cpttestdata, id = 3L))
data <- as.track_frame(data, id = "id")


clu <- parallel::makeCluster(3, "SOCK")

z <- change_point_test(data, alpha = alpha, q = q, N = N, tol = tol, clu = clu, seed = 2025L)
str(z)

all(z[z$id == 1, "sig"] == z[z$id == 2, "sig" ])
all(z[z$id == 1, "sig"] == z[z$id == 3, "sig" ])
all(z[z$id == 1, "cp_no"] == z[z$id == 2, "cp_no" ])
all(z[z$id == 1, "cp_no"] == z[z$id == 3, "cp_no" ])


all(z[z$id == 1, "sig"] == z2[["sig"]])

set.seed(0)

str(z)

y
colnames(y)

str(x[[1]])
warnings()

q("no")


tf_change_point_test <- function(data, alpha, q, N, tol) {
  checkmate::assert_true(NROW(data) > 0L)
  xyt <- tf_as_xyt(data)[, 1:3]
  # FIXME:
  # - [ ] Switch to change_point_test_xyt
  # - [ ] If correctly ordered at the correct time we can avoid using merge.
  cpt <- change_point_test_internal(xyt, alpha = alpha, q = q, N = N, tol = tol)
  data_out <- merge(data, cpt,
                    by = c(attr(data, "easting"), attr(data, "northing"), attr(data, "time")),
                    all.x = TRUE, sort = FALSE)
  if (is.matrix(data)) {
    # FIXME: What should this do?
    data_out <- as.matrix(data_out)
  }
  data_out <- as.track_frame(data_out,
                             easting_col = attr(data, "easting"),
                             northing_col = attr(data, "northing"),
                             time_col = attr(data, "time"))
  return(data_out)
}





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



#
#
#
library("trackframe")
library("cpt")
data("cpttestdata", package = "cpt")


data <- cpttestdata

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


change_point_fit_pvalue_new <- cpt:::change_point_fit_pvalue_new
change_point_fit_pvalue_cpp <- cpt:::rcpparma_change_point_test_pvalue

q_max = 6
N = 1000
tol = 0

# pv1 <- change_point_fit_pvalue_new(bx = b_xyt2[, x_col], by = b_xyt2[, y_col], q_max = q_max, N = N)
pv2 <- change_point_fit_pvalue_cpp(bx = b_xyt2[, x_col], by = b_xyt2[, y_col], q_max = q_max, N = N)
traceback()
pv1
pv2
