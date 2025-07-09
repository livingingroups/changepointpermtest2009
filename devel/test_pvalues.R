library("trackframe")
library("cpt")
data("cpttestdata", package = "cpt")


data <- cpttestdata

x_col <- colnames(data)[1]
y_col <- colnames(data)[2]
t_col <- colnames(data)[3]

# reverse order
b_xyt <- head(data[NROW(data):1,], 20)
# # calcuate diff of coordinates
# b_xy_diff <- structure(apply(b_xyt[, c(x_col, y_col)], 2, FUN = diff), dimnames = list(NULL,c("x_diff", "y_diff")))
# # remove points at which animal stays still #FIXME do we need it here?
# ind_new <- c(TRUE, sqrt(b_xy_diff[, "x_diff"]^2 + b_xy_diff[, "y_diff"]^2) > tol)
# b_xyt2 <- b_xyt[ind_new,]
b_xyt2 <- b_xyt


change_point_fit_pvalue_new <- cpt:::change_point_fit_pvalue_new
change_point_fit_pvalue_cpp <- cpt:::rcpparma_change_point_test_pvalue

q_max = 3
N = 200
tol = 0

set.seed(0)
# sink("out_R.txt")
pv1 <- change_point_fit_pvalue_new(bx = b_xyt2[, x_col], by = b_xyt2[, y_col], q_max = q_max, N = N)
# sink()
set.seed(0)
# sink("out_cpp.txt")
pv2 <- change_point_fit_pvalue_cpp(bx = b_xyt2[, x_col], by = b_xyt2[, y_col], q_max = q_max, N = N)
# sink()

traceback()

is.na(pv1)

pv1
pv2


head(pv1, 30)
head(pv2, 30)


summary(pv1 - pv2)
str(pv1)
str(pv2)
pv1$Rsumperm
pv2$Rsumperm



pv1$P
pv2$P



#
#
#
library("cpt")





