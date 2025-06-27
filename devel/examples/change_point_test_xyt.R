### Name: change_point_test_xyt
### Title: Change Point Test
### Aliases: change_point_test_xyt

### ** Examples


library("cpt")
data("cpttestdata", package = "cpt")

cpt <- change_point_test_xyt(cpttestdata[, "x"], cpttestdata[, "y"], cpttestdata[, "t"],
                             alpha = 0.05, q = 3, N = 500)
summary(cpt)



