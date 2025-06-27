### Name: summary.change_point_test
### Title: Summary - Extract Change Points from Movement Data
### Aliases: summary.change_point_test

### ** Examples

library(cpt)
data("cpttestdata", package = "cpt")

# First detect change points
cpt <- change_point_test(cpttestdata, alpha = 0.05, q = 3, N = 500)

# Then extract the change points into a summarized format
summary(cpt)



