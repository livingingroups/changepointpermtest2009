### Name: change_point_test_pvalue
### Title: Change Point Detection for Animal Movement Data
### Aliases: change_point_test_pvalue

### ** Examples

library(cpt)
data("cpttestdata", package = "cpt")

pvalues <- change_point_test_pvalue(cpttestdata, q_max = 6, N = 100)
pvalues



