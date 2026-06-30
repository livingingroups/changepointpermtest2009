#' @import checkmate
#' @import trackframe
#' @import Rcpp
#' @import tinyplot
#' @importFrom graphics arrows par
#' @importFrom utils modifyList
#' @importFrom reshape2 melt
#' @importFrom stats runif setNames as.formula time
#' @importFrom zoo na.fill na.locf
#' @importFrom parallel parLapply stopCluster makePSOCKcluster
#' @useDynLib cpt, .registration = TRUE
NULL

#' Test Data
#'
#' Small data set to test package.
"cpttestdata"

#' Test Data
#'
#' Tiny data set to test change_point_test_pvalue.
"cpttestdata_tiny"
