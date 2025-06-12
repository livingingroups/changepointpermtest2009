
#' @import checkmate
#' @import trackframe
#' @import Rcpp
#' @importFrom stats runif setNames
#' @importFrom zoo na.fill na.locf
#' @importFrom parallel parLapply stopCluster makePSOCKcluster
#' @useDynLib cpt, .registration = TRUE
NULL


#' Data Extracted from Figures
#'
#' These data were extracted from Figures 3,4,7 of Byrne, Noser, Bates, Jupp 2009
#' Since the process of pulling data from an image is error-prone, these data should **not**
#' be used for scientific inference. Especially the track in 7a is almost certainly
#' substantially different from the original.
"cptfiguredata"

#' Data Extracted from Figures as track_frame
#'
#' These data were extracted from Figures 3,4,7 of Byrne, Noser, Bates, Jupp 2009
#' Since the process of pulling data from an image is error-prone, these data should **not**
#' be used for scientific inference. Especially the track in 7a is almost certainly
#' substantially different from the original.
"cptfiguredata_tf"


#' Test Data
#'
#' Small data set to test package.
"cpttestdata"
