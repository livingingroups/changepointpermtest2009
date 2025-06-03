#TODOS
# - alpha NULL
# - tests output class
# - plots
# - vignette



#' Change Point Detection for Animal Movement Data
#'
#' Detects significant change points in animal movement trajectory data using a permutation-based approach.
#' This function identifies locations where the movement pattern significantly changes, which can represent
#' behavioral transitions or responses to environmental stimuli.
#'
#' @param data a matrix or data frame with columns for x-coordinates, y-coordinates, and timestamps.
#'   For the default method, this should be a matrix or data frame with at least 3 columns.
#' @param alpha Numeric value specifying the significance level for detecting change points (default: 0.05).
#'   Set to NULL to return probabilities across all potential change points. #FIXME: is this desired?
#' @param q Integer specifying the minimum segment length between potential change points (default: 4).
#' @param N Integer specifying the number of random permutations for the Monte Carlo test (default: 10000).
#'   Higher values provide more accurate p-values but increase computation time.
#' @param tol Numeric value specifying the maximum distance between indistinguishable positions (default: 0).
#'   Points with movements smaller than this threshold will be considered stationary.
#' @param ... Additional arguments passed to methods.
#'
#' @return An augmented data frame containing the original data with additional columns:
#'   \item{sig}{Binary indicator (1 or 0) of whether a point is a significant change point}
#'   \item{cp_no}{Sequential numbering of detected change points}
#'   
#'   When alpha is NULL, the function returns probability values for each potential change point. #FIXME
#' 
#' @details This function implements a sequential change point detection algorithm that uses
#'          a permutation test to identify significant changes in movement patterns. It compares
#'          the sum of distances between consecutive points against randomly permuted sequences
#'          to determine if a change point exists.
#'          
#'          The first point in the trajectory is never considered a change point, as it represents
#'          the starting location.
#'
#' @export
#'
#' @examples
#' library(cpt)
#' data("cpttestdata", package = "cpt")
#' 
#' # First detect change points
#' result <- change_point_test(cpttestdata, alpha = 0.05, q = 3, N = 500)
#' 
#' # Then extract the change points into a summarized format
#' summary(result)
#' 
#' # Get probability values instead of binary indicators
#' # prob_values <- change_point_test(cpttestdata, alpha = NULL, q = 3, N = 500)
change_point_test <- function(data, alpha = 0.05, q = 4, N = 10000, tol = 0, ...) {
  UseMethod("change_point_test")
}


# Change Point Test
# 
# Detecting change points in animal ranging data
#
# @param xyt a matrix with columns x, y (in cartesian coordinates) and t
# @param alpha nominal significance level
# @param q q value
# @param N total number of permutations
# @param tol maximum distance between indistinguishable positions
#
# @return TODO
#' @noRd
#' @export
change_point_test.default <- function(data, alpha = 0.05, q = 4, N = 1000, tol = 0, ...) {
  x_col <- colnames(data)[1]
  y_col <- colnames(data)[2]
  t_col <- colnames(data)[3]

  # reverse order
  b_xyt <- data[NROW(data):1,]
  # calcuate diff of coordinates
  b_xy_diff <- structure(apply(b_xyt[, c(x_col, y_col)], 2, FUN = diff), dimnames = list(NULL,c("x_diff", "y_diff")))
  # remove points at which animal stays still
  ind_new <- c(TRUE, sqrt(b_xy_diff[, "x_diff"]^2 + b_xy_diff[, "y_diff"]^2) > tol)
  b_xyt2 <- b_xyt[ind_new,]

  set.seed(2025)
  sig <- change_point_fit(bx = b_xyt2[, x_col], by = b_xyt2[, y_col], q = q, N = N, alpha = alpha)

  # if alpha is null, that means we want return the probabilities accross k #FIXME
  if(is.null(alpha)) return(sig)

  #  Remove putative goal from list of CP's
  sig[1] <- 0
  b_xyt2 <- cbind(b_xyt2,
                  "sig" = sig,
                  "cp_no" = ifelse(sig == 0, 0, cumsum(sig)))

  # re-index - merge with b_xyt
  b_xyt <- merge(b_xyt, b_xyt2[, c(t_col, "sig")], by = t_col, all.x = TRUE, sort = FALSE)
  # b_xyt <- b_xyt[rev(order(b_xyt[,"t"])), ]
  data <- b_xyt[order(b_xyt[, t_col]), ]
  data[, "cp_no"] <- ifelse(is.na(data[, "sig"]), NA,
                            ifelse(data[, "sig"] == 0, 0, cumsum(na.fill(data[, "sig"], fill = 0))))
  #na.locf
  data[, "sig"] <- na.locf(data[, "sig"], fromLast = TRUE, na.rm = FALSE)
  data[, "cp_no"] <- na.locf(data[, "cp_no"], fromLast = TRUE, na.rm = FALSE)
  rownames(data) <- NULL

  data <- data[, c(x_col, y_col, t_col, "sig", "cp_no")]
  class(data) <- c("change_point_test", "data.frame")
  #set attributes
  attr(data, "time_index") <- t_col
  attr(data, "easting_col") <- x_col
  attr(data, "northing_col") <- y_col
  return(data)
}


tf_change_point_test <- function(tf, alpha, q, N, tol) {
  xyt <- tf_to_xyt(tf)[, 1:3]
  cpt <- change_point_test(xyt, alpha = alpha, q = q, N = N, tol = tol)
  tf_out <- merge(tf, cpt,
                  by = c(attr(tf, "easting_col"), attr(tf, "northing_col"), attr(tf, "time_index")),
                  all.x = TRUE, sort = FALSE)
  tf_out <- as.track_frame(tf_out, easting_col = attr(tf, "easting_col"), northing_col = attr(tf, "northing_col"), time_index_col = attr(tf, "time_index"))
  return(tf_out)
}


# rbind_track_frame <- function(..., easting_col, northing_col, time_index_col){ #FIXME S3 method in trackframe
#   tf_df <- rbind.data.frame(...)
#   tf_out <- as.track_frame(tf_df, easting_col = easting_col, northing_col = northing_col, time_index_col = time_index_col)
#   return(tf_out)
# }

#' @noRd
#' @export
change_point_test.track_frame <- function(data, alpha = 0.05, q = 4, N = 1000, tol = 0, parallel = FALSE, ...) { #FIXME add parallel to docs
  tf_ids <- unlist(unique_ids(data))

  if(length(tf_ids) <= 1) {
    cpt <- tf_change_point_test(data, alpha = alpha, q = q, N = N, tol = tol)
  } else {
    if(parallel == FALSE) {
      cpt <- do.call(rbind, lapply(unlist(tf_ids), function(x) {
        # x <- tf_ids[1]
        print(x)
        tfx <- data[data[, attr(data, "track_id")] == x, ]
        cpt_i <- tf_change_point_test(tf = tfx, alpha = alpha, q = q, N = N, tol = tol)
        return(cpt_i)
      }))


    } else {
      stop("TODO")
      cpt <- parallel::parLapply(unlist(tf_ids), function(x) {
        tfx <- data[data[, attr(data, "track_id")] == x, ]
        cpt_i <- tf_change_point_test(tf = tfx, alpha = alpha, q = q, N = N, tol = tol)
        return(cpt_i)
      })
    }
    cpt <- as.track_frame(cpt, easting_col = attr(data, "easting_col"),
                          northing_col = attr(data, "northing_col"),
                          time_index_col = attr(data, "time_index"),
                          track_id = attr(data, "track_id"))
  }
  # colnames(cpt)[NCOL(cpt)] <- attr(data, "track_id")
  rownames(cpt) <- NULL
  class(cpt) <- c("change_point_test", class(cpt))
  return(cpt)
}


#' Change Point Test Fitting
#'
#' Detects change points in animal movement trajectory data using a permutation-based approach.
#' This function identifies locations where the movement pattern significantly changes.
#'
#' @param bx Numeric vector of x-coordinates of the trajectory backwards in time.
#' @param by Numeric vector of y-coordinates of the trajectory backwards in time.
#' @param q Integer specifying the minimum segment length between potential change points.
#' @param N Integer specifying the number of random permutations for the permutation test.
#' @param alpha Numeric value specifying the significance level for detecting change points.
#'
#' @return A numeric vector of the same length as the input coordinates, where 1 indicates
#'         a change point at that position and 0 indicates no change point.
#'
#' @details This function implements a sequential change point detection algorithm that uses
#'          a permutation test to identify significant changes in movement patterns. It compares
#'          the sum of distances between consecutive points against randomly permuted sequences
#'          to determine if a change point exists.
#'
#' @export
change_point_fit <- function(bx, by, q, N, alpha) {
  checkmate::assert_numeric(bx, any.missing = FALSE)
  checkmate::assert_numeric(by, len = length(bx), any.missing = FALSE)
  checkmate::assert_integerish(q, len = 1, any.missing = FALSE, lower = 1)
  checkmate::assert_integerish(N, len = 1, any.missing = FALSE, lower = 1)
  checkmate::assert_numeric(alpha, len = 1, any.missing = FALSE, lower = 0)
  drop(rcpparma_change_point_test_fit(bx, by, as.integer(q), as.integer(N), alpha))
}


#' Summary - Extract Change Points from Movement Data
#'
#' Extracts and summarizes change points detected by the change_point_test function.
#' This function processes the output from change_point_test to create a data frame
#' containing the temporal and spatial information for each identified change point.
#'
#' @param object An object of class change_point_test or change_point_test_list containing movement data with change point information,
#'   typically the output from change_point_test(). Must contain columns x and y coordinates, a timestamp,
#'   'sig', and 'cp_no'.
#' @param ... other arguments passed to summary
#'
#' @return A data frame with one row per change point and the following columns:
#'   \item{first}{Timestamp of the first observation in the change point segment}
#'   \item{last}{Timestamp of the last observation in the change point segment}
#'   \item{north}{X-coordinate (northing) of the change point location}
#'   \item{east}{Y-coordinate (easting) of the change point location}
#'
#' @export
#'
#' @examples
#' library(cpt)
#' data("cpttestdata", package = "cpt")
#' 
#' # First detect change points
#' result <- change_point_test(cpttestdata, alpha = 0.05, q = 3, N = 500)
#' 
#' # Then extract the change points into a summarized format
#' summary(result)
summary.change_point_test <- function(object, ...) {
  if(inherits(object, "track_frame")) {
    tf_ids <- unlist(unique_ids(object))
    if(length(tf_ids) <= 1) {
      xyt_cp <- object[object$sig != 0,]
      xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)
      summary <- do.call("rbind", lapply(xyt_cp_split, function(x) {
        cbind.data.frame("first" = min(x[, attr(object, "time_index")]),
                         "last" = max(x[, attr(object, "time_index")]),
                         "north" = x[, attr(object, "easting_col")][1],
                         "east" = x[, attr(object, "northing_col")][1])
      }))
    } else{
      unique(object[, attr(object, "track_id")])
      tf_split <- split(object, f = object[,attr(object, "track_id")])
      summary <- do.call("rbind", lapply(tf_split, function(xyt){
        xyt_cp <- xyt[xyt$sig != 0,]
        xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)
        
        summary_i <- do.call("rbind", lapply(xyt_cp_split, function(x) {
          # x <- xyt_cp_split[[1]]
          cbind.data.frame("first" = min(x[, attr(xyt, "time_index")]),
                           "last" = max(x[, attr(xyt, "time_index")]),
                           "north" = x[, attr(xyt, "easting_col")][1],
                           "east" = x[, attr(xyt, "northing_col")][1],
                           "track_id" = x[, attr(xyt, "track_id")][1])
        }))
        summary_i
      }))
    }
  } else {
  
    xyt_cp <- object[object$sig != 0,]
    xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)
    summary <- do.call("rbind", lapply(xyt_cp_split, function(x) {
      cbind.data.frame("first" = min(x[, attr(object, "time_index")]),
                       "last" = max(x[, attr(object, "time_index")]),
                       "north" = x[, attr(object, "easting_col")][1],
                       "east" = x[, attr(object, "northing_col")][1])
    }))
  }

  return(summary)
}

# #' @noRd
# #' @export
# summary.change_point_test_list <- function(object, ...) {
#   # xyt <- cpt2[[1]]
#   s_list <- lapply(object, function(xyt){
#     xyt_cp <- xyt[xyt$sig != 0,]
#     xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)
# 
#     summary <- do.call("rbind", lapply(xyt_cp_split, function(x) {
#       # x <- xyt_cp_split[[1]]
#       cbind.data.frame("first" = min(x[, attr(xyt, "time_index")]),
#                        "last" = max(x[, attr(xyt, "time_index")]),
#                        "north" = x[, attr(xyt, "easting_col")][1],
#                        "east" = x[, attr(xyt, "northing_col")][1])
#     }))
#   })
#   return(s_list)
# }
