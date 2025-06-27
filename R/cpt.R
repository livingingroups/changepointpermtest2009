# TODOs:
# - tests output class
# - plots
# - vignette
# - test parallel



#' Change Point Detection for Animal Movement Data
#'
#' Detects significant change points in animal movement trajectory data using a permutation-based approach.
#' This function identifies locations where the movement pattern significantly changes, which can represent
#' behavioral transitions or responses to environmental stimuli.
#'
#' @param data a track_frame, or an object coercible to track_frame
#' @param alpha a numeric value specifying the significance level for detecting change points.
#' @param q an integer specifying the minimum segment length between potential change points.
#' @param N an integer specifying the number of random permutations for thepermutation test.
#'   Higher values provide more accurate p-values but increase computation time.
#' @param tol a numeric value specifying the maximum distance between indistinguishable positions.
#'   Points with movements smaller than this threshold will be considered stationary.
#' @param ... additional arguments passed to methods.
#'
#' @return An augmented data frame containing the original data with additional columns:
#'   \item{sig}{Binary indicator (1 or 0) of whether a point is a significant change point}
#'   \item{cp_no}{Sequential numbering of detected change points}
#'   
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
#' library("cpt")
#' data("cpttestdata", package = "cpt")
#' # First detect change points
#' set.seed(2025L)
#' cpt <- change_point_test(cpttestdata, alpha = 0.05, q = 3, N = 500)
#' 
#' # Second show the change points in a summarized format
#' summary(cpt)
#' 
#' # with trackframe
#' library("trackframe")
#' df <- data.frame(x = cpttestdata[, 1],
#'                  y = cpttestdata[, 2],
#'                  t = as.POSIXct(seq_along(cpttestdata[, 3])))
#' tf <- as.track_frame(df, time_col = 't', easting_col = 'x', northing_col = 'y')
#' set.seed(2025L)
#' cpt_tf <- change_point_test(tf, alpha = 0.05, q = 3, N = 500, tol = 0)
#' summary(cpt_tf)
#' 
#' # Get probability values instead of binary indicators
#' pvalues <- change_point_test_pvalue(cpttestdata, q_max = 3, N = 500)
#' pvalues
change_point_test <- function(data, alpha = 0.05, q = 4, N = 10000, tol = 0, ...) {
  UseMethod("change_point_test")
}


#' Change Point Test
#' 
#' Detecting change points in animal ranging data
#'
#' @param easting a numeric vector of x-coordinates (easting) of the trajectory backwards in time.
#' @param northing a numeric vector of y-coordinates (northing) of the trajectory backwards in time.
#' @param time a vecor inheriting from \code{numeric} or \code{POSIXt} or \code{Date}
#'        containing the timestamps corresponding to the easting and northing coordinates.
#' @param alpha a numeric value specifying the significance level for detecting change points.
#' @param q an integer specifying the minimum segment length between potential change points.
#' @param N an integer specifying the number of random permutations for thepermutation test.
#'        Higher values provide more accurate p-values but increase computation time.
#' @param tol a numeric value specifying the maximum distance between indistinguishable positions.
#'        Points with movements smaller than this threshold will be considered stationary.
#' @param ... additional arguments passed to methods.
#' 
#' @return An augmented data frame containing the original data with additional columns:
#
#' @export
#' 
#' @examples 
#' 
#' library("cpt")
#' data("cpttestdata", package = "cpt")
#' 
#' cpt <- change_point_test_xyt(cpttestdata[, "x"], cpttestdata[, "y"], cpttestdata[, "t"],
#'                              alpha = 0.05, q = 3, N = 500)
#' summary(cpt)
change_point_test_xyt <- function(easting, northing, time, alpha = 0.05, q = 4, N = 1000, tol = 0, ...) {
  checkmate::assert_numeric(easting, min.len = 3L, any.missing = FALSE)
  checkmate::assert_numeric(northing, len = length(easting), any.missing = FALSE)
  checkmate::assert_numeric(time, len = length(easting), any.missing = FALSE)
  checkmate::assert_integerish(q, len = 1, any.missing = FALSE, lower = 1)
  checkmate::assert_integerish(N, len = 1, any.missing = FALSE, lower = 1)
  checkmate::assert_numeric(alpha, len = 1, any.missing = FALSE, lower = 0)
  checkmate::assert_numeric(tol, len = 1, any.missing = FALSE, lower = 0)
  
  # Reverse the time-ordering so that (bx[1], by[1]) refers to (final) 
  idx <- time[order(time, decreasing = TRUE)]
  bx <- easting[idx]
  by <- northing[idx]
  bt <- time[idx]
  # calcuate diff of coordinates
  bxdiff <- diff(bx)
  bydiff <- diff(by)
  # remove points at which animal stays still
  # FIXME: The first TRUE should depend on the second value!?!
  is_moving <- c(TRUE, sqrt(bxdiff^2 + bydiff^2) > tol)
  bxm <- bx[is_moving]
  bym <- by[is_moving]
  btm <- bt[is_moving]
  
  sig <- change_point_fit(bx = bxm, by = bym, q = q, N = N, alpha = alpha)
  
  #  Remove putative goal from list of CP's
  sig[1] <- 0L
  cp_no <- (sig != 0) * cumsum(sig)
  # re-index
  df <- data.frame(easting = bx, northing = by, time = bt, sig = NA_integer_, cp_no = NA_integer_)
  df[["sig"]][is_moving] <- sig
  df[["cp_no"]][is_moving] <- cp_no
  df <- df[order(df[["time"]]), ]
  df[["sig"]] <- na.locf(df[["sig"]], fromLast = TRUE, na.rm = FALSE)
  df[["cp_no"]] <- na.locf(df[["cp_no"]], fromLast = TRUE, na.rm = FALSE)
  class(df) <- union("change_point_test", class(df))
  # FIXME: @RH: Why not use our constructor?
  attr(df, "time") <- "time"
  attr(df, "easting") <- "easting"
  attr(df, "northing") <- "northing"
  return(df)
}




# FIXME:
# This was as quick fix to get a consistent interface.
# The internal function should be simplified.
# This for some reason was called change_point_test_xyt??
change_point_test_internal <- function(data, alpha = 0.05, q = 4, N = 1000, tol = 0, ...) { #TODO: change to x = , y = , t = 
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
  
  sig <- change_point_fit(bx = b_xyt2[, x_col], by = b_xyt2[, y_col], q = q, N = N, alpha = alpha)
  
  #  Remove putative goal from list of CP's
  sig[1] <- 0
  b_xyt2 <- cbind(b_xyt2,
                  "sig" = sig,
                  "cp_no" = ifelse(sig == 0, 0, cumsum(sig)))
  
  # re-index - merge with b_xyt
  data <- merge(b_xyt, b_xyt2[, c(t_col, "sig")], by = t_col, all.x = TRUE, sort = FALSE)
  if(is.matrix(b_xyt)) data <- as.matrix(data)
  # b_xyt <- b_xyt[rev(order(b_xyt[,"t"])), ]
  data <- data[order(data[, t_col]), ]
  # data[, "cp_no"] <- ifelse(is.na(data[, "sig"]), NA,
  #                           ifelse(data[, "sig"] == 0, 0, cumsum(na.fill(data[, "sig"], fill = 0))))
  data <- cbind(data, "cp_no" = ifelse(is.na(data[, "sig"]), NA,
                            ifelse(data[, "sig"] == 0, 0, cumsum(na.fill(data[, "sig"], fill = 0))))
  )
  
  #na.locf
  data[, "sig"] <- na.locf(data[, "sig"], fromLast = TRUE, na.rm = FALSE)
  data[, "cp_no"] <- na.locf(data[, "cp_no"], fromLast = TRUE, na.rm = FALSE)
  rownames(data) <- NULL
  
  data <- data[, c(x_col, y_col, t_col, "sig", "cp_no")]
  # class(data) <- c("change_point_test", "data.frame")
  class(data) <- union("change_point_test", class(data))
  #set attributes
  # FIXME @RH: Why not use our constructor? RH: shouldn't depend on trackframe here
  attr(data, "time") <- t_col
  attr(data, "easting") <- x_col
  attr(data, "northing") <- y_col
  return(data)
}


#' Change Point Test Fitting
#'
#' Detects change points in animal movement trajectory data using a permutation-based approach.
#' This function identifies locations where the movement pattern significantly changes.
#'
#' @param bx a numeric vector of x-coordinates of the trajectory backwards in time.
#' @param by a numeric vector of y-coordinates of the trajectory backwards in time.
#' @param q an integer specifying the minimum segment length between potential change points.
#' @param N an integer specifying the number of random permutations for the permutation test.
#' @param alpha a numeric value specifying the significance level for detecting change points.
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
  cpf <- rcpparma_change_point_test_fit(bx, by, as.integer(q), as.integer(N), alpha)
  drop(cpf)
}





# rbind_track_frame <- function(..., easting_col, northing_col, time_col){ #FIXME S3 method in trackframe
#   tf_df <- rbind.data.frame(...)
#   tf_out <- as.track_frame(tf_df, easting_col = easting_col, northing_col = northing_col, time_col = time_col)
#   return(tf_out)
# }

# TODO: Maybe use dplyr::bind_rows but dplyr dependency for just rbind seams to much.
#       Let's wait.
do_rbind <- function(x, make.row.names = FALSE) {
  do.call(rbind.data.frame, c(x, list(make.row.names = make.row.names)))
}


verify_cluster <- function(clu) {
  if (is.null(clu)) {
    return(clu)
  }
  if (is.numeric(clu)) {
    checkmate::check_integerish(clu, len = 1L, any.missing = FALSE)
  } else {
    checkmate::check_class(clu, "cluster")
  }
  return(clu)
}


#' @param clu optional parameter either of class \code{"NULL"}, \code{"numeric"}, or \code{"cluster"},
#'   \itemize{
#'     \item If \code{NULL} (default) no parallel processing is used.
#'     \item If of class \code{"numeric"}, it gives the number of cores,
#'       passed as integer to \code{parallel::makePSOCKcluster}.
#'     \item If of class \code{"cluster"}, it is assumed to be a cluster object from \code{parallel} package.
#'       Allowed is any object which inherits from \code{"cluster"} and can be passed to
#'       \code{parallel::parLapply}.
#'   }
#' @noRd
#' @export
change_point_test.default <- function(data,
                                      alpha = 0.05,
                                      q = 4,
                                      N = 1000,
                                      tol = 0,
                                      clu = NULL,
                                      ...) {
  data <- as.track_frame(data)
  checkmate::assert_true(NROW(data) > 0L)
  clu <- verify_cluster(clu)
  tf_ids <- unlist(unique_ids(data))
  if(length(tf_ids) <= 1) {
    cpt <- tf_change_point_test(data, alpha = alpha, q = q, N = N, tol = tol)
  } else {
    cpt <- split(data, data[, attr(data, "id")])
    if(is.null(clu)) {
      cpt <- lapply(cpt, tf_change_point_test, alpha = alpha, q = q, N = N, tol = tol)
    } else {
      if (is.numeric(clu)) {
        if (clu <= 0L) {
          ncores <- as.integer(max(1, parallel::detectCores() - 1))
        } else {
          ncores <- as.integer(clu)
        }
        clu <- makePSOCKcluster(as.integer(ncores))
        on.exit(stopCluster(clu), add = TRUE)
      }
      cpt <- parLapply(clu, cpt, tf_change_point_test, alpha = alpha, q = q, N = N, tol = tol)
    }
    cpt <- as.track_frame(do_rbind(cpt), easting_col = attr(data, "easting"),
                          northing_col = attr(data, "northing"),
                          time_col = attr(data, "time"),
                          id_col = attr(data, "id"))
  }
  # colnames(cpt)[NCOL(cpt)] <- attr(data, "id")
  rownames(cpt) <- NULL
  class(cpt) <- c("change_point_test", class(cpt))
  return(cpt)
}


tf_change_point_test <- function(data, alpha, q, N, tol) {
  checkmate::assert_true(NROW(data) > 0L)
  xyt <- tf_to_xyt(data)[, 1:3]
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

#' Summary - Extract Change Points from Movement Data
#'
#' Extracts and summarizes change points detected by the \code{change_point_test} function.
#' This function processes the output from \code{change_point_test} to create a data frame
#' containing the temporal and spatial information for each identified change point.
#'
#' @param object an object of class \code{change_point_test} or
#'   \code{change_point_test_list} containing movement data with change point information,
#'   typically the output from \code{change_point_test()}. Must contain columns x and y coordinates,
#'   a timestamp, \code{'sig'}, and \code{'cp_no'}.
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
#' cpt <- change_point_test(cpttestdata, alpha = 0.05, q = 3, N = 500)
#' 
#' # Then extract the change points into a summarized format
#' summary(cpt)
summary.change_point_test <- function(object, ...) {
  
  if(inherits(object, "matrix")) {
    xyt_cp <- object[object[, "sig"] != 0,]
    xyt_cp_split <- split(as.data.frame(xyt_cp), f = xyt_cp[, "cp_no"])
    summary <- do.call("rbind", lapply(xyt_cp_split, function(x) {
      cbind.data.frame("first" = min(x[, attr(object, "time")]),
                       "last" = max(x[, attr(object, "time")]),
                       "east" = x[, attr(object, "easting")][1],
                       "north" = x[, attr(object, "northing")][1])
    }))
  } else if(inherits(object, "track_frame")) {
    tf_ids <- unlist(unique_ids(object))
    if(length(tf_ids) <= 1) {
      xyt_cp <- object[object$sig != 0,]
      xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)
      summary <- do.call("rbind", lapply(xyt_cp_split, function(x) {
        cbind.data.frame("first" = min(x[, attr(object, "time")]),
                         "last" = max(x[, attr(object, "time")]),
                         "east" = x[, attr(object, "easting")][1],
                         "north" = x[, attr(object, "northing")][1])
      }))
    } else{
      unique(object[, attr(object, "id")])
      tf_split <- split(object, f = object[,attr(object, "id")])
      summary <- do.call("rbind", lapply(tf_split, function(xyt){
        xyt_cp <- xyt[xyt$sig != 0,]
        xyt_cp_split <- split(xyt_cp, f = xyt_cp$cp_no)
        
        summary_i <- do.call("rbind", lapply(xyt_cp_split, function(x) {
          # x <- xyt_cp_split[[1]]
          cbind.data.frame("first" = min(x[, attr(xyt, "time")]),
                           "last" = max(x[, attr(xyt, "time")]),
                           "east" = x[, attr(xyt, "easting")][1],
                           "north" = x[, attr(xyt, "northing")][1],
                           "id" = x[, attr(xyt, "id")][1])
        }))
        summary_i
      }))
    }
  } 
  
  return(summary)
}
