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
#' 
#' cpt <- change_point_test_xyt(cpttestdata[, "x"],
#'                              cpttestdata[, "y"],
#'                              cpttestdata[, "t"],
#'                              alpha = 0.05,
#'                              q = 3,
#'                              N = 500)
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
  idx <- order(time, decreasing = TRUE)
  bx <- easting[idx]
  by <- northing[idx]
  bt <- time[idx]
  # calcuate diff of coordinates
  bxdiff <- diff(bx)
  bydiff <- diff(by)
  # remove points at which animal stays still
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
  df[["cp_no"]][is_moving] <- as.integer((cp_no > 0) * (max(cp_no) + 1L - cp_no))
  df <- df[order(df[["time"]]), ]
  df[["sig"]] <- na.locf(df[["sig"]], fromLast = TRUE, na.rm = FALSE)
  df[["cp_no"]] <- na.locf(df[["cp_no"]], fromLast = TRUE, na.rm = FALSE)
  class(df) <- union("change_point_test", class(df))
  rownames(df) <- NULL
  # FIXME: Use our constructor?
  attr(df, "time") <- "time"
  attr(df, "easting") <- "easting"
  attr(df, "northing") <- "northing"
  return(df)
}


# Wrapper function for change_point_test_xyt to calculate for a single id.
change_point_test_trackframe_single_id <- function(data, alpha, q, N, tol, seed = NULL, verify = FALSE, ...) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  cpt <- change_point_test_xyt(easting = data[[attr(data, "easting")]],
                               northing = data[[attr(data, "northing")]],
                               time = data[[attr(data, "time")]],
                               alpha = alpha, q = q, N = N, tol = tol)
  if (isTRUE(verify)) {
    # just for testing
    stopifnot(all(data[[attr(data, "easting")]] == cpt[["easting"]]))
    stopifnot(all(data[[attr(data, "northing")]] == cpt[["northing"]]))
    stopifnot(all(data[[attr(data, "time")]] == cpt[["time"]]))
  }
  data[["sig"]] <- cpt[["sig"]]
  data[["cp_no"]] <- cpt[["cp_no"]]  
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


refine_cluster_input <- function(clu) {
  if (is.null(clu)) {
    return(clu)
  }
  if (is.numeric(clu)) {
    checkmate::assert_integerish(clu, len = 1L, any.missing = FALSE)
    return(as.integer(clu))
  }
  checkmate::assert_class(clu, "cluster")
  return(clu)
}


#' Change Point Detection for Animal Movement Data
#'
#' Detects significant change points in animal movement trajectory data using a permutation-based approach.
#' This function identifies locations where the movement pattern significantly changes, which can represent
#' behavioral transitions or responses to environmental stimuli.
#'
#' @param data a trackframe, or an object coercible to trackframe
#' @param alpha a numeric value specifying the significance level for detecting change points.
#' @param q an integer specifying the minimum segment length between potential change points.
#' @param N an integer specifying the number of random permutations for thepermutation test.
#'   Higher values provide more accurate p-values but increase computation time.
#' @param tol a numeric value specifying the maximum distance between indistinguishable positions.
#'   Points with movements smaller than this threshold will be considered stationary.
#' @param clu optional parameter determining whether parallelization with the parallel package is used.
#'   Either of class \code{"NULL"}, \code{"numeric"}, or \code{"cluster"}:
#'   \itemize{
#'     \item If \code{NULL} (default) no parallel processing is used.
#'     \item If of class \code{"numeric"}, it gives the number of cores,
#'       passed as integer to \code{parallel::makePSOCKcluster}.
#'     \item If of class \code{"cluster"}, it is assumed to be a cluster object from \code{parallel} package.
#'       Allowed is any object which inherits from \code{"cluster"} and can be passed to
#'       \code{parallel::parLapply}.
#'   }
#' @param seed seed to be passed to random number generator
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
#' tf <- as.trackframe(df, time_col = 't', easting_col = 'x', northing_col = 'y')
#' set.seed(2025L)
#' cpt_tf <- change_point_test(tf, alpha = 0.05, q = 3, N = 500, tol = 0)
#' summary(cpt_tf)
#' 
#' # Get probability values instead of binary indicators
#' pvalues <- change_point_test_pvalue(
#'   cpttestdata[, c("x", "y", "t")], q_max = 3, N = 500)
#' pvalues
change_point_test <- function(data, alpha = 0.05, q = 4, N = 10000, tol = 0, clu = NULL, seed = NULL, ...) {
  UseMethod("change_point_test")
}


#' @noRd
#' @export
change_point_test.trackframe <- function(data,
                                          alpha = 0.05,
                                          q = 4,
                                          N = 1000,
                                          tol = 0,
                                          clu = NULL,
                                          seed = NULL,
                                          ...) {
  checkmate::assert_true(NROW(data) > 2L)
  verify <- list(...)[["verify"]]
  clu <- refine_cluster_input(clu)
  tf_ids <- unique_ids(data)
  if (length(tf_ids) <= 1) {
    cpt <- change_point_test_trackframe_single_id(data, alpha = alpha, q = q, N = N, tol = tol,
                                                  seed = seed, verify = verify)
  } else {
    cpt <- split(data, data[[attr(data, "id")]])
    if(is.null(clu)) {
      cpt <- lapply(cpt, change_point_test_trackframe_single_id, alpha = alpha, q = q, N = N,
                    tol = tol, seed = seed, verify = verify)
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
      cpt <- parLapply(clu, cpt, change_point_test_trackframe_single_id, alpha = alpha, q = q,
                       N = N, tol = tol, seed = seed, verify = verify)
    }
    cpt <- do_rbind(cpt)
  }
  rownames(cpt) <- NULL
  class(cpt) <- c("change_point_test", class(cpt))
  return(cpt)
}


#' @noRd
#' @export
change_point_test.data.frame <- function(data,
                                         alpha = 0.05,
                                         q = 4,
                                         N = 1000,
                                         tol = 0,
                                         clu = NULL,
                                         ...) {
  change_point_test.trackframe(data = as.trackframe(data),
                                alpha = alpha,
                                q = q,
                                N = N,
                                tol = tol,
                                clu = clu,
                                ...)
}


#' @noRd
#' @export
change_point_test.move2 <- change_point_test.data.frame


#' @noRd
#' @export
change_point_test.sftrack <- change_point_test.data.frame





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
#' library("cpt")
#' 
#' # First detect change points
#' cpt <- change_point_test(cpttestdata, alpha = 0.05, q = 3, N = 500)
#' 
#' # Then extract the change points into a summarized format
#' summary(cpt)
summary.change_point_test <- function(object, ...) {
  

  if(inherits(object, "trackframe")) {
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
  } else if(inherits(object, c("matrix", "data.frame"))) {
    xyt_cp <- object[object[, "sig"] != 0,]
    xyt_cp_split <- split(as.data.frame(xyt_cp), f = xyt_cp[, "cp_no"])
    summary <- do.call("rbind", lapply(xyt_cp_split, function(x) {
      cbind.data.frame("first" = min(x[, attr(object, "time")]),
                       "last" = max(x[, attr(object, "time")]),
                       "east" = x[, attr(object, "easting")][1],
                       "north" = x[, attr(object, "northing")][1])
    })) 
  }
  
  return(summary)
}
