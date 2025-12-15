#' Change Point Test
#'
#' Detecting change points in animal ranging data
#'
#' @param x a numeric vector of x-coordinates (easting) of the trajectory backwards in time.
#' @param y a numeric vector of y-coordinates (northing) of the trajectory backwards in time.
#' @param time a vecor inheriting from \code{numeric} or \code{POSIXt} or \code{Date}
#'        containing the timestamps corresponding to the easting and northing coordinates.
#' @param q_max maximum number of q to be tested
#' @param n an integer specifying the number of random permutations for thepermutation test.
#'        Higher values provide more accurate p-values but increase computation time.
#' @param min_move_dist a numeric value specifying the minimum distance between two positions to be
#'   distinguishable. Points with movements smaller than this threshold will be considered
#'   stationary.(tol parameter in original code)
#' @param ... additional arguments passed to methods.
#'
#' @return a matrix of pvalues with dimension n x q_max, where n is the number of observation
#' determined by the length of x. Indistinguishable positions are set to NA.
#
#' @export
#'
#' @examples
#'
#' cpt_pvalues <- change_point_test_pvalue_xyt(cpttestdata[, "x"],
#'                              cpttestdata[, "y"],
#'                              cpttestdata[, "t"],
#'                              n = 500)
#' tail(cpt_pvalues)
change_point_test_pvalue_xyt <- function(
  x,
  y,
  time,
  q_max = 6,
  n = 1000,
  min_move_dist = 0,
  ...
) {
  checkmate::assert_numeric(x, min.len = 3L, any.missing = FALSE)
  checkmate::assert_numeric(y, len = length(x), any.missing = FALSE)
  checkmate::assert_numeric(time, len = length(x), any.missing = FALSE)
  checkmate::assert_integerish(q_max, len = 1, any.missing = FALSE, lower = 1)
  checkmate::assert_integerish(n, len = 1, any.missing = FALSE, lower = 1)
  checkmate::assert_numeric(min_move_dist, len = 1, any.missing = FALSE, lower = 0)

  # Reverse the time-ordering so that (bx[1], by[1]) refers to (final)
  idx <- order(time, decreasing = TRUE)
  bx <- x[idx]
  by <- y[idx]
  # bt <- time[idx]
  # calcuate diff of coordinates
  bxdiff <- diff(bx)
  bydiff <- diff(by)
  # remove points at which animal stays still
  is_moving <- if (isTRUE(list(...)[["legacy_movement_criteria"]])) {
    c(
      TRUE,
      abs(bxdiff) > min_move_dist & abs(bydiff) > min_move_dist
    )
  } else {
    c(
      TRUE,
      sqrt(bxdiff^2 + bydiff^2) > min_move_dist
    )
  }
  bxm <- bx[is_moving]
  bym <- by[is_moving]
  pvalues <- change_point_fit_pvalue(bx = bxm, by = bym, q_max = q_max, n = n)

  pvalues_all <- matrix(NA, nrow = length(x), ncol = q_max)
  pvalues_all[is_moving, ] <- pvalues

  class(pvalues_all) <- c("change_point_test_pvalue", class(pvalues_all))
  colnames(pvalues_all) <- paste0("q=", seq_len(NCOL(pvalues_all)))
  return(pvalues_all)
}


# Wrapper function for change_point_test_xyt to calculate for a single id.
change_point_test_pvalue_internal <- function(
  data,
  q_max = 6,
  n = 1000,
  min_move_dist = 0,
  seed = NULL,
  verify = FALSE,
  ...
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  change_point_test_pvalue_xyt(
    x = data[[easting_col(data)]],
    y = data[[northing_col(data)]],
    time = data[[time_col(data)]],
    q_max = q_max,
    n = n,
    min_move_dist = min_move_dist
  )
}


#' Change Point Test Fitting
#'
#' Detects change points in animal movement trajectory data using a permutation-based approach.
#' This function identifies locations where the movement pattern significantly changes.
#'
#' @param bx a numeric vector of x-coordinates of the trajectory backwards in time.
#' @param by a numeric vector of y-coordinates of the trajectory backwards in time.
#' @param q_max q_max maximum number of q to be tested
#' @param n an integer specifying the number of random permutations for the permutation test.
#'
#' @export
change_point_fit_pvalue <- function(bx, by, q_max, n) {
  bxdiff <- diff(bx)
  bydiff <- diff(by)
  # q_max = maximum value of q
  # q_max =6 is a convenient default

  # last_t = "time" (backwards in time) of last position of interest
  # (and includes all the positions)
  n_obs <- length(bx)

  # Set up matrix pmat in which to store pmat-values
  # pmat <- matrix(rep(exp(2), q_max * no_of_t), nrow = no_of_t, ncol = q_max)
  pmat <- matrix(rep(NA, q_max * n_obs), nrow = n_obs, ncol = q_max)

  # n = total number of permutations (1 observed and N-1 simulated)
  # n = 1000 is a convenient number

  r_sumrand <- 0 * seq_len(n)

  for (q in seq_len(q_max)) {
    # start of q loop
    k_max <- n_obs - q - 1L
    for (k in 1:k_max) {
      # start of k loop

      r1 <- sqrt(
        (bx[k + 1] - bx[1])^2 +
          (by[k + 1] - by[1])^2
      )
      r2 <- sqrt(
        (bx[k + q + 1] - bx[k + 1])^2 +
          (by[k + q + 1] - by[k + 1])^2
      )

      r_sum <- r1 + r2

      # r_sumrand[1] = observed value of statistic r1 + r2
      r_sumrand[1] <- r_sum

      # Now calculate statistic r1 + r2 for a further N-1 random permutations
      # and store in r_sumrand
      for (it in seq(2, n)) {
        # start of it loop
        u <- runif(k + q, 0, 1) # Q: Why here "+ q" is needed?
        perm <- order(u)
        bxr <- bx[1]
        byr <- by[1]
        for (j in 1:k) {
          # start of j loop
          bxr <- bxr + bxdiff[perm[j]]
          byr <- byr + bydiff[perm[j]]
        } # end of j loop

        r1_rand <- sqrt((bxr - bx[1])^2 + (byr - by[1])^2)
        r2_rand <- sqrt((bx[k + q + 1] - bxr)^2 + (by[k + q + 1] - byr)^2)
        r_sumrand[it] <- r1_rand + r2_rand
      } # end of it loop

      # calculate pmat-values
      pmat[k, q] <- sum(r_sumrand >= r_sum) / n
    } # end of k loop
  } # end of q loop
  pmat
}


#' Change Point Detection for Animal Movement Data
#'
#' Detects significant change points in animal movement trajectory data using a permutation-based
#' approach. This function identifies locations where the movement pattern significantly changes,
#' which can represent behavioral transitions or responses to environmental stimuli.
#'
#' @param data a matrix or data frame with columns for x-coordinates, y-coordinates, and time.
#'   For the default method, this should be a matrix or data frame with at least 3 columns.
#' @param q_max an integer specifying the maximum value of q.
#' @param n an integer specifying the number of random permutations for the permutation test
#'   Higher values provide more accurate p-values but increase computation time.
#' @param min_move_dist a numeric value specifying the minimum distance between two positions to be
#'   distinguishable. Points with movements smaller than this threshold will be considered
#'   stationary.(tol parameter in original code)
#' @param clu optional parameter determining whether parallelization with the parallel package is
#'  used.
#'   Either of class \code{"NULL"}, \code{"numeric"}, or \code{"cluster"}:
#'   \itemize{
#'     \item If \code{NULL} (default) no parallel processing is used.
#'     \item If of class \code{"numeric"}, it gives the number of cores,
#'       passed as integer to \code{parallel::makePSOCKcluster}.
#'     \item If of class \code{"cluster"}, it is assumed to be a cluster object from \code{parallel}
#'       package. Allowed is any object which inherits from \code{"cluster"} and can be passed to
#'       \code{parallel::parLapply}.
#'   }
#' @param seed seed to be passed to random number generator
#' @param ... additional arguments passed to methods.
#'
#' @return An augmented data frame containing the original data with additional columns:
#'   \item{sig}{Binary indicator (1 or 0) of whether a point is a significant change point}
#'   \item{cp_no}{Sequential numbering of detected change points}
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
#' pvalues <- change_point_test_pvalue(cpttestdata, q_max = 6, n = 100)
#' pvalues
#' @rdname change_point_test_pvalue
change_point_test_pvalue <- function(
  data,
  q_max = 4,
  n = 10000,
  min_move_dist = 0,
  clu = NULL,
  seed = NULL,
  ...
) {
  UseMethod("change_point_test_pvalue")
}


#' @examples
#' set.seed(2025L)
#' cpt_tf <- change_point_test_pvalue(cpttestdata, q_max = 3, n = 100, min_move_dist = 0)
#'
#' @export
#' @rdname change_point_test_pvalue
change_point_test_pvalue.trackframe <- function(
  data,
  q_max = 4,
  n = 10000,
  min_move_dist = 0,
  clu = NULL,
  seed = NULL,
  ...
) {
  checkmate::assert_true(NROW(data) > 2L)
  verify <- list(...)[["verify"]]
  clu <- refine_cluster_input(clu)
  tf_ids <- unique_ids(data)
  if (length(tf_ids) <= 1) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    cpt <- change_point_test_pvalue_xyt(
      x = data[[easting_col(data)]],
      y = data[[northing_col(data)]],
      time = data[[time_col(data)]],
      q_max = q_max,
      n = n,
      min_move_dist = min_move_dist
    )
  } else {
    cpt <- split(data, data[[id_col(data)]])
    if (is.null(clu)) {
      cpt <- lapply(
        cpt,
        change_point_test_pvalue_internal,
        q_max = q_max,
        n = n,
        min_move_dist = min_move_dist,
        seed = seed,
        verify = verify
      )
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
      cpt <- parLapply(
        clu,
        cpt,
        change_point_test_pvalue_internal,
        q_max = q_max,
        n = n,
        min_move_dist = min_move_dist,
        seed = seed,
        verify = verify
      )
    }
    # cpt <- do_rbind(cpt)
  }
  rownames(cpt) <- NULL
  return(cpt)
}


#' @examples
#' df <- cpttestdata
#' class(df)
#' set.seed(2025L)
#' cpt_df <- change_point_test_pvalue(df, q_max = 3, n = 100, min_move_dist = 0)
#'
#' @param tf_args args passed to as.trackframe in the case \code{"data"} is not
#'
#' @export
#' @rdname change_point_test_pvalue
change_point_test_pvalue.data.frame <- function(
  data,
  q_max = 4,
  n = 10000,
  min_move_dist = 0,
  clu = NULL,
  seed = NULL,
  tf_args = list(crs = NA),
  ...
) {
  change_point_test_pvalue.trackframe(
    data = do.call(as.trackframe, c(list(data), tf_args)),
    q_max = q_max,
    n = n,
    min_move_dist = min_move_dist,
    clu = clu,
    seed = seed,
    ...
  )
}


#' @examples
#' library(move2)
#' data("path_move2", package = "trackframe")
#' class(path_move2)
#' set.seed(2025L)
#' cpt_move2 <- change_point_test_pvalue(path_move2[1:200,], q_max = 3, n = 100, min_move_dist = 0)
#'
#' @export
#' @rdname change_point_test_pvalue
change_point_test_pvalue.move2 <- function(
  data,
  q_max = q_max,
  n = n,
  min_move_dist = 0,
  clu = NULL,
  seed = NULL,
  ...
) {
  change_point_test_pvalue.trackframe(
    data = as.trackframe(data),
    q_max = q_max,
    n = n,
    min_move_dist = min_move_dist,
    clu = clu,
    seed = seed,
    ...
  )
}


#' @examples
#' library(sftrack)
#' data("path_sftrack", package = "trackframe")
#' class(path_sftrack)
#' set.seed(2025L)
#' cpt_sftrack <- change_point_test_pvalue(path_sftrack[1:200,], q_max = 3, n = 100,
#'   min_move_dist = 0)
#'
#' @export
#' @rdname change_point_test_pvalue
change_point_test_pvalue.sftrack <- change_point_test_pvalue.move2
