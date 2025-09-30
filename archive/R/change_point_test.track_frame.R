#' @noRd
#' @export
change_point_test.trackframe <- function(data, alpha = 0.05, q = 4, N = 1000, min_move_dist = 0, parallel = FALSE, ...) { #FIXME add parallel to docs
  tf_ids <- unlist(unique_ids(data))
  
  if(length(tf_ids) <= 1) {
    cpt <- tf_change_point_test(data, alpha = alpha, q = q, N = N, min_move_dist = min_move_dist)
  } else {
    if(parallel == FALSE) {
      cpt <- do.call(rbind, lapply(unlist(tf_ids), function(x) {
        # x <- tf_ids[1]
        print(x)
        tfx <- data[data[, attr(data, "track_id")] == x, ]
        cpt_i <- cbind(tf_change_point_test(tf = tfx, alpha = alpha, q = q, N = N, min_move_dist = min_move_dist),
                       tfx[, attr(data, "track_id")])
        cpt_i
        # class(cpt_i) <- c("change_point_test", class(cpt_i))
        return(cpt_i)
      }))
    } else {
      stop("TODO")
      # cpt <- parallel::parLapply(unlist(tf_ids), function(x) {
      #   # print(x)
      #   tfx <- data[data[, attr(data, "track_id")] == x, ]
      #   cpt_i <- tf_change_point_test(tf = tfx, alpha = alpha, q = q, N = N, min_move_dist = min_move_dist)
      #   # class(cpt_i) <- c("change_point_test", class(cpt_i))
      #   return(cpt_i)
      # })
    }
  }
  colnames(cpt)[NCOL(cpt)] <- attr(data, "track_id")
  rownames(cpt) <- NULL
  attr(cpt, "time_index") <- t_col
  attr(cpt, "easting_col") <- x_col
  attr(cpt, "northing_col") <- y_col
  class(cpt) <- c("change_point_test", class(cpt))
  return(cpt)
}