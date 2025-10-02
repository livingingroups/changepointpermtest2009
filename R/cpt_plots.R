get_arrow_points <- function(tf, sort = TRUE) {
  x <- attr(tf, "easting")
  y <- attr(tf, "northing")
  id <- attr(tf, "id")
  # can we ensure that tf is already sorted?
  if (isTRUE(sort)) {
    tf <- tf[order(id(tf), time(tf)), ]
  }
  starting_points <- tf[!duplicated(tf[[id]]), ]
  tf2 <- tf[duplicated(tf[[id]]), ]
  direction_points <- tf2[!duplicated(tf2[[id]]), ]
  list(
    x0 = starting_points[, x],
    y0 = starting_points[, y],
    x1 = direction_points[, x],
    y1 = direction_points[, y]
  )
}


if (getRversion() <= "4.4.0") {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}


set_facet_ncol <- function(n) {
  if (n > 4) {
    if (n %% 4 == 0) {
      n_col <- 4
    } else if (n %% 3 == 0) {
      n_col <- 3
    } else {
      n_col <- 2
    }
  } else if (n == 4) {
    n_col <- 2
  } else {
    n_col <- n
  }
  return(n_col)
}


eval_list <- function(x) {
  mode(x) <- "call"
  eval(x)
}

plot_add <- function(call, ...) {
  args <- list(...)
  if ("data" %in% names(args)) {
    call$data <- args$data
  }
  new_call <- modifyList(call, args)
  new_call[["add"]] <- TRUE
  eval_list(new_call)
}


#' Plot trackframes
#'
#' Plots coordinates of objects of class \code{\link[trackframe]{trackframe}} based on
#' \code{\link[tinyplot]{tinyplot}} functionality.
#'
#' @param x an object of class \code{trackframe}
#' @param direction logical indicator if the path direction should be added to the plot
#' @param arrow_length length of the arrow of the direction (argument passed to
#' \code{\link[graphics]{arrows}})
#' @param arrow_code code of the arrow of the direction (argument passed to
#' \code{\link[graphics]{arrows}})
#' @param arrow_col color of the arrow of the direction (argument passed to
#' \code{\link[graphics]{arrows}})
#' @param arrow_lty line type of the arrow of the direction (argument passed to
#' \code{\link[graphics]{arrows}})
#' @param arrow_lwd line width of the arrow of the direction (argument passed to
#' \code{\link[graphics]{arrows}})
#' @param nfacet_col number of columns used in facet.args argument ncol
#' @param ... other arguments used in \code{\link[tinyplot]{tinyplot}}
#'
#' @examples
#' library(trackframe)
#' library(cpt)
#' library(tinyplot)
#'
#' data("tf_mini", package = "trackframe")
#'
#' data <- tf_mini
#' class(data)
#'
#' plot(data)
#' # set different theme
#' tinytheme("clean2")
#' plot(data)
#'
#' plot(data, direction = TRUE)
#'
#' track_1 <- select_id(data, "track_1")
#' plot(track_1)
#' plot(track_1, direction = TRUE)
#'
#' @export
plot.trackframe <- function(
  x,
  direction = FALSE,
  arrow_length = 0.1,
  arrow_code = 2,
  arrow_col = "black",
  arrow_lty = 3,
  arrow_lwd = 1,
  nfacet_col = NULL,
  ...
) {
  x_col <- attr(x, "easting")
  y_col <- attr(x, "northing")
  id_col <- attr(x, "id")
  n_id <- length(unique(id(x)))
  nfacet_col <- nfacet_col %||% set_facet_ncol(n_id)

  if (n_id > 1) {
    form <- as.formula(paste(y_col, "~", x_col, "|", id_col))
    default_options <- list(
      facet = "by",
      type = "l",
      facet.args = list("free" = TRUE, ncol = nfacet_col),
      grid = TRUE,
      main = "Paths"
    )
    arrows_facet <- "by"
  } else {
    form <- as.formula(paste(y_col, "~", x_col))
    default_options <- list(type = "l", grid = TRUE, main = unique(id(x)))
    arrows_facet <- id_col
  }

  # delete restricted elements
  restricted <- c("x", "y", "data")
  args <- list(...) # args = list()
  if (any(names(args) %in% restricted)) {
    warning(sprintf(
      "argument %s is restricted and therefore ignored",
      names(args)[names(args) %in% restricted]
    ))
  }
  control <- modifyList(default_options, args[!names(args) %in% restricted])
  plt_call <- c(list(tinyplot, form, data = x), control)
  eval_list(plt_call)

  if (isTRUE(direction)) {
    # add arrow in path direction from (x1, y1) to (x2, y2)
    arrow_points <- get_arrow_points(x)
    plot_add(
      plt_call,
      add = TRUE,
      type = type_arrows(
        x0 = arrow_points[["x0"]],
        y0 = arrow_points[["y0"]],
        x1 = arrow_points[["x1"]],
        y1 = arrow_points[["y1"]],
        length = arrow_length,
        code = arrow_code,
        arrow_col = arrow_col,
        arrow_lty = arrow_lty,
        arrow_lwd = arrow_lwd
      ),
      facet = arrows_facet
    )
  }
}


#' Plot Change point test output
#'
#' Plots change points of objects of class \code{\link[cpt]{change_point_test}}
#' based on \code{\link[tinyplot]{tinyplot}} functionality.
#'
#' @param x an object of class \code{change_point_test}
#' @param direction logical indicator if the path direction should be added to the plot
#' @param cp_col color of the change points
#' @param arrow_length length of the arrow of the direction (argument passed to
#'  \code{\link[graphics]{arrows}})
#' @param arrow_code code of the arrow of the direction (argument passed to
#'  \code{\link[graphics]{arrows}})
#' @param arrow_col color of the arrow of the direction (argument passed to
#'  \code{\link[graphics]{arrows}})
#' @param arrow_lty line type of the arrow of the direction (argument passed to
#'  \code{\link[graphics]{arrows}})
#' @param arrow_lwd line width of the arrow of the direction (argument passed to
#'  \code{\link[graphics]{arrows}})
#' @param nfacet_col number of columns used in facet.args argument ncol
#' @param ... other arguments used in \code{\link[tinyplot]{tinyplot}}
#'
#' @export
#'
#' @examples
#' library(cpt)
#' library(trackframe)
#' library(tinyplot)
#'
#' data("cptfiguredata_tf")
#' data <- cptfiguredata_tf[startsWith(cptfiguredata_tf$track_id, '4'),]
#' class(data)
#' tinytheme("clean2")
#' plot(data)
#' # single track
#' plot(select_id(data, "4a"))
#'
#' # calculate change points
#' cpt <- change_point_test(data, alpha = .01, n = 10000, q = 6)
#' class(cpt)
#'
#' plot(cpt)
#' # with path directions
#' plot(cpt, direction = TRUE)
#'
#' # only one track
#' cpt4a <- select_id(cpt, "4a")
#' plot(cpt4a)
#' # with path direction
#' plot(cpt4a, direction = TRUE)
plot.change_point_test <- function(
  x,
  direction = FALSE,
  cp_col = "black",
  arrow_length = 0.1,
  arrow_code = 2,
  arrow_col = "black",
  arrow_lty = 3,
  arrow_lwd = 1,
  nfacet_col = NULL,
  ...
) {
  # method dispatch for trackframe, sftrack, move2 +? data.frame
  plotcpt(
    cpt = x,
    direction = direction,
    cp_col = cp_col,
    arrow_length = arrow_length,
    arrow_code = arrow_code,
    arrow_col = arrow_col,
    arrow_lty = arrow_lty,
    arrow_lwd = arrow_lwd,
    nfacet_col = nfacet_col
  )
}


#' @keywords internal
plotcpt <- function(
  data,
  alpha = 0.05,
  q = 4,
  n = 10000,
  min_move_dist = 0,
  clu = NULL,
  seed = NULL,
  ...
) {
  UseMethod("plotcpt")
}


#' @keywords internal
plotcpt.trackframe <- function(
  cpt,
  direction = FALSE,
  cp_col = "red",
  arrow_length = 0.1,
  arrow_code = 2,
  arrow_col = "black",
  arrow_lty = 3,
  arrow_lwd = 1,
  nfacet_col = NULL,
  ...
) {
  x <- attr(cpt, "easting")
  y <- attr(cpt, "northing")
  id <- attr(cpt, "id")

  n_id <- length(unique(id(cpt)))
  nfacet_col <- nfacet_col %||% set_facet_ncol(n_id)

  # delete restricted elements
  restricted <- c("x", "y", "data")
  args <- list(...) # args <- list()
  if (any(names(args) %in% restricted)) {
    warning(sprintf(
      "argument %s is restricted and therefore ignored",
      names(args)[names(args) %in% restricted]
    ))
  }

  if (length(unique(id(cpt))) > 1) {
    form <- as.formula(paste(y, "~", x, "|", id))
    default_options <- list(
      facet = "by",
      type = "l",
      facet.args = list("free" = TRUE, ncol = nfacet_col),
      grid = TRUE,
      main = "Change Points"
    )
    arrows_facet <- "by"
  } else {
    # single id
    form <- as.formula(paste(y, "~", x))
    default_options <- list(
      type = "l",
      grid = TRUE,
      main = paste("Change Points", "-", unique(id(cpt)))
    )
    arrows_facet <- id
  }
  control <- modifyList(default_options, args[!names(args) %in% restricted])
  plt_call <- c(list(tinyplot, form, data = cpt), control)
  eval_list(plt_call)
  # add change points
  plot_add(plt_call, add = TRUE, data = cpt[cpt[["sig"]] == 1, ], type = "p", cex = 3, pch = "*",
    col = cp_col) # NOTE: do we want to add cp numbers?
  # add starting point
  plot_add(plt_call, add = TRUE, data = cpt[!duplicated(cpt[[id]]), ], type = "p", cex = 1,
    pch = "|", col = "green")
  # add end point
  plot_add(plt_call, add = TRUE, data = cpt[!duplicated(cpt[[id]], fromLast = TRUE), ], type = "p",
    cex = 1, pch = 4, col = "red")

  if (isTRUE(direction)) {
    # add arrow in path direction from (x1, y1) to (x2, y2)
    arrow_points <- get_arrow_points(cpt)
    plot_add(
      plt_call,
      data = cpt,
      add = TRUE,
      type = type_arrows(
        x0 = arrow_points[["x0"]],
        y0 = arrow_points[["y0"]],
        x1 = arrow_points[["x1"]],
        y1 = arrow_points[["y1"]],
        length = arrow_length,
        code = arrow_code,
        arrow_col = arrow_col,
        arrow_lty = arrow_lty,
        arrow_lwd = arrow_lwd
      ),
      facet = arrows_facet
    )
  }
}


#' @keywords internal
plotcpt.sftrack <- function(
  cpt,
  direction = FALSE,
  arrow_length = 0.1,
  arrow_code = 2,
  arrow_col = "black",
  arrow_lty = 3,
  arrow_lwd = 1,
  ...
) {
  cpt_tf <- as.trackframe(cpt)
  plotcpt(
    cpt = cpt_tf,
    direction = direction,
    arrow_length = arrow_length,
    arrow_code = arrow_code,
    arrow_col = arrow_col,
    arrow_lty = arrow_lty,
    arrow_lwd = arrow_lwd
  )
}


#' @keywords internal
plotcpt.move2 <- plotcpt.sftrack


#' Plot Change point test pvalues
#'
#' Plots change points of objects of class \code{\link[cpt]{change_point_test_pvalue}} based on
#' \code{\link[tinyplot]{tinyplot}} functionality.
#'
#' @param x an object of class \code{change_point_test_pvalue}
#' @param ... other arguments used in \code{\link[tinyplot]{tinyplot}}
#'
#' @export
#'
#' @examples
#' library(cpt)
#' library(trackframe)
#' library(tinyplot)
#'
#' data("cptfiguredata_tf")
#' data <- select_id(cptfiguredata_tf, "4a")
#' set.seed(2025)
#' P = change_point_test_pvalue(data, q_max = 10, n = 100)
#'
#' tinytheme("clean2")
#'
#' plot(P)
plot.change_point_test_pvalue <- function(x, ...) {
  p_long <- reshape2::melt(x)
  colnames(p_long) <- c("n", "q", "value")
  p_long$value <- -log(p_long$value)

  form <- value ~ n | q
  default_options <- list(
    facet = "by",
    facet.args = list(ncol = 2),
    col = "black",
    type = "b",
    pch = 4,
    legend = TRUE
  )

  # delete restricted elements
  restricted <- c("x", "y", "data")
  args <- list(...)
  if (any(names(args) %in% restricted)) {
    warning(sprintf(
      "argument %s is restricted and therefore ignored",
      names(args)[names(args) %in% restricted]
    ))
  }
  control <- modifyList(default_options, args[!names(args) %in% restricted])
  plt_call <- c(list(tinyplot, form, data = p_long), control)
  eval_list(plt_call)
  plot_add(plt_call, type = type_hline(h = -log(0.10)), col = "blue")
  plot_add(plt_call, type = type_hline(h = -log(0.05)), col = "red")
  plot_add(plt_call, type = type_hline(h = -log(0.01)), col = "green")
}


#' Plot number of change points depending on q
#'
#' Plots number of change points given different values of q
#'
#' @param data generated by \code{calculate_n_cp_by_q}
#' @param id_col column name of the track id column
#' @param ... other arguments used in \code{\link[tinyplot]{tinyplot}}
#'
#' @export
#'
#' @examples
#' library(tinyplot)
#' data("cptfiguredata_tf")
#' data <- cptfiguredata_tf[startsWith(cptfiguredata_tf$track_id, '4'),]
#'
#' calculate_n_cp_by_q <- function(tf, q=seq_len(10), alpha = 0.01) {
#'  if(length(attr(tf, 'id'))!=1) stop('only implemented for 1 id col')
#'  do.call(rbind, lapply(seq_len(10), function(q) {
#'    cp <- change_point_test(
#'        tf,
#'        alpha = alpha,
#'        q = q
#'  )
#'  df <- as.data.frame(rowsum(cp$sig, as.factor(cp[,attr(tf, 'id')])))
#'  names(df) <- 'n_cp'
#'  df$track_id <- rownames(df)
#'  rownames(df) <- NULL
#'    df$q <- q
#'    df
#'  }))
#' }
#'
#' n_cp_by_q <- calculate_n_cp_by_q(data)
#' tinytheme("clean2")
#' plot_n_cp_by_q(data = n_cp_by_q, id_col = "track_id")
plot_n_cp_by_q <- function(data, id_col, ...) {
  form <- as.formula(paste("n_cp ~ q |", id_col))
  default_options <- list(type = tinyplot::type_spline(n = 100), grid = TRUE)

  # delete restricted elements
  restricted <- c("x", "y", "data")
  args <- list(...) # args = list()
  if (any(names(args) %in% restricted)) {
    warning(sprintf(
      "argument %s is restricted and therefore ignored",
      names(args)[names(args) %in% restricted]
    ))
  }
  control <- modifyList(default_options, args[!names(args) %in% restricted])
  # do.call(tinyplot::tinyplot, c(list(form, data = data), control))
  plt_call <- c(list(tinyplot, form, data = data), control)
  eval_list(plt_call)
}


#' Add arrows to a plot
#'
#' This function adds an arrow to a current plot.
#'
#' @param x0 x0 in \code{\link[graphics]{arrows}}
#' @param y0 y0 in \code{\link[graphics]{arrows}}
#' @param x1 x1 in \code{\link[graphics]{arrows}}
#' @param y1 y1 in \code{\link[graphics]{arrows}}
#' @param length length in \code{\link[graphics]{arrows}}
#' @param angle angle in \code{\link[graphics]{arrows}}
#' @param code code in \code{\link[graphics]{arrows}}
#' @param arrow_col col in \code{\link[graphics]{arrows}}
#' @param arrow_lty lty in \code{\link[graphics]{arrows}}
#' @param arrow_lwd lwd in \code{\link[graphics]{arrows}}
#'
#' @export
type_arrows <- function(
  x0,
  y0,
  x1,
  y1,
  length = 0.25,
  angle = 30,
  code = 2,
  arrow_col = "black",
  arrow_lty = par("lty"),
  arrow_lwd = par("lwd")
) {
  # assert_numeric(x0)
  data_arrows <- function(datapoints, lwd, lty, col, ...) {
    if (nrow(datapoints) == 0) {
      msg <- "`type_hline() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }
    ul_lwd <- length(unique(lwd))
    ul_lty <- length(unique(lty))
    ul_col <- length(unique(col))
    return(list(
      type_info = list(ul_lty = ul_lty, ul_lwd = ul_lwd, ul_col = ul_col)
    ))
  }

  draw_arrows <- function() {
    fun <- function(
      ifacet,
      iby,
      data_facet,
      icol,
      ilty,
      ilwd,
      ngrps,
      nfacets,
      by_continuous,
      facet_by,
      type_info,
      ...
    ) {
      grp_aes <- type_info[["ul_col"]] == 1 ||
        type_info[["ul_lty"]] == ngrps ||
        type_info[["ul_lwd"]] == ngrps
      if (length(x0) != 1) {
        if (!length(x0) %in% c(ngrps, nfacets, ngrps * nfacets)) {
          msg <- "Length of 'x0' must be 1, or equal to the number of facets or number of groups 
          (or product thereof)."
          stop(msg, call. = FALSE)
        }
        if (!facet_by && length(x0) == nfacets) {
          x0 <- x0[ifacet]
          y0 <- y0[ifacet]
          x1 <- x1[ifacet]
          y1 <- y1[ifacet]
          if (!grp_aes && type_info[["ul_col"]] != ngrps) {
            icol <- 1
          } else if (by_continuous) {
            icol <- 1
          }
        } else if (!by_continuous && length(x0) == ngrps * nfacets) {
          x0 <- x0[ifacet * ngrps - c(ngrps - iby)]
          y0 <- y0[ifacet * ngrps - c(ngrps - iby)]
          x1 <- x1[ifacet * ngrps - c(ngrps - iby)]
          y1 <- y1[ifacet * ngrps - c(ngrps - iby)]
        } else if (!by_continuous) {
          x0 <- x0[iby]
          y0 <- y0[iby]
          x1 <- x1[iby]
          y1 <- y1[iby]
        }
      } else if (!grp_aes) {
        icol <- 1
      }
      arrows(
        x0 = x0,
        y0 = y0,
        x1 = x1,
        y1 = y1,
        length = length,
        angle = angle,
        code = code,
        col = arrow_col,
        lty = arrow_lty,
        lwd = arrow_lwd
      )
    }
    return(fun)
  }
  out <- list(draw = draw_arrows(), data = data_arrows, name = "hline")
  class(out) <- "tinyplot_type"
  return(out)
}
