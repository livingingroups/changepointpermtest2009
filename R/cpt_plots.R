#' Plot Change point test output
#'
#' Plots change points of objects of class \code{\link[cpt]{change_point_test}}
#' based on \code{\link[tinyplot]{tinyplot}} functionality.
#'
#' @param x an object of class \code{change_point_test}
#' @param direction logical indicator if the path direction should be added to the plot
#' @param direction_style a list of length, code, col, lty, lwd of the arrow of the direction
#' (argument passed to \code{\link[graphics]{arrows}}) specifying the style of the arrows
#' @param cp_col color of the change points
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
  direction_style = list(length = 0.1, code = 2, col = "black", lty = 3, lwd = 1),
  cp_col = "black",
  nfacet_col = NULL,
  ...
) {
  # method dispatch for trackframe, sftrack, move2 +? data.frame
  plotcpt(
    cpt = x,
    direction = direction,
    direction_style = direction_style,
    cp_col = cp_col,
    nfacet_col = nfacet_col
  )
}


#' @keywords internal
plotcpt <- function(
  cpt,
  direction = FALSE,
  direction_style = list(length = 0.1, code = 2, col = "black", lty = 3, lwd = 1),
  cp_col = "red",
  nfacet_col = NULL,
  ...
) {
  UseMethod("plotcpt")
}


#' @keywords internal
plotcpt.trackframe <- function(
  cpt,
  direction = FALSE,
  direction_style = list(length = 0.1, code = 2, col = "black", lty = 3, lwd = 1),
  cp_col = "red",
  nfacet_col = NULL,
  ...
) {
  assert_class(cpt, "trackframe")
  assert_logical(direction)
  assert_list(direction_style)
  assert_character(cp_col)
  assert_integerish(nfacet_col, null.ok = TRUE)
  x <- attr(cpt, "easting")
  y <- attr(cpt, "northing")
  id <- attr(cpt, "id")

  if (is.null(id)) {
    id <- "id_int"
    attr(cpt, "id") <- id
    cpt$id_int <- "id_1"
  }

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
      facet.args = list("free" = FALSE, ncol = nfacet_col),
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
  do.call(tinyplot, c(list(form, data = cpt), control))
  # add change points
  tinyplot_add(data = cpt[cpt[["sig"]] == 1, ], type = "p", cex = 3, pch = "*",
    col = cp_col) # NOTE: do we want to add cp numbers?
  # add starting point
  tinyplot_add(data = cpt[!duplicated(cpt[[id]]), ], type = "p", cex = 1,
    pch = "|", col = "green")
  # add end point
  tinyplot_add(data = cpt[!duplicated(cpt[[id]], fromLast = TRUE), ], type = "p",
    cex = 1, pch = 4, col = "red")

  if (isTRUE(direction)) {
    # add arrow in path direction from (x1, y1) to (x2, y2)
    starting_points <- get_starting_points(cpt)
    direction_points <- get_direction_points(cpt)
    if (NROW(starting_points) != NROW(direction_points)) {
      stop("direction points do not exist for all IDs. Set direction = FALSE.")
    }
    # needed to match ids to ensure correct ordering in id's
    uids <- unique(id(cpt))
    direction_style_defaults <- list(length = 0.1, code = 2, col = "black", lty = 3, lwd = 1)
    direction_style <- modifyList(direction_style_defaults, direction_style)
    tinyplot_add(
      data = cpt,
      add = TRUE,
      type = type_arrows(
        x0 = easting(starting_points)[match(uids, id(starting_points))],
        y0 = northing(starting_points)[match(uids, id(starting_points))],
        x1 = easting(direction_points)[match(uids, id(direction_points))],
        y1 = northing(direction_points)[match(uids, id(direction_points))],
        length = direction_style[["length"]],
        code = direction_style[["code"]]
      ),
      col = direction_style[["col"]],
      lty = direction_style[["lty"]],
      lwd = direction_style[["lwd"]],
      facet = arrows_facet
    )
  }
}


#' @keywords internal
plotcpt.sftrack <- function(
  cpt,
  direction = FALSE,
  direction_style = list(length = 0.1, code = 2, col = "black", lty = 3, lwd = 1),
  cp_col = "red",
  nfacet_col = NULL,
  ...
) {
  assert_class(cpt, "change_point_test")
  cpt_tf <- as.trackframe(cpt)
  plotcpt(
    cpt = cpt_tf,
    direction = direction,
    direction_style = direction_style,
    cp_col = cp_col,
    nfacet_col = nfacet_col
  )
}


#' @keywords internal
plotcpt.move2 <- plotcpt.sftrack

#' @keywords internal
plotcpt.data.frame <- function(
  cpt,
  direction = FALSE,
  direction_style = list(length = 0.1, code = 2, col = "black", lty = 3, lwd = 1),
  cp_col = "red",
  nfacet_col = NULL,
  ...
) {
  assert_class(cpt, "change_point_test")
  cpt_tf <- as.trackframe(cpt, crs = NA)
  plotcpt(
    cpt = cpt_tf,
    direction = direction,
    direction_style = direction_style,
    cp_col = cp_col,
    nfacet_col = nfacet_col
  )
}

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
  assert_class(x, "change_point_test_pvalue")
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
  do.call(tinyplot, c(list(form, data = p_long), control))
  tinyplot_add(type = type_hline(h = -log(0.10)), col = "blue")
  tinyplot_add(type = type_hline(h = -log(0.05)), col = "red")
  tinyplot_add(type = type_hline(h = -log(0.01)), col = "green")
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
  assert_character(id_col)
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
  do.call(tinyplot, c(list(form, data = data), control))
}
