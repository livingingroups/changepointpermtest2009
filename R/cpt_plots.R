#' Plot Change point test output
#'
#' Plots change points of objects of class \code{\link[cpt]{change_point_test}}
#' based on \code{\link[tinyplot]{tinyplot}} functionality.
#'
#' @param x an object of class \code{change_point_test}
#' @param cp_style list graphical parameters to apply to change points
#' @param lines logical should lines be plotted?
#' @param lines_style list of graphical parameters
#'  passed to [tinyplot::tinyplot()] when plotting lines
#' @param points logical should track data be plotted as points?
#' @param points_style graphical parameters
#'  passed to [tinyplot::tinyplot()] when plotting points
#' @param start_indicator logical indicator if arrows indicating
#' start point of each track should be added
#' @param start_indicator_style list of graphical parameters for start indicator.
#'  See section 'x_indicator_style'.
#' @param end_indicator logical indicator if arrows indicating
#' endpoint of each track should be added
#' @param end_indicator_style list of graphical parameters for end indicator.
#'  See section 'x_indicator_style'.
#' @param facet logical should the plot be facetted by track?
#' @param facet.args list of arguments controlling facet behavior.
#'  If `ncol` is unspecified, an attempt is made chose a visually pleasing value.
#'  See \code{\link[tinyplot]{tinyplot}}
#' @param asp numeric y/x aspect ratio
#' @param theme character or list:
#'   1) `NULL` (default): Use currently set `tinyplot` theme if one is set.
#'     Otherwise use trackframe default.
#'   2) a string naming a built-in tinyplot theme `vignette("themes", package = "tinyplot")` or
#'   3) a list of graphical parameters defining a custom theme
#'
#' @param ... additional graphical parameters passed to \code{\link[tinyplot]{tinyplot}}
#'
#' @details all args passed on to [plot::trackframe] (`cp_style`` passed as `marker_style`)
#' This method exists to provide different defaults
#'
#' @export
#'
#' @examples
#' library(changepointpermtest2009)
#' library(trackframe)
#' library(tinyplot)
#'
#' data("cpttestdata")
#' data <- trackframe::as.trackframe(cpttestdata, crs = NA)
#' class(data)
#' tinytheme("clean2")
#' plot(data)
#' # single track
#' plot(select_id(data, "track_1"))
#'
#' # calculate change points
#' cpt <- change_point_test(data, alpha = .01, n = 10000, q = 6)
#' class(cpt)
#'
#' plot(cpt)
#' # without path directions
#' plot(cpt, start_indicator = FALSE, end_indicator = FALSE)
#'
#' # only one track
#' cpt1 <- select_id(cpt, "track_1")
#' plot(cpt1)
#' # without path direction
#' plot(cpt1, start_indicator = FALSE, end_indicator = FALSE)
plot.change_point_test <- function(
  x,
  cp_style = list(col = "black", cex = 3, pch = "*"),
  lines = TRUE,
  lines_style = list(col = "black"),
  points = FALSE,
  points_style = list(pch = 19),
  start_indicator = TRUE,
  start_indicator_style = list(col = "green"),
  end_indicator = TRUE,
  end_indicator_style = list(col = "red"),
  facet = TRUE,
  facet.args = list(), # nolint: object_name_linter
  theme = NULL,
  asp = 1,
  ...
) {
  assert_class(x, "change_point_test")
  tf_options("crs", NA)
  cpt_tf <- as.trackframe(x)
  if (is.null(id(cpt_tf))) {
    id <- "id_int"
    attr(cpt_tf, "id") <- id
    cpt_tf$id_int <- "id_1"
  }
  cpt_tf$marker <- cpt_tf$cp_id != 0
  class(cpt_tf) <- class(cpt_tf)[!class(cpt_tf) == "change_point_test"]
  plot(
    cpt_tf,
    points = points,
    lines_style = lines_style,
    start_indicator = start_indicator,
    start_indicator_style = start_indicator_style,
    end_indicator = end_indicator,
    end_indicator_style = end_indicator_style,
    marker = "marker",
    marker_style = cp_style,
    facet = facet,
    facet.args = facet.args,
    theme = theme,
    asp = asp,
    ...
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
#' library(changepointpermtest2009)
#' library(trackframe)
#' library(tinyplot)
#'
#' data("cpttestdata")
#' data <- trackframe::as.trackframe(cpttestdata, crs = NA)
#' set.seed(2025)
#' P = change_point_test_pvalue(data, q_max = 10, n = 100)
#'
#' tinytheme("clean2")
#'
#' plot(P[[1]])
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
#' data("cpttestdata")
#' data <- trackframe::as.trackframe(cpttestdata, crs = NA)
#'
#' calculate_n_cp_by_q <- function(tf, q=seq_len(10), alpha = 0.01) {
#'  if(length(attr(tf, 'id'))!=1) stop('only implemented for 1 id col')
#'  do.call(rbind, lapply(seq_len(10), function(q) {
#'    cp <- change_point_test(
#'        tf,
#'        alpha = alpha,
#'        q = q
#'  )
#'  df <- as.data.frame(rowsum(as.integer(cp$cp_id != 0), as.factor(cp[,attr(tf, 'id')])))
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
  args <- list(...)
  if (any(names(args) %in% restricted)) {
    warning(sprintf(
      "argument %s is restricted and therefore ignored",
      names(args)[names(args) %in% restricted]
    ))
  }
  control <- modifyList(default_options, args[!names(args) %in% restricted])
  do.call(tinyplot, c(list(form, data = data), control))
}
