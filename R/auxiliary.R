do_rbind <- function(x, make_row_names = FALSE) {
  do.call(rbind.data.frame, c(x, list(make.row.names = make_row_names)))
}


if (getRversion() <= "4.4.0") {
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}
