

do_rbind <- function(x, make.row.names = FALSE) {
  do.call(rbind.data.frame, c(x, list(make.row.names = make.row.names)))
}



