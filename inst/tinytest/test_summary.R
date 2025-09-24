library("tinytest")
library("cpt")

# First detect change points
cpt <- change_point_test(cpttestdata, alpha = 0.05, q = 3, N = 500)
expect_inherits(cpt, class(cpttestdata))
expect_silent(summary(cpt))

# data.frame
data("path_data_frame", package = "trackframe")
expect_inherits(path_data_frame, "data.frame")
# transform to utm nothing/easting
set.seed(2025L)
cpt <- change_point_test(path_data_frame, alpha = 0.05, q = 3, N = 100)
expect_inherits(cpt, class(path_data_frame))
summary_df <- summary(cpt)

# trackframe
data("path_trackframe", package = "trackframe")
path_trackframe$time <- path_data_frame$time #FIXME: in package trackframe
expect_inherits(path_trackframe, "trackframe")
set.seed(2025L)
cpt <- change_point_test(path_trackframe, alpha = 0.05, q = 3, N = 100)
expect_inherits(cpt, class(path_trackframe))
summary_tf <- summary(cpt)
expect_equal(summary_df, summary_tf)

# move2
data("path_move2", package = "trackframe")
path_move2$time <- path_data_frame$time #FIXME: in package trackframe
expect_inherits(path_move2, "move2")
set.seed(2025L)
cpt <- change_point_test(path_move2, alpha = 0.05, q = 3, N = 100)
expect_inherits(cpt, class(path_move2))
summary_move2 <- summary(cpt)
# compare with tf by coordinates transformation
utm_coords <- cbind.data.frame(
  cpt$time,
  sf::st_coordinates(sf::st_transform(cpt, attr(path_trackframe, "utm_epsg")))
)
idx <- match(as.character(summary_tf$first), as.character(utm_coords$`cpt$time`))
expect_equal(utm_coords[idx, 2], summary_tf$east)
expect_equal(utm_coords[idx, 3], summary_tf$north)

# sftrack
data("path_sftrack", package = "trackframe")
path_sftrack$time <- path_move2$time #FIXME: in package trackframe
expect_inherits(path_sftrack, "sftrack")
set.seed(2025L)
cpt <- change_point_test(path_sftrack, alpha = 0.05, q = 3, N = 100)
expect_inherits(cpt, class(path_sftrack))
summary_sftrack <- summary(cpt)
expect_equal(summary_move2, summary_sftrack)

# multiple paths
# trackframe
data("paths_trackframe", package = "trackframe")
expect_inherits(paths_trackframe, "trackframe")
set.seed(2025L)
cpt <- change_point_test(paths_trackframe, alpha = 0.05, q = 3, N = 100)
expect_inherits(cpt, class(paths_trackframe))
summary_tf <- summary(cpt)

# move2
data("paths_move2", package = "trackframe")
paths_move2$time <- paths_trackframe$time #FIXME: in package trackframe
expect_inherits(paths_move2, "move2")
set.seed(2025L)
cpt <- change_point_test(paths_move2, alpha = 0.05, q = 3, N = 100)
expect_inherits(cpt, class(paths_move2))
summary_move2 <- summary(cpt)

# compare with tf by coordinates transformation
utm_coords <- cbind.data.frame(
  cpt$time,
  cpt$id,
  sf::st_coordinates(sf::st_transform(cpt, attr(paths_trackframe, "utm_epsg")))
)
idx <- match(
  paste0(as.character(summary_tf$first), summary_tf$id),
  paste0(as.character(utm_coords$`cpt$time`), utm_coords$`cpt$id`)
)
expect_equal(utm_coords[idx, 3], summary_tf$east)
expect_equal(utm_coords[idx, 4], summary_tf$north)

# sftrack
data("paths_sftrack", package = "trackframe")
paths_sftrack$time <- paths_trackframe$time #FIXME: in package trackframe
expect_inherits(paths_sftrack, "sftrack")
set.seed(2025L)
cpt <- change_point_test(paths_sftrack, alpha = 0.05, q = 3, N = 100)
expect_inherits(cpt, class(paths_sftrack))
summary_sftrack <- summary(cpt)
expect_equal(summary_sftrack, summary_move2)
