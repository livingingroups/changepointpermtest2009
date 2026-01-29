library(tinytest)
library(trackframe)
library(cpt)

projected_crs <- "EPSG:32632"

# cpt
# single track
data("path_data_frame", package = "trackframe")
tf <- as.trackframe(path_data_frame, crs = projected_crs)
set.seed(2025)
cpt_tf <- change_point_test(data = tf, n = 100)

tf_tibble <- as.trackframe(
  path_data_frame,
  crs = projected_crs,
  coerce_to = "tibble"
)
set.seed(2025)
cpt_tf_tibble <- change_point_test(data = tf_tibble, n = 100)
expect_equal(cpt_tf[["cp_no"]], cpt_tf_tibble[["cp_no"]])

tf_dt <- as.trackframe(
  path_data_frame,
  crs = projected_crs,
  coerce_to = "data.table"
)
set.seed(2025)
cpt_tf_dt <- change_point_test(data = tf_dt, n = 100)
expect_equal(cpt_tf[["cp_no"]], cpt_tf_dt[["cp_no"]])


# multipe tracks
data("paths_data_frame", package = "trackframe")
tf <- as.trackframe(paths_data_frame, crs = projected_crs)
set.seed(2025)
cpt_tf <- change_point_test(data = tf, n = 100)

tf_tibble <- as.trackframe(
  paths_data_frame,
  crs = projected_crs,
  coerce_to = "tibble"
)
set.seed(2025)
cpt_tf_tibble <- change_point_test(data = tf_tibble, n = 100)
expect_equal(cpt_tf[["cp_no"]], cpt_tf_tibble[["cp_no"]])

tf_dt <- as.trackframe(
  paths_data_frame,
  crs = projected_crs,
  coerce_to = "data.table"
)
set.seed(2025)
cpt_tf_dt <- change_point_test(data = tf_dt, n = 100)
expect_equal(cpt_tf[["cp_no"]], cpt_tf_dt[["cp_no"]])

# cpt pvalue
# single track
data("path_data_frame", package = "trackframe")
tf <- as.trackframe(path_data_frame, crs = projected_crs)
set.seed(2025)
cpt_tf <- change_point_test_pvalue(data = tf, n = 10)

tf_tibble <- as.trackframe(
  path_data_frame,
  crs = projected_crs,
  coerce_to = "tibble"
)
set.seed(2025)
cpt_tf_tibble <- change_point_test_pvalue(data = tf_tibble, n = 10)
expect_equal(cpt_tf, cpt_tf_tibble)

tf_dt <- as.trackframe(
  path_data_frame,
  crs = projected_crs,
  coerce_to = "data.table"
)
set.seed(2025)
cpt_tf_dt <- change_point_test_pvalue(data = tf_dt, n = 10)
expect_equal(cpt_tf, cpt_tf_dt)
