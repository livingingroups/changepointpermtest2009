library(tinytest)
# Test Suite for change_point_test clu functionality
library(changepointpermtest2009)
library(trackframe)
library(parallel)

ncores <- if (tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_")) %in% c("", "false")) {
  parallel::detectCores()
} else {
  min(parallel::detectCores(), 2)
}

tf <- as.trackframe(cpttestdata, crs = NA)

data("paths_trackframe", package = "trackframe")
tf <- paths_trackframe
set.seed(2025L)
cpt_tf <- change_point_test(
  tf,
  alpha = 0.05,
  q = 3,
  n = 500,
  min_move_dist = 0,
  seed = 2025
)
set.seed(2025L)
if (ncores >= 4) {
  cpt_tf_4 <- change_point_test(
    tf,
    alpha = 0.05,
    q = 3,
    n = 500,
    min_move_dist = 0,
    clu = 4,
    seed = 2025
  )
  expect_equal(cpt_tf, cpt_tf_4)
  set.seed(2025L)
  cpt_tf_100 <- change_point_test(
    tf,
    alpha = 0.05,
    q = 3,
    n = 500,
    min_move_dist = 0,
    clu = 100,
    seed = 2025
  )
  expect_equal(cpt_tf, cpt_tf_100)
  clu <- makeCluster(4)
  set.seed(2025L)
  cpt_tf_clu <- change_point_test(
    tf,
    alpha = 0.05,
    q = 3,
    n = 500,
    min_move_dist = 0,
    clu = clu,
    seed = 2025
  )
  expect_equal(cpt_tf, cpt_tf_clu)
  stopCluster(clu)
}
