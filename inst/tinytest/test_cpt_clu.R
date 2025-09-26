library(tinytest)
# Test Suite for change_point_test clu functionality
library(cpt)
library(trackframe)
library(parallel)

tf <- as.trackframe(cpttestdata)

data("paths_trackframe", package = "trackframe")
tf <- paths_trackframe
set.seed(2025L)
cpt_tf <- change_point_test(tf, alpha = 0.05, q = 3, n = 500, tol = 0, seed = 2025)
set.seed(2025L)
if (parallel::detectCores() >= 4) {
  cpt_tf_4 <- change_point_test(tf, alpha = 0.05, q = 3, n = 500, tol = 0, clu = 4, seed = 2025)
  expect_equal(cpt_tf, cpt_tf_4)
  set.seed(2025L)
  cpt_tf_0 <- change_point_test(tf, alpha = 0.05, q = 3, n = 500, tol = 0, clu = 0, seed = 2025)
  expect_equal(cpt_tf, cpt_tf_0)
  set.seed(2025L)
  cpt_tf_100 <- change_point_test(tf, alpha = 0.05, q = 3, n = 500, tol = 0, clu = 100, seed = 2025)
  expect_equal(cpt_tf, cpt_tf_100)
  clu <- makeCluster(4)
  set.seed(2025L)
  cpt_tf_clu <- change_point_test(tf, alpha = 0.05, q = 3, n = 500, tol = 0, clu = clu, seed = 2025)
  expect_equal(cpt_tf, cpt_tf_clu)
  stopCluster(clu)
}
