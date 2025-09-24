if (FALSE) {
  library(tinytest)
}
# Test Suite for change_point_test Function
library(cpt)
library(trackframe)

file <- "~/travelpaths-devel/data/8_7august11.txt"
if (file.exists(file)) {
  inp <- scan(file, list(x1 = 0, x2 = 0))
  df <- as.data.frame(inp)
  head(df)
  df$t <- 1:NROW(df) #seq.POSIXt(from = as.POSIXct("2025-01-01"), by = "min", length.out = nrow(df))
  colnames(df)[1:2] <- c("x", "y")
  cpttestdata <- df
  cpttestdata_tf <- as.trackframe(cpttestdata)
  attributes(cpttestdata_tf)
  # data("cpttestdata", package = "cpt")
  xyt <- cpttestdata
  set.seed(2025L)
  cpt <- change_point_test(xyt, alpha = 0.05, q = 4, N = 1000, tol = 0)
  # cat(deparse(cpt$sig))
  results_orig <- c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0
  )
  expect_equal(cpt[, "sig"], results_orig)
  expect_equal(summary(cpt)[, "last"], c(39, 65, 74, 88, 132))

  tf <- as.trackframe(
    data.frame(
      t = as.POSIXct(seq_along(cpttestdata[, 3])),
      x = cpttestdata[, 1],
      y = cpttestdata[, 2]
    ),
    't',
    'x',
    'y'
  )
  set.seed(2025L)
  cpt_tf <- change_point_test(tf, alpha = 0.05, q = 4, N = 1000, tol = 0)
  expect_equal(cpt_tf$sig, results_orig)
  expect_equal(
    summary(cpt_tf)[, "east"],
    c(688443.1545, 687898.0517, 687666.5149, 687345.6599, 687095.3092)
  )
}
