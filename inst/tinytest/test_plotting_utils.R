# Repro #119
tf <- trackframe::as.trackframe(data.frame(
  x = c(1, 1, 2),
  y = c(1, 1, 2),
  t = 1:3,
  id = 'a'
), crs = NA)
expect_equal(get_arrow_points(tf), list(x0 = 1, y0 = 1, x1 = 2, y1 = 2))
