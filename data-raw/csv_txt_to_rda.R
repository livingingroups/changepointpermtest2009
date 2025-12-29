cptfiguredata <- read.csv(system.file(
  "extdata",
  "cptfigdata.csv",
  package = "cpt"
))
cptfiguredata$time <- as.POSIXct(seq_len(nrow(cptfiguredata)))
cptfiguredata_tf <- trackframe::as.trackframe(cptfiguredata)
cpttestdata <- as.data.frame(scan(
  system.file("extdata", "cpttestdata.txt", package = "cpt"),
  list(x1 = 0, x2 = 0)
))
cpttestdata$time <- as.POSIXct(seq_len(nrow(cpttestdata)))
save(cptfiguredata, file = "data/cptfiguredata.rda")
save(cptfiguredata_tf, file = "data/cptfiguredata_tf.rda")
save(cpttestdata, file = "data/cpttestdata.rda")
