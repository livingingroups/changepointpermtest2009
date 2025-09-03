# FIXME: relative path
setwd("~/travelpaths-devel")
file <- "data/8_7august11.txt"
inp <- scan(file, list(x1 = 0, x2 = 0))
df <- as.data.frame(inp)
head(df)
df$t <- seq.POSIXt(from = as.POSIXct("2025-01-01"), by = "min", length.out = nrow(df))
colnames(df)[1:2] <- c("x", "y")
cpttestdata <- df
cpttestdata$x <- cpttestdata$x - 100000
cpttestdata$y <- cpttestdata$y + 60000
cpttestdata$x[duplicated(cpttestdata$x)] <- cpttestdata$x[duplicated(cpttestdata$x)] + 15
cpttestdata$y[duplicated(cpttestdata$y)] <- cpttestdata$y[duplicated(cpttestdata$y)] - 20
cpttestdata <- cpttestdata[1:150,]

cpttestdata_tf <- as.trackframe(cpttestdata)
attributes(cpttestdata_tf)
save(cpttestdata, file = "~/travelpaths-devel/pkgs/cpt/data/cpttestdata.rda", compress = "xz")
save(cpttestdata_tf, file = "~/travelpaths-devel/pkgs/cpt/data/cpttestdata_tf.rda", compress = "xz")
