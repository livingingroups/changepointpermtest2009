# load packages
library(trackframe)
library(cpt)

# set wd as path folder
setwd("~/travelpaths-devel/pkgs/cpt/devel")

# load data
FFT <- read.csv(file.path("..", "..", "..", "data", "FFT.csv"))

# travelpaths:::as.track_frame.data.frame
FFT$timestamp <- as.POSIXct(FFT$timestamp) #need Posixct timestamp
FFT_tf <- as.track_frame(FFT,
                         index = "timestamp",
                         lon_col = "location.long", #"utm.easting",
                         lat_col = "location.lat", #"utm.northing",
                         id_cols = c("individual.local.identifier", "tag.local.identifier") # id_col = "individual.local.identifier"
)

FFT_tf_abby <- dplyr::filter(FFT_tf, individual.local.identifier == "Abby", tag.local.identifier == 4652)
tf <- FFT_tf_abby

# tf <- tf[seq(1, NROW(tf), by = 10),] # for faster run

# ? better return track_frame instead of xts?
# sig_cpt <- change_point_test(tf, alpha = 0.05, q = 4, N = 10000, tol = 0) #authors recommend N=10000
sig_cpt <- change_point_test(tf, alpha = 0.05, q = 4, N = 1000, tol = 0)
# sig_cpt <- change_point_test(tf, alpha = 0.05, q = 4, N = 1000, tol = 0.00005)


head(sig_cpt)
str(sig_cpt)
cpt <- sig_cpt[sig_cpt$sig == 1, ]
head(cpt)
tail(cpt)


library(leaflet)
library(leaflet.extras2)
# pal <- colorBin("viridis", sig_xts$sig)
pal <- colorFactor(
  palette = c('blue', 'red'),
  domain = sig_cpt$sig
)

set_size <- function(x){
  ifelse(x == 0, 0.5, 5)
}
coredata(sig_cpt)
# Style 1
leaflet(as.data.frame(coredata(sig_cpt))) %>%
  addTiles() %>%
  addCircleMarkers(~x1 , ~x2, color = ~pal(sig), group = "circles",
                   opacity = 1,#0.5,
                   fillColor = ~pal(sig),
                   fillOpacity = 1,#0.2,
                   radius = ~set_size(sig)) %>%
  addLegend(pal = pal, values = ~sig, group = "circles", position = "bottomleft") %>%
  addArrowhead(lng = ~x1, lat = ~x2, weight = 1, color = "green",
               options = arrowheadOptions(size = "10%"))


# Style 2
sig_cpt$index2 <- (as.integer(index(sig_cpt)) - min(as.integer(index(sig_cpt)))) / max(as.integer(index(sig_cpt)) - min(as.integer(index(sig_cpt))))
# col_pal <- viridis(n=nrow(circles1d))
pal <- colorNumeric("viridis", sig_cpt$index2, reverse = TRUE)

leaflet(as.data.frame(coredata(sig_cpt))) %>%
  addTiles() %>%
  addCircleMarkers(~x1 , ~x2, color = ~pal(index2), group = "circles",
                   opacity = 1,#0.5,
                   fillColor = ~pal(index2),
                   fillOpacity = 1,#0.2,
                   radius = ~set_size(sig)) %>%
                   # radius = 1) %>%
  addLegend(pal = pal, values = ~index2, group = "circles", position = "bottomleft") %>%
  addArrowhead(lng = ~x1, lat = ~x2, weight = 0.3, color = "green",
               options = arrowheadOptions(size = "5%"))

# Style 3
leaflet(as.data.frame(coredata(sig_cpt))) %>%
  addTiles() %>%
  addCircleMarkers(~x1 , ~x2, color = ~pal(index2), group = "circles",
                   opacity = 1,#0.5,
                   fillColor = ~pal(index2),
                   fillOpacity = 1,#0.2,
                   radius = ~set_size(sig)) %>%
  addCircleMarkers(~x1[sig==1] , ~x2[sig==1], color = "red", group = "circles",
                   opacity = 1,#0.5,
                   fillColor = ~pal(index2[sig==1]),
                   fillOpacity = 1,#0.2,
                   radius = 5) %>%
  addLegend(pal = pal, values = ~sig, group = "circles", position = "bottomleft") %>%
  addArrowhead(lng = ~x1, lat = ~x2, weight = 0.3, color = "green",
               options = arrowheadOptions(size = "5%"))

# also add annotations like in old code?

# plot only change points
leaflet(as.data.frame(coredata(cpt))) %>%
  addTiles() %>%
  addCircleMarkers(~x1 , ~x2, color = "red", group = "circles",
                   opacity = 1,#0.5,
                   fillColor = "red",
                   fillOpacity = 1,#0.2,
                   radius = 5)
