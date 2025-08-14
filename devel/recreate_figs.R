library(ggplot2)
library(trackframe)
library(cpt)
# devtools::load_all('pkgs/trackframe')
# devtools::load_all('pkgs/cpt')

data(cptfiguredata)

figdata <- cptfiguredata
# TODO: just save the trackframe in the package 
figdata$index <- as.POSIXct(seq_len(nrow(figdata)))
figdata$y <- -figdata$y
?trackframe::as.trackframe.data.frame
tf <- trackframe::as.trackframe(
  figdata,
  easting_col = 'x',
  northing_col = 'y',
  time_col = 'index',
  id_col = 'track_id'
)


plot_raw_xy <- function(tf){
  list(
    geom_path(
      data = tf[order(index(tf)),],
      mapping = aes(x = easting(tf), y = northing(tf), group = track_id)
    ),
    coord_fixed()
  )
  # TODO:
  # - show starting point and direction.
  # - facet wrap instead of relying on fake coords being non-overlapping
}

plot_cp_loc <- function(cp) {
  geom_point(data = cp[cp$sig == 1,], aes(x=x, y=y), shape="\u2605", size = 8)
}

# plot_P <- function(df, alpha = c(.1, .05, .01)) {
#   # df <- fig5_data
#   checkmate::assert_data_frame(df)
#   checkmate::assert_subset(c('k', 'q', 'P'), names(df))
#   ggplot(data = df[!is.na(df$P), ], mapping = aes(x = k, y = -log(P))) +
#     geom_point(shape = 4) +
#     geom_line() +
#     facet_wrap(vars(q), ncol = 2) +
#     geom_hline(
#       data = data.frame(y = -log(alpha)),
#       mapping = aes(yintercept = y)
#     )
# }

calculate_n_cp_by_q <- function(tf, q=seq_len(10), alpha = 0.01) {
  # tf <- tf[startsWith(tf$track_id, '4'), ]
  if(length(attr(tf, 'id'))!=1) stop('only implemented for 1 id col')
  do.call(rbind, lapply(seq_len(10), function(q) {
    cp <- change_point_test(
      tf,
      alpha = alpha,
      q = q
    )
    # cp <- do.call(rbind, lapply(seq_len(cp), function(i) cbind(cp, "track_id" = i)))
    df <- as.data.frame(rowsum(cp$sig, as.factor(cp[,attr(tf, 'id')])))
    names(df) <- 'n_cp'
    df$track_id <- rownames(df)
    rownames(df) <- NULL
    df$q <- q
    df
  }))
}


## Fig  3

ggplot() +
  plot_raw_xy(tf[startsWith(tf$track_id, '3'),])

# Fig 4
# b and d seem to match up well
# a and c seem to be missing some changepoints
cp <- change_point_test(
  tf[startsWith(tf$track_id, '4'),],
  alpha = .01,
  q = 6
)

ggplot() + plot_raw_xy(tf[startsWith(tf$track_id, '4'),]) + plot_cp_loc(cp)

# Fig 5

# fig5_data <- do.call(rbind, lapply(1:10, function(q) {
#   data <- tf[tf$track_id == '4a', ]
#   data.frame(
#     P = change_point_test_pvalue(
#       data,
#       q_max = q,
#       N = 100
#     ),
#     q = q,
#     k = 0:(nrow(tf[tf$track_id == '4a', ]) - 1)
#   )
# }))



# Fig 5
N = 10000
data <- tf[tf$track_id == '4a', ]
P = change_point_test_pvalue(
# P = cpt:::change_point_test_pvalue_old( #gives different result due to different calculation order
  data,
  q_max = 10,
  N = N
)
round(-log(P), digits = 3)


# plot results (- log(P) vs k for q = 1, .. , qq)
# NOTE: currently qq = 6
dim(P)
no.of.t <- NROW(P)
k <- c(1:no.of.t)
s <- seq(no.of.t - 1)

par(mfrow = c(3, 2))
for(i in 1:NCOL(P)) {
  plot(k, -log(P[k, i]), xlim = c(0, no.of.t + 1), ylim = c(0, log(N)), pch = 4, main = sprintf("q = %i",i))
  y <- -log(P[k, i])
  segments(k[s], y[s], k[s + 1], y[s + 1])
  abline(h = -log(0.10), col = "blue")
  abline(h = -log(0.05), col = "red")
  abline(h = -log(0.01), col = "green")
}


# Fig 6
# Pretty different
fig6_data <- calculate_n_cp_by_q(tf[startsWith(tf$track_id, '4'), ])
ggplot(data = fig6_data, mapping = aes(x=q, y=n_cp, group=track_id, colour = track_id)) + geom_smooth(span = .4) + geom_point()

# Fig 7
# a - almost identical
# b - our version has one more CP, but others look the same
# c - our version seems to be missing one, but ohters look the same
# d - not too similar, likely because of transcription issue not algo issue

cp <- change_point_test(
  tf[startsWith(tf$track_id, '7'),],
  alpha = .05,
  q = 4
)
ggplot() + plot_raw_xy(tf[startsWith(tf$track_id, '7'),]) + plot_cp_loc(cp)

# Fig 8

# fig8_data <- do.call(rbind, lapply(1:6, function(q) {
#   data.frame(
#     P = change_point_test(
#       tf[tf$track_id == '7a', ],
#       alpha = NULL,
#       q = q,
#     ),
#     q = q,
#     k = 0:(nrow(tf[tf$track_id == '7a', ]) - 1)
#   )
# }))
# plot_P(fig8_data)
# Fig 5
N = 10000
data <- tf[tf$track_id == '7a', ]
# P = change_point_test_pvalue( #gives different result due to different calculation order
  P = cpt:::change_point_test_pvalue_old(
  data,
  q_max = 6,
  N = N
)
round(-log(P), digits = 3)


# plot results (- log(P) vs k for q = 1, .. , qq)
# NOTE: currently qq = 6
dim(P)
no.of.t <- NROW(P)
k <- c(1:no.of.t)
s <- seq(no.of.t - 1)

par(mfrow = c(3, 2))
for(i in 1:NCOL(P)) {
  plot(k, -log(P[k, i]), xlim = c(0, no.of.t + 1), ylim = c(0, log(N)), pch = 4, main = sprintf("q = %i",i))
  y <- -log(P[k, i])
  segments(k[s], y[s], k[s + 1], y[s + 1])
  abline(h = -log(0.10), col = "blue")
  abline(h = -log(0.05), col = "red")
  abline(h = -log(0.01), col = "green")
}

# Fig 9
# this one looks super different
fig9_data <- calculate_n_cp_by_q(tf[startsWith(tf$track_id, '7'), ], alpha = 0.05)
ggplot(data = fig9_data, mapping = aes(x=q, y=n_cp, group=track_id, colour = track_id)) + geom_smooth(span=.4) + geom_point()
