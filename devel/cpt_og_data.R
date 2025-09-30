file <- "data/8_7august11.txt"
file <- "data/CPTtestdata.txt"
file <- "data/CPTtestdata2.txt"


inp <- scan(file, list(x1 = 0, x2 = 0))
x1 <- inp[[1]]
x2 <- inp[[2]]

tf <- as.trackframe(data.frame(t=as.POSIXct(seq_along(x1)), y = x1, x=x2), 't', 'y', 'x')

sig_cpt <- change_point_test(tf, alpha = 0.05, q = 4, N = 10000, min_move_dist = 0) #authors recommend N=10000

plot.new()
x1 <- rev(x1)
x2 <- rev(x2)
plot(x1, x2, type='l')
sig <- sig_cpt$sig
points(x1[sig==1], x2[sig==1], col='red')


sig_cpt <- change_point_test(tf, alpha = 0.05, q = 4, N = 1000, min_move_dist = 0)
sig_cpt <- change_point_test(tf, alpha = 0.05, q = 4, N = 1000, min_move_dist = 0.00005)