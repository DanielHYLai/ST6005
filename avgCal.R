#-----
## example 4c: calculating averages
a <- 2 * seq(1, 1000) + pi
mean(a)  # true value

N <- 100000
x <- 2 * ceiling(1000 * runif(N, min = 0, max = 1)) + pi
sum(x) / N  # approximate value