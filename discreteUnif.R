#-----
## example: discrete uniform distribution
## ref: https://www.math.pku.edu.cn/teachers/lidf/docs/statcomp/html/_statcompbook/index.html
rng.discrete.unif <- function(n = 2, m = 3){
    ceiling(m * runif(n, min = 0, max = 1))  # U ~ U(0, 1) => mU ~ U(0, m)
}
rng.discrete.unif(n = 50, m = 6)  # with replacement

sample.int(n = 6, size = 50, replace = TRUE)  # base R