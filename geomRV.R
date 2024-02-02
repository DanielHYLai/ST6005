#-----
## example 4d: generate the geometric distribution
rng.geom <- function(n, p = 0.5){
    ceiling(log(runif(n)) / log(1-p))
}

n <- 1000
x <- rng.geom(n = n, p = 0.9)
mean(x)