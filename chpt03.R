#-----
## exercise 3.1
MCG.alg <- function(x0, m, a, c = 0, seqResult, totalLength){
    seqResult[1] <- x0
    for (i in c(2:totalLength)){
        seqResult[i] <- (seqResult[i - 1] * a + c) %% m
    }
    return(seqResult)
}

x0 <- 5
m <- 150
a <- 3
totalLength <- 10
seqResult <- rep(0, totalLength)
seqResult <- 
    MCG.alg(x0 = x0, m = m, a = a, 
            seqResult = seqResult, totalLength = totalLength); seqResult

#-----
## exercise 3.2
x0 <- 3
m <- 200
a <- 5
c = 7
totalLength <- 20
seqResult <- rep(0, totalLength)
seqResult <- 
    MCG.alg(x0 = x0, m = m, a = a, c = c,
            seqResult = seqResult, totalLength = totalLength); seqResult

#-----
## exercise 3.3
N <- 1000000
set.seed(41)
x <- runif(N)
mean(exp(exp(x)))  # truly approximate value: 6.3166

#-----
## exercise 3.4
mean((1 - x^2)^(3/2))  # truly approximate value: 0.5890

#-----
## exercise 3.5
x <- -2 + 4 * runif(N)
4 * mean(exp(x + x^2))  # truly approximate value: 93.1627

#-----
## exercise 3.6
g <- function(x){
    return(x / (1 + x^2)^2)
}

h <- function(y){
    return(g(1 / y - 1) / y^2)
}

mean(h(runif(N)))  # true value: 0.5

#-----
## exercise 3.7
g <- function(x){
    return(exp(-x^2))
}

h <- function(y){
    return(g(1 / y - 1) / y^2)
}

2*mean(h(runif(N)))  # truly approximate value: 1.7737

#-----
## exercise 3.8
x <- runif(N)
y <- runif(N)
mean(exp((x + y)^2))  # true value: 4.8924

#-----
## exercise 3.9
x <- runif(N)
y <- runif(N)
mean(exp(-(1 / x - 1) * (1 + y)) * x^(-2) * (1 / x - 1))  # true value: 0.5

#-----
## exercise 3.10
U <- runif(N)
mean(exp(U) * (U - 1/2))  # approximate value
cov(U, exp(U))  # true value

#-----
## exercise 3.11(a)
U <- runif(N)
cor(U, sqrt(1 - U^2))
(mean(U * sqrt(1 - U^2)) - mean(U) * mean(sqrt(1 - U^2))) / (sqrt(mean(U^2) - mean(U)^2) * sqrt(mean(sqrt(1 - U^2)^2) - mean(sqrt(1 - U^2))^2))

## exercise 3.11(b)
U <- runif(N)
cor(U^2, sqrt(1 - U^2))
(mean(U^2 * sqrt(1 - U^2)) - mean(U^2) * mean(sqrt(1 - U^2))) / (sqrt(mean(U^4) - mean(U^2)^2) * sqrt(mean(sqrt(1 - U^2)^2) - mean(sqrt(1 - U^2))^2))

#-----
## exercise 3.12
## ref: https://math.stackexchange.com/questions/214399/summing-0-1-uniform-random-variables-up-to-1
## ref: https://mathworld.wolfram.com/UniformSumDistribution.html
## ref: https://github.com/dbfin/R-Simulation-by-Ross/blob/master/3.12.R
n <- c(100, 1000, 10000, 100000)

N <- function() {
    N = 0
    sum = 0
    while (sum <= 1) {
        N <- N + 1
        sum <- sum + runif(1)
    }
    N
}
N_sample <- function(n) {
    replicate(n, N())
}
mapply(mean, mapply(N_sample, n))  # true value: exponential

#-----
## exercise 3.13(a)
restriction <- function(cond){
    n = 0
    u = 1  # u0
    while (TRUE){
        u <- u * runif(1)  # product
        
        if (u > cond){
            n <- n + 1
        }else{
            break
        }
    }
    n
}

N <- c()
cond <- exp(-3)
times <- 50000
for (i in c(1:times)){
    N[i] <- restriction(cond = cond)
}
mean(N)

## exercise 3.13(b)
P <- c()
for (i in c(1:7)){
    P[i] <- as.numeric(table(N)[i]) / times
}
P

#-----
## exercise 3.14
MCG.alg <- function(x1, x2, m, a1, a2, c = 0, seqResult, totalLength){
    seqResult <- c(x1, x2)
    for (i in c(3:totalLength)){
        seqResult[i] <- (a1 * seqResult[i - 1] + a2 * seqResult[i - 2] + c) %% m
    }
    return(seqResult)
}

x1 <- 23
x2 <- 66
m <- 100
a1 <- 3
a2 <- 5
totalLength <- 20
seqResult <- rep(0, totalLength)
seqResult <- 
    MCG.alg(x1 = x1, x2 = x2, m = m, a1 = a1, a2 = a2, 
            seqResult = seqResult, totalLength = totalLength); seqResult