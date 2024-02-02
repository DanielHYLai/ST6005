#-----
## example: general discrete random variable
rng.discrete.rv <- function(x, prob, n){
    p.cdf <- cumsum(prob)
    m <- length(p.cdf)
    result <- rep(0, n)
    
    for (k in c(1:n)){
        U <- runif(1, min = 0, max = 1)
        for (i in c(1:m)){
            if (U <= p.cdf[i]){
                result[k] <- x[i]
                break
            }
        }
    }
    
    return(result)
}

x <- c(1:3)               # 取值範圍
prob <- c(0.1, 0.3, 0.6)  # 給定機率
n <- 1000                 # sample size
result <- rng.discrete.rv(x, prob, n)
table(result) / n

#-----
## example: more efficient way of general discrete random variable
rng.discrete.rv <- function(x, prob, n){
    prob <- sort(prob, decreasing = TRUE)
    x <- sort(x, decreasing = TRUE)
    p.cdf <- cumsum(prob)
    m <- length(p.cdf)
    result <- rep(0, n)
    
    for (k in c(1:n)){
        U <- runif(1, min = 0, max = 1)
        for (i in c(1:m)){
            if (U <= p.cdf[i]){
                result[k] <- x[i]
                break
            }
        }
    }
    
    return(result)
}

x <- c(1:3)               # 取值範圍
prob <- c(0.1, 0.3, 0.6)  # 給定機率
n <- 1000                 # sample size
result <- rng.discrete.rv(x, prob, n)
table(result) / n

table(sample.int(n = 3, size = 1000, replace = TRUE, prob = prob)) / n  # base R