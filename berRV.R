#-----
## example 4e: generating a sequence of independent Bernoulli random variables
## original method
rng.ber.v1 <- function(n, prob){
    seqResult <- c(1:n)
    for (i in c(1:n)){
        if (runif(1) <= prob){
            seqResult[i] <- 0
        }else{
            seqResult[i] <- 1
        }
    }
    return(seqResult)
}

s = Sys.time()
x <- rng.ber.v1(n = 100000, prob = 0.2)
mean(x)
print(paste("elapsed time:", round(Sys.time() - s, 4)))

## using geometric distribution to generate Bernoulli random variables
rng.ber.v2 <- function(n, prob){
    seqResult <- rep(0, n)
    k = 0
    while (k <= n){
        U <- ceiling(log(runif(1)) / log(1-prob))
        if (k + U <= n){
            seqResult[k + U] <- 1
        }
        k <- k + U
    }
    return(seqResult)
}

s = Sys.time()
x <- rng.ber.v2(n = 100000, prob = 0.2)
mean(x)
print(paste("elapsed time:", round(Sys.time() - s, 4)))