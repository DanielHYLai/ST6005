#-----
## example of Multiplicative Congruential Generation
MCG.alg <- function(x0, m, a, c = 0, seqResult, totalLength){
    seqResult[1] <- x0
    for (i in c(2:totalLength)){
        seqResult[i] <- (seqResult[i - 1] * a + c) %% m
    }
    return(seqResult)
}

x0 <- 7
m <- 10
a <- 7
totalLength <- 1000
seqResult <- rep(0, totalLength)
seqResult <- 
    MCG.alg(x0 = x0, m = m, a = a, 
            seqResult = seqResult, totalLength = totalLength)

mean(seqResult / m)
var(seqResult / m)