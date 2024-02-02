#-----
## example 4b: generating a random permutation
## method 1
random.permutation <- function(size, k){
    seqResult <- c(1:size)                 # construct the order list
    k.copy <- k                            # copy the selected index of element
    while (k.copy > 1){                    # the last element does not need to be arranged
        I <- ceiling(size * runif(1))      # sampling
        seqResult[c(k.copy, I)] <- seqResult[c(I, k.copy)]
        k.copy <- k.copy - 1
    }
    return(seqResult[c(1:k)])
}

random.permutation(size = 10, k = 10)  # n takes n
random.permutation(size = 10, k = 5)   # n takes r, n > r

## method 2
randperm <- function(n, size=n){
    x <- seq(1, n)
    for (k in n:(n - size + 2)) {
        i <- ceiling(k * runif(1))
        if (i != k) {
            x[c(k, i)] <- x[c(i, k)]
        }
    }
    return(x[(n - size + 1):n])
}

randperm(n = 10, size = 7)

sample.int(10)  # base R
sample.int(10, size = 7)  # base R