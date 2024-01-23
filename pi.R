#-----
## example of estimation of pi
simulation.pi <- function(N, seed=NULL){
    if (is.null(seed) == FALSE){
        set.seed(seed = seed)
    }
    x <- runif(N, min = 0, max = 1)
    y <- runif(N, min = 0, max = 1)
    p <- mean((x^2 + y^2) <= 1)
    pi.hat <- 4*p
    pi.hat
}
simulation.pi(N = 100000, seed = 91)

N <- seq(from = 1, to = 100000, length.out = 100)

library(animation)
saveGIF({
    for (num in N) {
        radius <- 1
        theta <- seq(0, 2 * pi, length.out = 100)
        circle_x <- radius * cos(theta)
        circle_y <- radius * sin(theta)
        df <- data.frame(x = runif(num, min = -1, max = 1),
                         y = runif(num, min = -1, max = 1))
        p <- mean(((df$x)^2 + (df$y)^2) <= 1)
        pi.hat <- 4*p
        plot(circle_x, circle_y, type = "l", col = "red",  asp = 1, lwd = 15,
             xlab = "", ylab = "", main = paste("estimation of pi:", round(pi.hat, 4)))
        points(df[((df$x)^2 + (df$y)^2 <= 1), ]$x, df[(df$x)^2 + (df$y)^2 <= 1, ]$y, cex = 0.5)
    }
}, interval = 0.1, movie.name = "pi.gif")
