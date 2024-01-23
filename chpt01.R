#----
## exercise 1.1 (a)
arrival <- c(12, 31, 63, 95, 99, 154, 198, 221, 304, 346, 411, 455, 537)
service <- c(40, 32, 55, 48, 18, 50, 47, 18, 28, 54, 40, 72, 12)
departure <- c(arrival[1] + service[1])
for (i in c(2:13)){
    if (arrival[i] < departure[i-1]){
        departure[i] <- departure[i-1] + service[i]
        print(paste("cond1:",  # 因為下一位顧客抵達的時間 上一位顧客尚未離開
            "before departure", departure[i-1], "arrival:", arrival[i], 
            "service", service[i], "after departure", departure[i])
            )
    }else{
        departure[i] <- arrival[i] + service[i]
        # 因為下一位顧客抵達的時間 上一位顧客已經離開 所以會多出一段抵達的時間
        print(paste("cond2:", 
            "before departure", departure[i-1], "arrival:", arrival[i], 
            "service", service[i], "after departure", departure[i])
            )
    }
}; departure
result.a <- data.frame(customer = c(1:13), 
                       arrival = arrival,
                       service = service,
                       departure = departure)

## exercise 1.1 (b)
departure.2 <- c(arrival[1] + service[1], arrival[2] + service[2])
## 同 (a) 的做法，只是有兩臺機器可以同時服務顧客
for (i in c(3:13)){
    cond <- min(departure.2[i-1], departure.2[i-2])
    if (arrival[i] < cond){
        departure.2[i] <- cond + service[i]
        print(paste("cond1:",
                    "before departure", cond, "arrival:", arrival[i], 
                    "service", service[i], "after departure", departure.2[i])
        )
    }else{
        departure.2[i] <- arrival[i] + service[i]
        print(paste("cond2:",
                    "before departure", cond, "arrival:", arrival[i], 
                    "service", service[i], "after departure", departure.2[i])
        )
    }
}; departure.2
result.b <- data.frame(customer = c(1:13),
                       arrival = arrival,
                       service = service,
                       departure = departure.2)

## exercise 1.1 (c) LIFO problem
df <- data.frame(customer = c(1:13),
                 arrival = arrival,
                 service = service)
result <- c()
flag.skip <- FALSE
departure.3 <- arrival[1] + service[1]
result.c <- data.frame(customer = df$customer[1],
                       arrival = df$arrival[1],
                       service = df$service[1],
                       departure = departure.3)
df <- df[-c(1), ]
result <- append(result, departure.3)


i <- 1

while(nrow(df) != 0){
    ## 下下一位顧客還沒到
    if ((df$arrival[i] < departure.3) & (df$arrival[i+1] > departure.3)){
        departure.3 <- departure.3 + df$service[i]
        result.c <- rbind(result.c, c(
            df$customer[i], df$arrival[i], df$service[i], departure.3
            ))
        df <- df[-c(i), ]
        result <- append(result, departure.3)
    }
    ## 下下一位顧客已經到了，要先處理下下一位顧客
    else if ((df$arrival[i] < departure.3) & (df$arrival[i+1] < departure.3)){
        i <- 2
        departure.3 <- departure.3 + df$service[i]
        result.c <- rbind(result.c, c(
            df$customer[i], df$arrival[i], df$service[i], departure.3
        ))
        df <- df[-c(i), ]
        result <- append(result, departure.3)
        flag.skip <- TRUE
    }
    ## 沒有其他顧客，可以處理之前尚未處理的顧客
    else if ((df$arrival[i] > departure.3) & (flag.skip == TRUE)){
        i <- 1
        departure.3 <- departure.3 + df$service[i]
        result.c <- rbind(result.c, c(
            df$customer[i], df$arrival[i], df$service[i], departure.3
        ))
        df <- df[-c(i), ]
        result <- append(result, departure.3)
        flag.skip <- FALSE
    }
    ## 沒有其他顧客，也沒有顧客排隊，等人上門
    else if((df$arrival[i] > departure.3) & (flag.skip == FALSE)){
        departure.3 <- df$arrival[i] + df$service[i]
        result.c <- rbind(result.c, c(
            df$customer[i], df$arrival[i], df$service[i], departure.3
        ))
        df <- df[-c(i), ]
        result <- append(result, departure.3)
    }
}; result

#----
## exercise 1.2 (d)
result.a$left.formula <- result.a$departure - result.a$service
comparison <- data.frame(arrival = result.a$arrival,
                         dep.shift = c(0, result.a$departure[-c(13)]))
result.a$right.formula <- apply(comparison, 1, max)
nrow(result.a[which(result.a$left.formula == result.a$right.formula) == FALSE, ]) == 0

result.b$left.formula <- result.b$departure - result.b$service
comparison.1 <- data.frame(dep.shift1 = c(0, result.b$departure[-c(13)]),
                           dep.shift2 = c(0, 0, result.b$departure[-c(12, 13)]))
comparison.2 <- data.frame(arrival = result.b$arrival,
                           dep.shift = apply(comparison.1, 1, min))
result.b$right.formula <- apply(comparison.2, 1, max)
nrow(result.b[which(result.b$left.formula == result.b$right.formula) == FALSE, ]) == 0
