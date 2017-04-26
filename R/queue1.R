#In this example of a queueing network, customers must pass through two queues. 
#The arrival times to the first queue come in two waves starting at time 100 and time 500. 
#The arrival times to the second queue are the departure times of the first queue plus 
# the time they spent walking to the second queue.



library(queuecomputer)
library(ggplot2)
library(dplyr)

set.seed(1)

n <- 1000

arrivals_1 <- c(100 + cumsum(rexp(n)), 500 + cumsum(rexp(n)))
service_1 <- rexp(2*n, 1/2.5)

queue_1 <- queue_step(arrivals = arrivals_1, service = service_1, servers = 20)

walktimes <- rexp(2*n, 1/100)

arrivals_2 <- lag_step(arrivals = queue_1, service = walktimes)
service_2 <- rexp(2*n, 1/3)

queue_2 <- queue_step(arrivals = arrivals_2, service = service_2, servers = 20)

head(arrivals_1)
head(queue_1$departures_df)
head(arrivals_2)
head(queue_2$departures_df)
summary(queue_1)
summary(queue_2)
plot(arrivals_1)
#plot(arrivals_2)
#plot(queue_1)