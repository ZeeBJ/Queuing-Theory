#It is simple to set up a chain of queueing elements with queuecomputer. 
#Suppose passengers must walk to a queue, then wait for service and then wait for their bags.

library(queuecomputer)
library(dplyr)

set.seed(1) #随机数种子

n <- 100

arrivals <- cumsum(rexp(n)) # 请求到达呈指数分布，求积累和
service_l <- rexp(n, 0.8)   #服务时间呈指数分布
service_q <- rexp(n, 0.5)
arrivals_b <- cumsum(rexp(n, 0.8))

# The queue elements can be computed one by one. 

departures_1 <- lag_step(arrivals, service_l)
departures_2 <- queue(departures_1, service = service_q, servers = 4)
departures_3 <- wait_step(departures_2, arrivals_b)

# Or the queue elements can be chained together with the %>% operator. 

departures <- lag_step(arrivals, service_l) %>% queue_step(service = service_q, servers = 4) %>% wait_step(arrivals_b)

all(departures == departures_3)

# Plot densities for this tandem queueing network

colours <- rainbow(4)
plot(density(arrivals, from = 0), 
     col = colours[1], xlim = c(0, 220), ylim = c(0, 0.015), 
     main = "Density plot")
lines(density(departures_1, from = 0), col = colours[2])
lines(density(departures_2, from = 0), col = colours[3])
lines(density(departures_3, from = 0), col = colours[4])
legend(150,0.012, legend = c("Start walk",
                             "Finish walk",
                             "Finish service", 
                             "Pick up bag"),
       col = colours, lwd = 1, cex = 0.8
)