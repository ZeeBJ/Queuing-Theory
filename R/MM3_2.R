library(queuecomputer)
P_0_func <- function(rho, k){
  sum_i <- rep(NA, k)
  
  for(i in 0:I(k-1))
  {
    sum_i[i+1] <- rho^i / factorial(i)
  }
  
  p_0 <- 1/(sum(sum_i) + rho^k/(factorial(k - 1) * (k - rho)))
  return(p_0)
}

P_n <- function(rho,n,k){
  
  p_0 <- P_0_func(rho, k)
  if(n <= k){
    output <- rho^n / factorial(n) * p_0
  } else {
    output <- rho^n / (factorial(k) * k^(n-k)) * p_0
  }
  return(output)
}

Lq <- function(rho, k){
  p_0 <- P_0_func(rho, k)
  
  output <- p_0 * rho^{k+1} / ( factorial(k-1) * (k - rho)^2)
  return(output)
}


set.seed(2)

n_customers <- 1e6

lambda_a <- 1/1  #到达率
lambda_s <- 1/2.5 #服务率

#lambda_a <- rexp(n, 0.8)
#lambda_s <- rexp(n, 0.5)

interarrivals <- rexp(n_customers, lambda_a) #用户或请求呈指数分布
arrivals <- cumsum(interarrivals)         #用户或请求总数
service <- rexp(n_customers, lambda_s) #用户或请求得到的服务呈指数分布
rho <- (1/lambda_s) / (1/lambda_a)   #服务强度

# MM3 queue ------------------------------
k = 3
## Theoretical -------------------
p_0 <- P_n(rho, 0, k) #空闲率
### System lengths -----------------------
Vectorize(P_n, "n")(rho=rho, n=c(0:30), k = k) #向量化的系统队列长度
### Estimated queue length -----------------
LQ <- Lq(rho, k) #队列长度
LQ
### Estimated units in system -----------
LQ + rho #系统中的请求个数
### Waiting times -----------
Ws = 1/lambda_s # 服务时间
Wq = LQ / lambda_a #等待时间
W = Ws + Wq #请求响应时间=服务时间+等待时间
Wq # Mean waiting time (time in queue)
W # Mean response time (time in system)
MM3_2 <- queue_step(arrivals = arrivals, service = service, servers = k) #从到达时间和服务时间来计算响应时间和队列长度
MM3_2
MM3_2_summary <- summary(MM3_2)
MM3_2_summary
MM3_2_summary$slength_sum
# Mean queue length
MM3_2_summary$qlength_mean
# Mean system length (number of customers in system)
MM3_2_summary$slength_mean
MM3_2_summary$mwt # Mean waiting time
MM3_2_summary$mrt # Mean response time
