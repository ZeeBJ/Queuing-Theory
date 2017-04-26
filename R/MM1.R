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

set.seed(1)

n_customers <- 1e6

lambda_a <- 1/1
lambda_s <- 1/0.8

interarrivals <- rexp(n_customers, lambda_a)

arrivals <- cumsum(interarrivals)

service <- rexp(n_customers, lambda_s)

rho <- (1/lambda_s) / (1/lambda_a)
k = 1

p_0 <- P_n(rho, n=0, k)

### System lengths -----------------------
Vectorize(P_n, "n")(rho=rho, n=c(0:30), k = k)
LQ <- Lq(rho, k)
LQ
### Estimated units in system -----------
LQ + rho
Ws = 1/lambda_s
Wq = LQ / lambda_a
W = Ws + Wq
Wq # Mean waiting time (time in queue)
W # Mean response time (time in system)
MM1 <- queue_step(arrivals = arrivals, service = service, servers = k)
MM1_summary <- summary(MM1)
MM1_summary$slength_sum
# Mean queue length
MM1_summary$qlength_mean
# Mean system length (number of customers in system)
MM1_summary$slength_mean
MM1_summary$mwt # Mean waiting time
MM1_summary$mrt # Mean response time
