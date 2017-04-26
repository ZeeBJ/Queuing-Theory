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


depart <- function(x){
  if("numeric" %in% class(x)){
    departures <- x
  } else {
    departures <- x$departures_df$departures
  }
  return(departures)
}

queue <- function(arrivals, service, servers = 1, serveroutput = FALSE, adjust = 1){
  
  service = service * adjust
  check_queueinput(arrivals, service)
  
  ordstatement <- is.unsorted(arrivals)
  
  # Order arrivals and service according to time
  
  if(ordstatement){
    ord <- order(arrivals, method = "radix")
    arrivals <- arrivals[ord]
    service <- service[ord]
  }
  
  output <- queue_pass(arrivals = arrivals, service = service, servers = servers)
  
  departures <- output[1:length(arrivals)]
  queue_vector <- (output[I(length(arrivals) + 1):I(length(output) - 1)])
  
  if(ordstatement){
    new_ord <- order(ord)
    departures <- departures[new_ord]
    queue_vector <- queue_vector[new_ord]
  }
  
  if(serveroutput){
    attr(departures, "server") <- queue_vector
  }
  
  return(departures)
}

queue_step <- function(arrivals, service, servers = 1, labels = NULL, adjust = 1){
  
  arrivals <- depart(arrivals)
  
  departures <- queue(arrivals = arrivals, service = service, servers = servers, serveroutput = TRUE, adjust = 1)
  
  server <- attr(departures, "server")
  attributes(departures) <- NULL
  
  if(is.null(labels) == FALSE){
    departures_df <- dplyr::data_frame(
      labels = labels,
      arrivals = arrivals,
      service = service,
      departures = departures,
      waiting = departures - arrivals - service,
      system_time = departures - arrivals,
      server = server
    )
  } else {
    departures_df <- dplyr::data_frame(
      arrivals = arrivals,
      service = service,
      departures = departures,
      waiting = departures - arrivals - service,
      system_time = departures - arrivals,
      server = server
    )
  }
  
  # set up
  

set.seed(2)

Requests<- 1e6

#lambda_a <- 1/1  #?????????
#lambda_s <- 1/2.5 #?????????

ArrivalRate <- 1/1 #?????????
SeviceRate <- 1/2.5   #?????????

#lambda_a <- rexp(n, 0.8)
#lambda_s <- rexp(n, 0.5)

interarrivals <- rexp(Requests, ArrivalRate) #??????????????????????????????
arrivals <- cumsum(interarrivals)         #?????????????????????
service <- rexp(Requests, SeviceRate) #?????????????????????????????????????????????
rho <- (1/SeviceRate) / (1/ArrivalRate)   #????????????

# MM3 queue ------------------------------
k = 3
## Theoretical -------------------
p_0 <- P_n(rho, 0, k) #?????????
### System lengths -----------------------
Vectorize(P_n, "n")(rho=rho, n=c(0:30), k = k) #??????????????????????????????
### Estimated queue length -----------------
LQ <- Lq(rho, k) #????????????
LQ
### Estimated units in system -----------
LQ + rho #????????????????????????
### Waiting times -----------
Ws = 1/lambda_s # ????????????
Wq = LQ / lambda_a #????????????
W = Ws + Wq #??????????????????=????????????+????????????
Wq # Mean waiting time (time in queue)
W # Mean response time (time in system)
MM3_2 <- queue_step(arrivals = arrivals, service = service, servers = k) #??????????????????????????????????????????????????????????????????
MM3_2_summary <- summary(MM3_2)
MM3_2_summary$slength_sum
# Mean queue length
MM3_2_summary$qlength_mean
# Mean system length (number of customers in system)
MM3_2_summary$slength_mean
MM3_2_summary$mwt # Mean waiting time
MM3_2_summary$mrt # Mean response time



