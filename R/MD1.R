svc_ms <- 1               #????????????IO??????????????????ms
util_min <- 0
util_max <- 100
ms_min <- 0
ms_max <- 10
# Plot mean response time vs utilization(M/D/1)
plot(x <- c(util_min:util_max), svc_ms * (2 - x/100) / (2 * (1 - x/100)),
    type="l", lty=1, lwd=1,
    xlim=c(util_min, util_max), ylim=c(ms_min, ms_max),
    xlab="Utilization %", ylab="Mean Response Time (ms)")