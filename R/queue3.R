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