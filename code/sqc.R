library(qcc)

u0 <- 0.04
u1 <- 0.06
n <- 100

# Create a range of shifts to detect
shifts <- seq(0.01, 0.25, by=0.01)
beta <- sapply(shifts, function(u_shift) {
  # Simulate a large number of samples with the shifted defect rate
  samples <- rpois(100000, lambda = n * u_shift)
  # Calculate the control limits based on u0
  UCL <- 7.5
  LCL <- 0.5  
  # Calculate the probability of a sample falling outside the control limits
  mean(samples < UCL & samples > LCL)
})

plot(shifts, beta, type="l", xlab="u", ylab= expression(beta), main="OC Curve for u Chart")
