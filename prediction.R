# Function
Predict <- function(p.ki, p.m = 0, n = 1) {
  if ((p.ki == 0) && (p.m == 0)) {
    return(c(100, 0, 0, 0))
  }
  if ((p.ki + p.m) > 1) {
    return(c(NA, NA, NA, NA))
  }
  p <- (p.ki - p.ki * (1 - p.ki - p.m)^n) / (p.ki + p.m)
  None <- (1 - p)^2
  Green <- p * (1 - p) + p^2 / 4
  Red <- Green
  Yellow <- p^2 / 2
  return(c(None, Green, Red, Yellow) * 100)
}

# Initialize
d <- 100
P <- seq(0, 1, length = d)

# Calculate
Res <- matrix(0, ncol = 4, nrow = d)
for (i in 1:d) {
  Res[i, ] <- Predict(p.ki = P[i], p.m = 0, n = 1)
}

# Plot
plot(P, Res[, 1],
  type = "l", xlim = c(0, 1), ylim = c(0, 100), col = "black", lwd = 4,
  xlab = expression(italic(p)), ylab = "(%)", cex.main = 1
)
abline(h = seq(0, 100, 10), lwd = 0.5)
lines(P, Res[, 2] + Res[, 4], type = "l", col = "green", lwd = 4)
lines(P, Res[, 3], type = "l", col = "red", lwd = 4)
lines(P, Res[, 4], type = "l", col = "orange", lwd = 4)
legend("topright", c("None", "Green or Red (single)", "Green or Red (total)", "Yellow"),
  lwd = 3,
  col = c("black", "red", "green", "orange"), cex = 0.75
)
box(lwd = 2)
