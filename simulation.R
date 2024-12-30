KI.sim <- function(p = 0.5, p.gr = 0.5, n = 1000, seed = 1) {
  ### Parameters ###
  # p     Probability of knock-in (default = 0.5)
  # p.gr  Ratio of Green (default = 0.5)
  # n     Number of cells (default = 1000)
  # seed  seed for random number
  
  ### Result matrix RES ###
  # 0 = None
  # 1 = Green
  # 2 = Red
  #
  # EX) row (2,1,0,0) means (R,G),(N,N) = Y & N daughters
  #     row (1,0,0,2) means (G,N),(G,R) = G & R daughters
  
  set.seed(seed)
  RES <- matrix(0, ncol = 4, nrow = n)
  ch <- c(1:4)
  for (i in 1:n) {
    for (j in ch) {
      if (runif(1) <= p) {
        if (runif(1) <= p.gr) {
          RES[i, j] <- 1
        } else {
          RES[i, j] <- 2
        }
      }
    }
  }

  # Encode daughter cells
  RES.div <- c(RES[, 1] * 10 + RES[, 2], RES[, 3] * 10 + RES[, 4])

  ### COLOR RESULT ###
  COL <- c(0, 0, 0, 0)
  names(COL) <- c("None", "Green", "Red", "Yellow")

  for (i in 1:(2 * n)) {
    if (RES.div[i] == 0) COL[1] <- COL[1] + 1
    if (RES.div[i] == 1 || RES.div[i] == 10 || RES.div[i] == 11) COL[2] <- COL[2] + 1
    if (RES.div[i] == 2 || RES.div[i] == 20 || RES.div[i] == 22) COL[3] <- COL[3] + 1
    if (RES.div[i] == 12 || RES.div[i] == 21) COL[4] <- COL[4] + 1
  }
  return(COL / (2 * n) * 100)
}

# Initialize
Len <- 21
p.seq <- seq(0, 1, length = Len)
res <- matrix(0, ncol = 4, nrow = Len)

# Run simulation
for (i in 1:Len) {
  res[i, ] <- KI.sim(p = p.seq[i], n = 1000)
}

# Plot
plot(p.seq, res[, 1], type = "o", xlab = expression(italic("p")), ylab = "(%)", lwd = 2)
lines(p.seq, res[, 2], type = "o", col = "green", lwd = 2)
lines(p.seq, res[, 2] + res[, 4], type = "l", lty = 3, col = "green", lwd = 2)
lines(p.seq, res[, 3], type = "o", col = "red", lwd = 2)
lines(p.seq, res[, 3] + res[, 4], type = "l", lty = 3, col = "red", lwd = 2)
lines(p.seq, res[, 4], type = "o", col = "orange", lwd = 2)
abline(h = seq(0, 100, 10), lwd = 0.5)
legend("topright", c("None", "Green (single)", "Green (total)", "Red (single)", "Red (total)", "Yellow"),
  col = c("black", "green", "green", "red", "red", "yellow"), lty = c(1, 1, 3, 1, 3, 1), ncol=2, cex = 0.75, lwd = 2
)
box(lwd = 2)
