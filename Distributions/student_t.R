# Experimentally reproducing Student's t-distribution
# Constants: `trials` samples of size `n` from N(`mu`, `sigma`)
mu <- 2
sigma <- 3
n <- 7
trials <- 10000

# Obtain samples
t_vals <- numeric(trials)
for (i in 1:trials) {
  samples = rnorm(n, mu, sigma)
  s = sd(samples)
  x_bar = mean(samples)
  t_vals[i] = (x_bar - mu) / (s / sqrt(n))
}

# Histogram of simulated t-values
# Set probability=True for a probability density histogram
hist(
  t_vals,
  breaks = 50,
  probability = TRUE,
  main = "Simulated t-Distribution",
  xlab = "t values",
  col = "lightblue",
  border = "white"
)

# Overlay theoretical t-distribution curve
x <- seq(min(t_vals), max(t_vals), length.out = 500)
lines(x, dt(x, df = n - 1), col = "red", lwd = 2)

# Normal distribution for comparison (mean 0, sd = 1)
lines(
  x,
  dnorm(x, mean = 0, sd = 1),
  col = "darkgreen",
  lwd = 2,
  lty = 2
)
