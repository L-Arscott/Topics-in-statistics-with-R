set.seed(42)

# Generate a small sample from a known distribution (normal)
sample_size = 20
data_normal <- rnorm(n = sample_size, mean = 5, sd = 2)

# Compute mean and 95% CI using traditional method: the t-distribution
# For a normal distribution, the sampling distribution of the sample mean, when 
# variance is estimated, is described using the t-distribution.
mean_normal <- mean(data_normal)
sd_normal <- sd(data_normal)
se_mean_normal <- sd(data_normal) / sqrt(length(data_normal))

margin_error <- qt(0.975, df = sample_size - 1) * se_mean_normal  #  two-tailed
ci_normal <- c(mean_normal - margin_error, mean_normal + margin_error)

# Bootstrap the mean:
# Generate samples from our sample data_normal with replacement
# Take the 2.5% and 97.5% percentiles of the resultant sample means
bootstrap_means <- replicate(1000, {
  sample_data <- sample(data_normal, size = sample_size, replace = TRUE)
  mean(sample_data)
})

ci_bootstrap <- quantile(bootstrap_means, probs = c(0.025, 0.975))

# Show results
cat("Normal Approximation CI:", round(ci_normal, 3), "\n")
cat("Bootstrap CI:", round(ci_bootstrap, 3), "\n")

# Plot the bootstrap distribution
hist(bootstrap_means, breaks = 30, main = "Bootstrap Distribution of the Mean",
     xlab = "Bootstrapped Mean", col = "skyblue", border = "white")
abline(v = ci_bootstrap, col = "red", lwd = 2, lty = 2)
abline(v = mean_normal, col = "darkgreen", lwd = 2)
legend("topright", legend = c("Bootstrap CI", "Sample Mean"),
       col = c("red", "darkgreen"), lty = c(2, 1), lwd = 2)
