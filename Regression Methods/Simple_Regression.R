# Simple linear regression

# Create some sample data and plug into dataframe
test_x <- c(1, 2, 4, 6, 7)
test_y <- c(2.8, 6.1, 11.9, 19, 21.8)
df = data.frame(test_x, test_y)
n = nrow(df)

# Estimate population variance in x
estimate_population_var <- function(x) sum((x-mean(x))^2) / (length(x) - 1)
var_x = estimate_population_var(df[,1])
sprintf("Estimated population variance for x is %.3f", var_x)

# Estimate population covariance in x, y
estimate_population_cov <- function (x, y) {
  sum((x - mean(x)) * (y - mean(y))) / (length(x) - 1)
}
cov_xy = estimate_population_cov(test_x, test_y)
sprintf("Estimated covariance in x and y is %.3f", cov_xy)

# Calculate slope and intercept
beta <- cov_xy/var_x
alpha <- mean(df[,2]) - beta * mean(df[,1])
sprintf("Slope is %.3f and intercept is %.3f.", beta, alpha)

# Plot our values and line of best fit:
plot(test_x, test_y, xlim = c(0, 8), ylim = c(0, 22), main = "Line of best fit")
abline(alpha, beta)

# We could have used inbuilt functions:
cov(test_x, test_y) / var(test_x)
# Note in R the standard functions cov(), var() estimate population 
# variance/covariance from samples.

# Even simpler yet, we may use R's inbuilt simple LR function lm:
model <- lm(test_y ~ test_x)
summary(model)

# Extract the intercept and slope
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Note we get the same intercept and slope
cat("Intercept:", intercept)
cat("Slope:", slope)


###
# Standard errors in alpha, beta:
###
# The model summary has given us the standard errors in estimating our
# parameters. Let us try to reproduce these values from scratch.

# Beta
# Record predictions and errors squared:
df$predictions <- alpha + beta*df$test_x
df$errors_squared <- (df$test_y - df$predictions)^2
error_var = sum(df$errors_squared) / (n - 2)
sxx = sum((test_x - mean(test_x))^2)

beta_var <- error_var/sxx
beta_se <- sqrt(beta_var)
print(beta_se)

# Alpha
alpha_var = error_var/n + mean(test_x)^2 * beta_var
alpha_se = sqrt(alpha_var)
print(alpha_se)


###
# The t-statistic
###
# Calculate t-values
alpha_t = alpha/alpha_se
beta_t = beta/beta_se

print(alpha_t)
print(beta_t)

# Calculate the probabilities of observing such extreme t-values
# We use the inbuilt function pt (cumulative function for the t-distribution)
alpha_p_t = pt(alpha_t, 3)*2
beta_p_t = pt(-beta_t, 3)*2

print(alpha_p_t)
print(beta_p_t)

