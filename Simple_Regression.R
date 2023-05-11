# Simple linear regression

# Create some sample data and plug into dataframe
test_x <- c(1, 2, 4, 6, 7)
test_y <- c(2.8, 6.1, 11.9, 19, 21.8)
df = data.frame(test_x, test_y)
n = nrow(df)

# Compute population variance in x manually
popvar <- function (x) mean((x-mean(x))^2)
var_x = popvar(df[,1])
cat(var_x)

# Compute population covariance in x, y
popcov <- function (x, y) mean(x*y) - mean(x) * mean(y)
cov_xy = popcov(test_x, test_y)

# Calculate slope and intercept
beta <- cov_xy/var_x
alpha <- mean(df[,2]) - beta * mean(df[,1])

# Plot our values and line of best fit:
plot(test_x, test_y, xlim = c(0, 8), ylim = c(0, 22), main = "Line of best fit")
abline(alpha, beta)

# We could have used inbuilt functions:
cov(test_x, test_y) / var(test_x)
# Note R returns sample variance/covariance while we used population var/cov
# It does not matter in our case since factors get cancelled out

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
