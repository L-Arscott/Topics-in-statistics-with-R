# Multiple linear regression

# Create data
test_y = c(20, 4.1, 3.8, 20.7, 10.9)
test_x1 = c(9, 2, 5, 12, 8)
test_x2 = c(2, 3, 9, 5, 8)
df = data.frame(test_y, test_x1, test_x2)

# Population variance function
popvar <- function (x) mean((x-mean(x))^2)

# Normalisation function
norm <- function (x) (x - mean(x))/sqrt(popvar(x))

# Normalise data
norm_df = as.data.frame(apply(df, 2, norm))

# Check normalisation
mean(norm_df$test_y)
popvar(norm_df$test_y)

# Note R has the built-in function "scale", though this normalises w.r.t.
# sample variance

# Obtain normalised coefficients
X_mat = as.matrix(norm_df[, c(2, 3)])
beta_norm = solve(t(X_mat) %*% X_mat) %*% t(X_mat)  %*% norm_df$test_y

# Rescale
rescale = c(sqrt(popvar(test_y)/popvar(test_x1)),
            sqrt(popvar(test_y)/popvar(test_x2)))
beta = rescale * beta_norm
alpha = mean(test_y) - sum(beta * c(mean(test_x1), mean(test_x2)))

# See predictions
df$pred = alpha + beta[1]*df$test_x1 + beta[2]*df$test_x2

# Note the lm() function in R also supports multiple linear regression:
model <- lm(test_y ~ test_x1 + test_x2, data = df)
summary(model)  # Note we get the same parameters

# With this we directly get predictions as follows
predict(model, df)
