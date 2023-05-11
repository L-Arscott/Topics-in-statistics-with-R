# Polynomial regression

# Create data
test_y = c(137.2, 45.9, 204.7, 314.2, 271.0, 272.1, 37.1, 72.6)
test_x1 = c(9, 2, 5, 12, 8, 14, 1, 3)
test_x2 = c(2, 3, 9, 5, 8, 2, 3, 4)
df = data.frame(test_y, test_x1, test_x2)

# Add polynomial columns
df$test_x1_sqr = test_x1^2
df$test_x2_sqr = test_x2^2
df$test_x1x2 = test_x1 * test_x2

# This could also be done using R commands:
poly_features <- poly(cbind(test_x1, test_x2), degree = 2, raw=TRUE)

# Fit model
model <- lm(test_y ~ ., data = df)
summary(model)

# This suggests involving x1^2, x1*x2 and x2
new_model <- lm(test_y ~ test_x1_sqr + test_x1x2 + test_x2, data = df)
summary(new_model)

# Test model
df$predicted_y <- predict(new_model, newdata = df)
