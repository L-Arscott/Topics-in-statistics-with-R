## Understanding statistics given by R's model summary
# Create data
test_y = c(20, 4.1, 3.8, 20.7, 10.9)
test_x1 = c(9, 2, 5, 12, 8)
test_x2 = c(2, 3, 9, 5, 8)
df = data.frame(test_y, test_x1, test_x2)

# Create and summarise model
model <- lm(df$test_y ~ df$test_x1 + test_x2)
summary(model)

## Reproducing residual standard error
# This measures the spread in residuals: by how much do residuals deviate from
# the fitted line?
# n parameters can always perfectly fit n observations.
# Hence we take the mean by dof = n_obs - n_parameters: the number of free
# observations.
n_obs <- nrow(df)
n_parameters <- 3
dof <- n_obs - n_parameters  # 3 fitted parameters
df$estimates <- fitted(model)

sum_of_squared_residuals <- sum((df$estimates - df$test_y)^2)
residual_var <- sum_of_squared_residuals / dof
residual_se <- sqrt(residual_var)
print(residual_se)

## Reproducing multiple R squared
# Measures the amount of variation in the response variable that can be
# explained by the predictor variables.
# We take the sum of squared residuals around the mean, and compare to the sum
# of squared residuals around the fitted model. This varies from 1 (no
# difference) and can tend to 0 (residuals far smaller for fitted model)
sum_of_squares <- sum((df$estimates - mean(df$test_y))^2)
multiple_r_squared <- 1 - sum_of_squared_residuals / sum_of_squares
print(multiple_r_squared)

## Reproducing adjusted R-squared
# Adds a penalty on multiple R squared for number of parameters used
# Provides a balance between model goodness-of-fit and simplicity
adjusted_r_squared <- 1 - ((1-multiple_r_squared)*(n-1) / (n - dof - 1))
print(adjusted_r_squared)

## F-test
# The F-statistic tests H_0: all parameters are 0 against
# H_1: at least one parameter is not 0
corrected_dof <- n_parameters - 1 
mean_sum_squares_reg <- sum_of_squares / corrected_dof

mean_sum_squares_reg / residual_var
