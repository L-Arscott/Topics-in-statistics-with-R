# Raisin dataset: https://www.muratkoklu.com/datasets/
# CINAR I., KOKLU M. and TASDEMIR S., (2020).
# Classification of Raisin Grains Using Machine Vision and Artificial
# Intelligence Methods, Gazi Journal of Engineering Sciences,
# vol. 6, no. 3, pp. 200-209, December, 2020,
# DOI: https://doi.org/10.30855/gmbd.2020.03.03

## Prepare data
library(readxl)
library(ggplot2)
library(e1071)

our_data <- read_xlsx("SVM/Raisin_Dataset.xlsx")

# Set raisin class to -1 or 1
our_data$Class <- ifelse(our_data$Class == "Besni", -1,
                         ifelse(our_data$Class == "Kecimen", 1, NA))

# Data split
set.seed(123)  # Set the seed for reproducibility
indices <- sample(nrow(our_data))
train_rows <- round(0.75 * nrow(our_data))

train_data <- our_data[indices[1:train_rows], ]
test_data <- our_data[indices[(train_rows + 1):nrow(our_data)], ]

# Normalise data for SVM
# Use training data to obtain normalisation coefficients
scaled_train_data <- scale(train_data)
scaled_test_data <- scale(test_data,
                          center = attr(scaled_train_data, "scaled:center"),
                          scale = attr(scaled_train_data, "scaled:scale"))
scaled_train_data <- as.data.frame(scaled_train_data)
scaled_test_data <- as.data.frame(scaled_test_data)
scaled_train_data$Class <- train_data$Class
scaled_test_data$Class <- test_data$Class

# Record variable names as list of strings
variables <- names(train_data)[-ncol(train_data)]

## 2D example: use features MajorAxisLength, Perimeter
# Visualise with scatterplot
scatter_plot <- ggplot(scaled_train_data,
                       aes(x = MajorAxisLength, y = Perimeter,
                           color = factor(Class))) + 
  geom_point() + 
  labs(title = "Raisin class", x = "Major axis length", y = "Perimeter",
       color = "Legend Title")

scatter_plot

# Train the SVM model
# We can use either normalised or non-normalised data so long as we're
# consistent, since svm() automatically normalises data.
svm_model <- svm(Class ~ MajorAxisLength + Perimeter,
                 data = train_data, kernel = "linear")
test_data$Predicted_Class <- predict(svm_model, newdata = test_data)

## Plot separation
# svm() returns coefficients for normalised data, so we must either plot the
# separation line on a normalised scatter plot, or rescale the line to fit
# the original data.

# Get the coefficients of the SVM model
svm_coefficients <- coef(svm_model)

# Extract the intercept and coefficients
intercept <- -svm_coefficients[1] / svm_coefficients[3]
slope <- -svm_coefficients[2] / svm_coefficients[3]

# Plot the decision boundary line
scatter_plot + geom_abline(intercept = intercept, slope = slope, color = "green")

