# DATASET: https://www.muratkoklu.com/datasets/ 
# Citation : KOKLU, M., SARIGIL, S., & OZBEK, O. (2021).
# The use of machine learning methods in classification of pumpkin seeds
# (Cucurbita pepo L.). Genetic Resources and Crop Evolution, 68(7), 2713-2726.
# Doi: https://doi.org/10.1007/s10722-021-01226-0 
# https://link.springer.com/article/10.1007/s10722-021-01226-0
# https://link.springer.com/content/pdf/10.1007/s10722-021-01226-0.pdf

### Random Forest: Pumpkin Seed
library(RMySQL)
library(rpart)
library(rpart.plot)

## Obtain data
my_password <- readLines("password.txt", n = 1)

conn <- dbConnect(
  MySQL(),
  dbname = "my_database",
  host = "localhost",
  user = "root",
  password = my_password
)

query <- "SELECT *
  FROM pumpkin_seeds_dataset"

# Set pumpkin class to 0 or 1
result <- dbGetQuery(conn, query)
result$Class <- ifelse(result$Class == "Çerçevelik", 0,
                       ifelse(result$Class == "Ürgüp Sivrisi", 1, NA))

## Data split
set.seed(123)  # Set the seed for reproducibility
indices <- sample(nrow(result))
train_rows <- round(0.7 * nrow(result))
cv_rows <- round(0.15 * nrow(result))

our_train_data <- result[indices[1:train_rows], ]
our_cv_data <- result[indices[(train_rows + 1):(train_rows + cv_rows)], ]
our_test_data <- result[indices[(train_rows + cv_rows + 1):nrow(result)], ]

# Record variable names as list of strings
variables <- names(our_train_data)[-ncol(our_train_data)]

## Accuracy functions: return accuracy, F1-score of a model
acc <- function(test_data){
  misClasificError <- mean(test_data$predictions != test_data$Class)
  print(paste('Accuracy',1-misClasificError))
}

f1 <- function(test_data){
  tp <- sum(test_data$predictions == test_data$Class &
              test_data$predictions == 1)
  fp <- sum(test_data$predictions != test_data$Class &
              test_data$predictions == 1)
  fn <- sum(test_data$predictions != test_data$Class &
              test_data$predictions == 0)
  score <- tp / (tp + 0.5*(fp + fn))
  print(score)
  
  return(score)
}


### Analysis
## Part 1: decision tree
# Fit the decision tree model
our_model <- rpart(Class ~ ., data = rbind(our_train_data, our_cv_data))

# Print the summary of the decision tree
print(our_model)

# Plot the decision tree
rpart.plot(our_model)

# Assess accuracy
our_test_data$predictions <- predict(our_model, newdata = our_test_data)
our_test_data$predictions <- ifelse(our_test_data$predictions > 0.5,1,0)
acc(our_test_data)
f1(our_test_data)


## Part 2: Random Forest
## Bootstrapping and random_feature_selection
n_samples <- 10
n_features <- 3
bootstrap_samples <- lapply(1:n_samples, function(x) {
  sample_rows <- sample(1:train_rows, replace = TRUE)  # Randomly sample row indices
  our_train_data[sample_rows, ]  # Subset the dataset with the sampled row indices
})
feature_samples <- lapply(1:n_samples,
                          function(x) sample(variables, size = n_features))

trees <- lapply(1:n_samples,
                function(x) rpart(Class ~ .,
                                  data = bootstrap_samples[[x]][,c(unlist(feature_samples[x]), 'Class')]))

predict(trees[[1]], our_cv_data)
predictions_list <- lapply(1:n_samples, function(x) round(predict(trees[[x]], our_cv_data)))

# Add the new columns to the dataframe
for (i in seq_along(predictions_list)) {
  our_cv_data[paste0("Prediction_", i)] <- predictions_list[[i]]
}

our_cv_data['predictions'] <- round(rowMeans(our_cv_data[, paste0("Prediction_", seq(n_samples))]))                       
f1(our_cv_data)
acc(our_cv_data)
