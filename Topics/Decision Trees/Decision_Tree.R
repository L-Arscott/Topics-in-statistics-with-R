# Data set: https://archive.ics.uci.edu/ml/datasets/wine+quality
# P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis.
# Modeling wine preferences by data mining from physicochemical properties.
# In Decision Support Systems, Elsevier, 47(4):547-553, 2009.

### Decision Trees: red wine quality
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
  FROM winequality_red"
result <- dbGetQuery(conn, query)

# Set quality to 0 (bad) or 1 (good)
result$quality <- ifelse(result$quality >=7, 1,
                         0)

## Data split
set.seed(123)  # Set the seed for reproducibility
indices <- sample(nrow(result))
train_rows <- round(0.75 * nrow(result))

our_train_data <- result[indices[1:train_rows], ]
our_test_data <- result[indices[(train_rows + 1):nrow(result)], ]

## Accuracy functions: return accuracy, F1-score of a model
acc <- function(model, test_data){
  misClasificError <- mean(test_data$predictions != test_data$quality)
  print(paste('Accuracy',1-misClasificError))
}

f1 <- function(model, test_data){
  tp <- sum(test_data$predictions == test_data$quality &
              test_data$predictions == 1)
  fp <- sum(test_data$predictions != test_data$quality &
              test_data$predictions == 1)
  fn <- sum(test_data$predictions != test_data$quality &
              test_data$predictions == 0)
  
  return(tp / (tp + 0.5*(fp + fn)))
}

## Analysis
# Fit the decision tree model
our_model <- rpart(quality ~ ., data = our_train_data)

# Print the summary of the decision tree
print(our_model)

# Plot the decision tree
rpart.plot(our_model)

# Assess accuracy
our_test_data$predictions <- predict(our_model, newdata = our_test_data)
our_test_data$predictions <- ifelse(our_test_data$predictions > 0.5,1,0)
acc(our_model, our_test_data)
f1(our_model, our_test_data)
