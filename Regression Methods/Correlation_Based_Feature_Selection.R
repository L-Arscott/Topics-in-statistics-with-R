## Madelon dataset
library(RMySQL)

# Obtain data
my_password <- readLines("password.txt", n = 1)

conn <- dbConnect(
  MySQL(),
  dbname = "my_database",
  host = "localhost",
  user = "root",
  password = my_password
)

query <- "SELECT *
  FROM madelon"

result <- dbGetQuery(conn, query)

# Set Class variables to 0 or 1
result['Class'] <- result['Class'] - 1

# Data split
set.seed(123)  # Set the seed for reproducibility
indices <- sample(nrow(result))
train_rows <- round(0.75 * nrow(result))

train_data <- result[indices[1:train_rows], ]
test_data <- result[indices[(train_rows + 1):nrow(result)], ]

# Record variable names as list of strings
variables <- names(train_data)[-ncol(train_data)]

# Accuracy function
acc <- function(our_model){
  test_data$predictions <- predict(our_model, test_data, type='response')
  test_data$predictions <- ifelse(test_data$predictions > 0.5,1,0)
  misClasificError <- mean(test_data$predictions != test_data$Class)
  print(paste('Accuracy',1-misClasificError))
}


## Test model including all features
model1 <- glm(Class ~ .,
              family=binomial(link='logit'), data=train_data)
acc(model1)
# We get an accuracy of 57%, though we are likely paying a price for the high
# number of features


## Merit function
merit <- function(df, feature_list, class_col){
  k <- length(feature_list)
  
  correlation_coeffs <- sapply(df[feature_list],
                               function(x) cor(x, df[class_col]))
  mean_rfc <- mean(correlation_coeffs)
  
  cor_matrix <- abs(cor(df[feature_list]))
  mean_rff <- (sum(cor_matrix) - k) / (2)
  
  merit <- (k * mean_rfc) / sqrt(k + mean_rff)
  return(abs(merit))
}

# We could try all combinations of features and find which combination scores
# highest. This quickly becomes unfeasable with large numbers of features.
# A more basic approach would be to add features one at a time until adding
# is no longer beneficial.
beneficial_features <- function(df, feature_list, class_col, min_merit){
  to_return = c()
  for (feature in variables) {
    # Optional: Check not in list
    if (!(feature %in% feature_list)) {
      expanded_list <- c(feature_list, feature)
      feature_merit <- merit(df, expanded_list, class_col)
      if (feature_merit > min_merit) {
        feature_details <- c(c(feature_list, feature), feature_merit)
        to_return <- c(to_return, feature_details)}
    }
  }
  return(to_return)
}

print(beneficial_features(train_data, c(), 'Class', 0.2))
print(beneficial_features(train_data, c('V476'), 'Class', 0.25))
print(beneficial_features(train_data, c('V476', 'V242'), 'Class', 0.253))
print(beneficial_features(train_data, c('V476', 'V242', 'V49'), 'Class', 0.273))

# We don't get very far. However this is enough for an increase in accuracy:
model2 <- glm(Class ~ V476 + V242 + V49,
              family=binomial(link='logit'), data=train_data)
acc(model2)
# Model 2 is clearly superior, greater accuracy with only three features.

# Note we have not necessarily found the set of features with the greatest merit
# Consider:
print(merit(train_data,
            c('V476', 'V242', 'V49', 'V129', 'V330', 'V458',
              'V425', 'V334', 'V379', 'V215'), 
            'Class'))

# This yields a very slightly higher accuracy, though this may not be worth
# the additional complexity.
model3 <- glm(Class ~ V476 + V242 + V49 + V129 + V330 + V458 +
                V425 + V334 + V379 + V215,
              family=binomial(link='logit'), data=train_data)
acc(model3)

# A good compromise between scanning all possibilities and finding a good set
# is "best first search".