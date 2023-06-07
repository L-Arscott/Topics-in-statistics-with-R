# DATASET: https://www.muratkoklu.com/datasets/ 
# Citation : KOKLU, M., SARIGIL, S., & OZBEK, O. (2021).
# The use of machine learning methods in classification of pumpkin seeds
# (Cucurbita pepo L.). Genetic Resources and Crop Evolution, 68(7), 2713-2726.
# Doi: https://doi.org/10.1007/s10722-021-01226-0 
# https://link.springer.com/article/10.1007/s10722-021-01226-0
# https://link.springer.com/content/pdf/10.1007/s10722-021-01226-0.pdf


## Logistic regression: Pumpkin Seeds
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
  FROM pumpkin_seeds_dataset"

result <- dbGetQuery(conn, query)

# Set pumpkin class to 0 or 1
result$Class <- ifelse(result$Class == "Çerçevelik", 0,
                       ifelse(result$Class == "Ürgüp Sivrisi", 1, NA))

# Data split
set.seed(123)  # Set the seed for reproducibility
indices <- sample(nrow(result))
train_rows <- round(0.75 * nrow(result))

train_data <- result[indices[1:train_rows], ]
test_data <- result[indices[(train_rows + 1):nrow(result)], ]

# Record variable names as list of strings
variables <- names(train_data)[-ncol(train_data)]

# Accuracy function: returns accuracy of a model
acc <- function(our_model){
  test_data$predictions <- predict(our_model, test_data, type='response')
  test_data$predictions <- ifelse(test_data$predictions > 0.5,1,0)
  misClasificError <- mean(test_data$predictions != test_data$Class)
  print(paste('Accuracy',1-misClasificError))
}


## Forward selection, backward elimination
## Compare log-likelihood ratios for univariate logistic regression models
# For each variable, print the ll-ratio (against null model)
for (variable in variables){
  two_col_train_data = train_data[, c("Class", variable)]
  model <- glm(Class ~ .,
               family = binomial(link = 'logit'), data = two_col_train_data)
  log_like_ratio <- summary(model)$null.deviance - summary(model)$deviance
  cat("The log-likelihood ratio for variable", variable,
      "is ", log_like_ratio, "\n")
}

# The statistic is distributed according to a chi-squared distr. with 1 d.o.f.
print(1 - pchisq(1380, 1))  # More significant than R can handle

# Fit univariate logistic model and assess accuracy
model1 <- glm(Class ~ Aspect_Ration,
              family=binomial(link='logit'), data=train_data)
acc(model1)


## Compare log-likelihood ratios for bivariate logistic regression models
# Add a second variable and compare ll (note we get 0 when adding
# aspect ratio a second time).
for (variable in variables){
  three_col_train_data = train_data[, c("Class", "Aspect_Ration", variable)]
  model <- glm(Class ~ .,
               family = binomial(link = 'logit'), data = three_col_train_data)
  log_like_ratio <- summary(model1)$deviance - summary(model)$deviance
  cat("The log-likelihood ratio for variable", variable,
      "is ", log_like_ratio, "\n")
}

print(1 - pchisq(41.5, 1))  # chi-squared statistic: accept

# Fit logistic model and read summary and assess accuracy
model2 <- glm(Class ~ Aspect_Ration + Solidity,
              family = binomial(link = 'logit'), data = train_data)
acc(model2)


## Compare log-likelihood ratios for three-variable logistic regression models
for (variable in variables){
  four_col_train_data = train_data[, c("Class",
                                       "Aspect_Ration", "Solidity", variable)]
  model <- glm(Class ~ .,
               family = binomial(link = 'logit'), data = four_col_train_data)
  log_like_ratio <- summary(model2)$deviance - summary(model)$deviance
  cat("The log-likelihood ratio for variable", variable,
      "is ", log_like_ratio, "\n")
}

print(1 - pchisq(7.291813, 1))  # Accept

# Fit logistic model and assess accuracy
model3 <- glm(Class ~ Aspect_Ration + Solidity + Roundness,
              family = binomial(link = 'logit'), data = train_data)
acc(model3)


## Backwards step: assess whether we keep Aspect_Ratio
model3.1 <- glm(Class ~ Solidity + Roundness,
                family = binomial(link='logit'), data = train_data)
summary(model3.1)$deviance - summary(model3)$deviance
pchisq(78.9, 1)  # Keep


## Compare log-likelihood ratios for four-variable logistic regression models
for (variable in variables){
  five_col_train_data = train_data[, c("Class",
                                       "Aspect_Ration", "Solidity", "Roundness",
                                       variable)]
  model <- glm(Class ~ .,
               family = binomial(link = 'logit'), data = five_col_train_data)
  log_like_ratio <- summary(model3)$deviance - summary(model)$deviance
  cat("The log-likelihood ratio for variable", variable,
      "is ", log_like_ratio, "\n")
}

# Fit logistic model and assess accuracy
model4 <- glm(Class ~ Aspect_Ration + Solidity + Roundness + Eccentricity,
              family = binomial(link = 'logit'), data = train_data)
acc(model4)


## Backwards step: assess whether we keep Aspect_Ratio, Roundness
model4.1 <- glm(Class ~ Aspect_Ration + Roundness + Eccentricity,
                family = binomial(link = 'logit'), data = train_data)
model4.2 <- glm(Class ~ Solidity + Roundness + Eccentricity,
                family = binomial(link = 'logit'), data = train_data)

summary(model4.1)$deviance - summary(model4)$deviance # Keep
summary(model4.2)$deviance - summary(model4)$deviance # Keep

# We will stop here for now, and use model 2, which uses features
# 'Aspect_Ration' and 'Solidity'. Model 2 strikes a good balance between
# accuracy and number of features.
# Model 4 ('Aspect_Ration' + 'Solidity' + 'Roundness' + 'Eccentricity') scores
# only very slightly higher on the accuracy scale. Model 1 could also be
# considered for its greater simplicity at low cost.
summary(model2)
