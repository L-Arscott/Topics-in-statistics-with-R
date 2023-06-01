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

# Set pumpkin class to 0 or 1
result <- dbGetQuery(conn, query)
result$Class <- ifelse(result$Class == "Çerçevelik", 0,
                       ifelse(result$Class == "Ürgüp Sivrisi", 1, NA))

# Data split
set.seed(123)  # Set the seed for reproducibility
indices <- sample(nrow(result))
train_rows <- round(0.75 * nrow(result))

train_data <- result[indices[1:train_rows], ]
test_data <- result[indices[(train_rows + 1):nrow(result)], ]

# Fit logistic model and read summary
model <- glm(Class ~ Eccentricity,family=binomial(link='logit'),data=train_data)
summary(model)


## Deviance: reproduce model values
# Deviance of fitted model
train_data$fit <- predict(model, train_data, type='response')
train_data$fit_likelihood <- ifelse(train_data$Class == 1,
                                train_data$fit, 1-train_data$fit)

print(-2 *log(prod(train_data$fit_likelihood)))

# Deviance of saturated model
n1 = sum(train_data$Class)
n0 = train_rows - n1
null_log_likelihood <- n1 * log(n1 / train_rows) + n0 * log(n0 / train_rows)
print(-2 * null_log_likelihood)


## Assess accuracy on test data
test_data$predictions <- predict(model, test_data, type='response')
test_data$predictions <- ifelse(test_data$predictions > 0.5,1,0)

misClasificError <- mean(test_data$predictions != test_data$Class)
print(paste('Accuracy',1-misClasificError))
