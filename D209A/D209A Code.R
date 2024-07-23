#############Libraries Used######################
library(readxl)
library(visdat)
library(dplyr)
library(caret)
library(class)
library(ggplot2)
library(pROC)




CC <- read_excel("School/D209A/churn_clean.xlsx")

#################Data Cleaning#####################
#Checking for repeated data
CC <- distinct(CC)
#Checking for missing values throughout dataset
vis_miss(CC)

# Select columns of interest
SC <- select(CC, Churn, MonthlyCharge,Tenure)

#Inspecting Data
str(SC)
summary(SC)

# Convert "yes" to 1 and "no" to 0
ifelse(SC$Churn == "Yes", 1, 0)
SC$Churn <- as.factor(SC$Churn)

# Standardization (Z-score) - Standardizing only the continuous variables
SC$z_MonthlyCharge <- (SC$MonthlyCharge - mean(SC$MonthlyCharge)) / sd(SC$MonthlyCharge)
SC$z_Tenure <- (SC$Tenure - mean(SC$Tenure)) / sd(SC$Tenure)

#####################Download data into csv file for my computer############################


# Specify the file path and name for the CSV file
csv_file_path <- "C:/Users/nshai/OneDrive/Pictures/Documents/School/D209A/CleanedData.csv"

# Export the dataset to a CSV file
write.csv(SC, file = csv_file_path, row.names = FALSE)

# Print a message indicating the successful export
cat("Dataset exported to:", csv_file_path, "\n")


###################Analysis##########################
###############Splitting data#########################

train_index <- createDataPartition(SC$Churn, p = 0.8, list = FALSE)
train_data <- SC[train_index, ]
test_data <- SC[-train_index, ]

# Specify the file path and name for the CSV file- Training Data
csv_file_path <- "C:/Users/nshai/OneDrive/Pictures/Documents/School/D209A/TrainingData.csv"
# Export the dataset to a CSV file
write.csv(train_data, file = csv_file_path, row.names = FALSE)
# Print a message indicating the successful export
cat("Dataset exported to:", csv_file_path, "\n")

# Specify the file path and name for the CSV file - test Data
csv_file_path <- "C:/Users/nshai/OneDrive/Pictures/Documents/School/D209A/TestData.csv"
# Export the dataset to a CSV file
write.csv(test_data, file = csv_file_path, row.names = FALSE)
# Print a message indicating the successful export
cat("Dataset exported to:", csv_file_path, "\n")

# Check dimensions of training and test sets
dim(train_data)
dim(test_data)

# View the structure of training and test sets
str(train_data)
str(test_data)

###################KNN#####################################

# Train the KNN model
k <- 1  # Number of neighbors
knn_model <- knn(train = train_data[, -1], test = test_data[, -1], cl = train_data$Churn, k = k)
knn_model

# Evaluate the model
confusion_matrix <- table(Actual = test_data$Churn, Predicted = knn_model)
confusion_matrix

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

# Example of tuning the number of neighbors using cross-validation
k_values <- c(1:50)  # List of k values to try
cv_results <- sapply(k_values, function(k) {
  cv_model <- train(Churn ~ ., data = train_data, method = "knn", trControl = trainControl(method = "cv"), tuneGrid = data.frame(k = k))
  return(max(cv_model$results$Accuracy))
})
cv_results


final.knn_model <- knn(train = train_data[, -1], test = test_data[, -1], cl = train_data$Churn, k =39)

# Evaluate the model
final.confusion_matrix <- table(Actual = test_data$Churn, Predicted = final.knn_model)
final.confusion_matrix

accuracy <- sum(diag(final.confusion_matrix)) / sum(final.confusion_matrix)
accuracy

##################AUC Calculations############################

# Train the KNN model with probabilities
knn_model <- knn(train = train_data[, -1], test = test_data[, -1], cl = train_data$Churn, k = 39, prob = TRUE)

# Extract the predicted probabilities
predicted_probabilities <- attr(knn_model, "prob")

# Check the structure of predicted probabilities
str(predicted_probabilities)



# Compute ROC curve
roc_curve <- roc(test_data$Churn, predicted_probabilities)
# Calculate AUC
auc_value <- auc(roc_curve)
print(auc_value)

