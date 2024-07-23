#############Libraries Used######################
library(readxl)
library(visdat)
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)


CC <- read_excel("School/D209B/churn_clean.xlsx")

#################Data Cleaning#####################
#Checking for repeated data
CC <- distinct(CC)
#Checking for missing values throughout dataset
vis_miss(CC)

# Remove unnescessary columns 
SC <- CC[, !(names(CC) %in% c("CaseOrder", "Customer_id", "Interaction", "UID", "City", "State", "County", "Zip", "Lat", "Lng", "Population", "TimeZone", "Income", "Job"))]

#renaming Columns for a proper naming
names(SC)[names(SC) == "Item1"] <- "TimelyResponse"
names(SC)[names(SC) == "Item2"] <- "TimelyFixes"
names(SC)[names(SC) == "Item3"] <- "TimelyReplacements"
names(SC)[names(SC) == "Item4"] <- "Reliability"
names(SC)[names(SC) == "Item5"] <- "Options"
names(SC)[names(SC) == "Item6"] <- "RespectfulResponse"
names(SC)[names(SC) == "Item7"] <- "CourteousExchange"
names(SC)[names(SC) == "Item8"] <- "ActiveListening"

#Inspecting Data
str(SC)
summary(SC)
# Select only numeric columns
numeric_SC <- SC[sapply(SC, is.numeric)]
# Check for outliers in each numeric column using boxplots
boxplot(numeric_SC)

# Create a correlation matrix of numeric variables
correlation_matrix <- cor(numeric_SC)

# Create a heatmap of the correlation matrix
heatmap(correlation_matrix, symm = TRUE, margins = c(5, 10))

# Perform one-hot encoding
SC <- predict(dummyVars(~ ., data = SC, fullRank = TRUE), newdata = SC)

SC <- as.data.frame(SC)



#####################Download data into csv file for my computer############################


# Specify the file path and name for the CSV file
csv_file_path <- "C:/Users/nshai/OneDrive/Pictures/Documents/School/D209B/CleanedData.csv"

# Export the dataset to a CSV file
write.csv(SC, file = csv_file_path, row.names = FALSE)

# Print a message indicating the successful export
cat("Dataset exported to:", csv_file_path, "\n")


###################Analysis##########################
###############Splitting data#########################

train_index <- createDataPartition(SC$ChurnYes, p = 0.8, list = FALSE)
train_data <- SC[train_index, ]
test_data <- SC[-train_index, ]

# Specify the file path and name for the CSV file- Training Data
csv_file_path <- "C:/Users/nshai/OneDrive/Pictures/Documents/School/D209B/TrainingData.csv"
# Export the dataset to a CSV file
write.csv(train_data, file = csv_file_path, row.names = FALSE)
# Print a message indicating the successful export
cat("Dataset exported to:", csv_file_path, "\n")

# Specify the file path and name for the CSV file - test Data
csv_file_path <- "C:/Users/nshai/OneDrive/Pictures/Documents/School/D209B/TestData.csv"
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

###################Decision Tree#####################################

# Create the decision tree model
tree_model <- rpart(ChurnYes ~ ., data = train_data, method = "class")

# Print the summary of the decision tree model
summary(tree_model)

# Visualize the decision tree
rpart.plot(tree_model)

# Predicting using the decision tree model
predicted_values <- predict(tree_model, newdata = test_data, type = "class")

# Convert factor levels to numeric
predicted_values <- as.numeric(as.character(predicted_values))

# Calculate MSE
mse <- mean((test_data$Churn - predicted_values)^2)
mse


