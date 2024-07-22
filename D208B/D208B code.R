#############Upload Data and needed libraries######################

library(readxl)
library(visdat)
library(dplyr)
library(psych)
library(ggplot2)


churn_clean <- read_excel("C:/Users/nshai/OneDrive/Pictures/Documents/School/D208B/churn_clean.xlsx")

CC<- churn_clean



###############################Data Cleaning#######################################

#Checking for missing values
vis_miss(CC)

#no missing values so check for repeat rows
CC <- distinct(CC)


#Checking for negative entries
inconsistent_entries <- filter(CC, Outage_sec_perweek < 0)
CC$Outage_sec_perweek[inconsistent_entries$Outage_sec_perweek] <- 0

inconsistent_entries <- filter(CC, Yearly_equip_failure < 0)
CC$Yearly_equip_failure[inconsistent_entries$Yearly_equip_failure] <- 0

inconsistent_entries <- filter(CC, Contacts < 0)
CC$Contacts[inconsistent_entries$Contacts] <- 0

inconsistent_entries <- filter(CC, MonthlyCharge < 0)
CC$MonthlyCharge[inconsistent_entries$MonthlyCharge] <- 0

#checking for outliers
boxplot(CC$MonthlyCharge,data = CC)
boxplot(CC$Outage_sec_perweek ,data = CC)
boxplot(CC$Yearly_equip_failure ,data = CC)
boxplot(CC$Contacts ,data = CC)

#Count and replace outage outliers with mean
variable <- CC$Outage_sec_perweek
# Calculate the IQR
Q1 <- quantile(variable, 0.25)
Q3 <- quantile(variable, 0.75)
IQR <- Q3 - Q1
# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Count the number of outliers
num_outliers <- sum(variable < lower_bound | variable > upper_bound)
# Print the result
cat("Number of outliers in Outage_sec_perweek:", num_outliers, "\n")
# Replace outliers with the mean
variable[variable < lower_bound | variable > upper_bound] <- mean(variable, na.rm = TRUE)
# Update the 'Outage_sec_perweek' column in the dataframe
CC$Outage_sec_perweek <- variable
boxplot(CC$Outage_sec_perweek ,data = CC)

#Count and replace outage outliers with mean
variable <- CC$Yearly_equip_failure
# Calculate the IQR
Q1 <- quantile(variable, 0.25)
Q3 <- quantile(variable, 0.75)
IQR <- Q3 - Q1
# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Count the number of outliers
num_outliers <- sum(variable < lower_bound | variable > upper_bound)
# Print the result
cat("Number of outliers in Outage_sec_perweek:", num_outliers, "\n")
# Replace outliers with the mean
variable[variable < lower_bound | variable > upper_bound] <- mean(variable, na.rm = TRUE)
# Update the 'Outage_sec_perweek' column in the dataframe
CC$Yearly_equip_failure <- variable
boxplot(CC$Yearly_equip_failure ,data = CC)

#Count and replace outage outliers with mean
variable <- CC$Contacts
# Calculate the IQR
Q1 <- quantile(variable, 0.25)
Q3 <- quantile(variable, 0.75)
IQR <- Q3 - Q1
# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
# Count the number of outliers
num_outliers <- sum(variable < lower_bound | variable > upper_bound)
# Print the result
cat("Number of outliers in Outage_sec_perweek:", num_outliers, "\n")
# Replace outliers with the mean
variable[variable < lower_bound | variable > upper_bound] <- mean(variable, na.rm = TRUE)
# Update the 'Outage_sec_perweek' column in the dataframe
CC$Contacts <- variable
boxplot(CC$Contacts ,data = CC)


############################Variables############################

describe(churn_clean[c("Churn", "Outage_sec_perweek","MonthlyCharge", "Contacts","Yearly_equip_failure")])

###########################################Visualizations#################################################
###########Univariate##############
ggplot(CC, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  labs(title = "Distribution of Churn Status", x = "Churn Status", y = "Count") +
  scale_fill_manual(values = c("skyblue", "salmon"))

ggplot(CC, aes(x = Outage_sec_perweek)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Outage_sec_perweek")

ggplot(CC, aes(x = Contacts)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Contacts")

ggplot(CC, aes(x = Yearly_equip_failure)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Yearly_equip_failure")

#Monthly Charge - Histogram
ggplot(CC, aes(x = MonthlyCharge)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Monthly Charge Distribution", x = "Monthly Charge", y = "Frequency") +
  theme_minimal()

##################Bivariate##################
#churn vs outage
ggplot(CC, aes(x = Churn, y = Outage_sec_perweek, fill = Churn)) +
  geom_violin()+
  labs(title = "Boxplot of Outage_sec_perweek by Churn Status")

# Churn vs Contacts
ggplot(CC, aes(x = Churn, y = Contacts, fill = Churn)) +
  geom_violin() +
  labs(title = "Churn vs Contacts", x = "Churn Status", y = "Contacts") +
  scale_fill_manual(values = c("skyblue", "salmon"))  
# Churn vs Yearly_equip_failure
ggplot(CC, aes(x = Churn, y = Yearly_equip_failure, fill = Churn)) +
  geom_violin() +
  labs(title = "Churn vs Yearly Equipment Failures", x = "Churn Status", y = "Yearly Equipment Failures") +
  scale_fill_manual(values = c("skyblue", "salmon"))  

# Churn vs. Monthly Charge - Box Plot
ggplot(CC, aes(x = Churn, y = MonthlyCharge, fill = Churn)) +
  geom_violin() +
  labs(title = "Churn vs. Monthly Charge", x = "Churn", y = "Monthly Charge") +
  scale_fill_manual(values = c("skyblue", "salmon")) 

 #############################Data Transformation#########################
#Turn churn into binary
CC <- CC %>%
  mutate(Churn_Binary = ifelse(Churn == "Yes", 1, 0))

#Scaling numerical values
CC$Contacts <- scale(CC$Contacts)
CC$Yearly_equip_failure <- scale(CC$Yearly_equip_failure)
CC$Outage_sec_perweek <- scale(CC$Outage_sec_perweek)
CC$MonthlyCharge <- scale(CC$MonthlyCharge)

# Select columns of interest
pcc <- select(CC, Churn_Binary, Outage_sec_perweek, MonthlyCharge, Contacts,Yearly_equip_failure)


###########################Export data set################################################
# Specify the file path and name for the CSV file
csv_file_path <- "C:/Users/nshai/OneDrive/Pictures/Documents/School/D208B/pcc.csv"

# Export the dataset to a CSV file
write.csv(pcc, file = csv_file_path, row.names = FALSE)

# Print a message indicating the successful export
cat("Dataset exported to:", csv_file_path, "\n")

###############################Logistic Regression###########################################


# Fit initial logistic regression model
initial_model <- glm(Churn_Binary ~ ., data = pcc, family = "binomial")

# Summary of the model
summary(initial_model)

M_vars <- c("Outage_sec_perweek", "MonthlyCharge", "Yearly_equip_failure")

#Middle model
M_model <- glm(Churn_Binary ~ ., data = pcc[,c("Churn_Binary", M_vars)])
summary(M_model)

M_vars <- c("Outage_sec_perweek", "MonthlyCharge")

#Middle model
M_model <- glm(Churn_Binary ~ ., data = pcc[,c("Churn_Binary", M_vars)])
summary(M_model)


# Identify independent variables 
final_vars <- c("MonthlyCharge")

# Constructing an initial multiple linear regression model
final_model <- glm(Churn_Binary ~ ., data = pcc[,c("Churn_Binary", final_vars)])

print(final_model)

# Print a summary of the model
summary(final_model)


########################Output of Analysis#######################################

predictions_reduced <- predict(final_model, newdata = pcc[, c("MonthlyCharge", "Outage_sec_perweek")], type = "response")
predictions_reduced <- ifelse(predictions_reduced > 0.5, 1, 0)
actual <- pcc$Churn_Binary

# Confusion Matrix for the Reduced Model
conf_matrix_reduced <- table(Actual = actual, Predicted = predictions_reduced)

# Display Confusion Matrix for the Reduced Model
print(conf_matrix_reduced)

# Calculate Accuracy for the Reduced Model
TP_reduced <- conf_matrix_reduced[2, 2]
TN_reduced <- conf_matrix_reduced[1, 1]
FP_reduced <- conf_matrix_reduced[1, 2]
FN_reduced <- conf_matrix_reduced[2, 1]

accuracy_reduced <- (TP_reduced + TN_reduced) / (TP_reduced + TN_reduced + FP_reduced + FN_reduced)
cat("\nAccuracy for the Reduced Model:", accuracy_reduced, "\n")






