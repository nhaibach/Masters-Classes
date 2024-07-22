library(readxl)
library(dplyr)
library(visdat)
library(psych)
library(ggplot2)


C <- read_excel("C:/Users/nshai/OneDrive/Pictures/Documents/School/D208A/CC.xlsx")

CC <- C
View(CC)

#############################################Data Cleaning#####################################################

#Checking for repeated data
CC <- distinct(CC)
#Checking for missing values throughout dataset
vis_miss(CC)

#Checking for Outliers
boxplot(CC$MonthlyCharge,data = CC)
boxplot(CC$Age,data = CC)
boxplot(CC$Bandwidth_GB_Year, data = CC)
#below have outliers
boxplot(CC$Yearly_equip_failure ,data = CC)
boxplot(CC$Email, data = CC)
boxplot(CC$Contacts, data = CC)

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

# Email
variable <- CC$Email
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
cat("Number of outliers in Email:", num_outliers, "\n")
# Replace outliers with the mean
variable[variable < lower_bound | variable > upper_bound] <- mean(variable, na.rm = TRUE)
# Update the 'Email' column in the dataframe
CC$Email <- variable
# Plot the boxplot after replacing outliers
boxplot(CC$Email, data = CC)


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


columns_of_interest <- c("Age", "MonthlyCharge", "Tenure", "Yearly_equip_failure", "Bandwidth_GB_Year", "Email", "Contacts")

describe(CC[, columns_of_interest])


##################################### Univariate Visualizations ###################################################

#Tenure - Histogram
ggplot(CC, aes(x = Tenure)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Tenure Distribution", x = "Tenure", y = "Frequency") +
  theme_minimal()

# Age  - Bar Plot
ggplot(CC, aes(x = Age, fill = Age)) +
  geom_histogram(binwidth = 15, fill = "cyan", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

#Monthly Charge - Histogram
ggplot(CC, aes(x = MonthlyCharge)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Monthly Charge Distribution", x = "Monthly Charge", y = "Frequency") +
  theme_minimal()

#Yearly equip failure
ggplot(CC, aes(x = Yearly_equip_failure)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Yearly_equip_failure")

#Bandwidth_GB_Year - Histogram
ggplot(CC, aes(x = Bandwidth_GB_Year)) +
  geom_histogram(binwidth = 200, fill = "yellow", color = "black") +
  labs(title = "Bandwidth_GB_Year Distribution", x = "Bandwidth_GB_Year", y = "Frequency") +
  theme_minimal()

#Email - Histogram
ggplot(CC, aes(x = Email)) +
  geom_histogram(binwidth = 2, fill = "green", color = "black") +
  labs(title = "Email Distribution", x = "Email", y = "Frequency") +
  theme_minimal()

#Contacts
ggplot(CC, aes(x = Contacts)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Contacts") +
  theme_minimal()

################################ Bivariate Visualizations ###################################################

# Tenure vs. Monthly Charge 
ggplot(CC, aes(x = Tenure, y = MonthlyCharge)) +
  geom_hex() +
  labs(title = "Bivariate Visualization: Tenure vs. Monthly Charge",
       x = "Tenure",
       y = "Monthly Charge") +
  theme_minimal()

# Tenure vs. Age
ggplot(CC, aes(x = Tenure, y = Age)) +
  geom_hex() +
  labs(title = "Bivariate Visualization: Tenure vs. Age",
       x = "Tenure",
       y = "Age") +
  theme_minimal()

#Yearly equip failure
ggplot(CC, aes(x =Tenure , y = Yearly_equip_failure)) +
  geom_hex() +
  labs(title = "Bivariate Visualization: Tenure vs. Yearly Equipment Failure",
       x = "Tenure",
       y = "Yearly Equipment Failure") +
  theme_minimal()

#Bandwidth_GB_Year
ggplot(CC, aes(x =Tenure , y = Bandwidth_GB_Year)) +
  geom_hex() +
  labs(title = "Bivariate Visualization: Tenure vs. Bandwidth_GB_Year",
       x = "Tenure",
       y = "Bandwidth_GB_Year") +
  theme_minimal()

#Email
ggplot(CC, aes(x =Tenure , y = Email)) +
  geom_hex() +
  labs(title = "Bivariate Visualization: Tenure vs. Email",
       x = "Tenure",
       y = "Email") +
  theme_minimal()

#Contacts
ggplot(CC, aes(x =Tenure , y = Contacts)) +
  geom_hex() +
  labs(title = "Bivariate Visualization: Tenure vs. Contacts",
       x = "Tenure",
       y = "Contacts") +
  theme_minimal()


###########################################Data Transformation########################################

# Standardization (Z-score) - Standardizing only the continuous variables
CC$z_Yearly_equip_failure <- (CC$Yearly_equip_failure - mean(CC$Yearly_equip_failure)) / sd(CC$Yearly_equip_failure)
CC$z_MonthlyCharge <- (CC$MonthlyCharge - mean(CC$MonthlyCharge)) / sd(CC$MonthlyCharge)
CC$z_Bandwidth_GB_Year <- (CC$Bandwidth_GB_Year - mean(CC$Bandwidth_GB_Year)) / sd(CC$Bandwidth_GB_Year)
CC$z_Email <- (CC$Email - mean(CC$Email)) / sd(CC$Email)
CC$z_Contacts <- (CC$Contacts - mean(CC$Contacts)) / sd(CC$Contacts)
CC$z_Age <- (CC$Age - mean(CC$Age)) / sd(CC$Age)
CC$z_Tenure <- (CC$Tenure - mean(CC$Tenure)) / sd(CC$Tenure)


# Select columns of interest
scc <- select(CC, z_Tenure, z_Yearly_equip_failure, z_MonthlyCharge,z_Bandwidth_GB_Year, z_Email, z_Contacts ,z_Age)


# View the transformed dataset
head(scc)


#####################Download data into csv file for my computer############################


# Specify the file path and name for the CSV file
csv_file_path <- "C:/Users/nshai/OneDrive/Pictures/Documents/School/D208A/scc.csv"

# Export the dataset to a CSV file
write.csv(scc, file = csv_file_path, row.names = FALSE)

# Print a message indicating the successful export
cat("Dataset exported to:", csv_file_path, "\n")


###############End of Part III: Data Preparation ################################################################

# Identify independent variables 
independent_vars <- c("z_Yearly_equip_failure", "z_MonthlyCharge","z_Bandwidth_GB_Year", "z_Email", "z_Contacts" ,"z_Age")

# Constructing an initial multiple linear regression model
initial_model <- lm(z_Tenure ~ ., data = scc[, c("z_Tenure", independent_vars)])

# Print a summary of the model
summary(initial_model)

#Taking out the Email Variable
M_vars <- c("z_Yearly_equip_failure", "z_MonthlyCharge","z_Bandwidth_GB_Year", "z_Contacts" ,"z_Age")
M_model <- lm(z_Tenure ~ ., data = scc[,c("z_Tenure", M_vars)])
summary(M_model)

#Taking out the yearly equipment failure Variable
M_vars <- c("z_MonthlyCharge","z_Bandwidth_GB_Year", "z_Contacts" ,"z_Age")
M_model <- lm(z_Tenure ~ ., data = scc[,c("z_Tenure", M_vars)])
summary(M_model)

#Taking out the contacts Variable
M_vars <- c("z_MonthlyCharge","z_Bandwidth_GB_Year", "z_Age")
M_model <- lm(z_Tenure ~ ., data = scc[,c("z_Tenure", M_vars)])
summary(M_model)

# Identify independent variables 
final_vars <- c("z_MonthlyCharge","z_Bandwidth_GB_Year", "z_Age")

# Constructing an initial multiple linear regression model
final_model <- lm(z_Tenure ~ ., data = CC[,c("z_Tenure", final_vars)])

# Print a summary of the model
summary(final_model)

#######################Residual plot and Standard error##############################

residuals <- final_model$residuals
fitted_values <- predict(final_model, type = "response")

# Plotting the residuals against the fitted values
plot(final_model$fitted.values, residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot", pch = 16, col = "blue") 
abline(h = 0, col = "red", lty = 2)


residual_standard_error <- summary(final_model)$sigma
cat("Residual Standard Error:", residual_standard_error, "\n")



