library(readxl)
mc <- read_excel("C:/Users/nshai/OneDrive/Pictures/Documents/School/D208A/mc.xlsx")

install.packages("GGally")
library(GGally)

model <- lm(formula = Age ~ Initial_days + TotalCharge + Additional_charges, 
            data = mcc)
summary(model)

# Select columns of interest
pcc <- select(CC, Children, Age, Income, Churn, Tenure, Outage_sec_perweek, Email, Contacts, Yearly_equip_failure, MonthlyCharge, Bandwidth_GB_Year)
lm(Age ~ . , data = pcc) %>% 
  step(direction = "backward", trace = 0) %>%
  summary()

mcc <- select(mc, Children, Tenure, Age, Income, ReAdmis, Doc_visits, Full_meals_eaten, Initial_days, TotalCharge, Additional_charges )

lm(Tenure ~ . , data = mcc) %>% 
  step(direction = "backward", trace = 0) %>%
  summary()

residuals <- model$residuals
fitted_values <- predict(model, type = "response")

# Plotting the residuals against the fitted values
plot(model$fitted.values, residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot", pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)

qplot(mc$Initial_days, mc$TotalCharge)
ggpairs(mcc)
