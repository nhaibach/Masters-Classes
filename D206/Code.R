#visdat
install.packages("visdat")
library(visdat)
#diplyr
install.packages("diplyr")
library(diplyr)
#plyr
install.packages("plyr")
library(plyr)
#tidyverse
install.packages("tidyverse")
library(tidyverse)
#factoextra
install.packages("factoextra")
library(factoextra)

data <- read.csv("C:/Users/nshai/OneDrive/Desktop/churn.csv")
print(summary(data))

#detecting duplicates or missing data

sum(duplicated(data))
colSums(is.na(data))
vis_miss(data)

#Clean data name change
dc <- data

#Cleaning All Missing Values

#got rid of children na (Skewed distribution)
dc$Children[is.na(dc$Children)]<-median(dc$Children, na.rm = TRUE)
#Age NA (uniform Distribution)
dc$Age[is.na(dc$Age)]<-mean(dc$Age, na.rm = TRUE)
#Income NA (Skewed distribution)
dc$Income[is.na(dc$Income)]<-median(dc$Income, na.rm = TRUE)
#Tenure(bimodal distribution)
 dc$Tenure[is.na(dc$Tenure)]<-median(dc$Tenure, na.rm = TRUE)
#Bandwith (bimodal distribution)
dc$Bandwidth_GB_Year[is.na(dc$Bandwidth_GB_Year)]<-median(dc$Bandwidth_GB_Year, na.rm = TRUE)

#Techie(used Mode due to logical data type)
dc$Techie <-recode(dc$Techie,"Yes" =1 ,"No" = 0)
dc$Techie<- as.numeric(dc$Techie)
dc$Techie[is.na(dc$Techie)]<- (names(which.max(table(dc$Techie))))
dc$Techie<- as.numeric(dc$Techie)
dc$Techie <-recode(dc$Techie, "1" ="Yes"  ,"0" = "No")
print(dc$Techie)


#Phone(used Mode due to logical data type)
dc$Phone <-recode(dc$Phone,"Yes" =1 ,"No" = 0)
dc$Phone<- as.numeric(dc$Phone)
dc$Phone[is.na(dc$Phone)]<- (names(which.max(table(dc$Phone))))
dc$Phone<- as.numeric(dc$Phone)
dc$Phone <-recode(dc$Phone, "1" ="Yes"  ,"0" = "No")
print(dc$Phone)


#TechSupport(used Mode due to logical data type)
dc$TechSupport <-recode(dc$TechSupport,"Yes" =1 ,"No" = 0)
dc$TechSupport<- as.numeric(dc$TechSupport)
dc$TechSupport[is.na(dc$TechSupport)]<- (names(which.max(table(dc$TechSupport))))
dc$TechSupport<- as.numeric(dc$TechSupport)
dc$TechSupport <-recode(dc$TechSupport, "1" ="Yes"  ,"0" = "No")
print(dc$TechSupport)

#Showing no more missing values

colSums(is.na(dc))
summary(dc)
vis_miss(dc)

#Detection of Outliers

#has 11 outliers over 90000
pop_bp <- hist(dc$Population, xlab ="Population")
length(pop_bp)
pop_out <- str(dc[which(dc$Population>90000),])

#has 451 outliers greater than 6
Children_bp <- boxplot(dc$Children, xlab ="Children")
length(Children_bp)
Children_out <- str(dc[which(dc$Children>6),])

#has no outliers
Age_bp <- boxplot(dc$Age, xlab ="Age")$out
length(Age_bp)

#has 3 outliers over 200,000
Income_bp <- boxplot(dc$Income, xlab ="Income")$out
Income_hist <- hist(dc$Income, xlab ="Income")
Income_out <- str(dc[which(dc$Income>200000),])
length(Income_bp)

#has 500 over 30 sec and 11 under 0 sec
outage_bp <- boxplot(dc$Outage_sec_perweek, xlab ="Outage")$out
outage_out <- str(dc[which(dc$Outage_sec_perweek>30),])
outagelow_out <- str(dc[which(dc$Outage_sec_perweek<0),])
length(outage_bp)

#has 38 outliers that are less than 4 or greater than 20
Email_bp <- boxplot(dc$Email, xlab ="Email")$out
length(Email_bp)

#has 8 outliers that are greater than 5
Contacts_bp <- boxplot(dc$Contacts, xlab ="Contacts")$out
length(Contacts_bp)

#has 94 outliers that are greater than 2
equip_bp <- boxplot(dc$Yearly_equip_failure, xlab ="Equip failure")$out
length(equip_bp)

#has no Outliers
Tenure_bp <- boxplot(dc$Tenure, xlab ="Tenure")$out
length(Tenure_bp)

#has 5 outliers that are greater than 300
Charge_bp <- boxplot(dc$MonthlyCharge, xlab ="MonthlyCharge")$out
length(Charge_bp)

#has no Outliers
Bandwidth_bp <- boxplot(dc$Bandwidth_GB_Year, xlab ="Bandwidth")$out
length(Bandwidth_bp)

#outages
dc$Outage_sec_perweek[dc$Outage_sec_perweek > 13] <- NA
dc$Outage_sec_perweek[dc$Outage_sec_perweek < 2] <- NA
colSums(is.na(dc))
#Outages NA (uniform Distribution)
dc$Outage_sec_perweek[is.na(dc$Outage_sec_perweek)]<-mean(dc$Outage_sec_perweek, na.rm = TRUE)
outage_bp <- boxplot(dc$Outage_sec_perweek, xlab ="Outage")$out


#equipment failure
dc$Yearly_equip_failure[dc$Yearly_equip_failure > 2] <- NA
#got rid of Yearly_equip_failure na (Skewed distribution)
dc$Yearly_equip_failure[is.na(dc$Yearly_equip_failure)]<-median(dc$Yearly_equip_failure, na.rm = TRUE)
colSums(is.na(dc))
equip_bp <- boxplot(dc$Yearly_equip_failure, xlab ="Equip failure")$out


#Download clean Data
write.csv(dc, "C:/Users/nshai/OneDrive/Desktop/clean_churn.csv")

#creating subset for pca [In-Text Citation:(Kassambara, 2017)]


pcaValues <- dc[,c("Population", "Income", "Yearly_equip_failure","Tenure", "Email", "Contacts", "Age")]

view(pcaValues)

mtdc.pca <- prcomp(pcaValues,center = TRUE, scale= TRUE)

mtdc.pca
summary(mtdc.pca)
plot(mtdc.pca, type ="l")
fviz_eig(mtdc.pca, addlabels= TRUE)

fviz_pca_var(mtdc.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)






