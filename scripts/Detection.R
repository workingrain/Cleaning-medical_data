#Import and open medical raw data 
library(readxl)
medical_raw_data <- read_excel("C:/Users/kolgi/OneDrive - Western Governors University/d206/medical_raw_data.xlsx")
View(medical_raw_data)

#Understanding and exploring the data
summary(medical_raw_data)
head(medical_raw_data)
str(medical_raw_data) 

#convert children, age, income and initial days into numeric 
#round initial days
medical_raw_data$Children <- as.numeric(medical_raw_data$Children)
medical_raw_data$Age <- as.numeric(na.omit(medical_raw_data$Age))
medical_raw_data$Income <- as.numeric(na.omit(medical_raw_data$Income))
medical_raw_data$Initial_days <- as.numeric(na.omit(medical_raw_data$Initial_days))
medical_raw_data$Initial_days <- round(medical_raw_data$Initial_days)

#convert into numeric
install.packages("plyr")
library(plyr)

medical_raw_data$Soft_drink <- as.numeric(revalue(medical_raw_data$Soft_drink, c("Yes" = 1, "No" = 0, "NA" = NA)))
medical_raw_data$HighBlood <- as.numeric(revalue(medical_raw_data$HighBlood, c("Yes" = 1, "No" = 0, "NA" = NA)))
medical_raw_data$Stroke <- as.numeric(revalue(medical_raw_data$Stroke, c("Yes" = 1, "No" = 0, "NA" = NA)))
medical_raw_data$Complication_risk <- factor(medical_raw_data$Complication_risk, levels = c("Low", "Medium", "High"))
medical_raw_data$Overweight <- as.numeric(medical_raw_data$Overweight)
medical_raw_data$Arthritis <- as.numeric(revalue(medical_raw_data$Arthritis, c("Yes" = 1, "No" = 0, "NA" = NA)))
medical_raw_data$Diabetes <- as.numeric(revalue(medical_raw_data$Diabetes, c("Yes" = 1, "No" = 0, "NA" = NA)))
medical_raw_data$Hyperlipidemia <- as.numeric(revalue(medical_raw_data$Hyperlipidemia, c("Yes" = 1, "No" = 0, "NA" = NA)))
medical_raw_data$BackPain <- as.numeric(revalue(medical_raw_data$BackPain, c("Yes" = 1, "No" = 0, "NA" = NA)))
medical_raw_data$Anxiety <- as.numeric(medical_raw_data$Anxiety)
medical_raw_data$Allergic_rhinitis <- as.numeric(revalue(medical_raw_data$Allergic_rhinitis, c("Yes" = 1, "No" = 0, "NA" = NA)))
medical_raw_data$Reflux_esophagitis <- as.numeric(revalue(medical_raw_data$Reflux_esophagitis, c("Yes" = 1, "No" = 0, "NA" = NA)))
medical_raw_data$Asthma <- as.numeric(revalue(medical_raw_data$Asthma, c("Yes" = 1, "No" = 0, "NA" = NA)))

#count duplicates
sum(duplicated(medical_raw_data)) 
filter(medical_raw_data, duplicated(medical_raw_data))

library(dplyr)
medical_raw_data %>%
  count(Customer_id) %>%
  filter(n >1)

#use naniar to find missing values 
install.packages("naniar")
library(naniar) 
miss_var_summary(medical_raw_data)
miss_case_summary(medical_raw_data)


#find NAs #shows all missing values per column
sum(is.na(medical_raw_data))
colSums(is.na(medical_raw_data))

#Find outliers through visualization 
library(ggplot2)

#Find outliers for population
boxplot(medical_raw_data$Population, outline = TRUE, main = "Box Plot of Population", ylab = "Population")
#Calculate how many cases are outliers
pop_query <- subset(medical_raw_data, Population > 25000)
str(pop_query)

#Find outliers for children
boxplot(medical_raw_data$Children, outline = TRUE, main = "Box Plot of Children", ylab = "Children")
#Calculate how many cases are outliers
chld_query <- subset(medical_raw_data, Children > 7)
str(chld_query)

#Find outliers for age
boxplot(medical_raw_data$Age, outline = TRUE, main = "Box Plot of Age", ylab = "Age")
#no outliers so no need to find cases

#Find outliers for income
boxplot(medical_raw_data$Income, outline = TRUE, main = "Box Plot of Income", ylab = "Income")
#Calculate how many cases are outliers
inc_query <- subset(medical_raw_data, Income > 105000)
str(inc_query)

#Find outliers for Vitamin D Supplement
boxplot(medical_raw_data$VitD_supp, outline = TRUE, main = "Box Plot of Vitamin D supplement", ylab = "Vitamin D supplement")
#Calculate how many cases are outliers
sup_query <- subset(medical_raw_data, VitD_supp > 3)
str(sup_query)

#Find outliers for Vitamin D levels
boxplot(medical_raw_data$VitD_levels, outline = TRUE, main = "Box Plot of Vitamin D levels", ylab = "Vitamin D levels")
#Calculate how many cases are outliers
dlvl_query <- subset(medical_raw_data, VitD_levels < 11 | VitD_levels > 24)
str(dlvl_query)

#Find outliers for doc visits
boxplot(medical_raw_data$Doc_visits, outline = TRUE, main = "Box Plot of Doc Visits", ylab = "Doc Visits")
#no outliers so no need to find cases

#Find outliers for full meals eaten
boxplot(medical_raw_data$Full_meals_eaten, outline = TRUE, main = "Box Plot of Full Meals Eaten", ylab = "Full Meals Eaten")
#Calculate how many cases are outliers
meal_query <- subset(medical_raw_data, Full_meals_eaten >5)
str(meal_query)

#Find outliers for initial days
boxplot(medical_raw_data$Initial_days, outline = TRUE, main = "Box Plot of Initial Days", ylab = "Initial Days")
#no outliers so no need to find cases

#Find outliers for total charge
boxplot(medical_raw_data$TotalCharge, outline = TRUE, main = "Box Plot of Total Charge", ylab = "Total Charge")
#Calculate how many cases are outliers
total_query <- subset(medical_raw_data, TotalCharge > 14500)
str(total_query)

#Find outliers for additional charges
boxplot(medical_raw_data$Additional_charges, outline = TRUE, main = "Box Plot of Additional Charges", ylab = "Additional Charges")
#Calculate how many cases are outliers
adtl_query <- subset(medical_raw_data, Additional_charges > 26000)
str(adtl_query)

#Find outliers for Item1, Item2, Item3, Item4, Item5, Item6, Item7 and Item8 in one plane
library(ggplot2)
library(reshape2)
melted_data <- melt(medical_raw_data[, c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6", "Item7", "Item8")])
ggplot(melted_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Box Plots of Items", x = "Item", y = "Value") +
  theme_minimal()