# Install and load the 'dplyr' package if you haven't already
install.packages("dplyr")
library(dplyr)

# Assuming 'medical_raw_data' is your data frame and 'Population' is the variable you want to Winsorize
# You can adjust the quantiles (percentiles) based on your specific needs
lower_limit <- quantile(medical_raw_data$Population, 0.10)  # 5th percentile
upper_limit <- quantile(medical_raw_data$Population, 0.90)  # 95th percentile

# Winsorize the 'Population' variable
medical_raw_data <- medical_raw_data %>%
  mutate(Population_winsorized = ifelse(Population < lower_limit, lower_limit, ifelse(Population > upper_limit, upper_limit, Population)))

# Display the summary statistics of the Winsorized variable
summary(medical_raw_data$Population_winsorized)


boxplot(medical_raw_data$Population_winsorized, outline = TRUE, main = "Box Plot of Population", ylab = "Population")


# Install and load the 'mice' package if you haven't already
install.packages("mice")
library(mice)


# Create a subset of data with non-missing values for 'Children', 'Marital', and 'Age'
subsetcma_data <- subset(medical_raw_data, !is.na(Children) & !is.na(Marital) & !is.na(Age))

# Create a mice imputation model
imputation_model <- mice(subset_data, method = "pmm", m = 5)  # You can adjust the number of imputations (m) based on your needs

# Impute missing values
imputed_data <- complete(imputation_model)

# Merge imputed values back into the original dataset
medical_raw_data <- merge(medical_raw_data, imputed_data, all.x = TRUE)


# Assuming 'Soft_drink' and 'Diabetes' are in your dataset
# Replace missing values in 'Soft_drink' using the mode of 'Diabetes'




# Assuming 'Income', 'Employment', 'Age', and 'Education' are in your dataset
library(mice)

# Create a subset of data with non-missing values for 'Income', 'Employment', 'Age', and 'Education'
subset_data <- subset(medical_raw_data, !is.na(Income) & !is.na(Employment) & !is.na(Age) & !is.na(Education))

# Create a mice imputation model
imputation_model <- mice(subset_data, method = "pmm", m = 5)  # You can adjust the number of imputations (m) based on your needs

# Impute missing values
imputed_data <- complete(imputation_model)

# Merge imputed values back into the original dataset
medical_raw_data <- merge(medical_raw_data, imputed_data, all.x = TRUE)


# Assuming 'Income', 'Employment', 'Age', 'Education', 'Children', 'Marital' are in your dataset
library(mice)

# Create a subset of data with non-missing values for 'Income', 'Employment', 'Age', 'Education', 'Children', 'Marital'
subset_data <- subset(medical_raw_data, 
                      !is.na(Income) & !is.na(Employment) & !is.na(Age) & !is.na(Education) & 
                        !is.na(Children) & !is.na(Marital))

# Separate models for 'Income' and 'Children'
imputation_model_income <- mice(subset_data[, c('Income', 'Employment', 'Age', 'Education')], method = "pmm", m = 5)
imputation_model_children <- mice(subset_data[, c('Children', 'Marital', 'Age')], method = "pmm", m = 5)

# Impute missing values for 'Income'
imputed_data_income <- complete(imputation_model_income, action = 1)
# Impute missing values for 'Children'
imputed_data_children <- complete(imputation_model_children, action = 1)

# Merge imputed values back into the original dataset
medical_raw_data <- merge(medical_raw_data, imputed_data_income, all.x = TRUE)
medical_raw_data <- merge(medical_raw_data, imputed_data_children, all.x = TRUE)




# Install and load the 'mice' package if you haven't already
install.packages("mice")
library(mice)

# Assuming 'medical_raw_data' is your data frame
# Select relevant variables for imputation
imputation_vars_soft_drink <- c("Soft_drink", "Diabetes")

# Create a mice imputation model for 'Soft_drink' using 'Diabetes'
imputation_model_soft_drink <- mice(medical_raw_data[imputation_vars_soft_drink], method = "pmm")

# Impute missing values for 'Soft_drink'
imputed_data_soft_drink <- complete(imputation_model_soft_drink)

# Create a new column for the imputed 'Soft_drink' variable in the original data frame
medical_raw_data$Soft_drink_imputed <- imputed_data_soft_drink$Soft_drink

str(medical_raw_data$Soft_drink_imputed)

sft_unique <- unique(medical_raw_data$Soft_drink_imputed)
print(sft_unique)

# Assuming 'medical_raw_data' is your data frame
soft_drink_counts <- table(medical_raw_data$Soft_drink_imputed)

# Print the counts
print(soft_drink_counts)
