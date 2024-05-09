#Treat missing values using the imputation model and using a set of selected predictor variables
install.packages("mice")

#Impute children by using marital, age, employment
library(mice)
imputation_vars_children <- c("Children", "Marital", "Age", "Employment")
imputation_model_children <- mice(medical_raw_data[imputation_vars_children], method = "pmm")
imputed_data_children <- complete(imputation_model_children)
medical_raw_data$Children <- imputed_data_children$Children

#Impute soft drink by using age, diabetes, overweight
imputation_vars_soft_drink <- c("Soft_drink", "Age", "Diabetes", "Overweight")
imputation_model_soft_drink <- mice(medical_raw_data[imputation_vars_soft_drink], method = "pmm")
imputed_data_soft_drink <- complete(imputation_model_soft_drink)
medical_raw_data$Soft_drink <- imputed_data_soft_drink$Soft_drink

#Impute income by using education, employment, age, marital, children
imputation_vars_income <- c("Income", "Education", "Employment", "Age", "Marital", "Children")
imputation_model_income <- mice(medical_raw_data[imputation_vars_income], method = "pmm")
imputed_data_income <- complete(imputation_model_income)
medical_raw_data$Income <- imputed_data_income$Income

#Impute age using education, employment, income, marital, children 
imputation_vars_age <- c("Age", "Education", "Employment", "Income", "Marital", "Children")
imputation_model_age <- mice(medical_raw_data[imputation_vars_age], method = "pmm")
imputed_data_age <- complete(imputation_model_age)
medical_raw_data$Age <- imputed_data_age$Age

#Impute initial days using initial admin, doc visits, services, high blood, diabetes
imputation_vars_initial_days <- c("Initial_days", "Initial_admin", "Doc_visits", "Services", "HighBlood", "Diabetes")
imputation_model_initial_days <- mice(medical_raw_data[imputation_vars_initial_days], method = "pmm")
imputed_data_initial_days <- complete(imputation_model_initial_days)
medical_raw_data$Initial_days <- imputed_data_initial_days$Initial_days

#Impute anxiety using age, gender, income, education, services
imputation_vars_anxiety <- c("Anxiety", "Age", "Gender", "Income", "Education", "Services")
imputation_model_anxiety <- mice(medical_raw_data[imputation_vars_anxiety], method = "pmm")
imputed_data_anxiety <- complete(imputation_model_anxiety)
medical_raw_data$Anxiety <- imputed_data_anxiety$Anxiety

#Impute overweight using age, gender, high blood, diabetes, hyperlipidimia, arthritis, back pain, asthma, soft drink
imputation_vars_overweight <- c("Overweight" ,"Age", "Gender", "HighBlood", "Diabetes", "Hyperlipidemia", "Arthritis", "BackPain", "Asthma", "Soft_drink")
imputation_model_overweight <- mice(medical_raw_data[imputation_vars_overweight], method = "pmm")
imputed_data_overweight <- complete(imputation_model_overweight)
medical_raw_data$Overweight <- imputed_data_overweight$Overweight



