# =============================================================================
# 01_data_prep.R
# Lung Cancer Logistic Regression — Data Preparation
# Author: mariannawicks-sys
# =============================================================================

# Load libraries
library(dplyr)

# Load data
lung <- read.csv("data/lung_cancer_dataset.csv", stringsAsFactors = FALSE)

# Check it loaded correctly
dim(lung)
head(lung)

# Create binary outcome (1 = survived, 0 = did not)
lung$outcome <- ifelse(lung$Survived == "Yes", 1, 0)
cat("Survival distribution:\n")
print(table(lung$outcome))
print(prop.table(table(lung$outcome)))

# Recode smoking status (reference = Never Smoked)
lung$Smoking_Status <- factor(lung$Smoking_Status,
  levels = c("Never Smoked", "Former Smoker", "Current Smoker"))

# Recode cancer stage (reference = Stage I)
lung$Cancer_Stage <- factor(lung$Cancer_Stage,
  levels = c("Stage I", "Stage II", "Stage III", "Stage IV"))

# Recode gender (reference = Female)
lung$Gender <- factor(lung$Gender, levels = c("Female", "Male"))

# Recode air pollution (reference = Low)
lung$Air_Pollution_Exposure <- factor(lung$Air_Pollution_Exposure,
  levels = c("Low", "Moderate", "High"))

# Recode Yes/No variables to 1/0
yes_no_vars <- c("Secondhand_Smoke", "Family_History", "Occupational_Hazard",
  "Chronic_Lung_Disease", "Asbestos_Exposure", "Radon_Exposure",
  "Previous_Cancer_History", "Metastasis", "Coughing",
  "Shortness_of_Breath", "Chest_Pain", "Coughing_Blood",
  "Fatigue", "Weight_Loss", "Wheezing", "Recurrent_Infections",
  "Swallowing_Difficulty", "Finger_Clubbing")

for (var in yes_no_vars) {
  lung[[var]] <- ifelse(lung[[var]] == "Yes", 1, 0)
}

# Check for missing values
cat("\nMissing values:\n")
print(colSums(is.na(lung))[colSums(is.na(lung)) > 0])

# Save cleaned data
write.csv(lung, "data/lung_cancer_prepped.csv", row.names = FALSE)
cat("\nDone! Data saved to data/lung_cancer_prepped.csv\n")
cat("Total records:", nrow(lung), "\n")
