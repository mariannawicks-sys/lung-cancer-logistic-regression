# =============================================================================
# 02_logistic_model.R
# Lung Cancer Logistic Regression — Model Fitting
# Author: mariannawicks-sys
# =============================================================================

# Load libraries
library(dplyr)

# Load prepped data
lung <- read.csv("data/lung_cancer_prepped.csv", stringsAsFactors = FALSE)

# Re-apply factor levels
lung$Smoking_Status <- factor(lung$Smoking_Status,
  levels = c("Never Smoked", "Former Smoker", "Current Smoker"))
lung$Cancer_Stage <- factor(lung$Cancer_Stage,
  levels = c("Stage I", "Stage II", "Stage III", "Stage IV"))
lung$Gender <- factor(lung$Gender, levels = c("Female", "Male"))
lung$Air_Pollution_Exposure <- factor(lung$Air_Pollution_Exposure,
  levels = c("Low", "Moderate", "High"))

# =============================================================================
# SECTION 1: Fit Logistic Regression Model
# =============================================================================
logit_model <- glm(
  outcome ~ Age + Gender + Smoking_Status + Cancer_Stage +
    Metastasis + Tumor_Size_cm + Family_History +
    Chronic_Lung_Disease + Air_Pollution_Exposure,
  data = lung,
  family = binomial(link = "logit")
)

cat("=== Logistic Regression Summary ===\n")
print(summary(logit_model))

# =============================================================================
# SECTION 2: Odds Ratios
# =============================================================================
cat("\n=== Odds Ratios with 95% Confidence Intervals ===\n")
or_table <- exp(cbind(OR = coef(logit_model), confint(logit_model)))
print(round(or_table, 3))

# =============================================================================
# SECTION 3: Confounder Check
# =============================================================================
cat("\n=== Confounder Check: Model without Age ===\n")
logit_no_age <- glm(
  outcome ~ Gender + Smoking_Status + Cancer_Stage +
    Metastasis + Tumor_Size_cm + Family_History +
    Chronic_Lung_Disease + Air_Pollution_Exposure,
  data = lung,
  family = binomial(link = "logit")
)

# Compare Cancer_Stage OR with and without Age
cat("Cancer Stage IV OR WITH age:", 
    round(exp(coef(logit_model)["Cancer_StageStage IV"]), 3), "\n")
cat("Cancer Stage IV OR WITHOUT age:", 
    round(exp(coef(logit_no_age)["Cancer_StageStage IV"]), 3), "\n")
cat("(If these differ by >10%, age may be a confounder)\n")

# =============================================================================
# SECTION 4: Interaction Term
# =============================================================================
cat("\n=== Interaction: Cancer Stage x Metastasis ===\n")
logit_interaction <- glm(
  outcome ~ Age + Gender + Smoking_Status + Cancer_Stage +
    Metastasis + Tumor_Size_cm + Family_History +
    Chronic_Lung_Disease + Air_Pollution_Exposure +
    Cancer_Stage:Metastasis,
  data = lung,
  family = binomial(link = "logit")
)

# LRT to test if interaction is significant
lrt <- anova(logit_model, logit_interaction, test = "LRT")
cat("\nLikelihood Ratio Test for interaction term:\n")
print(lrt)

# =============================================================================
# SECTION 5: Save Results
# =============================================================================
dir.create("results", showWarnings = FALSE)

sink("results/logistic_model_summary.txt")
cat("=== Logistic Regression Summary ===\n")
print(summary(logit_model))
cat("\n=== Odds Ratios ===\n")
print(round(or_table, 3))
cat("\n=== LRT: Interaction Term ===\n")
print(lrt)
sink()

cat("\nResults saved to results/logistic_model_summary.txt\n")
cat("Model fitting complete!\n")
