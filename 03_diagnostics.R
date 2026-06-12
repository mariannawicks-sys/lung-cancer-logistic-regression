# =============================================================================
# 03_diagnostics.R
# Lung Cancer Logistic Regression — Model Diagnostics
# Author: mariannawicks-sys
# =============================================================================

# Load libraries
library(dplyr)
library(ggplot2)
library(car)

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

# Refit model
logit_model <- glm(
  outcome ~ Age + Gender + Smoking_Status + Cancer_Stage +
    Metastasis + Tumor_Size_cm + Family_History +
    Chronic_Lung_Disease + Air_Pollution_Exposure,
  data = lung,
  family = binomial(link = "logit")
)

# Make figures folder
dir.create("figures", showWarnings = FALSE)

# =============================================================================
# SECTION 1: Multicollinearity Check (VIF)
# =============================================================================
cat("=== Variance Inflation Factors (VIF) ===\n")
vif_values <- vif(logit_model)
print(vif_values)
cat("(VIF > 5 suggests multicollinearity concern)\n")

# =============================================================================
# SECTION 2: Influential Observations (Cook's Distance)
# =============================================================================
cooks_d <- cooks.distance(logit_model)
influential <- which(cooks_d > 4/nrow(lung))

cat("\n=== Cook's Distance ===\n")
cat("Number of influential observations:", length(influential), "\n")

if (length(influential) > 0) {
  cat("Top influential observations:\n")
  top_influential <- head(sort(cooks_d[influential], decreasing = TRUE), 10)
  print(top_influential)
}

# Plot Cook's Distance
png("figures/cooks_distance.png", width = 800, height = 600)
plot(cooks_d, type = "h",
     main = "Cook's Distance - Influential Observations",
     xlab = "Observation Index",
     ylab = "Cook's Distance",
     col = ifelse(cooks_d > 4/nrow(lung), "red", "black"))
abline(h = 4/nrow(lung), col = "red", lty = 2)
legend("topright", legend = c("Influential", "Threshold"),
       col = c("red", "red"), lty = c(1, 2))
dev.off()
cat("Saved: figures/cooks_distance.png\n")

# =============================================================================
# SECTION 3: Linearity Check for Continuous Variables
# =============================================================================
lung$log_age <- log(lung$Age) * lung$Age
lung$log_tumor <- log(lung$Tumor_Size_cm + 1) * lung$Tumor_Size_cm

logit_box_tidwell <- glm(
  outcome ~ Age + log_age + Tumor_Size_cm + log_tumor +
    Gender + Smoking_Status + Cancer_Stage + Metastasis +
    Family_History + Chronic_Lung_Disease + Air_Pollution_Exposure,
  data = lung,
  family = binomial(link = "logit")
)

cat("\n=== Box-Tidwell Test (Linearity in Log-Odds) ===\n")
cat("Checking log_age and log_tumor interaction terms:\n")
bt_summary <- summary(logit_box_tidwell)
print(bt_summary$coefficients[c("log_age", "log_tumor"), ])
cat("(p > 0.05 means linearity assumption is met)\n")

cat("\nDiagnostics complete!\n")
