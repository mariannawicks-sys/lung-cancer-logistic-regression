# =============================================================================
# 04_model_evaluation.R
# Lung Cancer Logistic Regression — ROC Curve & Model Evaluation
# Author: mariannawicks-sys
# =============================================================================

# Load libraries
library(dplyr)
library(ggplot2)
library(pROC)

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
# SECTION 1: Predicted Probabilities
# =============================================================================
lung$predicted_prob <- predict(logit_model, type = "response")

cat("=== Predicted Probability Summary ===\n")
print(summary(lung$predicted_prob))

# =============================================================================
# SECTION 2: ROC Curve and AUC
# =============================================================================
roc_obj <- roc(lung$outcome, lung$predicted_prob)
auc_val <- auc(roc_obj)
cat("\n=== AUC (Area Under the ROC Curve) ===\n")
cat("AUC:", round(auc_val, 3), "\n")

if (auc_val >= 0.9) {
  cat("Interpretation: Excellent discrimination\n")
} else if (auc_val >= 0.8) {
  cat("Interpretation: Good discrimination\n")
} else if (auc_val >= 0.7) {
  cat("Interpretation: Acceptable discrimination\n")
} else {
  cat("Interpretation: Poor discrimination\n")
}

# Plot ROC curve
png("figures/roc_curve.png", width = 800, height = 600)
plot(roc_obj,
     main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"),
     col = "#2196F3",
     lwd = 2,
     print.auc = TRUE,
     print.auc.x = 0.4,
     print.auc.y = 0.2)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()
cat("Saved: figures/roc_curve.png\n")

# =============================================================================
# SECTION 3: Odds Ratio Plot
# =============================================================================
or_data <- data.frame(
  Variable = names(coef(logit_model))[-1],
  OR = exp(coef(logit_model))[-1],
  Lower = exp(confint(logit_model))[-1, 1],
  Upper = exp(confint(logit_model))[-1, 2]
)

# Remove very wide CIs for cleaner plot
or_data <- or_data[or_data$Upper < 20, ]

p_or <- ggplot(or_data, aes(x = reorder(Variable, OR),
                             y = OR, ymin = Lower, ymax = Upper)) +
  geom_pointrange(color = "#2196F3", size = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Odds Ratios with 95% Confidence Intervals",
       x = "Variable", y = "Odds Ratio") +
  theme_minimal()

ggsave("figures/odds_ratio_plot.png", p_or, width = 10, height = 7, dpi = 300)
cat("Saved: figures/odds_ratio_plot.png\n")

# =============================================================================
# SECTION 4: Save Evaluation Results
# =============================================================================
sink("results/model_evaluation.txt")
cat("=== Model Evaluation ===\n\n")
cat("AUC:", round(auc_val, 3), "\n")
cat("AIC:", round(AIC(logit_model), 1), "\n")
cat("Null deviance:", round(logit_model$null.deviance, 1), "\n")
cat("Residual deviance:", round(logit_model$deviance, 1), "\n")
cat("McFadden R-squared:", 
    round(1 - logit_model$deviance/logit_model$null.deviance, 3), "\n")
sink()

cat("\nResults saved to results/model_evaluation.txt\n")
cat("Model evaluation complete!\n")
