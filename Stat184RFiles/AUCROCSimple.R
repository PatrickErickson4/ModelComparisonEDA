library(ggplot2)
library(dplyr)

# calculate ROC and AUC values
testset <- read.csv("Stat184ProjectTables/testset.csv")
random_probs <- predict(singleregression, newdata = testset, type = "response")
random_auc <- auc(testset$game_decision, random_probs)
random_roc <- roc(testset$game_decision, random_probs)

#construct df for ggplot w values from prev
random_roc_df <- data.frame(
  FPR = 1 - random_roc$specificities,
  TPR = random_roc$sensitivities
)

ggplot(random_roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "ROC Curve for Simple Model",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  annotate("text", x = 0.7, y = 0.1, label = paste("AUC =", round(random_auc, 10)), size = 5, color = "blue")