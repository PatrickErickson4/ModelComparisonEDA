testset <- read.csv("Stat184ProjectTables/testset.csv")
parsimonious_probs <- predict(parsimoniousmodel, newdata = testset, type = "response")
parsimonious_auc <- auc(testset$game_decision, parsimonious_probs)
parsimonious_roc <- roc(testset$game_decision, parsimonious_probs)
parsimonious_roc_df <- data.frame(
  FPR = 1 - parsimonious_roc$specificities,
  TPR = parsimonious_roc$sensitivities
)
# BIC model plot
roc_plot_parsimonious <- ggplot(parsimonious_roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "red", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "ROC Curve for Parsimonious Model",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  annotate("text", x = 0.7, y = 0.1, label = paste("AUC =", round(parsimonious_auc, 10)), size = 5, color = "red")
roc_plot_parsimonious