testset <- read.csv("Stat184ProjectTables/testset.csv")

naivemodel_probs <- predict(naivemodel, newdata = testset, type = "response")
naivemodel_auc <- auc(testset$game_decision, naivemodel_probs)
naivemodel_roc <- roc(testset$game_decision, naivemodel_probs)

# Data frame cast for correct ggplot usage
naivemodel_roc_df <- data.frame(
  FPR = 1 - naivemodel_roc$specificities,
  TPR = naivemodel_roc$sensitivities
)

# naivemodel plot
roc_plot_naivemodel <- ggplot(naivemodel_roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(slope = 1,
              intercept = 0,
              linetype = "dashed",
              color = "black") +
  labs(
    title = "ROC Curve for Full Model",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  annotate("text", x = 0.7, y = 0.1, label = paste("AUC =", round(naivemodel_auc, 10)), size = 5, color = "blue")

roc_plot_naivemodel