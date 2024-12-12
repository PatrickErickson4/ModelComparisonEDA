randomvarregression <- glm(game_decision ~ 
                             points+
                             field_goals+
                             Home,
                           data = trainingmodel,
                           family = binomial)

summary(randomvarregression)
random_probs <- predict(randomvarregression, newdata = testset, type = "response")
random_auc <- auc(testset$game_decision, random_probs)
random_roc <- roc(testset$game_decision, random_probs)

random_roc_df <- data.frame(
  FPR = 1 - random_roc$specificities,
  TPR = random_roc$sensitivities
)

roc_plot_random <- ggplot(random_roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "ROC Curve for Simple Model",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  annotate("text", x = 0.7, y = 0.1, label = paste("AUC =", round(random_auc, 3)), size = 5, color = "blue")
ggsave("Stat184ProjectPictures/ROC_Simple.png", plot = roc_plot_random, width = 10, height = 8, dpi = 300)  