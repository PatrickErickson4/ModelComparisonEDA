library(pROC)
library(ggplot2)


ts <- read.csv("Stat184ProjectTables/trainingset.csv")

game_decision <- ts$game_decision
points <- ts$points
field_goals<- ts$field_goals
field_goal_attempts<- ts$field_goal_attempts
field_goal_percentage<- ts$field_goal_percentage
`3pointers`<- ts$`3pointers`
`3pointer_attempts`<- ts$`3pointer_attempts`
`3pointer_percentage`<- ts$`3pointer_percentage`
free_throws<- ts$free_throws
free_throw_attempts<- ts$free_throw_attempts
free_throw_percentage<- ts$free_throw_percentage
offensive_rebounds<- ts$offensive_rebounds
total_rebounds<- ts$total_rebounds
assists<- ts$assists
steals<- ts$steals
blocks<- ts$blocks
turnovers<- ts$turnovers
personal_fouls<- ts$personal_fouls
Home<- ts$Home
Away<- ts$Away
Neutral<- ts$Neutral



naivemodel <- glm(game_decision ~ 
                    points +
                    field_goals +
                    field_goal_attempts +
                    field_goal_percentage +
                    `3pointers` +
                    `3pointer_attempts` +
                    `3pointer_percentage`+
                    free_throws+
                    free_throw_attempts+
                    free_throw_percentage+
                    offensive_rebounds+
                    total_rebounds+
                    assists+
                    steals+
                    blocks+
                    turnovers+
                    personal_fouls+
                    Home+
                    Away+
                    Neutral,
                  data = trainingmodel,
                  family = binomial
)


# Predict probabilities for the test set
naivemodel_probs <- predict(naivemodel, newdata = testset, type = "response")
parsimonious_probs <- predict(parsimoniousmodel, newdata = testset, type = "response")

naivemodel_auc <- auc(testset$game_decision, naivemodel_probs)
parsimonious_auc <- auc(testset$game_decision, parsimonious_probs)

naivemodel_roc <- roc(testset$game_decision, naivemodel_probs)
parsimonious_roc <- roc(testset$game_decision, parsimonious_probs)

# Data frame cast for correct ggplot usage
naivemodel_roc_df <- data.frame(
  FPR = 1 - naivemodel_roc$specificities,
  TPR = naivemodel_roc$sensitivities
)

parsimonious_roc_df <- data.frame(
  FPR = 1 - parsimonious_roc$specificities,
  TPR = parsimonious_roc$sensitivities
)

# naivemodel plot
roc_plot_naivemodel <- ggplot(naivemodel_roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "ROC Curve for Full Model",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  annotate("text", x = 0.7, y = 0.1, label = paste("AUC =", round(res_auc, 10)), size = 5, color = "blue")

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

ggsave("Stat184ProjectPictures/ROC_Naive.png", plot = roc_plot_naivemodel, width = 10, height = 8, dpi = 300)
ggsave("Stat184ProjectPictures/ROC_Parsimonious.png", plot = roc_plot_parsimonious, width = 10, height = 8, dpi = 300)  