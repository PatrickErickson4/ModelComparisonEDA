library(ggplot2)
library(pROC)

# Create random predictions and true labels for the random classifier
set.seed(112911)
n <- 1000
true_labels <- sample(c(0, 1), size = n, replace = TRUE)
random_predictions <- runif(n)  # Random predictions between 0 and 1

# Compute the ROC curve for random predictions
roc_random <- roc(true_labels, random_predictions)
auc_random <- auc(roc_random)

# Extract data for the random ROC curve
roc_data_random <- data.frame(
  specificity = rev(roc_random$specificities),  # X-axis (1 - specificity)
  sensitivity = rev(roc_random$sensitivities)   # Y-axis
)

# Create a perfect ROC curve matrix
roc_data_perfect <- data.frame(
  specificity = c(1, 1, 0),   
  sensitivity = c(0, 1, 1)
)
auc_perfect <- 1.0  # Perfect classifier has AUC = 1


# Combine the data and add a 'Type' column for ggplot
roc_data_random$Type <- "Random Classifier"
roc_data_perfect$Type <- "Perfect Classifier"
roc_data_combined <- rbind(roc_data_random, roc_data_perfect)

# Plot the ROC curves with AUC annotations
ggplot(roc_data_combined, aes(x = 1 - specificity, y = sensitivity, color = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Random Classifier" = "red", "Perfect Classifier" = "green")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1, show.legend = TRUE) +
  theme_minimal() +
  labs(
    title = "ROC Curves: Random Classifier vs Perfect Classifier",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Classifier Type"
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.2, 
    label = paste("Random AUC =", round(.489, 3)), 
    color = "red", 
    size = 5, 
    hjust = 0
  ) +
  annotate(
    "text",
    x = 0.5, y = 0.1, 
    label = paste("Perfect AUC =", round(auc_perfect, 10)), 
    color = "green", 
    size = 5, 
    hjust = 0
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )