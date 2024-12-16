# It is necessary to have JVM 18.0.2.1 installed to be able to render this
# Neural Net correctly.
library(h2o)
library(ggplot2)

testset <- read.csv("Stat184ProjectTables/testset.csv")
trainingset <- read.csv("Stat184ProjectTables/trainingset.csv")

h2o.init()

trainingset$game_decision <- as.factor(trainingset$game_decision)
testset$game_decision <- as.factor(testset$game_decision)

training_h2o <- as.h2o(trainingset)
test_h2o <- as.h2o(testset)

predictors <- colnames(training_h2o)[-which(colnames(training_h2o) == "game_decision")]
target <- "game_decision"

##NN model with tuned hyperparameters after sweeps based on intuition
model <- h2o.deeplearning(
  x = predictors,
  y = target,
  training_frame = training_h2o,
  nfolds = 5,                  #This runs on VM, so can do cross-validation
  validation_frame = test_h2o,
  hidden = c(13),              # One hidden layer with 13 nodes
  epochs = 39,                 
  activation = "Rectifier",    # ReLU activation
  loss = "CrossEntropy",       
  stopping_metric = "logloss", 
  stopping_rounds = 3,         # Stop training if no improvement for 3 rounds
  stopping_tolerance = 0.001,  
  seed = 112911,              
)

perf <- h2o.performance(model, newdata = test_h2o)
# Make predictions on the test set
predictions <- h2o.predict(model, test_h2o)

nn_probs <- as.data.frame(predictions)$p1 

nn_true_labels <- as.vector(test_h2o$game_decision)  

# Compute AUC
nn_roc <- roc(nn_true_labels, nn_probs)
nn_auc <- auc(nn_roc)

# Generate ROC data for ggplot
nn_roc_df <- data.frame(
  FPR = 1 - nn_roc$specificities,  
  TPR = nn_roc$sensitivities       
)

nn_plot <- ggplot(nn_roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "ROC Curve for Neural Network",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  annotate(
    "text", x = 0.7, y = 0.2,
    label = paste("AUC =", round(nn_auc, 10)),
    size = 5, color = "blue"
  )

nn_plot