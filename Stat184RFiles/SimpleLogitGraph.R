library(ggplot2)
trainingset <- read.csv("Stat184ProjectTables/trainingset.csv")

#needed in order to vectorize the variables, otherwise glm will not work
points <- trainingset$points

#logistic regression model
singleregression <- glm(game_decision ~ 
                          points,
                        data = trainingset,
                        family = binomial)

#creation of dataframe with min and max values for a graph to plug into ggplot
points_seq <- seq(min(trainingset$points, na.rm = TRUE), max(trainingset$points, na.rm = TRUE), length.out = 100)

#exponentiate as talked about above
predicted_odds <- exp(predict(singleregression, newdata = data.frame(points = points_seq)))

#logit then convert to dataframe
predicted_probabilities <- predicted_odds / (1 + predicted_odds)
prediction_df <- data.frame(points = points_seq, probability = predicted_probabilities)

# Plot the change in odds
ggplot(prediction_df, aes(x = points, y = probability)) +
  geom_line(color = "blue", size = 1) +
  theme_minimal() +
  labs(
    title = "Change in Odds for unit increases of Points",
    x = "Points",
    y = "Probability of Winning"
  )