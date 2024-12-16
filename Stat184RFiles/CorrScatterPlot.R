library(dplyr)
library(tidyr)
library(ggplot2)

basketball.data <- read.csv("Stat184ProjectTables/renamed_games.csv")

# most of this dataset is not in terms of numeric. We will generally have to turn it into numeric
# for certain visualizations
basketball.data <- basketball.data %>%
  mutate(
    points = as.numeric(as.character(points)),
    field_goals = as.numeric(as.character(field_goal_percentage))
  )

#construct scatterplot
ggplot(basketball.data, aes(x = points, y = field_goal_percentage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Scatterplot of Points vs Field Goal Percentages",
    x = "Points",
    y = "Field Goal Percentage"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_cartesian(xlim = c(0, 150), ylim = c(0, 1)) +  # Manually set bounds
  annotate(
    "text",
    x = 140,
    y = 0.9,
    label = paste(
      "Correlation: ", 
      round(cor(basketball.data$field_goal_percentage, basketball.data$points, use = "complete.obs"), 2)
    ),
    color = "red",
    hjust = 1
  )