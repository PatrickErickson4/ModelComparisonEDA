library(dplyr)
library(tidyr)
library(ggplot2)

# Load the dataset
basketball.data <- read.csv("Stat184ProjectTables/renamed_games.csv")

# Convert w_l to numeric (1 for Win, 0 for Loss)
basketball.data <- basketball.data %>%
  mutate(
    game_decision = ifelse(game_decision == "W", 1,0)
  ) %>%
  na.omit(basketball.data)

# Select and summarize the data
grouped_data <- basketball.data %>%
  select(
    points,
    field_goals,
    field_goal_attempts,
    `X3pointers`,
    `X3pointer_attempts`,
    free_throws,
    free_throw_attempts,
    offensive_rebounds,
    total_rebounds,
    assists,
    steals,
    blocks,
    turnovers,
    personal_fouls,
    game_decision) %>%
  mutate(across(-game_decision, as.numeric)) %>%  # Convert all except w_l to numeric
  group_by(game_decision) %>%
  summarize(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(-game_decision, names_to = "Metric", values_to = "Average")

# Map w_l values to labels
grouped_data <- grouped_data %>%
  mutate(Outcome = ifelse(game_decision == 1, "Win", "Loss"))

# Create the bar plot
ggplot(grouped_data, aes(x = Metric, y = Average, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(
    title = "Average Metrics Grouped by Win/Loss",
    x = "Metrics",
    y = "Average Value",
    fill = "Outcome"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16)
  )