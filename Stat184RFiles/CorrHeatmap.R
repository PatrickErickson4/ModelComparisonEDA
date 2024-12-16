library(dplyr)

basketball.data <- read.csv("Stat184ProjectTables/renamed_games.csv")

# Changes 0 and 1 so we can create some sort of numerical correlation coefficient
basketball.data <- basketball.data %>%
  mutate(game_decision = ifelse(game_decision == "W", 1, 0)) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# remove categorical variables
numeric_data <- basketball.data %>%
  select(-team_name, -game_num, -date) %>%
  select(where(is.numeric))


# Computing correlations
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
cor_data <- as.data.frame(as.table(cor_matrix))
colnames(cor_data) <- c("Var1", "Var2", "Correlation")

# Add a correlation greater than .95 or less than -.995
cor_data <- cor_data %>%
  mutate(Color = ifelse(abs(Correlation) >= 0.95, "black", NA))

#construct heatmap
ggplot(cor_data, aes(Var1, Var2, fill = Correlation)) +
  #baseline color is white
  geom_tile(color = "white") +
  # approaching -1 is blue, approaching 1 is red
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_tile(data = cor_data %>% filter(!is.na(Color)), aes(Var1, Var2), fill = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Correlation Heatmap of Game Statistics",
    x = "Variable 1",
    y = "Variable 2",
    fill = "Correlation"
  )