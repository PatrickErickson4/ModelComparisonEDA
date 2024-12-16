library(tidyr)
library(ggplot2)
library(dplyr)

basketball_data <- read.csv("Stat184ProjectTables/renamed_games.csv")
basketball_data <- na.omit(basketball_data)

# turn our binary variable of interest to 1's and 0's
fullmodel <- basketball_data %>%
  select(-team_name,-date,-game_num) %>%
  mutate(
    game_decision = ifelse(game_decision == "W", 1, 0)
  )
fullmodel <- fullmodel %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# Set a seed for reproducibility random sampling
# We do NOT use k-fold due to computational limitations
set.seed(112911)
train_index <- sample(1:nrow(fullmodel), 0.8 * nrow(fullmodel))
trainingset <- fullmodel[train_index, ]  # 80% training data

#Create and SAVE test and training sets, as well as the full model for some comaparisons in the future
testset <- fullmodel[-train_index, ] #20%
write.csv(fullmodel, "Stat184ProjectTables/fullmodel.csv", row.names = FALSE)
write.csv(trainingset,"Stat184ProjectTables/trainingset.csv",row.names = FALSE)
write.csv(testset,"Stat184ProjectTables/testset.csv",row.names = FALSE)