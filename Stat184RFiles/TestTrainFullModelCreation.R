library(tidyr)
library(ggplot2)
library(dplyr)

basketball_data <- read.csv("Stat184ProjectTables/games.csv")
basketball_data <- na.omit(basketball_data)


# Rename specific columns
basketball_data <- basketball_data %>%
  rename(
    game_decision = w_l,
    points = pts,
    opp_points = opp_pts,
    field_goals = fg,
    field_goal_attempts = fga,
    field_goal_percentage = fg_per,
    `3pointers` = `X3p`,
    `3pointer_attempts` = `X3pa`,
    `3pointer_percentage` = `X3p_per`,
    free_throws = ft,
    free_throw_attempts = fta,
    free_throw_percentage = ft_per,
    offensive_rebounds = orb,
    total_rebounds = trb,
    assists = ast,
    steals = stl,
    blocks = blk,
    turnovers = tov,
    personal_fouls = pf,
    opp_fieldgoals = opp_fg,
    opp_fieldgoal_attempts = opp_fga,
    opp_fieldgoal_percentage = opp_fg_per,
    `opp_3pointers` = `opp_3p`,
    `opp_3pointer_attempts`= `opp_3pa`,
    `opp_3pointer_percentage` = `opp_3p_per`,
    opp_freethrows = opp_ft,
    opp_freethrow_attempts = opp_ft_per,
    opp_offensive_rebounds = opp_orb,
    opp_assists = opp_ast,
    opp_steals = opp_stl,
    opp_blocks = opp_blk,
    opp_turnovers = opp_tov,
    opp_personal_fouls = opp_pf
  )

# From all number values, remove numbers < 0
basketball_data <- basketball_data %>%
  filter(if_all(everything(), ~ . >= 0))


# Make it so that the opposing team values are removed, and categorical variables that can't be 1-hot encoded are removed
basketball_data <- basketball_data %>%
  select(-game_num, -date, -contains("opp")) %>%
  filter(site %in% c("Home", "Away", "Neutral")) %>%
  mutate(
    Home = ifelse(site == "Home", 1, 0),
    Away = ifelse(site == "Away", 1, 0),
    Neutral = ifelse(site == "Neutral", 1, 0)
  ) %>%
  select(-site)  # Drop the original site column


fullmodel <- basketball_data %>%
  select(-team_name) %>%
  mutate(
    game_decision = ifelse(game_decision == "W", 1, 0)
  )
fullmodel <- fullmodel %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
fullmodel <- na.omit(fullmodel)

# Set a seed for reproducibility random sampling
set.seed(112911)
train_index <- sample(1:nrow(fullmodel), 0.8 * nrow(fullmodel))
trainingset <- fullmodel[train_index, ]  # 80% training data

testset <- fullmodel[-train_index, ] #20%
write.csv(fullmodel, "Stat184ProjectTables/fullmodel.csv", row.names = FALSE)
write.csv(trainingset,"Stat184ProjectTables/trainingset.csv",row.names = FALSE)
write.csv(testset,"Stat184ProjectTables/testset.csv",row.names = FALSE)