library(pROC)
library(ggplot2)
ts <- read.csv("Stat184ProjectTables/trainingset.csv")


# need to vectorize all columns of the training set
# courtesy of Dr. Hyungusk Tak
game_decision <- ts$game_decision
points <- ts$points
field_goals<- ts$field_goals
field_goal_attempts<- ts$field_goal_attempts
field_goal_percentage<- ts$field_goal_percentage
`X3pointers`<- ts$`X3pointers`
`X3pointer_attempts`<- ts$`X3pointer_attempts`
`X3pointer_percentage`<- ts$`X3pointer_percentage`
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


#logisitic regression
naivemodel <- glm(game_decision ~ 
                    points +
                    field_goals +
                    field_goal_attempts +
                    field_goal_percentage +
                    `X3pointers` +
                    `X3pointer_attempts` +
                    `X3pointer_percentage`+
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
                  data = trainingset,
                  family = binomial
)

# Extract null and residual deviance
null_deviance <- naivemodel$null.deviance
residual_deviance <- naivemodel$deviance

# Compute degrees of freedom
df_null <- naivemodel$df.null
df_residual <- naivemodel$df.residual
df_diff <- df_null - df_residual

# Compute chi-squared statistic
chisq_stat <- null_deviance - residual_deviance

# Compute p-value
p_value <- 1 - pchisq(chisq_stat, df_diff)