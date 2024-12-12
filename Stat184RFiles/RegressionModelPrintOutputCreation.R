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

summary(naivemodel)
parsimoniousmodel <- step(naivemodel, k=log(nrow(fullmodel)))
summary(parsimoniousmodel)