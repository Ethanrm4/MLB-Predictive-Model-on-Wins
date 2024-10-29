library(Lahman)
library(dplyr)
library(ggplot2)
library(caret)

teams <- Teams
head(teams)

#Exploratory Data Analysis

teams <- teams %>%
  mutate(runs_game = R/(W+L))

head(teams)

#Average number of runs per game across all teams in a given year
teams_year <- teams %>%
  group_by(yearID) %>%
  summarise(mean_runs = mean(runs_game, na.rm = TRUE))

head(teams_year)

#Graphing the above dataset
teams_year %>%
  ggplot(aes(x=yearID,y=mean_runs))+
  geom_line() +
  geom_point() +
  ggtitle("Average MLB Runs by Year")

#From the graph above we can see overtime the average runs per game has decrease in which we can imply that pitchers have gotten better overtime

#Predictive Model to predict wins by team
df_clean <- teams %>%
  select(name, yearID, W, L, R, H, X2B, X3B, HR, SO, RA) %>%
  filter(yearID>=2010)

head(df_clean)

tail(df_clean)

#Linear Model 1
lm1 <- lm(W ~ R+H+X2B+X3B+HR+SO+RA, data = df_clean)
summary(lm1)

#Linear Model 2
lm2 <- lm(W ~ R+H+X3B+SO+RA, data = df_clean)
summary(lm2)

#Making Predictions on our dataset
pred <- predict(lm2,df_clean)

df_clean$pred <- pred

RMSE(df_clean$pred,df_clean$W)

df_clean$difference <- df_clean$W - df_clean$pred 

#Plot the information above
df_clean %>%
  ggplot(aes(pred,W))+
  geom_point()+
  geom_smooth() +
  ggtitle("Predicted Wins Versus Actual")

