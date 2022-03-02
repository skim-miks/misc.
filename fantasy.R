library(tidyverse)
library(dplyr)

seasons <- read.csv(file = "Seasons_Stats.csv", header=TRUE)
players <- read.csv(file = "player_data.csv", header=TRUE)

seasons$Player <- gsub("[*]", "", seasons$Player)

### create separate columns for height feet/inches ### Use when height is converted to date in excel ###
#players$feet <- sub("-.*", "", players$height)
#players$inches <- sub("*.-", "", players$height)
#players$inches <- (as.numeric(players$feet)*12) + as.numeric(players$inches)
#players <- players[,-9]

#write.csv(players, file="player_data.csv")

####
recent <- subset(seasons, seasons$Year >= 1985) 
rownames(recent) <- NULL
recent <- recent %>%
  left_join(players, by = "Player")

recent <- recent[!is.na(recent$year_start),]

pts = 1
reb = 1.2
ast = 1.5
blk = 3
stl = 3
tov = -1

recent$avgFantasy <- (recent$PTS + (recent$TRB*reb) + (recent$AST*ast) + (recent$BLK*blk) + (recent$STL*stl) + (recent$TOV*tov))/recent$G  
recent <- recent %>%
  mutate(Pts.gm = PTS/G,
         reb.gm = TRB/G,
         ast.gm = AST/G,
         blk.gm = BLK/G,
         stl.gm = STL/G,
         tov.gm = TOV/G) %>%
  select(-c(X.x, X.y, blanl, blank2, year_end, position, height))

recent$Traded <- 0
for(i in 1:(nrow(recent)-1)) {
  if(recent$Player[i] == recent$Player[i+1] && recent$Year[i] == recent$Year[i+1] && recent$Age[i] == recent$Age[i+1]) {
    recent$Traded[i+1] <- recent$Traded[i] + 1
  }
}

recent$YrsPlayed <- recent$Year - recent$year_start

top75 <- recent %>%
  group_by(Year) %>%
  top_n(75, wt=avgFantasy) %>%
  select(Year, Player, Pos, Age, YrsPlayed, avgFantasy, Pts.gm, reb.gm, ast.gm, blk.gm, stl.gm, tov.gm, MP, PER, TS., eFG., USG.) %>%
  arrange(Year, avgFantasy)
  
top100 <- recent %>%
  group_by(Year) %>%
  top_n(100, wt=avgFantasy) %>%
  select(c(Year, Player, Pos, Age, YrsPlayed, avgFantasy, Pts.gm, reb.gm, ast.gm, blk.gm, stl.gm, tov.gm, MP, PER, TS., USG.)) %>%
  arrange(Year, avgFantasy)

mean(top75$Age)

two10 <- subset(top75, top75$Year >= 2010)

moddat <- recent %>%
  select(avgFantasy, Age, MP, PER, TS., USG., inches, Traded, YrsPlayed) 
moddat <- na.omit(moddat)

library(caret)
set.seed(42)
train <- moddat
target <- train$avgFantasy

train$avgFantasy <- NULL
folds <- createFolds(target, k = 5, list=FALSE)

res <- rep(0, nrow(train))

for(this.round in 1:nrounds) {
  valid <- c(1:length(target)) [folds == this.round]
  dev <- c(1:length(target)) [folds != this.round]
  
  dtrain <- data.frame(target[dev], train[dev,])
  dvalid <- train[valid,]
  
  model <- lm(target.dev. ~ ., data=dtrain)
  
  pred <- predict(model, dvalid)
  res[valid] <- pred 
}
RMSE(target, res)
