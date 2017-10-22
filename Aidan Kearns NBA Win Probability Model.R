pbp = read.csv(file.choose(), as.is = TRUE)#Read in Play By Play
#convert time from MM:SS to decimal
playclock = pbp$play_clock
playclock = sapply(strsplit(playclock,":"),
                   function(x) {
                     x <- as.numeric(x)
                     x[1]+x[2]/60
                   }
)
pbp["play_clock_decimal"] = playclock

#break score from Home-Away into home and away 
home_score = pbp$score
home_score = sapply(strsplit(home_score," - "), 
                    function(x){
                      x <- as.numeric(x)
                      x = x[2] 
                    })
away_score = pbp$score
away_score = sapply(strsplit(away_score," - "), 
                    function(x){
                      x <- as.numeric(x)
                      x <-x[1]
                    })


#Fill in home and away score
for (i in 2:length(home_score)){
  if((is.na(home_score[i])) == TRUE){
    
    home_score[i] = home_score[(i-1)]
  }
}
for (i in 2:length(away_score)){
  if((is.na(away_score[i])) == TRUE){
    away_score[i] = away_score[(i-1)]
  }
}
pbp["home_score"] = home_score
pbp["away_score"] = away_score

#create point differential column 
point_differential = (home_score) - (away_score)
pbp["point_differential"] = point_differential


#Win or Loss Column for Classification
current_game = 1
game_result <- rep(0,length(pbp$game_id))
for (i in length(pbp$sequence_id):1){
  if (pbp$game_id[i] == pbp$game_id[current_game]) {
    game_result[i] = game_result[current_game]
  }
  else {
    if(pbp$point_differential[i] > 0){
      game_result[i] = 1
    }
    if(pbp$point_differential[i] < 0){
      game_result[i] = 0
    }
    current_game = i
  }
  
}
pbp["game_result"] = game_result

#Find Home team
number_of_entries = as.data.frame(table(pbp$game_id))
home_team = rep(0,length(pbp$sequence_id))
for(i in 2:length(pbp$sequence_id)){
  if(pbp$home_score[i] != pbp$home_score[(i-1)]){
    home_team[i] = pbp$player1_team[i]
  }
}
home_team = subset(home_team, home_team != "0")
home_team = subset(home_team, home_team != "")
c = rep(0,length(home_team))
c[1] = 1
for(i in 2:length(home_team)){
  if(home_team[i] == home_team[(i-1)]){
    c[i] = c[(i-1)] 
  }
  if(home_team[i] != home_team[(i-1)]){
    c[i] = i
  }
}
home_team = data.frame(c,home_team)
home_team = unique(home_team)
home_team$home_team = as.character(home_team$home_team)
home_team = home_team[,-1]
home.team = rep(home_team[1], number_of_entries$Freq[1])
for(i in 2:length(number_of_entries$Var1)){
  home.team = c(home.team, rep(home_team[i], number_of_entries$Freq[i]))
}
pbp$home_team = home.team
#Find Away Team
number_of_entries = as.data.frame(table(pbp$game_id))
away_team = rep(0,length(pbp$sequence_id))
for(i in 2:length(pbp$sequence_id)){
  if(pbp$away_score[i] != pbp$away_score[(i-1)]){
    away_team[i] = pbp$player1_team[i]
  }
}
away_team = subset(away_team, away_team != "0")
away_team = subset(away_team, away_team != "")
c = rep(0,length(away_team))
c[1] = 1
for(i in 2:length(away_team)){
  if(away_team[i] == away_team[(i-1)]){
    c[i] = c[(i-1)] 
  }
  if(away_team[i] != away_team[(i-1)]){
    c[i] = i
  }
}
away_team = data.frame(c,away_team)
away_team = unique(away_team)
away_team$away_team = as.character(away_team$away_team)
away_team = away_team[,-1]
away.team = rep(away_team[1], number_of_entries$Freq[1])
for(i in 2:length(number_of_entries$Var1)){
  away.team = c(away.team, rep(away_team[i], number_of_entries$Freq[i]))
}
pbp$away_team = away.team

#Possession for Each Entry Column
possession = rep(NA,length(pbp$sequence_id))
for(i in 1:length(pbp$sequence_id)){
  if(pbp$event_type[i] == "Made Shot " && pbp$home_team[i] == pbp$player1_team[i]){
    possession[i] = 0
  }
  if(pbp$event_type[i] == "Made Shot " && pbp$home_team[i] != pbp$player1_team[i]){
    possession[i] = 1
  }
  if(pbp$event_type[i] == "Missed Shot " && pbp$home_team[i] == pbp$player1_team[i]){
    possession[i] = 1
  }
  if(pbp$event_type[i] == "Missed Shot " && pbp$home_team[i] != pbp$player1_team[i]){
    possession[i] = 0
  }
  if(pbp$event_type[i] == "Free Throw " && pbp$home_team[i] == pbp$player1_team[i]){
    possession[i] = 1
  }
  if(pbp$event_type[i] == "Free Throw " && pbp$home_team[i] != pbp$player1_team[i]){
    possession[i] = 0
  }
  if(pbp$event_type[i] == "Rebound " && pbp$home_team[i] == pbp$player1_team[i]){
    possession[i] = 1
  }
  if(pbp$event_type[i] == "Rebound " && pbp$home_team[i] != pbp$player1_team[i]){
    possession[i] = 0
  }
  if(pbp$event_type[i] == "Jump Ball " && pbp$home_team[i] == pbp$player1_team[i]){
    possession[i] = 1
  }
  if(pbp$event_type[i] == "Jump Ball " && pbp$home_team[i] != pbp$player1_team[i]){
    possession[i] = 0
  }
  if(pbp$event_type[i] == "Turnover " && pbp$home_team[i] == pbp$player1_team[i]){
    possession[i] = 0
  }
  if(pbp$event_type[i] == "Turnover " && pbp$home_team[i] != pbp$player1_team[i]){
    possession[i] = 1
  }
  if(pbp$event_type[i] == "Foul " && pbp$event_description[i] == "Loose Ball " && pbp$home_team[i] == pbp$player1_team[i]){
    possession[i] = 0
  }
  if(pbp$event_type[i] == "Foul " && pbp$event_description[i] == "Loose Ball " && pbp$home_team[i] != pbp$player1_team[i]){
    possession[i] = 1
  }
  
}

for(i in 2:length(possession)){
  if(is.na(possession[i]) == TRUE && pbp$game_id[i] == pbp$game_id[(i-1)]){
    possession[i] = possession[(i-1)]
  }
}
for(i in 1:length(possession)){
  if(pbp$event_type[i] == "Start Period " && (pbp$event_type[(i+1)] == "Made Shot " || pbp$event_type[(i+1)] == "Turnover " || pbp$event_description[(i+1)] == "Loose Ball ")){
    if(possession[(i+1)] == 1){
      possession[i] = 0
    }
    else{
      possession[i] = 1
    }
  }
}
for(i in 1:length(possession)){
  if(pbp$event_type[i] == "Start Period " && is.na(possession[i]) == TRUE){
    possession[i] = possession[(i+1)]
  }
}

for(i in 1:length(possession)){
  if(is.na(possession[i]) == TRUE && is.na(possession[(i+1)]) == TRUE && is.na(possession[(i+2)]) == FALSE){
    possession[i] = possession[(i+2)]
    possession[i+1] = possession[(i+2)]
  }
  if(is.na(possession[i]) == TRUE && is.na(possession[(i+1)]) == TRUE && is.na(possession[(i+2)]) == TRUE && is.na(possession[(i+3)]) == FALSE){
    possession[i] = possession[(i+3)]
    possession[i+1] = possession[(i+3)]
    possession[i+2] = possession[(i+3)]
  }
  if(is.na(possession[i]) == TRUE && is.na(possession[(i+1)]) == TRUE && is.na(possession[(i+2)]) == TRUE && is.na(possession[(i+3)]) == TRUE && is.na(possession[i+4]) == FALSE){
    possession[i] = possession[(i+4)]
    possession[i+1] = possession[(i+4)]
    possession[i+2] = possession[(i+4)]
    possession[i+3] = possession[(i+4)]
  }
  if(is.na(possession[i]) == TRUE && is.na(possession[(i+1)]) == TRUE && is.na(possession[(i+2)]) == TRUE && is.na(possession[(i+3)]) == TRUE && is.na(possession[i+4]) == TRUE){
    possession[i] = possession[(i+5)]
    possession[i+1] = possession[(i+5)]
    possession[i+2] = possession[(i+5)]
    possession[i+3] = possession[(i+5)]
    possession[i+4] = possession[(i+5)]
  }
}
pbp$possession = possession

#Add in team Win Percentage
win_percentage = read.csv(file.choose())
win_percentage = win_percentage[,c(1,5)]
pbp$home_team = as.factor(pbp$home_team)
pbp$win_percentage = rep(0,length(pbp$sequence_id))
for(i in 1:length(pbp$home_team)){
  pbp$win_percentage[i] = win_percentage$Win.Percentage[pbp$home_team[i]]
}
pbp$away_team = as.factor(pbp$away_team)
pbp$win_percentage_away = rep(0,length(pbp$sequence_id))
for(i in 1:length(pbp$away_team)){
  pbp$win_percentage_away[i] = win_percentage$Win.Percentage[pbp$away_team[i]]
}
pbp$win_percentage_difference = ((pbp$win_percentage - pbp$win_percentage_away)*100)

#Add Entries for every 30 seconds for increased possession accuracy 
X_Data_New = data.frame(game_id = NA, period = NA, play_clock_decimal = NA, point_differential = NA, game_result = NA , possession = NA, win_percentage_difference = NA)
cur_time = 12.0
for(i in 1:(length(X_Data$game_id)-1)){
  if(cur_time == X_Data$play_clock_decimal[i] && cur_time != 0){
    cur_time = (cur_time - (1/2))
  }
  else if(X_Data$play_clock_decimal[i] < cur_time && cur_time != 0.0){
    newrow = data.frame(game_id = X_Data$game_id[i], period = X_Data$period[i], play_clock_decimal = cur_time, point_differential = NA, game_result = X_Data$game_result[i], possession = NA, win_percentage_difference = X_Data$win_percentage_difference[i])
    X_Data_New <- rbind(X_Data_New, newrow)
    cur_time = (cur_time - (1/2))
  }
  else if(X_Data$play_clock_decimal[i] == 0.0 && X_Data$game_id[i] == X_Data$game_id[i+1]){
    cur_time = 5.0
  }
  else if(X_Data$play_clock_decimal[i] == 0.0 && X_Data$game_id[i + 1] == X_Data$game_id[i] + 1){
    cur_time = 12.0
  }
  
}


X_Data = pbp[,c(2,3,19,22,23,26,29)]
X_Data = unique(X_Data)
rownames(X_Data) = 1:nrow(X_Data)
X_Data_New = X_Data_New[-1,]


X_Data = rbind(X_Data,X_Data_New)
X_Data <- X_Data[order(X_Data$game_id,X_Data$period,-(X_Data$play_clock_decimal)),]
rownames(X_Data) = 1:nrow(X_Data)

for(i in 2:length(X_Data$possession)){
  if(is.na(X_Data$possession[i]) == TRUE){
    X_Data$possession[i] = X_Data$possession[i-1]
  }
}
for(i in 2:length(X_Data$possession)){
  if(is.na(X_Data$point_differential[i]) == TRUE){
    X_Data$point_differential[i] = X_Data$point_differential[i-1]
  }
}

#logistic regression
set.seed(44)
X_Data_Log = X_Data[,-1]
#X_Data = unique(X_Data)
#X_Data = X_Data[,-1]
X_Data_Log$game_result = as.factor(X_Data$game_result)
X_Data_Log$possession = as.factor(X_Data$possession)
glm_fit = glm(game_result ~ . , data = X_Data_Log, family = "binomial")
glm_pred <- predict(glm_fit, type ="response")
glm.pred = rep(0,length(X_Data_Log))
for(i in 1:length(glm_pred)){
  if(glm_pred[i] > 0.5){
    glm.pred[i] = 1 
  }
  else {glm.pred[i] = 0
  }
}
y <- table(glm.pred, X_Data_Log$game_result)
y = as.data.frame(y)
print(y)
misclass_error_log = (y$Freq[1]+y$Freq[4])/(length(X_Data_Log$game_result)) 
print(misclass_error_log)
glm_fit$coefficients
win_probability = function(Quarter, Time, Point_Differential, Possession, Win_Percentage){
  x = 1/(1+exp(-(0.652030818+(Quarter*-0.142162518)+(0.006181817*Time)+(0.287000778*Point_Differential)+(0.224967369*Possession)+(0.030783363*Win_Percentage))))
  
  return (x)
}

#local regression
#install.packages("locfit")
require(locfit)
set.seed(77)
#X_Data = pbp[,c(2,3,19,22,23,25,29)]
#X_Data = unique(X_Data)
#row.names(X_Data) = 1:nrow(X_Data)
#X_Data = X_Data[,-1]
X_Data_Loc = X_Data_Log
X_Data_Loc$game_result = as.character(X_Data$game_result) 
X_Data_Loc$possession = as.factor(X_Data$possession)
locfit = locfit(game_result~lp(period,play_clock_decimal,point_differential,possession,win_percentage_difference), data = X_Data_Loc)
win_probability_local = function(Quarter, Time, Point_Differential, Possession, Win_Percentage_Difference){
  return (predict(locfit,data.frame(period = Quarter, play_clock_decimal = Time, point_differential = Point_Differential, possession = Possession, win_percentage_difference = Win_Percentage_Difference)))
}

predict_locfit = rep(0,length(X_Data$period))
for(i in 1:(length(X_Data$period))){
  predict_locfit[i] = win_probability_local(X_Data$period[i],X_Data$play_clock_decimal[i], X_Data$point_differential[i], X_Data$possession[i], X_Data$win_percentage_difference[i])
}
for(i in 1:length(predict_locfit)){
  if(predict_locfit[i] > 0.5){
    predict_locfit[i] = 1
  }
  else{
    predict_locfit[i] =0
  }
}
z <- table(predict_locfit, X_Data$game_result)
z = as.data.frame(z)
print(z)
misclass_error_loc = (z$Freq[1]+z$Freq[4])/(length(X_Data$game_result)) 
print(misclass_error_loc)

# Final Function
win_probability_local()
