# simple function to convert the type of columns to numeric
Convert.Function <- function(Input.Column){
  Input.Column <- as.numeric(Input.Column)
}
library(dplyr)
library("plyr")

#function to filter the championship teams from 2008 - 2022 into a separate data frame
Champion.function <- function(filename, team){
  output = filename %>%
    filter(filename$Tm == as.quoted(team))
  return(output)
}

#function to remove the championship teams from 2008 - 2022 into a seperate data frame
Rest.function <- function(filename, team){
  output = filename %>%
    filter(filename$Tm != as.quoted(team))
  return(output)
}

#list of all champions from 2008 - 2022
champs = c("DET", "PIT", "CHI", "BOS", "LAK", "CHI", "LAK", "CHI", "PIT", "PIT", "WHS", "STL", "TBL", "TBL", "COL")

#creating data frames containing just the champions for the playoffs
all_champs = Champion.function(ploffs_data_NHL_2008_skaters.html, champs[1])

for(i in 1 : 14){
  all_champs = rbind(all_champs, Champion.function(eval(str2lang(paste0("ploffs_data_NHL_", i + 2008, "_skaters.html"))), champs[i+1]))
}


#creating data frames containing just the champions for the regular season
all_champs_reg = Champion.function(data_NHL_2008_skaters.html, champs[1])

for(i in 1 : 14){
  all_champs_reg = rbind(all_champs_reg, Champion.function(eval(str2lang(paste0("data_NHL_", i + 2008, "_skaters.html"))), champs[i+1]))
}

#Removing all blank values in the block and shot percentage columns
for(i in 1 : nrow(all_champs)){
  if(all_champs$BLK[i] == ''){
    all_champs$BLK[i] = '0'
  }
  else if(all_champs$`S%`[i] == ""){
    all_champs$`S%`[i] = "0"
  }
}

for(i in 1 : nrow(all_champs_reg)){
  if(all_champs_reg$BLK[i] == ''){
    all_champs_reg$BLK[i] = '0'
  }
  else if(all_champs_reg$`S%`[i] == ""){
    all_champs_reg$`S%`[i] = "0"
  }
}

#creating data frames containing every team but the champions
all_rest = Rest.function(ploffs_data_NHL_2008_skaters.html, champs[1])

for(i in 1 : 14){
  all_rest = rbind(all_rest, Rest.function(eval(str2lang(paste0("ploffs_data_NHL_", i + 2008, "_skaters.html"))), champs[i+1]))
}

all_rest_reg = Rest.function(data_NHL_2008_skaters.html, champs[1])

for(i in 1 : 14){
  all_rest_reg = rbind(all_rest_reg, Rest.function(eval(str2lang(paste0("data_NHL_", i + 2008, "_skaters.html"))), champs[i+1]))
}


#Removing all blank values in the block and shot percentage columns
for(j in 1 : 2){
  for(i in 1 : nrow(all_rest)){
    if(all_rest$BLK[i] == ""){
      all_rest$BLK[i] = "0"
    }
    else if(all_rest$`S%`[i] == ""){
      all_rest$`S%`[i] = "0"
    }
  }
}

for(j in 1 : 2){
  for(i in 1 : nrow(all_rest_reg)){
    if(all_rest_reg$BLK[i] == ""){
      all_rest_reg$BLK[i] = "0"
    }
    else if(all_rest_reg$`S%`[i] == ""){
      all_rest_reg$`S%`[i] = "0"
    }
  }
}

#Creating a data frame for champions from 2008-2022 with averages per game for all the skaters
#on the roster

all_gamesplayed = Convert.Function(all_champs$GP)
champ_stats = data.frame(avg_age = mean(Convert.Function(all_champs$Age)),
                         avg_goals = mean(Convert.Function(all_champs$G) / all_gamesplayed),
                         avg_assists = mean(Convert.Function(all_champs$A) / all_gamesplayed),
                         avg_pts = mean(Convert.Function(all_champs$PTS) / all_gamesplayed),
                         avg_plus_minus = mean(Convert.Function(all_champs$`+/-`) / all_gamesplayed),
                         avg_penalty_minutes = mean(Convert.Function(all_champs$PIM) / all_gamesplayed),
                         avg_even_strength_goals = mean(Convert.Function(all_champs$EV) / all_gamesplayed),
                         avg_power_play_goals = mean(Convert.Function(all_champs$PP) / all_gamesplayed),
                         avg_shorthanded_goals = mean(Convert.Function(all_champs$SH) / all_gamesplayed),
                         avg_shots = mean(Convert.Function(all_champs$S) / all_gamesplayed),
                         avg_shooting_percent = mean(Convert.Function(all_champs$`S%`) / all_gamesplayed),
                         avg_hits = mean(Convert.Function(all_champs$HIT) / all_gamesplayed),
                         avg_blocks = mean(Convert.Function((all_champs$BLK)) / all_gamesplayed))

champ_stats_reg = data.frame(avg_age = mean(Convert.Function(all_champs_reg$Age)),
                         avg_goals = mean(Convert.Function(all_champs_reg$G)),
                         avg_assists =  mean(Convert.Function(all_champs_reg$A)),
                         avg_pts =  mean(Convert.Function(all_champs_reg$PTS)),
                         avg_plus_minus =  mean(Convert.Function(all_champs_reg$`+/-`)),
                         avg_penalty_minutes =  mean(Convert.Function(all_champs_reg$PIM)),
                         avg_even_strength_goals =  mean(Convert.Function(all_champs_reg$EV)),
                         avg_power_play_goals =  mean(Convert.Function(all_champs_reg$PP)),
                         avg_shorthanded_goals =  mean(Convert.Function(all_champs_reg$SH)),
                         avg_shots =  mean(Convert.Function(all_champs_reg$S)),
                         avg_shooting_percent =  mean(Convert.Function(all_champs_reg$`S%`)),
                         avg_hits =  mean(Convert.Function(all_champs_reg$HIT)),
                         avg_blocks =  mean(Convert.Function(all_champs_reg$BLK)))

#Creating a data frame containing the averages per game for the remaining 15 teams in the playoffs 
#from 2008 - 2022 who did not win the Stanley cup

rest_gamesplayed = Convert.Function(all_rest$GP)
rest_stats = data.frame(avg_age = mean(Convert.Function(all_rest$Age)),
                         avg_goals = mean(Convert.Function(all_rest$G) / rest_gamesplayed),
                         avg_assists = mean(Convert.Function(all_rest$A) / rest_gamesplayed),
                         avg_pts = mean(Convert.Function(all_rest$PTS) / rest_gamesplayed),
                         avg_plus_minus = mean(Convert.Function(all_rest$`+/-`) / rest_gamesplayed),
                         avg_penalty_minutes = mean(Convert.Function(all_rest$PIM) / rest_gamesplayed),
                         avg_even_strength_goals = mean(Convert.Function(all_rest$EV) / rest_gamesplayed),
                         avg_power_play_goals = mean(Convert.Function(all_rest$PP) / rest_gamesplayed),
                         avg_shorthanded_goals = mean(Convert.Function(all_rest$SH) / rest_gamesplayed),
                         avg_shots = mean(Convert.Function(all_rest$S) / rest_gamesplayed),
                         avg_shooting_percent = mean(Convert.Function(all_rest$`S%`) / rest_gamesplayed),
                         avg_hits = mean(Convert.Function(all_rest$HIT)/ rest_gamesplayed),
                         avg_blocks = mean(Convert.Function(all_rest$BLK) / rest_gamesplayed))

rest_stats_reg = data.frame(avg_age = mean(Convert.Function(all_rest_reg$Age)),
                             avg_goals = mean(Convert.Function(all_rest_reg$G)),
                             avg_assists =  mean(Convert.Function(all_rest_reg$A)),
                             avg_pts =  mean(Convert.Function(all_rest_reg$PTS)),
                             avg_plus_minus =  mean(Convert.Function(all_rest_reg$`+/-`)),
                             avg_penalty_minutes =  mean(Convert.Function(all_rest_reg$PIM)),
                             avg_even_strength_goals =  mean(Convert.Function(all_rest_reg$EV)),
                             avg_power_play_goals =  mean(Convert.Function(all_rest_reg$PP)),
                             avg_shorthanded_goals =  mean(Convert.Function(all_rest_reg$SH)),
                             avg_shots =  mean(Convert.Function(all_rest_reg$S)),
                             avg_shooting_percent =  mean(Convert.Function(all_rest_reg$`S%`)),
                             avg_hits =  mean(Convert.Function(all_rest_reg$HIT)),
                             avg_blocks =  mean(Convert.Function(all_rest_reg$BLK)))

#Difference between championship teams and the rest of the teams in the playoffs
dif = champ_stats - rest_stats
dif_reg = champ_stats_reg - rest_stats_reg

