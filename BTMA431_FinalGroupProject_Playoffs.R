
library(dplyr)

# for loop to import all data files from memory

for(i in 2006:2023){
  x = paste0("C:/Users/ntruz/Downloads/data_NHL_", i, ".html.rda")
  load(x)
}

# cleaning up data 
# removing the final row 
# removing an unsed variables

stats_2006 = data_NHL_2006.html[-31, ]
stats_2006 = stats_2006 %>% select(3:32)
stats_2006 =  select(stats_2006, PTS | AvAge | GP | GF | GA | `S%`| `PK%`| 
                     `PP%` | `PIM/G` | `oPIM/G` | S | SA | `SV%`)

stats = select(stats_2006, PTS | AvAge | `PIM/G` | `oPIM/G` 
           |`SV%`)

# Calculate new metrics 
# Change percentages to decimals

`GD/G` = (as.numeric(stats_2006$GF) - as.numeric(stats_2006$GA)) / as.numeric(stats_2006$GP)
`S/G` = as.numeric(stats_2006$S) / as.numeric(stats_2006$GP)
`SA/G` = as.numeric(stats_2006$SA) / as.numeric(stats_2006$GP)
`SD/G` = `S/G` - `SA/G`
`S%` = as.numeric(stats_2006$`S%`) / 100
`PK%` = as.numeric(stats_2006$`PK%`) / 100
`PP%` = as.numeric(stats_2006$`PP%`) / 100

# appending new columns to the end of the data 

stats = cbind(stats, `GD/G`, `SD/G`, `S%`, `PK%`, `PP%`)

# this does the same as above for all years from 2007 till 2017
# making it all congruent and appending the row to the bottom of the file

for(i in 2007:2017){

  data = eval(str2lang(paste0("data_NHL_", i , ".html")))
    
  temp1 = data[-31, ]
  temp1 = temp1 %>% select(3:32)
  temp1 =  select(temp1, PTS | AvAge | GP | GF |GA | `PP%` | `PK%` | 
                         `PIM/G` | `oPIM/G` | S | `S%` | SA | `SV%`)
  
  temp2 = select(temp1, PTS | AvAge | `PIM/G` | `oPIM/G` | `SV%` )
  `GD/G` = (as.numeric(temp1$GF) - as.numeric(temp1$GA)) / as.numeric(temp1$GP)
  `S/G` = as.numeric(temp1$S) / as.numeric(temp1$GP)
  `SA/G` = as.numeric(temp1$SA) / as.numeric(temp1$GP)
  `SD/G` = `S/G` - `SA/G`
  `S%` = as.numeric(temp1$`S%`) / 100
  `PK%` = as.numeric(temp1$`PK%`) / 100
  `PP%` = as.numeric(temp1$`PP%`) / 100
  
  temp2 = cbind(temp2, `GD/G`, `SD/G`, `S%`, `PK%`, `PP%`)
  
  stats = rbind(stats, temp2)
  
}


# this does the same as above for all years from 2018 till 2021 after vegas was added to the league
# making it all congruent and appending the row to the bottom of the file


for(i in 2018:2021){
  
  data = eval(str2lang(paste0("data_NHL_", i , ".html")))
  
  temp1 = data[-32, ]
  temp1 = temp1 %>% select(3:32)
  temp1 =  select(temp1, PTS | AvAge | GP | GF | GA | `PP%` | `PK%` | 
                    `PIM/G` | `oPIM/G` | S | `S%` | SA | `SV%`)
  
  temp2 = select(temp1, PTS | AvAge | `PIM/G` | `oPIM/G`|`SV%`)
  `GD/G` = (as.numeric(temp1$GF) - as.numeric(temp1$GA)) / as.numeric(temp1$GP)
  `S/G` = as.numeric(temp1$S) / as.numeric(temp1$GP)
  `SA/G` = as.numeric(temp1$SA) / as.numeric(temp1$GP)
  `SD/G` = `S/G` - `SA/G`
  `S%` = as.numeric(temp1$`S%`) / 100
  `PK%` = as.numeric(temp1$`PK%`) / 100
  `PP%` = as.numeric(temp1$`PP%`) / 100
  
  temp2 = cbind(temp2, `GD/G`, `SD/G`, `S%`, `PK%`, `PP%`)
  
  stats = rbind(stats, temp2)
  
}


# this does the same as above for all years from 2022 till 2023 after Seattle was added to the league
# making it all congruent and appending the row to the bottom of the file


for(i in 2022:2023){
  
  data = eval(str2lang(paste0("data_NHL_", i , ".html")))
  
  temp1 = data[-33, ]
  temp1 = temp1 %>% select(3:32)
  temp1 =  select(temp1, PTS | AvAge | GP | GF |GA | `PP%` | `PK%` | 
                    `PIM/G` | `oPIM/G` | S | `S%` | SA | `SV%`)
  
  temp2 = select(temp1, PTS | AvAge | `PIM/G` | `oPIM/G` |`SV%`)
  `GD/G` = (as.numeric(temp1$GF) - as.numeric(temp1$GA)) / as.numeric(temp1$GP)
  `S/G` = as.numeric(temp1$S) / as.numeric(temp1$GP)
  `SA/G` = as.numeric(temp1$SA) / as.numeric(temp1$GP)
  `SD/G` = `S/G` - `SA/G`
  `S%` = as.numeric(temp1$`S%`) / 100
  `PK%` = as.numeric(temp1$`PK%`) / 100
  `PP%` = as.numeric(temp1$`PP%`) / 100
  
  temp2 = cbind(temp2, `GD/G`, `SD/G`, `S%`, `PK%`, `PP%`)
  
  stats = rbind(stats, temp2)
  
}

##############
#
#
# Changing from HTML to CSV to so that it can work for the linear regression
#
#
##############

write.csv(stats, "C:/Users/ntruz/Downloads/NHL_stats.csv", row.names = TRUE)
the_stats = read.csv("C:/Users/ntruz/Downloads/NHL_stats.csv")
the_stats = the_stats %>% select(2:11)

library(ggplot2)

# Checking correlations with the dependant variable

qplot(x = the_stats$AvAge, y = the_stats$PTS)
qplot(the_stats$PP., the_stats$PTS)
qplot(the_stats$PK., the_stats$PTS)
qplot(the_stats$PIM.G, the_stats$PTS)
qplot(the_stats$oPIM.G, the_stats$PTS)
qplot(the_stats$S., the_stats$PTS)
qplot(the_stats$SV., the_stats$PTS)
qplot(the_stats$GD.G, the_stats$PTS)
qplot(the_stats$SD.G, the_stats$PTS)

#
# Running the linear regresion on the newly created CSV file
#

point_predictor <- lm(PTS ~., the_stats)
summary(point_predictor)


#################
#
#
# Changing the stats from this year from HTML to CSV
#
#
#################

write.csv(data_NHL_2023.html, "C:/Users/ntruz/Downloads/This_Year_stats.csv", row.names = TRUE)
This_Year_stats = read.csv("C:/Users/ntruz/Downloads/This_Year_stats.csv")


# cleaning up data 
# removing the final row 
# removing an unsed variables

team_data = This_Year_stats[-31, ]
team_data = This_Year_stats %>% select(3:32)
team_data =  select(team_data, X.1 | PTS | AvAge | GP | GF | GA | S.| PK.| 
                       PP.| PIM.G | oPIM.G | S | SA | SV.)

# Calculate new metrics 
# Change percentages to decimals

team = select(team_data, PTS | AvAge | PIM.G | oPIM.G 
               |SV.)
GD.G = (as.numeric(team_data$GF) - as.numeric(team_data$GA)) / as.numeric(team_data$GP)
S.G = as.numeric(team_data$S) / as.numeric(team_data$GP)
SA.G = as.numeric(team_data$SA) / as.numeric(team_data$GP)
SD.G = S.G - SA.G
S. = as.numeric(team_data$S.) / 100
PK. = as.numeric(team_data$PK.) / 100
PP. = as.numeric(team_data$PP.) / 100

# appending new columns to the end of the data 

team = cbind(team, GD.G, SD.G, S., PK., PP.)

#
# For loop runs the regression model on all teams this year and calculate their stats
#

predictedStandings = data.frame(x = numeric(32), y = numeric(32))
for(i in 1:32){
  x = team[i, ]
  predictedStandings[[1]][[i]] = team_data[[1]][[i]]
  predictedStandings[[2]][[i]] = round(predict(point_predictor, x))
}


#Orders teams from 1 to 32 and writes it to a CSV file

predictedStandings = predictedStandings[order(predictedStandings$y, decreasing = TRUE),]

write.csv(predictedStandings, "C:/Users/ntruz/Downloads/PredictedStandings2.csv", row.names = FALSE)


####################################### 
###
# 
# 
#  Prediction who will win the Stanley cup 
# 
# 
###
#######################################



# This function take the probability that a given team will win and 
# simulates a game series comparing each number to the threshold and determinig which team will win
# and returns true if that team wins false if it loses

winLose <- function(teamprob){
  winDif = round(teamprob * 1000)
  underDog = 0
  fav = 0
  wins = sample(1:1000, 7)
  for(j in length(wins)){
    if(wins[j] > winDif){
      underDog = underDog + 1
    }
    if(wins[j] <= winDif){
      fav = fav + 1
    }
  }
  return(fav > underDog)
}


set.seed(5)


# this function will be run on all the rounds of the play off taking the number of match ups
# as well as a list of all the teams playing in the round as arguments
# it will then calculate the probability that a team will win and run the winLose funtion 
# it will then return a data frame with the remaining teams

roundPredict <- function(matchUpNum, matchUps){
  
  teamMatchUps = data.frame(fav = character(matchUpNum), favPT = numeric(matchUpNum), 
                            underDog = character(matchUpNum), underPT = numeric(matchUpNum), 
                            outCome = logical(matchUpNum))
  newStandings = data.frame(team_name = character(matchUpNum), teamPT = numeric(matchUpNum))
  
  j = 1
  k = matchUpNum * 2
  for(i in 1:matchUpNum){
    fav = matchUps[[2]][[j]]
    underDog = matchUps[[2]][[k]]
    prob = fav / (fav + underDog)
    teamMatchUps$fav[i] = matchUps[[1]][[j]]
    teamMatchUps$underDog[i] = matchUps[[1]][[k]]
    teamMatchUps$favPT[i] = fav
    teamMatchUps$underPT[i] = underDog
    teamMatchUps$outCome[i] = winLose(prob)
    j = j + 1
    k = k - 1
  }
  
  h = 1
  for(i in 1:NROW(teamMatchUps)){
    if(teamMatchUps$outCome[i]){
      newStandings$team_name[h] = teamMatchUps$fav[i] 
      newStandings$teamPT[h] = teamMatchUps$favPT[i]
      h = h + 1
    }else{
      newStandings$team_name[h] = teamMatchUps$underDog[i]
      newStandings$teamPT[h] = teamMatchUps$underPT[i]
      h = h + 1
    }
  }

  return(newStandings)  
}

# read the standings from this year
# and take the top 16 teams to play in the first round of playoffs

Standings = read.csv("C:/Users/ntruz/Downloads/PredictedStandings2.csv")
matchUps = Standings[c(1:16),]

# run the simulation on each round of the playoffs
round1 = roundPredict(8, matchUps)
round2 = roundPredict(4, round1)
round3 = roundPredict(2, round2)
round4 = roundPredict(1, round3)

# show which teams will move on

matchUps
round1
round2
round3

#
# at the end of round 4 we will have our 2023 Stanley Cup Champs!!!
#


round4


