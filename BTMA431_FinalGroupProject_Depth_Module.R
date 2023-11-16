# Depth Calculation Module

# simple function to convert the type of columns to numeric
Convert.Function <- function(Input.Column){
  Input.Column <- as.numeric(Input.Column)
}

# function to calculate depth rating, takes a filled in relative stats dataframe and weights for each
  # category as an input, calculating the depth rating as the sum of all relative stats * the respective weight
Depth.Rating.Function <- function(Relative.Stats.Dataframe,Goal.Weight,Assists.Weight,SOG.Weight,ATOI.Weight,Blocks.Weight,Hits.Weight){
  Depth.Rating.Vector <- Relative.Stats.Dataframe[2] * Goal.Weight + Relative.Stats.Dataframe[3] * Assists.Weight +
    Relative.Stats.Dataframe[4] * SOG.Weight + Relative.Stats.Dataframe[5] * ATOI.Weight + 
    Relative.Stats.Dataframe[6] * Blocks.Weight + Relative.Stats.Dataframe[7] * Hits.Weight
  return(Depth.Rating.Vector)
}

# all of the following functions until Full.Depth.Function are different methods of calculating depth,
  # and are used within the Full.Depth.Function when the corresponding "type" is input

# calculates the mean of each statistic for each team and the league
Mean.Stats.Calculate.Function <- function(Dataframe.Input,Stats.Dataframe,Team.Vec,Order.Vec){
  # determining statistics for each team,
  for (i in 2:length(Stats.Dataframe)){
    for (j in 1:length(Team.Vec)-1){
      Temp.Data <- Dataframe.Input %>% filter(Dataframe.Input[4] == Team.Vec[j])
      Stats.Dataframe[j,i] <- mean(Temp.Data[[Order.Vec[i]]])
    }
  }
  # determining statistics for the league as a whole
  for (i in 2:length(Stats.Dataframe)){
    Stats.Dataframe[33,i] <- mean(Dataframe.Input[[Order.Vec[i]]])
  }
  return(Stats.Dataframe)
}

# calculates the median of each statistic for each team and the league
Median.Stats.Calculate.Function <- function(Dataframe.Input,Stats.Dataframe,Team.Vec,Order.Vec){
  # determining statistics for each team
  for (i in 2:length(Stats.Dataframe)){
    for (j in 1:length(Team.Vec)-1){
      Temp.Data <- Dataframe.Input %>% filter(Dataframe.Input[4] == Team.Vec[j])
      Stats.Dataframe[j,i] <- median(Temp.Data[[Order.Vec[i]]])
    }
  }
  # determining statistics for the league as a whole
  for (i in 2:length(Stats.Dataframe)){
    Stats.Dataframe[33,i] <- median(Dataframe.Input[[Order.Vec[i]]])
  }
  return(Stats.Dataframe)
}

# calculates the median of each statistic for each team and the league, minus the standard deviation
  # of that statistic because more variation means less consistency between players, meaning less depth
Mean.Minus.SD.Stats.Calculate.Function <- function(Dataframe.Input,Stats.Dataframe,Team.Vec,Order.Vec){
  # determining statistics for each team
  for (i in 2:length(Stats.Dataframe)){
    for (j in 1:length(Team.Vec)-1){
      Temp.Data <- Dataframe.Input %>% filter(Dataframe.Input[4] == Team.Vec[j])
      Stats.Dataframe[j,i] <- mean(Temp.Data[[Order.Vec[i]]]) - sd(Temp.Data[[Order.Vec[i]]])
    }
  }
  # determining statistics for the league as a whole
  for (i in 2:length(Stats.Dataframe)){
    Stats.Dataframe[33,i] <- mean(Dataframe.Input[[Order.Vec[i]]]) - sd(Dataframe.Input[[Order.Vec[i]]])
  }
  return(Stats.Dataframe)
}

# calculates the median of each statistic for each team and the league, divided by the standard deviation
  # of that statistic because more variation means less consistency between players, meaning less depth
Mean.Ratio.SD.Stats.Calculate.Function <- function(Dataframe.Input,Stats.Dataframe,Team.Vec,Order.Vec){
  # determining statistics for each team
  for (i in 2:length(Stats.Dataframe)){
    for (j in 1:length(Team.Vec)-1){
      Temp.Data <- Dataframe.Input %>% filter(Dataframe.Input[4] == Team.Vec[j])
      Stats.Dataframe[j,i] <- mean(Temp.Data[[Order.Vec[i]]]) / sd(Temp.Data[[Order.Vec[i]]])
    }
  }
  # determining statistics for the league as a whole
  for (i in 2:length(Stats.Dataframe)){
    Stats.Dataframe[33,i] <- mean(Dataframe.Input[[Order.Vec[i]]]) / sd(Dataframe.Input[[Order.Vec[i]]])
  }
  return(Stats.Dataframe)
}

# function that uses each function defined above to calculate the depth of each team
  # takes Type, Skaters Data input, Minimum number of games played for consideration, and the weight for 
  # each category for use in the depth rating function
Full.Depth.Function <- function(Type,Dataframe.Input,Min.Games.Played,Goal.Weight,Assists.Weight,SOG.Weight,ATOI.Weight,Blocks.Weight,Hits.Weight){
  library(dplyr)
  # filter the input data to include only players with the min. GP and including only team specific data
  Dataframe.Input <- Dataframe.Input %>% filter(GP >= Min.Games.Played & Tm != 'TOT')
  # convert the necessary columns to type numeric
  Dataframe.Input <- data.frame(Dataframe.Input[1:5],sapply(Dataframe.Input[6:22],Convert.Function),Dataframe.Input[23],sapply(Dataframe.Input[24:27],Convert.Function))
  # recalculates average time on ice to avoid dealing with the provided minutes:seconds format
  Dataframe.Input[23] <- Dataframe.Input[22]/Dataframe.Input[6]
  # vector that stores the number of the columns with the desired stats, the beginning 0 is a place holder
  Order.Vec <- c(0,7,8,20,23,24,25)
  # list of team codes that appear in the dataset
  Team.Vec <- c('ANA','ARI','BOS','BUF','CAR','CBJ','CGY','CHI','COL','DAL','DET','EDM','FLA','LAK','MIN','MTL','NJD','NSH','NYI','NYR','OTT','PHI','PIT','SEA','SJS','STL','TBL','TOR','VAN','VEG','WPG','WSH','League')
  # define empty vectors to initialize the Stats data frame
  Goals.Vec <- numeric(33)
  Assists.Vec <- numeric(33)
  SOG.Vec <- numeric(33)
  ATOI.Vec <- numeric(33)
  Blocks.Vec <- numeric(33)
  Hits.Vec <- numeric(33)
  Stats.Dataframe <- data.frame(Team.Vec,Goals.Vec,Assists.Vec,SOG.Vec,ATOI.Vec,Blocks.Vec,Hits.Vec)
  # if statement to determine the type of calculation wanted, could be changed to do all of them and place
    # the results in separate columns
  if (Type == "Mean"){
    Stats.Dataframe <- Mean.Stats.Calculate.Function(Dataframe.Input,Stats.Dataframe,Team.Vec,Order.Vec)
  } else if (Type == "Median"){
    Stats.Dataframe <- Median.Stats.Calculate.Function(Dataframe.Input,Stats.Dataframe,Team.Vec,Order.Vec)
  } else if (Type == "MeanMinusSD"){
    Stats.Dataframe <- Mean.Minus.SD.Stats.Calculate.Function(Dataframe.Input,Stats.Dataframe,Team.Vec,Order.Vec)
  } else if (Type == "MeanRatioSD"){
    Stats.Dataframe <- Mean.Ratio.SD.Stats.Calculate.Function(Dataframe.Input,Stats.Dataframe,Team.Vec,Order.Vec)
  } else {
    break
  }
  # define relative stats data frame as the rows of stats.dataframe that contain teams and adds a depth
    # rating column
  Depth.Rating.Vector <- numeric(32)
  Relative.Stats.Dataframe <- data.frame(Stats.Dataframe[1:32,],Depth.Rating.Vector)
  # calculate the relative stats for each team & category by taking their stats values and dividing by
    # the value of the league for that stat
  for (i in 2:length(Stats.Dataframe)){
    for (j in 1:length(Team.Vec)-1){
      Relative.Stats.Dataframe[j,i] <- Stats.Dataframe[j,i] / Stats.Dataframe[33,i] * 100
    }
  }
  # calculate the 'total depth rating' using the depth rating function, filling in the column added above
  Relative.Stats.Dataframe[8] <- Depth.Rating.Function(Relative.Stats.Dataframe = Relative.Stats.Dataframe,Goal.Weight,Assists.Weight,SOG.Weight,ATOI.Weight,Blocks.Weight,Hits.Weight)
  # sort the data frame in decreasing order of depth rating
  Relative.Stats.Dataframe <- Relative.Stats.Dataframe[order(Relative.Stats.Dataframe[,8],decreasing = TRUE),]
  Depth.Ranking <- 1:32
  Relative.Stats.Dataframe <- data.frame(Relative.Stats.Dataframe,Depth.Ranking)
  return(Relative.Stats.Dataframe)
} 

# run the function using each type
DepthMean <- Full.Depth.Function("Mean",data_NHL_2023_skaters.html,40,0.2,0.2,0.2,0.2,0.1,0.1)
DepthMedian <- Full.Depth.Function("Median",data_NHL_2023_skaters.html,40,0.2,0.2,0.2,0.2,0.1,0.1)
DepthMeanMinusSD <- Full.Depth.Function("MeanMinusSD",data_NHL_2023_skaters.html,40,0.2,0.2,0.2,0.2,0.1,0.1)
DepthMeanRatioSD <- Full.Depth.Function("MeanRatioSD",data_NHL_2023_skaters.html,40,0.2,0.2,0.2,0.2,0.1,0.1)

# filter the data frame to exclude the league average row
data_NHL_2023.html <- data_NHL_2023.html[1:32,]
# reorder the data in alphabetical order
data_NHL_2023.html <- data_NHL_2023.html[order(data_NHL_2023.html[,2]),]
Team.Vec <- c('ANA','ARI','BOS','BUF','CGY','CAR','CHI','COL','CBJ','DAL','DET','EDM','FLA','LAK','MIN','MTL','NSH','NJD','NYI','NYR','OTT','PHI','PIT','SJS','SEA','STL','TBL','TOR','VAN','VEG','WSH','WPG')
# rename the team column to a form that matches that of the output of Full.Depth.Function
data_NHL_2023.html[2] <- Team.Vec
# reorder the data into alphabetical order, which changes slightly because of the differences between
  # full names and acronyms
data_NHL_2023.html <- data_NHL_2023.html[order(data_NHL_2023.html[,2]),]

# reorder the outputs of Full.Depth.Function into alphabetical order
DepthMean <- DepthMean[order(DepthMean[,1],decreasing = FALSE),]
DepthMedian <- DepthMedian[order(DepthMedian[,1],decreasing = FALSE),]
DepthMeanMinusSD <- DepthMeanMinusSD[order(DepthMeanMinusSD[,1],decreasing = FALSE),]
DepthMeanRatioSD <- DepthMeanRatioSD[order(DepthMeanRatioSD[,1],decreasing = FALSE),]

# add the actual ranking of each team to the outputs of Full.Depth.Function for graphing purposes
DepthMean <- data.frame(DepthMean,Actual.Ranking = as.numeric(data_NHL_2023.html[1:32,1]))
DepthMedian <- data.frame(DepthMedian,Actual.Ranking = as.numeric(data_NHL_2023.html[1:32,1]))
DepthMeanMinusSD <- data.frame(DepthMeanMinusSD,Actual.Ranking = as.numeric(data_NHL_2023.html[1:32,1]))
DepthMeanRatioSD <- data.frame(DepthMeanRatioSD,Actual.Ranking = as.numeric(data_NHL_2023.html[1:32,1]))

# create a new row that records the absolute difference between the depth ranking and actual ranking
DepthMean$Ranking.Dif <- abs(DepthMean[9]-as.numeric(DepthMean[[10]]))
DepthMedian$Ranking.Dif <- abs(DepthMedian[9]-as.numeric(DepthMedian[[10]]))
DepthMeanMinusSD$Ranking.Dif <- abs(DepthMeanMinusSD[9]-as.numeric(DepthMeanMinusSD[[10]]))
DepthMeanRatioSD$Ranking.Dif <- abs(DepthMeanRatioSD[9]-as.numeric(DepthMeanRatioSD[[10]]))

# create a unified data frame to show the rankings of the teams among all 4 methods, along with 
  # actual rankings for appendix purposes
Unified.Depth.Ranking.Dataframe <- data.frame(DepthMean[c(1,10,0)],DepthMean[9],DepthMedian[9],DepthMeanMinusSD[9],DepthMeanRatioSD[9])
colnames(Unified.Depth.Ranking.Dataframe) <- c("Team","Actual Ranking","Mean Ranking","Median Ranking","Mean Minus SD Ranking","Mean Divided by SD Ranking")

# Graphing Section for Depth Module
library(ggplot2)
# plot of with the teams on the x-axis and Rank on the y-axis, comparing the rankings of the Mean  (Red)
  # vs Median (Blue) methods
ggplot() + 
  geom_point(data = DepthMean, aes(x = Team.Vec, y = Depth.Rating.Vector), color = "red") +
  geom_point(data = DepthMedian, aes(x = Team.Vec, y = Depth.Rating.Vector), color = "blue") +
  xlab('Team') +
  ylab('Rank')

# plot of each of the Methods and their associated trendline, with actual ranking on the x-axis and
  # Depth Rating on the y-axis
ggplot() +
  geom_point(data = DepthMean, aes(x = Actual.Ranking, y = Depth.Rating.Vector), color = "red") +
  geom_smooth(data = DepthMean, method = 'lm', se = FALSE, aes(x = Actual.Ranking, y = Depth.Rating.Vector,color = 'Mean')) +
  geom_point(data = DepthMedian, aes(x = Actual.Ranking, y = Depth.Rating.Vector), color = "purple") +
  geom_smooth(data = DepthMedian, method = 'lm', se = FALSE, aes(x = Actual.Ranking, y = Depth.Rating.Vector,col = "Median")) +
  geom_point(data = DepthMeanMinusSD, aes(x = Actual.Ranking, y = Depth.Rating.Vector), color = "green") +
  geom_smooth(data = DepthMeanMinusSD, method = 'lm', se = FALSE, aes(x = Actual.Ranking, y = Depth.Rating.Vector,col = 'Mean - SD')) +
  geom_point(data = DepthMeanRatioSD, aes(x = Actual.Ranking, y = Depth.Rating.Vector), color = "blue") +
  geom_smooth(data = DepthMeanRatioSD, method = 'lm', se = FALSE, aes(x = Actual.Ranking, y = Depth.Rating.Vector,col = "Mean/SD")) +
  xlab('Actual Ranking') +
  ylab('Depth Rating')

# same as the plot above, but excluding the MeanMinusSD method, to better look at the results for the 
  # other three
ggplot() +
  geom_point(data = DepthMean, aes(x = Actual.Ranking, y = Depth.Rating.Vector), color = "red") +
  geom_smooth(data = DepthMean, method = 'lm', se = FALSE, aes(x = Actual.Ranking, y = Depth.Rating.Vector,color = 'Mean')) +
  geom_point(data = DepthMedian, aes(x = Actual.Ranking, y = Depth.Rating.Vector), color = "blue") +
  geom_smooth(data = DepthMedian, method = 'lm', se = FALSE, aes(x = Actual.Ranking, y = Depth.Rating.Vector,col = "Median")) +
  geom_point(data = DepthMeanRatioSD, aes(x = Actual.Ranking, y = Depth.Rating.Vector), color = "green") +
  geom_smooth(data = DepthMeanRatioSD, method = 'lm', se = FALSE, aes(x = Actual.Ranking, y = Depth.Rating.Vector,col = "Mean/SD")) +
  xlab('Actual Ranking') +
  ylab('Depth Rating')