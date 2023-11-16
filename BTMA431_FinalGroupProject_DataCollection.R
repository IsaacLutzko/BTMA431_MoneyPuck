########################################################################################
# W23 BTMA 431
# BTMA431_FinalGroupProject_WebScrapingOutput.R
# Authors: Isaac Lutzko, Nolan Ruzicki, Karl Specht, Doug Strueby
# Instructor: Duy Dao
# Description: This program is meant to accomplish the task of gathering the data we need for our final project
#              via web scraping different web pages on the "https://www.hockey-reference.com/" website. This is
#              the data collection aspect of the project. This program will output multiple rda files that will 
#              then be used by our other .R files for the data visualization and data analysis aspects of our 
#              project. To run this program we simply selected "CTRL + ENTER" all the way through the program
#              (and followed any instructive comments throughout) to get the data and output the proper .rda files.
# Notes:       
# References:  For completing this program, the main reference I used was the course slides,
#              specifically the "Day6Slides_W2023_BTMA431" and the "Day7Slides_W2023_BTMA431" slides.
#              On top of that, there are also a number of web references I used that helped me with
#              certain aspects of the assignment. These web links that I used as references are as follows: 
#              - https://stackoverflow.com/questions/23209330/how-to-change-the-first-row-to-be-the-header-in-r
#              - https://www.statology.org/r-remove-character-from-string/
#              - https://stackoverflow.com/questions/20283624/removing-duplicate-words-in-a-string-in-r
#              - https://stackoverflow.com/questions/39420282/how-to-read-a-commented-out-html-table-using-readhtmltable-in-r
#              - https://www.tutorialkart.com/r-tutorial/r-check-if-specific-item-is-present-in-list/#:~:text=To%20check%20if%20specific%20item%20is%20present%20in,in%20the%20given%20list%2C%20or%20FALSE%20if%20not.
#              
#              It is also important to reiterate that the data we gathered by web scraping is from the 
#              "https://www.hockey-reference.com/" website so this site needs appropriate referencing. 
########################################################################################

# Load R Packages that are necessary and/or beneficial
install.packages("installr")
library(installr)
install.packages("plyr")
library("plyr")
install.packages('Rmpfr')
library('Rmpfr')
install.packages('lubridate')
library(lubridate)
install.packages('dplyr')
library(dplyr)
install.packages('tidyr')
library(tidyr)

install.packages('rvest')
library(rvest)
install.packages('XML')
library(XML)
install.packages('httr')
library(httr)

##################################################################

# setting working directory for myself for work below (commented out for myself) - can change
# the working directory path to set the working directory for yourself
setwd("C:/Users/lutzk/OneDrive/Desktop/W23 School/BTMA431")

# We decided to get NHL data for both the teams and for the individual players to answer our 
# project questions. The data we are going to gather are from the seasons 2005-2006 to 2022-2023.
# This is because the salary cap was introduced in the NHL for the 2005-2006 season. Many of our
# project questions are relevant to this current season (2022-2023), so by only looking at seasons
# with a salary cap set in place we hope to reduce some of the outliers that may exist from
# team data for pre-salary-cap era seasons. 
# (Note that pre-salary-cap seasons would mean certain teams may have spent a lot more on player's
# salary compared to another. Now in the salary cap era it is a little more fair because each team
# has an NHL standard salary cap on how much they can spend each year)
# 
# So, for each of these seasons we'll look at, we want to get the following data:
#   - for each team:
#     - general team summary data (wins, losses, ties, etc.)
#   - for each player:
#     - for each skater:
#       - basic statistics
#       - advanced statistics
#     - for each goalie:
#       - general goalie data

# main page URL: https://www.hockey-reference.com/

# general team summary data (example for 2023)
# https://www.hockey-reference.com/leagues/NHL_2023.html

# basic stats (example for 2023)
# https://www.hockey-reference.com/leagues/NHL_2023_skaters.html

# advanced stats (example for 2023) (note: not available for the 2005-2006 and 2006-2007 seasons)
# https://www.hockey-reference.com/leagues/NHL_2023_skaters-advanced.html

# goalie data (example for 2023)
# https://www.hockey-reference.com/leagues/NHL_2023_goalies.html

######################################################################## 

############################ Regular Season ############################ 
# first web scrape to get a list of all of the main page URLs of the seasons we 
# want (2005-2006 to 2022-2023)
mainPageURL <- "https://www.hockey-reference.com/leagues/"
links <- getHTMLLinks(htmlParse(GET(mainPageURL)))
thisYear <- year(today())
links_05_06 <- links[grepl(paste0(2005:thisYear, collapse = "|"), links)]
links_leagues_05_06 <- links_05_06[grepl(paste0('/leagues/NHL_', collapse = "|"), links_05_06)]

editedURL <- gsub('/leagues/', '', mainPageURL)
mainPageURL_plus_endpoint <- paste0(editedURL, links_leagues_05_06)

links_seasonURLs <- paste0(unique(mainPageURL_plus_endpoint), sep = "") # remove duplicates
links_seasonURLs <- head(links_seasonURLs, -4) # remove the URL's from the list we don't need

# iterate through each of these main links to create list of more specific links for each season
for (i in 1:length(links_seasonURLs)) {
  link_season <- gsub('.html', '', links_seasonURLs[i])

  if (grepl('2006|2007', link_season)) { # 2005-06 and 2006-07 season does not have 'skaters-advanced' data
    link_season_cleaning <- c(paste0(link_season, ".html"), paste0(link_season, "_skaters.html"),
                              paste0(link_season, "_goalies.html"))
  }
  else {
    link_season_cleaning <- c(paste0(link_season, ".html"), paste0(link_season, "_skaters.html"),
                              paste0(link_season, "_skaters-advanced.html"), paste0(link_season, "_goalies.html"))
  }
  
  assign(paste('linked_', 2024-i, sep=''), link_season_cleaning)
}

# put together list that contains all of the regular season URL's to iterate through
linked_seasons <- c(linked_2006, linked_2007, linked_2008, linked_2009, linked_2010, linked_2011,
                    linked_2012, linked_2013, linked_2014, linked_2015, linked_2016, linked_2017,
                    linked_2018, linked_2019, linked_2020, linked_2021, linked_2022, linked_2023)

# for each link in each of the lists created above, create data frames that contain our desired data for each season
for (j in 1:length(linked_seasons)) {
    url <- linked_seasons[j]
    scraped_html <- read_html(url)
    
    # throughout the web scraping process, we realized that in certain html elements (specifically 
    # certain tables in this case) are commented out and this can be seen by inspecting the web page.
    # We can get the 'skaters', 'goalies', and 'skaters-advanced' data tables using 
    # ' html_nodes(scraped_html, "table") ' because they are not commented out in the web pages, but
    # to get the team statistics table for each season, we need to use the code block that is in
    # the if-statement if the condition passes. The team statistics table is found in each of the 
    # main season web pages (e.g., "https://www.hockey-reference.com/leagues/NHL_2018.html"), so
    # if we come across one of those types of URL's throughout this for loop, we are still able to get
    # the team statistics table as needed. To do this, we used the following web page as reference:
    # https://stackoverflow.com/questions/39420282/how-to-read-a-commented-out-html-table-using-readhtmltable-in-r
    if (url %in% links_seasonURLs) {
          Table.Scraped <- html_nodes(scraped_html, "#all_stats") %>% # div with #all_stats id has team stats table within
            html_nodes(xpath = 'comment()') %>%   # select the commented out nodes (one being the team stats table)
            html_text() %>%                       # return the above node as text
            read_html() %>%                       # parse that text as html
            html_node('table')                    # get the team team stats table
    } else {
      Table.Scraped <- html_nodes(scraped_html, "table")
    }

    Scraped.data <- as.data.frame(html_table(Table.Scraped, header = TRUE))
    names(Scraped.data) <- Scraped.data[1,]
    Scraped.data <- Scraped.data[-1,]
    
    # data cleaning step --> some rows still contain header titles throughout the data frames
    if (grepl("goalie", url) | grepl("skater", url))
      Scraped.data <- subset(Scraped.data, Scraped.data$Player != 'Player')

    assign(paste('data_', gsub('https://www.hockey-reference.com/leagues/|-', '', url), sep=''), Scraped.data)

    Sys.sleep(3)  # due to HTTP request limitations, sleep briefly so there aren't too many requests all at once
}
######################################################################## 


############################# PLAYOFFS ################################# 
# first web scrape to get a list of all of the main page URLs of the playoffs for the
# seasons we want (2005-2006 to 2022-2023)
mainPlayoffPageURL <- "https://www.hockey-reference.com/playoffs/"
links <- getHTMLLinks(htmlParse(GET(mainPlayoffPageURL)))
links_05_06 <- links[grepl(paste0(2005:thisYear, collapse = "|"), links)]
links_leagues_playoffs_05_06 <- links_05_06[grepl(paste0('/playoffs/NHL_', collapse = "|"), links_05_06)]

editedPlayoffURL <- gsub('/playoffs/', '', mainPlayoffPageURL)
mainPlayoffPageURL_plus_endpoint <- paste0(editedPlayoffURL, links_leagues_playoffs_05_06)

links_playoffURLs <- paste0(unique(mainPlayoffPageURL_plus_endpoint), sep = "") # remove duplicates

# iterate through each of these main links to create list of more specific links for each season
for (i in 1:length(links_playoffURLs)) {
  link_season <- gsub('.html', '', links_playoffURLs[i])
  
  if (grepl('2006|2007', link_season)) { # 2005-06 and 2006-07 season does not have 'skaters-advanced' data
    link_season_cleaning <- c(paste0(link_season, ".html"), paste0(link_season, "_skaters.html"),
                              paste0(link_season, "_goalies.html"))
  }
  else {
    link_season_cleaning <- c(paste0(link_season, ".html"), paste0(link_season, "_skaters.html"),
                              paste0(link_season, "_skaters-advanced.html"), paste0(link_season, "_goalies.html"))
  }
  
  assign(paste('linked_ploffs_', 2024-i, sep=''), link_season_cleaning)
}

# put together list that contains all of the playoffs URL's to iterate through
linked_ploffs_seasons <- c(linked_ploffs_2006, linked_ploffs_2007, linked_ploffs_2008, linked_ploffs_2009, linked_ploffs_2010, linked_ploffs_2011,
                    linked_ploffs_2012, linked_ploffs_2013, linked_ploffs_2014, linked_ploffs_2015, linked_ploffs_2016, linked_ploffs_2017,
                    linked_ploffs_2018, linked_ploffs_2019, linked_ploffs_2020, linked_ploffs_2021, linked_ploffs_2022)

# for each link in each of the lists created above, create data frames that contain our desired data for each season
for (m in 1:length(linked_ploffs_seasons)) {
  url <- linked_ploffs_seasons[m]
  scraped_html <- read_html(url)
  
  # team stats are on the main page which contains many tables to gather.
  # however, for the html_table() function and the as.data.frame() function
  # we need only one table, not multiple node sets. therefore we add in the
  # #all_teams id which is an attribute of the div tag that contains the team
  # statistics table. in the if statement we gather the team data there, but for
  # the skater or goalie data we do that through the line that is in the else statement.
  if (url %in% links_playoffURLs) {
    Table.Scraped <- html_nodes(scraped_html, "#all_teams table")
  } 
  else {
    Table.Scraped <- html_nodes(scraped_html, "table")
  }
    
  Scraped.data <- as.data.frame(html_table(Table.Scraped, header = TRUE))
  names(Scraped.data) <- Scraped.data[1,]
  Scraped.data <- Scraped.data[-1,]
  
  # data cleaning step --> some rows still contain header titles throughout the data frames
  if (grepl("goalie", url) | grepl("skater", url))
    Scraped.data <- subset(Scraped.data, Scraped.data$Player != 'Player')
  
  assign(paste('ploffs_data_', gsub('https://www.hockey-reference.com/playoffs/|-', '', url), sep=''), Scraped.data)
  
  Sys.sleep(3)  # due to HTTP request limitations, sleep briefly so there aren't too many requests all at once
}
######################################################################## 

# set new folder for data files (regular season) to the working directory you are currently in.
#
# However, to run our other 3 files, it is not necessary to run the rest of the below code to
# save our data frames to .rda files. With that being said, it could be useful for any other
# files or analysis.

# save(data_NHL_2006.html, file="data_NHL_2006.html.rda")
# save(data_NHL_2006_skaters.html, file="data_NHL_2006_skaters.html.rda")
# save(data_NHL_2006_goalies.html, file="data_NHL_2006_goalies.html.rda")
# 
# save(data_NHL_2007.html, file="data_NHL_2007.html.rda")
# save(data_NHL_2007_skaters.html, file="data_NHL_2007_skaters.html.rda")
# save(data_NHL_2007_goalies.html, file="data_NHL_2007_goalies.html.rda")
# 
# save(data_NHL_2008.html, file="data_NHL_2008.html.rda")
# save(data_NHL_2008_skaters.html, file="data_NHL_2008_skaters.html.rda")
# save(data_NHL_2008_skatersadvanced.html, file="data_NHL_2008_skatersadvanced.html.rda")
# save(data_NHL_2008_goalies.html, file="data_NHL_2008_goalies.html.rda")
# 
# save(data_NHL_2009.html, file="data_NHL_2009.html.rda")
# save(data_NHL_2009_skaters.html, file="data_NHL_2009_skaters.html.rda")
# save(data_NHL_2009_skatersadvanced.html, file="data_NHL_2009_skatersadvanced.html.rda")
# save(data_NHL_2009_goalies.html, file="data_NHL_2009_goalies.html.rda")
# 
# save(data_NHL_2010.html, file="data_NHL_2010.html.rda")
# save(data_NHL_2010_skaters.html, file="data_NHL_2010_skaters.html.rda")
# save(data_NHL_2010_skatersadvanced.html, file="data_NHL_2010_skatersadvanced.html.rda")
# save(data_NHL_2010_goalies.html, file="data_NHL_2010_goalies.html.rda")
# 
# save(data_NHL_2011.html, file="data_NHL_2011.html.rda")
# save(data_NHL_2011_skaters.html, file="data_NHL_2011_skaters.html.rda")
# save(data_NHL_2011_skatersadvanced.html, file="data_NHL_2011_skatersadvanced.html.rda")
# save(data_NHL_2011_goalies.html, file="data_NHL_2011_goalies.html.rda")
# 
# save(data_NHL_2012.html, file="data_NHL_2012.html.rda")
# save(data_NHL_2012_skaters.html, file="data_NHL_2012_skaters.html.rda")
# save(data_NHL_2012_skatersadvanced.html, file="data_NHL_2012_skatersadvanced.html.rda")
# save(data_NHL_2012_goalies.html, file="data_NHL_2012_goalies.html.rda")
# 
# save(data_NHL_2013.html, file="data_NHL_2013.html.rda")
# save(data_NHL_2013_skaters.html, file="data_NHL_2013_skaters.html.rda")
# save(data_NHL_2013_skatersadvanced.html, file="data_NHL_2013_skatersadvanced.html.rda")
# save(data_NHL_2013_goalies.html, file="data_NHL_2013_goalies.html.rda")
# 
# save(data_NHL_2014.html, file="data_NHL_2014.html.rda")
# save(data_NHL_2014_skaters.html, file="data_NHL_2014_skaters.html.rda")
# save(data_NHL_2014_skatersadvanced.html, file="data_NHL_2014_skatersadvanced.html.rda")
# save(data_NHL_2014_goalies.html, file="data_NHL_2014_goalies.html.rda")
# 
# save(data_NHL_2015.html, file="data_NHL_2015.html.rda")
# save(data_NHL_2015_skaters.html, file="data_NHL_2015_skaters.html.rda")
# save(data_NHL_2015_skatersadvanced.html, file="data_NHL_2015_skatersadvanced.html.rda")
# save(data_NHL_2015_goalies.html, file="data_NHL_2015_goalies.html.rda")
# 
# save(data_NHL_2016.html, file="data_NHL_2016.html.rda")
# save(data_NHL_2016_skaters.html, file="data_NHL_2016_skaters.html.rda")
# save(data_NHL_2016_skatersadvanced.html, file="data_NHL_2016_skatersadvanced.html.rda")
# save(data_NHL_2016_goalies.html, file="data_NHL_2016_goalies.html.rda")
# 
# save(data_NHL_2017.html, file="data_NHL_2017.html.rda")
# save(data_NHL_2017_skaters.html, file="data_NHL_2017_skaters.html.rda")
# save(data_NHL_2017_skatersadvanced.html, file="data_NHL_2017_skatersadvanced.html.rda")
# save(data_NHL_2017_goalies.html, file="data_NHL_2017_goalies.html.rda")
# 
# save(data_NHL_2018.html, file="data_NHL_2018.html.rda")
# save(data_NHL_2018_skaters.html, file="data_NHL_2018_skaters.html.rda")
# save(data_NHL_2018_skatersadvanced.html, file="data_NHL_2018_skatersadvanced.html.rda")
# save(data_NHL_2018_goalies.html, file="data_NHL_2018_goalies.html.rda")
# 
# save(data_NHL_2019.html, file="data_NHL_2019.html.rda")
# save(data_NHL_2019_skaters.html, file="data_NHL_2019_skaters.html.rda")
# save(data_NHL_2019_skatersadvanced.html, file="data_NHL_2019_skatersadvanced.html.rda")
# save(data_NHL_2019_goalies.html, file="data_NHL_2019_goalies.html.rda")
# 
# save(data_NHL_2020.html, file="data_NHL_2020.html.rda")
# save(data_NHL_2020_skaters.html, file="data_NHL_2020_skaters.html.rda")
# save(data_NHL_2020_skatersadvanced.html, file="data_NHL_2020_skatersadvanced.html.rda")
# save(data_NHL_2020_goalies.html, file="data_NHL_2020_goalies.html.rda")
# 
# save(data_NHL_2021.html, file="data_NHL_2021.html.rda")
# save(data_NHL_2021_skaters.html, file="data_NHL_2021_skaters.html.rda")
# save(data_NHL_2021_skatersadvanced.html, file="data_NHL_2021_skatersadvanced.html.rda")
# save(data_NHL_2021_goalies.html, file="data_NHL_2021_goalies.html.rda")
# 
# save(data_NHL_2022.html, file="data_NHL_2022.html.rda")
# save(data_NHL_2022_skaters.html, file="data_NHL_2022_skaters.html.rda")
# save(data_NHL_2022_skatersadvanced.html, file="data_NHL_2022_skatersadvanced.html.rda")
# save(data_NHL_2022_goalies.html, file="data_NHL_2022_goalies.html.rda")
# 
# save(data_NHL_2023.html, file="data_NHL_2023.html.rda")
# save(data_NHL_2023_skaters.html, file="data_NHL_2023_skaters.html.rda")
# save(data_NHL_2023_skatersadvanced.html, file="data_NHL_2023_skatersadvanced.html.rda")
# save(data_NHL_2023_goalies.html, file="data_NHL_2023_goalies.html.rda")

# set new folder for data files (playoffs) to the working directory you are currently in.
#
# However, to run our other 3 files, it is not necessary to run the rest of the below code to
# save our data frames to .rda files. With that being said, it could be useful for any other
# files or analysis.

# save(data_NHL_2006.html, file="ploffs_data_NHL_2006.html.rda")
# save(data_NHL_2006_skaters.html, file="ploffs_data_NHL_2006_skaters.html.rda")
# save(data_NHL_2006_goalies.html, file="ploffs_data_NHL_2006_goalies.html.rda")
# 
# save(data_NHL_2007.html, file="ploffs_data_NHL_2007.html.rda")
# save(data_NHL_2007_skaters.html, file="ploffs_data_NHL_2007_skaters.html.rda")
# save(data_NHL_2007_goalies.html, file="ploffs_data_NHL_2007_goalies.html.rda")
# 
# save(data_NHL_2008.html, file="ploffs_data_NHL_2008.html.rda")
# save(data_NHL_2008_skaters.html, file="ploffs_data_NHL_2008_skaters.html.rda")
# save(data_NHL_2008_skatersadvanced.html, file="ploffs_data_NHL_2008_skatersadvanced.html.rda")
# save(data_NHL_2008_goalies.html, file="ploffs_data_NHL_2008_goalies.html.rda")
# 
# save(data_NHL_2009.html, file="ploffs_data_NHL_2009.html.rda")
# save(data_NHL_2009_skaters.html, file="ploffs_data_NHL_2009_skaters.html.rda")
# save(data_NHL_2009_skatersadvanced.html, file="ploffs_data_NHL_2009_skatersadvanced.html.rda")
# save(data_NHL_2009_goalies.html, file="ploffs_data_NHL_2009_goalies.html.rda")
# 
# save(data_NHL_2010.html, file="ploffs_data_NHL_2010.html.rda")
# save(data_NHL_2010_skaters.html, file="ploffs_data_NHL_2010_skaters.html.rda")
# save(data_NHL_2010_skatersadvanced.html, file="ploffs_data_NHL_2010_skatersadvanced.html.rda")
# save(data_NHL_2010_goalies.html, file="ploffs_data_NHL_2010_goalies.html.rda")
# 
# save(data_NHL_2011.html, file="ploffs_data_NHL_2011.html.rda")
# save(data_NHL_2011_skaters.html, file="ploffs_data_NHL_2011_skaters.html.rda")
# save(data_NHL_2011_skatersadvanced.html, file="ploffs_data_NHL_2011_skatersadvanced.html.rda")
# save(data_NHL_2011_goalies.html, file="ploffs_data_NHL_2011_goalies.html.rda")
# 
# save(data_NHL_2012.html, file="ploffs_data_NHL_2012.html.rda")
# save(data_NHL_2012_skaters.html, file="ploffs_data_NHL_2012_skaters.html.rda")
# save(data_NHL_2012_skatersadvanced.html, file="ploffs_data_NHL_2012_skatersadvanced.html.rda")
# save(data_NHL_2012_goalies.html, file="ploffs_data_NHL_2012_goalies.html.rda")
# 
# save(data_NHL_2013.html, file="ploffs_data_NHL_2013.html.rda")
# save(data_NHL_2013_skaters.html, file="ploffs_data_NHL_2013_skaters.html.rda")
# save(data_NHL_2013_skatersadvanced.html, file="ploffs_data_NHL_2013_skatersadvanced.html.rda")
# save(data_NHL_2013_goalies.html, file="ploffs_data_NHL_2013_goalies.html.rda")
# 
# save(data_NHL_2014.html, file="ploffs_data_NHL_2014.html.rda")
# save(data_NHL_2014_skaters.html, file="ploffs_data_NHL_2014_skaters.html.rda")
# save(data_NHL_2014_skatersadvanced.html, file="ploffs_data_NHL_2014_skatersadvanced.html.rda")
# save(data_NHL_2014_goalies.html, file="ploffs_data_NHL_2014_goalies.html.rda")
# 
# save(data_NHL_2015.html, file="ploffs_data_NHL_2015.html.rda")
# save(data_NHL_2015_skaters.html, file="ploffs_data_NHL_2015_skaters.html.rda")
# save(data_NHL_2015_skatersadvanced.html, file="ploffs_data_NHL_2015_skatersadvanced.html.rda")
# save(data_NHL_2015_goalies.html, file="ploffs_data_NHL_2015_goalies.html.rda")
# 
# save(data_NHL_2016.html, file="ploffs_data_NHL_2016.html.rda")
# save(data_NHL_2016_skaters.html, file="ploffs_data_NHL_2016_skaters.html.rda")
# save(data_NHL_2016_skatersadvanced.html, file="ploffs_data_NHL_2016_skatersadvanced.html.rda")
# save(data_NHL_2016_goalies.html, file="ploffs_data_NHL_2016_goalies.html.rda")
# 
# save(data_NHL_2017.html, file="ploffs_data_NHL_2017.html.rda")
# save(data_NHL_2017_skaters.html, file="ploffs_data_NHL_2017_skaters.html.rda")
# save(data_NHL_2017_skatersadvanced.html, file="ploffs_data_NHL_2017_skatersadvanced.html.rda")
# save(data_NHL_2017_goalies.html, file="ploffs_data_NHL_2017_goalies.html.rda")
# 
# save(data_NHL_2018.html, file="ploffs_data_NHL_2018.html.rda")
# save(data_NHL_2018_skaters.html, file="ploffs_data_NHL_2018_skaters.html.rda")
# save(data_NHL_2018_skatersadvanced.html, file="ploffs_data_NHL_2018_skatersadvanced.html.rda")
# save(data_NHL_2018_goalies.html, file="ploffs_data_NHL_2018_goalies.html.rda")
# 
# save(data_NHL_2019.html, file="ploffs_data_NHL_2019.html.rda")
# save(data_NHL_2019_skaters.html, file="ploffs_data_NHL_2019_skaters.html.rda")
# save(data_NHL_2019_skatersadvanced.html, file="ploffs_data_NHL_2019_skatersadvanced.html.rda")
# save(data_NHL_2019_goalies.html, file="ploffs_data_NHL_2019_goalies.html.rda")
# 
# save(data_NHL_2020.html, file="ploffs_data_NHL_2020.html.rda")
# save(data_NHL_2020_skaters.html, file="ploffs_data_NHL_2020_skaters.html.rda")
# save(data_NHL_2020_skatersadvanced.html, file="ploffs_data_NHL_2020_skatersadvanced.html.rda")
# save(data_NHL_2020_goalies.html, file="ploffs_data_NHL_2020_goalies.html.rda")
# 
# save(data_NHL_2021.html, file="ploffs_data_NHL_2021.html.rda")
# save(data_NHL_2021_skaters.html, file="ploffs_data_NHL_2021_skaters.html.rda")
# save(data_NHL_2021_skatersadvanced.html, file="ploffs_data_NHL_2021_skatersadvanced.html.rda")
# save(data_NHL_2021_goalies.html, file="ploffs_data_NHL_2021_goalies.html.rda")
# 
# save(data_NHL_2022.html, file="ploffs_data_NHL_2022.html.rda")
# save(data_NHL_2022_skaters.html, file="ploffs_data_NHL_2022_skaters.html.rda")
# save(data_NHL_2022_skatersadvanced.html, file="ploffs_data_NHL_2022_skatersadvanced.html.rda")
# save(data_NHL_2022_goalies.html, file="ploffs_data_NHL_2022_goalies.html.rda")
