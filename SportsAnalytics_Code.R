#### Basketball Data Analytics ---- 

library(googleVis)
library(SportsAnalytics)
library(XML); 
library(stringr)
library(httr)

#------------------------------------------------------------------------------
#Retrieve the NBA data for the 2007-2008 season.
nba0708 <- fetch_NBAPlayerStatistics("07-08")
nba0708
#------------------------------------------------------------------------------
save(nba0708, file="nba0708.RData")
load(file="nba0708.RData")
#names(nba0708)

#------------------------------------------------------------------------------
#Data subset for BOS team

#Team statistics of BOS
nba0708.bos <- subset(nba0708, Team == 'BOS')

#player with best three point percentage
best_three_point_percentage <- nba0708.bos$ThreesMade * 100/nba0708.bos$ThreesAttempted
nba0708.bos$Name[which.max(best_three_point_percentage)]

#player with largest number of minutes
nba0708$Name[which.max(nba0708$TotalMinutesPlayed)]

#player with most "Steals"
nba0708$Name[which.max(nba0708$Steals)]

#names of the NBA teams that played in the term 2007-08
levels(nba0708$Team)
#------------------------------------------------------------------------------
#Teams with max wins in 07-08
web <- paste0("https://www.landofbasketball.com/yearbyyear/2007_2008_standings.htm")
temp.data <- readHTMLTable(rawToChar(GET(web)$content), header = T, stringsAsFactors = F)
temp.data <- as.character(unlist(temp.data))
split_data <- strsplit(temp.data, split='(\n|\t)')

#5 teams for the 2007-2008 season that have the most wins in descending order
unlist(split_data[24:16])[ c(TRUE,FALSE) ]

#dataframe with top 10 teams for 2007-2008 season
data1 <- split_data[16:30]
data2 <- split_data[31:45]
data.list_1 <- unlist(data1)[ c(TRUE,FALSE) ]
data.list_2 <- unlist(data2)[ c(TRUE,FALSE) ]
data_frame <- do.call(rbind.data.frame, Map(cbind, data.list_1, data.list_2))
#------------------------------------------------------------------------------
#5 google charts

#Player information for BOS
chart1 <- gvisTable(nba0708.bos[,c(2,4:12)])
plot(chart1)
print(chart1, file = "chart1.html")

#Bar graph representing Field Goals Made and attempted by each player
chart2 <- 
  gvisColumnChart(
    nba0708.bos,
    xvar = "Name",
    yvar = c("FieldGoalsMade", "FieldGoalsAttempted"),
    options=list(
      legend="top",
      height=500, width=850))
plot(chart2)
print(chart2, file = "chart2.html")

#### Basketball Championship Data -----
#Basketball championship data table

webpage <- paste0("http://www.landofbasketball.com/", "championships/year_by_year.htm")
# data <- readHTMLTable(webpage,which = 1, stringsAsFactors = FALSE)
LoB.temp.data <- readHTMLTable(rawToChar(GET(webpage)$content), header = T, stringsAsFactors = F)
LoB.temp.data <- as.character(unlist(LoB.temp.data))

split.data <- strsplit(LoB.temp.data, split='(\n|\t)')
#head(split.data)
Year <- sapply(split.data, FUN = function (x) substr(x[[1]][1], 3, 7))
Winner <- sapply(split.data,FUN = function (x) str_trim(x[[7]]))
Series <- sapply(split.data,FUN = function (x) str_trim(x[[11]]))
Opponent <- sapply(split.data,FUN = function (x) str_trim(x[[13]]))


LoB.data <- data.frame(Year=Year, Winner= Winner, Series=Series, Opponent=Opponent)
#Data information using https://www.landofbasketball.com/championships/year_by_year.htm
chart3 <- gvisTable(LoB.data,
                    options=list(
                      height=500))
plot(chart3)
print(chart3, file = "chart3.html")

#player information for the team NYK
nba0708.nyk <- subset(nba0708, Team == 'NYK')
chart4 <- gvisTable(nba0708.nyk[,c(2,3:10)])
plot(chart4)
print(chart4, file = "chart4.html")

#Bar graph representing number of threes made and attempted by the players of nyk
chart5 <- 
  gvisColumnChart(
    nba0708.nyk,
    xvar = "Name",
    yvar = c("ThreesMade","ThreesAttempted"),
    options=list(
      legend="top",
      height=500, width=850))
plot(chart5)
print(chart5, file = "chart5.html")

#------------------------------------------------------------------------------
#gvisGeoChart function

webpages <- paste0("https://www.landofbasketball.com/world_cup_stats/medals_by_year.htm")
Loa.temp.data <- readHTMLTable(rawToChar(GET(webpages)$content), header = T, stringsAsFactors = F)
Loa.temp.data <- as.character(unlist(Loa.temp.data))
splits.data <- strsplit(Loa.temp.data, split='(\n|\t)')
a <- splits.data[75:108]
c <- unlist(a)[ c(TRUE,FALSE) ]
data <- do.call(rbind, lapply(c, data.frame, stringsAsFactors=FALSE))

#gvis geo map does't consider USA as US, soviet union as russia
data[data=="USA"] <- "US"
data[data=="Soviet Union"] <- "Russia"
data
plot(gvisGeoChart(data, locationvar = "", colorvar = "", sizevar = "",
                  hovervar = "", options = list()))
#------------------------------------------------------------------------------