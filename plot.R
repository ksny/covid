rm(list=ls())

library(ggplot2)
library(lubridate)

# setwd('~/Documents/Corona/covid-19-data')

States <- read.csv('https://github.com/nytimes/covid-19-data/raw/master/us-states.csv',stringsAsFactors = F)
Counties <- read.csv('https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv',stringsAsFactors = F)

today <- max(as.Date(States$date))

States$day <- as.Date(today)-as.Date(States$date)
Counties$day <- as.Date(today) - as.Date(Counties$date)

NY <- States[which(States$state=='New York'),]
MD <- States[which(States$state=='Maryland'),]

Montgomery <- Counties[which((Counties$county=='Montgomery')&(Counties$state=='Maryland')),]
Montgomery <- Montgomery[,-3]
colnames(Montgomery)[2] <- 'state'

Data <- rbind(NY,MD,Montgomery)

Data$percentChange <- Data$cases
Data$change <- Data$cases
for (i in seq(nrow(Data))) {
  State <- Data$state[i]
  maxDay <- max(Data$day[which(Data$state==State)])
  if (Data$day[i] < maxDay) {
    Data$percentChange[i] <- (Data$cases[i]-Data$cases[i-1])/Data$cases[i-1]*100
    Data$change[i] <- Data$cases[i] - Data$cases[i-1]
  } else {
    Data$percentChange[i] <- NA
    Data$change[i] <- 0
  }
}

p <- ggplot(Data,aes(x=day,y=change)) + geom_line(aes(color=state))
print(p)
