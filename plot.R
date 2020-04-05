rm(list=ls())

library(ggplot2)
library(lubridate)

# setwd('~/Documents/Corona/covid-19-data')

States <- read.csv('https://github.com/nytimes/covid-19-data/raw/master/us-states.csv',stringsAsFactors = F)
Counties <- read.csv('https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv',stringsAsFactors = F)

today <- max(as.Date(States$date))

States$day <- as.Date(today)-as.Date(States$date)
Counties$day <- as.Date(today) - as.Date(Counties$date)

# NY <- States[which(States$state=='New York'),]
MD <- States[which(States$state=='Maryland'),]
NJ <- States[which(States$state=='New Jersey'),]
CA <- States[which(States$state=='California'),]

NY <- Counties[which((Counties$county=='New York City')&(Counties$state=='New York')),]
NY <- NY[,-3]
colnames(NY)[2] <- 'state'

Montgomery <- Counties[which((Counties$county=='Montgomery')&(Counties$state=='Maryland')),]
Montgomery <- Montgomery[,-3]
colnames(Montgomery)[2] <- 'state'

Data <- rbind(NY,NJ,CA,MD,Montgomery)

NYpop <- 8.623
NJpop <- 8.909
CApop <- 39.560
MDpop <- 6.043
montPop <- 1.059

NYratio <- NYpop/montPop
NJratio <- NJpop/montPop
MDratio <- NYpop/montPop
CAratio  <- CApop/montPop

caseThreshold <- 20

Data$percentChange <- Data$cases
Data$cumPercentChange <- Data$cases
Data$change <- Data$cases
Data$syncDay <- Data$day
Data$casesScaled <- Data$cases
for (i in seq(nrow(Data))) {
  State <- Data$state[i]
  stateIndex <- which(Data$state==State)
  maxDay <- max(Data$day[which(Data$state==State)])
  if (Data$day[i] < maxDay) {
    Data$percentChange[i] <- (Data$cases[i]-Data$cases[i-1])/Data$cases[i-1]*100
    Data$cumPercentChange[i] <- Data$cumPercentChange[i-1] + Data$percentChange[i]
    Data$change[i] <- Data$cases[i] - Data$cases[i-1]
  } else {
    Data$percentChange[i] <- 0
    Data$cumPercentChange[i] <- 0
    Data$change[i] <- 0
    if (State=='New York City') {
      Data$casesScaled[stateIndex] <- Data$cases[stateIndex]/NYratio
    } else if (State=='Maryland') {
      Data$casesScaled[stateIndex] <- Data$cases[stateIndex]/MDratio
    } else if (State=='New Jersey') {
      Data$casesScaled[stateIndex] <- Data$cases[stateIndex]/NJratio
    } else if (State=='California') {
      Data$casesScaled[stateIndex] <- Data$cases[stateIndex]/CAratio
    }
    thresholdDay <- max(Data$day[which((Data$state==State)&(Data$casesScaled>=caseThreshold))])
    print(State)
    print(thresholdDay)
  }
  Data$syncDay[i] <- Data$syncDay[i]-thresholdDay
}

# p <- ggplot(Data,aes(x=syncDay,y=log(cases,10))) + geom_line(aes(color=state))
# p <- ggplot(Data,aes(x=syncDay,y=cases)) + geom_line(aes(color=state))
# p <- ggplot(Data,aes(x=syncDay,y=percentChange)) + geom_line(aes(color=state))
# p <- ggplot(Data,aes(x=syncDay,y=cumPercentChange)) + geom_line(aes(color=state))
# p <- ggplot(Data,aes(x=syncDay,y=change)) + geom_line(aes(color=state))
# p <- ggplot(Data,aes(x=syncDay,y=log(change,10))) + geom_line(aes(color=state))
# p <- ggplot(Data,aes(x=syncDay,y=casesScaled)) + geom_line(aes(color=state))
p <- ggplot(Data,aes(x=syncDay,y=log(casesScaled,10))) + geom_line(aes(color=state),size=1) +
  theme_classic()

print(p)

