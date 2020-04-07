library(shiny)
library(ggplot2)
library(lubridate)

'%ni%' <- Negate('%in%')

local <- T

if (local==F) {
  stateData <- read.csv('https://github.com/nytimes/covid-19-data/raw/master/us-states.csv',stringsAsFactors = F)
  countyData <- read.csv('https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv',stringsAsFactors = F)
  
  saveRDS(stateData,'stateData.rds')
  saveRDS(countyData,'countyData.rds')
} else {
  stateData <- readRDS('stateData.rds')
  countyData <- readRDS('countyData.rds')
}

today <- max(as.Date(States$date))

States$day <- as.Date(today)-as.Date(States$date)
Counties$day <- as.Date(today) - as.Date(Counties$date)

States <- sort(unique(stateData$state))
Counties <- sort(unique(paste0(countyData$county,' (',countyData$state,')')))

presetStates <- c('Maryland','New Jersey','Florida','Texas')
presetCounties <- c('Montgomery (Maryland)','New York City (New York)')

populationData <- readRDS('populationData.rds')

ui <- fluidPage(
  
  titlePanel('Corona Virus Tracker'),
  
  sidebarLayout(
    
    sidebarPanel(
      selectizeInput('states','Select States of Interest:',States,selected=presetStates,multiple=T),
      selectizeInput('counties','Select Counties of Interest:',Counties,selected=presetCounties,multiple=T),
      uiOutput('popRef'),
      checkboxInput('useCaseThreshold','Sync Days by Case Theshold?',value=F),
      numericInput('caseThreshold','Case Threshold:',value=50)
    ),
    
    mainPanel(
      plotOutput('plot')
    )
  )
  
)

values <- reactiveValues()
values$state <- NULL
values$popFlag <- T

server <- function(input,output,session) {
  
  output$popRef <- renderUI({
    if (values$popFlag==T) {
      stateList <- c(input$states,input$counties)
      selectInput('popRef','Select Population Reference:',stateList)
    }
  })
  
  observe({
    stateList <- c(input$states,input$counties)
    for (state in stateList) {
      if (state %ni% populationData$State) {
        values$state <- state
        showModal(modalDialog(
          numericInput('statePop',paste0('Enter Population Size of ',state,':'),value=NULL),
          footer=tagList(actionButton('enter','Enter'))
        ))
      }
    }
  })
  
  observeEvent(input$enter,{
    if (!is.null(input$statePop)) {
      State <- values$state
      Population <- input$statePop
      newRow <- cbind(State,Population)
      populationData <- rbind(populationData,newRow)
    } else {
      values$popFlag <- F
    }
    removeModal()
  })
  
  getData <- reactive({
    
    stateIndex <- NULL
    for (state in input$states) {
      index <- which(stateData$state==state)
      stateIndex <- c(stateIndex,index)
    }
    
    countyIndex <- NULL
    for (county in input$counties) {
      state <- unlist(strsplit(county,'('))[2]
      state <- unlist(strsplit(state,')'))[1]
      county <- unlist(strsplit(county,'('))[1]
      index <- ((which(countyData$state==state))&(which(countyData$county==county)))
      countyIndex <- c(countyIndex,index)
    }
    
    Data <- rbind(stateData[stateIndex,],countyData[countyIndex,-3])
    
    Data$percentChange <- Data$cases
    Data$cumPercentChange <- Data$cases
    Data$change <- Data$cases
    Data$syncDay <- Data$day
    Data$casesScaled <- Data$cases
    Data$changeScaled <- Data$cases
    Data$deathsScaled <- Data$deaths
    for (i in seq(nrow(Data))) {
      State <- Data$state[i]
      stateIndex <- which(Data$state==State)
      if (values$popFlag==T) {
        popIndex <- which(populationData$State==State)
        popRefIndex <- which(populationData$State==input$popRef)
      }
      maxDay <- max(Data$day[which(Data$state==State)])
      if (Data$day[i] < maxDay) {
        Data$percentChange[i] <- (Data$cases[i]-Data$cases[i-1])/Data$cases[i-1]*100
        Data$cumPercentChange[i] <- Data$cumPercentChange[i-1] + Data$percentChange[i]
        Data$change[i] <- Data$cases[i] - Data$cases[i-1]
        if (values$popFlag==T) {
          Data$changeScaled[i] <- Data$casesScaled[i] - Data$casesScaled[i-1]
        }
      } else {
        Data$percentChange[i] <- 0
        Data$cumPercentChange[i] <- 0
        Data$change[i] <- 0
        Data$changeScaled[i] <- 0
        scaleFactor <- populationData$Population[popIndex]/populationData$Population[popRefIndex]
        if (values$popFlag==T) {
          Data$casesScaled[stateIndex] <- Data$cases[stateIndex]/scaleFactor
          Data$deathsScaled[stateIndex] <- Data$deaths[stateIndex]/scaleFactor
        }
        thresholdDay <- max(Data$day[which((Data$state==State)&(Data$casesScaled>=caseThreshold))])
      }
      Data$syncDay[i] <- Data$syncDay[i]-thresholdDay
    }

    # Flip axis and remove negative days
    removeIndex <- which(Data$syncDay>0)
    Data <- Data[-removeIndex,]
    Data$syncDay <- -1*Data$syncDay

    # Right-align syncDay
    maxSyncDay <- max(Data$syncDay)
    for (State in unique(Data$state)) {
      index <- which(Data$state==State)
      maxStateSyncDay <- max(Data$syncDay[index])
      adjustment <- maxSyncDay - maxStateSyncDay
      Data$syncDay[index] <- Data$syncDay[index] + adjustment
    }
  })
  
}

# Run Shiny App
shinyApp(ui = ui, server = server)