library(shiny)
library(ggplot2)
library(lubridate)
library(plotly)

# setwd('~/Documents/Corona/covid')
# setwd('~/Corona/covid')

# Access Shiny App from:
# https://imperfect-gold-standard.shinyapps.io/covid/

'%ni%' <- Negate('%in%')

local <- F

if (local==F) {
  stateData <- read.csv('https://github.com/nytimes/covid-19-data/raw/master/us-states.csv',stringsAsFactors = F)
  countyData <- read.csv('https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv',stringsAsFactors = F)
  
  saveRDS(stateData,'stateData.rds')
  saveRDS(countyData,'countyData.rds')
} else {
  stateData <- readRDS('stateData.rds')
  countyData <- readRDS('countyData.rds')
}

today <- max(as.Date(stateData$date))

stateData$day <- as.Date(today)-as.Date(stateData$date)
countyData$day <- as.Date(today) - as.Date(countyData$date)

States <- sort(unique(stateData$state))
Counties <- sort(unique(paste0(countyData$county,' (',countyData$state,')')))

presetStates <- c('')
presetCounties <- c('')

populationData <- readRDS('populationData.rds')

statistics <- list('Cases'='cases','Deaths'='deaths','Change in Cases'='change','Percent Change in Cases'='percentChange','Percent Death Rate'='deathRate')

presets <- list('DC Metro Area'=list('States'=c('District of Columbia'),
                                     'Counties'=c('Montgomery (Maryland)',
                                                  'Baltimore (Maryland)',
                                                  'Baltimore city (Maryland)',
                                                  "Prince George's (Maryland)",
                                                  'Frederick (Maryland)',
                                                  'Howard (Maryland)',
                                                  'Anne Arundel (Maryland)',
                                                  'Arlington (Virginia)',
                                                  'Fairfax (Virginia)',
                                                  'Loudoun (Virginia)',
                                                  'Prince William (Virginia)')),
                'Family States'=list('States'=c('New Jersey','Maryland','Texas','Pennsylvania'),
                                     'Counties'=''),
                'All States'=list('States'=States,
                                  'Counties'=''))


values <- reactiveValues()
values$state <- NULL
values$popFlag <- T
values$populationData <- populationData
values$popEdit <- NULL
values$scaled <- 'No'
# values$stateData <- stateData
# values$countyData <- countyData

ui <- fluidPage(
  
  titlePanel('Corona Virus Tracker'),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 checkboxInput('usePresets','Use Preset State/County Selections?',value=F),
                 conditionalPanel(
                   condition='input.usePresets==true',
                   selectInput('presetSelection','Preset State/County Selections:',names(presets)),
                   actionButton('presetGo','Use Selected Preset'),br(),br()
                 ),
                 selectizeInput('states','Select States of Interest:',States,selected=presetStates,multiple=T),
                 selectizeInput('counties','Select Counties of Interest:',Counties,selected=presetCounties,multiple=T),
                 checkboxInput('useCaseThreshold','Sync Days by Case Theshold?',value=F),
                 conditionalPanel(
                   condition='input.useCaseThreshold==true',
                   numericInput('caseThreshold','Case Threshold:',value=50)
                 ),
                 checkboxInput('rightAlign','Match Days on Right?',value=F),
                 selectInput('statistic','Select Statistic to Plot:',statistics,selected = statistics[1]),
                 conditionalPanel(
                   condition='input.statistic=="change"',
                   numericInput('lag','Number of Days for Running Average:',value=7,step=1)
                 ),
                 checkboxInput('logScale','Log Scale?',value=F),
                 uiOutput('scaled'),
                 conditionalPanel(
                   condition='input.scaled=="Yes"',
                   uiOutput('popRef'),
                   checkboxInput('editPop','Edit Reference State/County Population?',value=F),
                   conditionalPanel(
                     condition='input.editPop==true',
                     uiOutput('popEdit')
                   )
                 ),
                 hr(),
                 h4('Last Updated:'),
                 h5(today)
    ),
    
    mainPanel(
      plotlyOutput('plot',height = '700px')
    )
  )
  
)

server <- function(input,output,session) {
  
  observeEvent(input$presetGo,{
    selection <- presets[[which(names(presets)==input$presetSelection)]]
    states <- selection$States
    counties <- selection$Counties
    updateSelectizeInput(session,'states',selected = states)
    updateSelectizeInput(session,'counties',selected = counties)
  })
  
  output$scaled <- renderUI({
    if (input$statistic!='percentChange') {
      if (values$popFlag==T) {
        selectInput('scaled','Scale by Population Size?',c('No','Yes'),selected=values$scaled)
      }
    }
  })
  
  observe({
    values$scaled <- input$scaled
  })
  
  output$popRef <- renderUI({
    if (values$popFlag==T) {
      stateList <- c(input$states,input$counties)
      selectInput('popRef','Select Population Reference:',stateList)
    }
  })
  
  output$popEdit <- renderUI({
    if (values$popFlag==T) {
      index <- which(values$populationData$State==input$popRef)
      indexValue <- as.numeric(values$populationData$Population)[index]
      numericInput('popEdit','Edit Reference State Population:',value=indexValue)
    }
  })
  
  observeEvent(input$popRef,{
    index <- which(values$populationData$State==input$popRef)
    values$popEdit <- as.numeric(values$populationData$Population)[index]
    updateNumericInput(session,'popEdit',value=values$popEdit)
  })
  
  observe({
    req(input$popEdit)
    if (values$popFlag==T) {
      if (input$editPop==T) {
        index <- which(values$populationData$State==input$popRef)
        values$popEdit <- as.numeric(input$popEdit)
        values$populationData$Population[index] <- values$popEdit
      }
    }
  })
  
  observe({
    stateList <- c(input$states,input$counties)
    for (state in stateList) {
      if (state %ni% values$populationData$State) {
        values$state <- state
        showModal(modalDialog(
          numericInput('statePop',paste0('Enter Population Size of ',state,':'),value=NULL),
          footer=tagList(actionButton('enter','Enter'))
        ))
      }
    }
  })
  
  observeEvent(input$enter,{
    if (!is.na(input$statePop)) {
      State <- values$state
      Population <- input$statePop
      newRow <- cbind(State,Population)
      populationData <- rbind(values$populationData,newRow)
      saveRDS(populationData,'populationData.rds')
      values$populationData <- populationData
    }
    removeModal()
  })
  
  observe({
    values$popFlag <- T
    input$states
    input$counties
    isolate(Data <- getData())
    States <- unique(Data$state)
    for (State in States) {
      if (State %ni% values$populationData$State) {
        values$popFlag <- F
      }
    }
  })
  
  observeEvent(input$scaled,{
    selection <- input$statistic
    if (input$scaled=='Yes') {
      updateSelectInput(session,'statistic',choices=statistics[1:3],selected=selection)
    } else {
      updateSelectInput(session,'statistic',choices=statistics[1:5],selected=selection)
    }
  })
  
  getData <- reactive({
    req(input$scaled)
    if (((is.null(input$states))&(is.null(input$counties)))) {
      Data <- NULL
    } else {
      stateIndex <- NULL
      for (state in input$states) {
        index <- which(stateData$state==state)
        stateIndex <- c(stateIndex,index)
      }
      
      countyIndex <- NULL
      for (county in input$counties) {
        state <- unlist(strsplit(county,' (',fixed=T))[2]
        state <- unlist(strsplit(state,')',fixed=T))[1]
        County <- unlist(strsplit(county,' (',fixed=T))[1]
        countyStateIndex <- which(countyData$state==state)
        countyCountyIndex <- which(countyData$county==County)
        index <- intersect(countyStateIndex,countyCountyIndex)
        countyData$county[index] <- county
        countyIndex <- c(countyIndex,index)
      }
      colnames(countyData)[2] <- 'state'
      countyData <- countyData[,-3]
      Data <- rbind(stateData[stateIndex,],countyData[countyIndex,])
      
      Data$deathRate <- Data$cases
      Data$percentChange <- Data$cases
      Data$cumPercentChange <- Data$cases
      Data$change <- Data$cases
      Data$syncDay <- Data$day
      Data$casesScaled <- Data$cases
      Data$changeScaled <- Data$cases
      Data$deathsScaled <- Data$deaths
      for (i in seq(nrow(Data))) {
        Data$deathRate[i] <- Data$deaths[i]/Data$cases[i]*100
        State <- Data$state[i]
        stateIndex <- which(Data$state==State)
        if (values$popFlag==T) {
          popIndex <- which(values$populationData$State==State)
          popRefIndex <- which(values$populationData$State==input$popRef)
        }
        maxDay <- max(Data$day[which(Data$state==State)])
        maxDayChange <- max(Data$day[which(Data$state==State)])-input$lag+1
        if (Data$day[i] < maxDay) {
          Data$percentChange[i] <- (Data$cases[i]-Data$cases[i-1])/Data$cases[i-1]*100
          Data$cumPercentChange[i] <- Data$cumPercentChange[i-1] + Data$percentChange[i]
          if (Data$day[i] < maxDayChange) {
            Data$change[i] <- mean(Data$cases[(i-input$lag+1):i] - Data$cases[(i-input$lag):(i-1)])
            if (values$popFlag==T) {
              Data$changeScaled[i] <- mean(Data$casesScaled[(i-input$lag+1):i] - Data$casesScaled[(i-input$lag):(i-1)])
            }
          } else {
            Data$change[i] <- NA
            if (values$popFlag==T) {
              Data$changeScaled[i] <- NA
            }
          }
        } else {
          Data$percentChange[i] <- NA
          Data$cumPercentChange[i] <- NA
          Data$change[i] <- NA
          Data$changeScaled[i] <- NA
          Data$day[stateIndex] <- Data$day[stateIndex] - maxDay
          if ((values$popFlag==T)&(input$scaled=='Yes')) {
            scaleFactor <- as.numeric(values$populationData$Population[popIndex])/as.numeric(values$populationData$Population[popRefIndex])
            Data$casesScaled[stateIndex] <- Data$cases[stateIndex]/scaleFactor
            Data$deathsScaled[stateIndex] <- Data$deaths[stateIndex]/scaleFactor
          }
          if (input$useCaseThreshold==T) {
            if (input$scaled=='Yes') {
              thresholdDay <- max(Data$day[which((Data$state==State)&(Data$casesScaled>=input$caseThreshold))])
            } else {
              thresholdDay <- max(Data$day[which((Data$state==State)&(Data$cases>=input$caseThreshold))])
            }
            Data$syncDay[stateIndex] <- Data$day[stateIndex]-thresholdDay
          }
        }
      }
      
      # Flip axis and remove negative days
      if (input$useCaseThreshold==T) {
        removeIndex <- which(Data$syncDay>0)
        Data <- Data[-removeIndex,]
        Data$syncDay <- -1*Data$syncDay
      }
      Data$day <- -1*Data$day
      
      # Right-align syncDay
      if (input$rightAlign==T) {
        if (input$useCaseThreshold==T) {
          maxSyncDay <- max(Data$syncDay)
          for (State in unique(Data$state)) {
            index <- which(Data$state==State)
            maxStateSyncDay <- max(Data$syncDay[index])
            adjustment <- maxSyncDay - maxStateSyncDay
            Data$syncDay[index] <- Data$syncDay[index] + adjustment
          }
        } else {
          maxDay <- max(Data$day)
          for (State in unique(Data$state)) {
            index <- which(Data$state==State)
            maxStateDay <- max(Data$day[index])
            adjustment <- maxDay - maxStateDay
            Data$day[index] <- Data$day[index] + adjustment
          }
        }
      }
    }
    
    return(Data)
  })
  
  output$plot <- renderPlotly({
    
    Data <- getData()
    
    if (!is.null(Data)) {
      if (input$useCaseThreshold==T) {
        X <- 'syncDay'
        xLabel <- paste0('Days After Reaching ',input$caseThreshold,' Cases')
      } else {
        X <- 'day'
        xLabel <- 'Days Since First Reported Case'
      }
      
      if (input$scaled=='Yes') {
        Y <- paste0(input$statistic,'Scaled')
        yLabel <- paste0(names(statistics)[which(statistics==input$statistic)],' (Scaled with Respect to ',input$popRef,')')
        xLabel <- paste0(xLabel,' (Scaled with Respect to ',input$popRef,')')
      } else {
        Y <- input$statistic
        yLabel <- names(statistics)[which(statistics==input$statistic)]
      }
      
      if (input$logScale==T) {
        yLabel <- paste0('Log Scaled ',yLabel)
        p <- ggplot(Data,aes(x=get(X),y=log(get(Y),10),label=state))
      } else {
        p <- ggplot(Data,aes(x=get(X),y=get(Y),label=state))
      }
      p <-  p + geom_line(aes(color=state),size=1) + geom_point(aes(color=state,
                                                                    text=paste0(state,'<br>',
                                                                                names(statistics[which(statistics==input$statistic)]),': ',signif(get(Y),digits=3)))) +
        theme_classic(base_size = 14) + theme(legend.position='none',legend.title=element_blank()) + labs(title='',x=xLabel,y=yLabel)
      p <- ggplotly(p=p,tooltip = c('text')) %>%
        layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
      p
    }
  })
  
}

# Run Shiny App
shinyApp(ui = ui, server = server)