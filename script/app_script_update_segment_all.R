# list.of.packages <- c("shiny", "shinydashboard", "stringr","dplyr", "plotly")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

#install.packages("shinydashboard")
## app.R ##
library(shiny)
library(shinydashboard)
library(stringr)
library(dplyr)
library(plotly)
library(scales)
library(lubridate)
library(readr)

options(scipen = 9999)


#######################################################################################################
##                                       UDF   Functoions                                            ##
#######################################################################################################
checkDateFormat <- function(mydate, date.format = "%m/%d/%y") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}

#######################################################################################################
##                                                                                                   ##
#######################################################################################################

topicTable <- read_csv("../data_3/topic_table.csv")
topicTable <- topicTable[order(topicTable$Topic),]
#topicTable$Date <- as.POSIXct(topicTable$Date,format='%Y-%m-%d',tz= "UTC")

screenOneTable <- read_csv("../data_3/Screen_1_Data.csv")
screenOneTable[, c("Matched_60_Top_3", "Matched_60_Top_5", "Matched_50_Top_10")] <- round(screenOneTable[, c("Matched_60_Top_3", "Matched_60_Top_5", "Matched_50_Top_10")] *100, 1)

# Rearranging the columns
screenOneTable <- screenOneTable[c("Topic_No", "Topic_Name", "% of Corpus", "SRPV_Share", "sumSRPV", 
                                   "Date", "Market", "Matched_50_Top_10", "Matched_60_Top_3", "Matched_60_Top_5", "Top_20_Words")]

# We are getting multiple date formats and thus bringing them to same format
screenOneTable$Date <-  as.Date(parse_date_time(x =  screenOneTable$Date,
                                               orders = c('m/d/y', 'y-m-d'),
                                               locale = "eng", tz = "UTC"))

screenTwoTable <- read_csv("../data_3/screen_2_data.csv")
screenTwoTable$Words <- str_replace_all(screenTwoTable$Words, "[^[:alnum:]+?&/\\-]", " ")
screenTwoTable$Words <- str_trim(screenTwoTable$Words, side = 'both')
screenTwoTable$Words <- str_replace(gsub("\\s+", " ", str_trim(screenTwoTable$Words)), "B", "b")
screenTwoTable$Words <- str_replace_all(screenTwoTable$Words," ","_")
#screenTwoTable$Date <- as.POSIXct(screenTwoTable$Date,format='%Y/%m/%d',tz= "UTC")




########################################################################################################
## This part in only for testing purpose. This data will be collected while fetching data from cosmos ##
########################################################################################################

# screenOneTable[, "Dated"] <- "03-09-2018"
# screenOneTable[, "Market"] <- "de-GE"
# screenOneTable[, "Size"] <- 500000

########################################################################################################
## This part in only for testing purpose. This data will be collected while fetching data from cosmos ##
########################################################################################################


ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Tail Quey Analysis", 
                                    titleWidth = 300),
                    dashboardSidebar(width = 300,
                                     menuItem("Dashboard", 
                                              tabName = "dashboard", 
                                              icon = icon("dashboard"),
                                              size = 10),
                                    
                                     uiOutput("radioButton"),
                                     
                                     uiOutput("dateSelector"),
                                     
                                     sliderInput("cutoff_5",
                                                 "Top 5 match percent:",
                                                 min = 0L,
                                                 max = 100L,
                                                 value = 30L),
                                     
                                     uiOutput("topicSelector"),
                                     
                                     uiOutput("checkBoxes"),
                                     br(),
                                     box(title ="Info", 
                                         collapsible = TRUE ,
                                         width = 150,
                                         collapsed = TRUE,
                                         status = "info",
                                         helpText("Topic models are build daily by fetching",
                                                  "data from SLAPI. We use NFM algorithm to",
                                                  "identify topics and the output from those models",
                                                  "is used to create Tail Query dashboard"))
                    ),
                    
                    dashboardBody(
                      tags$head( 
                        tags$style(HTML(".main-sidebar { font-size: 18px; }")) #change the font size to 20
                      ),
                      
                      fluidRow(
                        # Dynamic infoBoxes
                        valueBoxOutput("marketBox"),
                        
                        valueBoxOutput("dateBox")
                        
                      ),
                      
                      tabsetPanel(type = "tabs",
                                  tabPanel("Topic Table", dataTableOutput("dataTable1")),
                                  
                                  tabPanel("Topic Trend", 
                                           fluidRow(box(collapsible = TRUE, 
                                                        title = "SRPV Trend Chart", 
                                                        status = "success", 
                                                        solidHeader = TRUE, 
                                                        width = 250,
                                                        plotlyOutput("plotSRPV")),
                                                    br(),
                                                    box(collapsible = TRUE, 
                                                        title = "RPM Trend Chart", 
                                                        status = "success", 
                                                        solidHeader = TRUE, 
                                                        width = 250,
                                                        plotlyOutput("plotRPM"))
                                                    )
                                           ),
                                  
                                  tabPanel("N-Gram Trends", 
                                           fluidRow(
                                             box(collapsible = TRUE, 
                                                 title = "Trend Chart", 
                                                 status = "success", 
                                                 solidHeader = TRUE, 
                                                 width = 250,
                                                 plotlyOutput("plot"))
                                           )
                                  )
                      )
                    )
)

server <- function(input, output) { 
  output$topicSelector <- renderUI({
    selectInput(inputId = "topicSelected",
                label = "Select Topic",
                choices = unique(screenOneTable$Topic_No))
  })
  
  output$dataTable1 <- renderDataTable({
    screenOneTable <- screenOneTable
    filter(screenOneTable[ , -c(6:9)], 
           Matched_60_Top_5 >= as.numeric(input$cutoff_5) & screenOneTable$Market == input$radio & screenOneTable$Date == input$date)
    
  })
  
  output$checkBoxes <- renderUI({
    
    topicTable <- filter(topicTable, Topic == as.numeric(input$topicSelected), Date == input$date & Market == input$radio)
    
    checkboxGroupInput("checkGroup", label = h3("Select Words to see the trends"), 
                       choices = topicTable$Words[1:5],
                       selected = 1
    )
    
  })
  

  output$plot <- renderPlotly({
    filterValue <- input$checkGroup
    filterValue <- paste0(filterValue, collapse = "_")
    filterValue <- str_replace_all(filterValue," ","_")
    
    screenTwoTableSRPV <- screenTwoTable[(screenTwoTable$Words == filterValue) & (screenTwoTable$Market == input$radio), ]
    screenTwoTableSRPV$SRPV_Share <- round(screenTwoTableSRPV$SRPVS/sum(screenTwoTableSRPV$SRPVS),4)*100
    
    plotDataSRPVTwo <- screenTwoTableSRPV %>%
      transform(screenTwoTableSRPV, delta_SRPV_Share = c(rep(NA,7), diff(SRPV_Share, lag = 7)))%>%
      group_by(Date) %>% 
      summarise(Total_SRPVS = sum(SRPVS),
                SRPV_Share = sum(SRPV_Share),
                Delta = round(delta_SRPV_Share,2))
    
    plot_ly(plotDataSRPVTwo) %>%
      add_trace(x = ~Date, y = ~Delta, type = 'bar', name = "Delta") %>%
      add_trace(x = ~Date, y = ~SRPV_Share, type = 'scatter', mode = 'lines', 
                name = 'Volume % Share', yaxis = 'y2', 
                text = ~paste("Total Volume :", formatC(Total_SRPVS, format="f", big.mark=",", digits=1),
                              "<br> Market Share :", SRPV_Share,
                              "<br> T1 Delta :", Delta), hoverinfo = "text") %>%
      layout(title = '',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = 'Delta', showgrid = FALSE, zeroline = FALSE),
             yaxis2 = list(side = 'right', overlaying = "y", title = 'Market Share', showgrid = FALSE, zeroline = FALSE))
  })
  
  output$dateBox <- renderValueBox({
    valueBox(
      tags$p("Date", style = "font-size: 80%;"), tags$p(unique(input$date), style = "font-size: 150%;") , icon = icon("calendar"),
      color = "purple"
    )
  })
  
  
  output$marketBox <- renderValueBox({
    valueBox(
      tags$p("Market", style = "font-size: 80%;"),  tags$p(input$radio, style = "font-size: 150%;"), icon = icon("map-marker", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$radioButton <- renderUI({
    market_choices <- unique(screenOneTable$Market)
    # Copy the line below to make a set of radio buttons
    radioButtons("radio", label = h3("Choose Market"),
                 choices = market_choices,
                 selected = market_choices[1])
    
  })
  

  output$dateSelector <- renderUI({
    # You can access the value of the widget with input$date, e.g.
    Table <- filter(screenOneTable, Market == input$radio)
    selectInput("date", label = h3("Select box"), 
                choices = unique(Table$Date), 
                selected = 1)
    })
############################################################################################################################################
#################################### TAB 2 - Topic Trend Charts - KPI Charts  ##############################################################
############################################################################################################################################  
  
    
  #screenOneTableSRPV <- screenOneTable[screenOneTable$Topic_Name == topic_name , ]
  output$plotSRPV <- renderPlotly({
    topic_name <- as.character(screenOneTable[(screenOneTable$Topic_No == as.numeric(input$topicSelected)) & (screenOneTable$Market == input$radio) & (screenOneTable$Date == input$date) , "Topic_Name" ])
    screenOneTableSRPV <- screenOneTable[(screenOneTable$Topic_Name == topic_name) & (screenOneTable$Market == input$radio), ]
    plotDataSRPV <- screenOneTableSRPV %>%
      transform(screenOneTableSRPV, delta_SRPV_Share = c(rep(NA,7), diff(SRPV_Share, lag = 7)))%>%
      group_by(Date) %>% 
      summarise(Total_SRPVS = sum(sumSRPV),
                SRPV_Share = sum(SRPV_Share),
                Delta = round(delta_SRPV_Share,2))
    
    plot_ly(plotDataSRPV) %>%
      add_trace(x = ~Date, y = ~Delta, type = 'bar', name = "Delta", showlegend = TRUE) %>%
      add_trace(x = ~Date, y = ~SRPV_Share, type = 'scatter', mode = 'lines', 
                name = 'Volume % Share', yaxis = 'y2', 
                text = ~paste("Total Volume :", formatC(Total_SRPVS, format="f", big.mark=",", digits=1),
                              "<br> Market Share :", SRPV_Share,
                              "<br> T1 Delta :", Delta), hoverinfo = "text") %>%
      layout(title = '',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = 'Delta', showgrid = FALSE, zeroline = FALSE),
             yaxis2 = list(side = 'right', overlaying = "y", title = 'Market Share', showgrid = FALSE, zeroline = FALSE))
  })
  
  output$plotImpact <- renderPlotly({
    plotData <- screenOneTable %>%
      filter(Topic_No == as.numeric(input$topicSelected), Market == input$radio) %>%
      group_by(Date) %>% summarise(Total_SRPVS = sum(sumSRPV))
    
    plot_ly(plotData, x = ~Date, y = ~Total_SRPVS, type = 'bar')
  })
  
}

shinyApp(ui, server)