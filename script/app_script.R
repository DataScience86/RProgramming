list.of.packages <- c("shiny", "shinydashboard", "stringr","dplyr", "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#install.packages("shinydashboard")
## app.R ##
library(shiny)
library(shinydashboard)
library(stringr)
library(dplyr)
library(plotly)
library(scales)
library(lubridate)

options(scipen = 9999)

topicTable <- read.csv("../data_1/topic_table.csv")
topicTable <- topicTable[order(topicTable$Topic),]
#topicTable$Date <- as.POSIXct(topicTable$Date,format='%Y-%m-%d',tz= "UTC")

screenOneTable <- read.csv("../data_1/Screen_1_Data.csv")
colnames(screenOneTable)[3] <- "corpus_percent"
screenOneTable[, c("Matched_60_Top_3", "Matched_60_Top_5", "Matched_50_Top_10")] <- screenOneTable[, c("Matched_60_Top_3", "Matched_60_Top_5", "Matched_50_Top_10")] *100

screenTwoTable <- read.csv("../data_1/screen_2_data.csv")
screenTwoTable$Words <- str_replace_all(screenTwoTable$Words, "[^[:alnum:]+?&/\\-]", " ")
screenTwoTable$Words <- str_trim(screenTwoTable$Words, side = 'both')
screenTwoTable$Words <- str_replace(gsub("\\s+", " ", str_trim(screenTwoTable$Words)), "B", "b")
screenTwoTable$Words <- str_replace_all(screenTwoTable$Words," ","_")
#screenTwoTable$Date <- as.POSIXct(screenTwoTable$Date,format='%Y-%m-%d',tz= "UTC")


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
                                                 value = 50L),
                                     
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
                        valueBoxOutput("sizeBox"),
                        valueBoxOutput("dateBox")
                        
                      ),
                      
                      tabsetPanel(type = "tabs",
                                  tabPanel("Topic Table", dataTableOutput("dataTable1")),
                                  tabPanel("Trends", 
                                           fluidRow(
                                             box(collapsible = TRUE, 
                                                 title = "Trend Chart", 
                                                 status = "success", 
                                                 solidHeader = TRUE, 
                                                 width = 250,
                                                 plotlyOutput("plot")),
                                             br(),
                                             box(collapsible = TRUE, 
                                                 collapsed = TRUE,
                                                 title = "Trend Table", 
                                                 status = "warning", 
                                                 solidHeader = TRUE, dataTableOutput("value"))
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
    filter(screenOneTable[, c(1:8)], 
           Matched_60_Top_5 >= as.numeric(input$cutoff_5) & screenOneTable$Market == input$radio & screenOneTable$Date == input$date)
    
  })
  
  output$checkBoxes <- renderUI({
    
    topicTable <- filter(topicTable, Topic == as.numeric(input$topicSelected), Date == input$date & Market == input$radio)
    
    checkboxGroupInput("checkGroup", label = h3("Select Words to see the trends"), 
                       choices = topicTable$Words[1:5],
                       selected = 1
    )
    
  })
  
  output$value <- renderDataTable({ 
    filterValue <- input$checkGroup
    filterValue <- paste0(filterValue, collapse = "_")
    filterValue <- str_replace_all(filterValue," ","_")
    #filter(screenTwoTable, Words == filterValue )
    screenTwoTable %>%
      filter(Words == filterValue, Market == input$radio) %>%
      select(Words, Date, SRPVS) %>%
      group_by(Date) %>% summarise(Total_SRPVS =sum(SRPVS))
  })
  
  output$plot <- renderPlotly({
    filterValue <- input$checkGroup
    filterValue <- paste0(filterValue, collapse = "_")
    filterValue <- str_replace_all(filterValue," ","_")
    
    plotData <- screenTwoTable %>%
      filter(Words == filterValue, Market == input$radio) %>%
      group_by(Date) %>% summarise(Total_SRPVS =sum(SRPVS))
    
    plot_ly(plotData, x = ~Date, y = ~Total_SRPVS, type = 'bar')
  })
  
  output$dateBox <- renderValueBox({
    valueBox(
      tags$p("Date", style = "font-size: 80%;"), tags$p(unique(input$date), style = "font-size: 150%;") , icon = icon("calendar"),
      color = "purple"
    )
  })
  
  output$segmentBox <- renderValueBox({
    valueBox(
      tags$p("Segment", style = "font-size: 80%;"), tags$p(input$segmentRadio, style = "font-size: 150%;"), icon = icon("file", lib = "glyphicon"),
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
  
  output$radioButton <- renderUI({
    segment_choices <- unique(screenOneTable$Decile)
    # Copy the line below to make a set of radio buttons
    radioButtons("segmentRadio", label = h3("Choose Segment"),
                 choices = segment_choices,
                 selected = segment_choices[1])

  })
  
  output$dateSelector <- renderUI({
    # You can access the value of the widget with input$date, e.g.
    Table <- filter(screenOneTable, Market == input$radio)
    selectInput("date", label = h3("Select box"), 
                choices = unique(Table$Date), 
                selected = 1)
    })
  
}

shinyApp(ui, server)