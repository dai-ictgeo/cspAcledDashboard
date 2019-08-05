library(shiny)
library(shinythemes)
library(jsonlite)
#library(ggplot2)
library(plotly)
library(ggrepel)
library(geomnet)
library(RColorBrewer)
library(leaflet)
library(dplyr)
library(lubridate)
library(shinydashboard)


#IMPORT ACLED DATA. DON"T USE THIS AT THE BEGINNING IN PRODUCTION
# ACLED_API_Query <- stream_in(file("https://api.acleddata.com/acled/read?country=myanmar&limit=1000000&terms=accept"))
#ACLED_API_Query_Monadic <- stream_in(file("https://api.acleddata.com/acled/read?country=myanmar&limit=1000000&export_type=monadic&terms=accept"))
# ACLED_Data <- as.data.frame(ACLED_API_Query$data)
#ACLED_Data_Monadic <- as.data.frame(ACLED_API_Query_Monadic$data)

# CleanACLED <- function(x) {
#   x$event_date <- as.Date(x$event_date)
#   x$fatalities <- as.integer(x$fatalities)
#   x$longitude <- as.double(x$longitude)
#   x$latitude <- as.double(x$latitude)
#   x$data_id <- as.integer(x$data_id)
#   return(x)
# }
# 
# ACLED_Data <- CleanACLED(ACLED_Data)
# ACLED_Data_Monadic <- CleanACLED(ACLED_Data_Monadic)
# 
# 
# save(ACLED_Data, file = "data/ACLED_Data.rdata")
# save(ACLED_Data_Monadic, file = "data/ACLED_Data_Monadic.rdata")

#thirtydays <- Sys.Date() - 30
#eventslastthirty <- ACLED_Data %>% filter(event_date >= as.Date(thirtydays))     

load(file = "data/ACLED_Data.rdata")
load(file = "data/ACLED_Data_Monadic.rdata")
thirtydays <- Sys.Date() - 30
eventslastthirty <- ACLED_Data %>% filter(event_date >= as.Date(thirtydays))     

# Define UI for application that draws a histogram
ui <-
  navbarPage(
    "Conflict Events Report",
    theme = shinytheme("paper"),
    tabPanel("Overview",
             fluidPage(
               h4("Country Overview"),
               fluidRow(
                 column(
                   8,
                   leafletOutput("overviewMap"),
                   plotlyOutput("eventsPerMonth", height = 200)
                 ),
                 column(4,
                        p("Last Event Reported"),
                        h4(textOutput("lastEvent")),
                        p("Events Recorded"),
                        h4(textOutput("eventCount")),
                        p("Recorded Fatalities"),
                        h4(textOutput("fatalityCount")),
                        hr(),
                        plotlyOutput("eventsPerAdmin1")
                        )
                 
               )
             )),
    navbarMenu(
      "Regional Snapshots",
      tabPanel("Shan",
               h4("30 Day Snapshot: Shan State"),
               fluidRow(
                 column(6,
                        p('Paragraph of latest events'),
                        hr()                        
                        ),
                 column(6,
                        leafletOutput("shanLast30Map", height = 600),
                        plotlyOutput("shanEvents30", height = 200))
               
                        )
               
               ),
      tabPanel("Rakhine"),
      tabPanel("Kachin")
    ),
    tabPanel(
      "Events over Time",
      h2("Total State Level Events Per Year"),
      plotlyOutput("eventsPerYear"),
      plotlyOutput("fatalitiesPerYear")
    ),
    tabPanel("Key Actors",
             dateRangeInput("dates",
                            "Date range",
                            start = "2013-01-01",
                            end = as.character(Sys.Date())),
             
             leafletOutput("KeyActorActivityMap"),
             dataTableOutput("eventTable")
             
             ),
    tabPanel(
      "About",
      h1("About"),
      p(
        "This dashboard shows Myanmar conflict event data produced by the Armed Conflict Location and Event Dataset (ACLED). Data is generally updated weekly. Read about the Data Methodology to learn more about the dataset."
      ),
      p(
        "Source: Armed Conflict Location & Event Project (ACLED) Codebook, 2017, Version 8"
      )
      
    ),
    tags$footer("Data Source: Armed Conflict Location & Event Data Project (ACLED);", tags$a(href = "https://wwww.acleddata.com", "acleddata.com"), align = "left", style = "position:absolute;
                bottom:0;
                width:95%;
                height:50px;
                color: black;
                z-index: 1000;"
    )    

  )


# Define server logic
server <- function(input, output) {
  ######### OVERVIEW PAGE
      
    tcu_map <-
        "https://api.mapbox.com/styles/v1/gamaly/cjmhaei90a3xh2sp6lfqftcqg/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZ2FtYWx5IiwiYSI6ImNpZmswdTM3bGN2eXFzNG03OTd6YWZhNmEifQ.srtQMx2-zlTgvAT90pAOTw"
      map_attr <-
        "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a>"
      
      #Leaflet Map
      output$overviewMap <- renderLeaflet({
        leaflet(data = ACLED_Data) %>% addTiles(urlTemplate = tcu_map, attribution = map_attr)  %>% addCircleMarkers( ~
                                                                                                                        longitude, ~ latitude, radius = sqrt(ACLED_Data$fatalities))
      })
      
      #Events Over Time
      Monthly <-
        ACLED_Data %>% group_by(month = floor_date(event_date, "month")) %>% summarise(total = length(month))
      
      ACLED_Data %>% group_by(month = floor_date(event_date, "month")) %>% summarise(total = length(month))
      
      monthlyEvents <-
        ggplot(Monthly, aes(month, total)) + geom_line(color = "darkblue") + ggtitle("Total events per month over time") + theme_classic()
      
      output$eventsPerMonth <- renderPlotly({
        monthlyEvents
      })
      
      
      totalAdmin1 <-
        ggplot(ACLED_Data, aes(admin1)) + geom_bar(fill = "darkblue") + theme_classic() + coord_flip() + ggtitle("Total Events Per State")
      
      output$eventsPerAdmin1 <- renderPlotly({
        totalAdmin1
      })
      
      #Last Date Reported
      output$lastEvent <- renderText({
        paste(head(ACLED_Data$event_date, 1))
      })
      
      #Total Events Reported
      output$eventCount <- renderText({nrow(ACLED_Data)})
      
      #Total Fatalities Recorded
      output$fatalityCount <- renderText(sum(ACLED_Data$fatalities))
      
  ####### SHAN THIRTY DAY SNAPSHOT
      output$shanLast30Map <- renderLeaflet({
        leaflet(data = filter(eventslastthirty, admin1 == "Shan")) %>% addTiles(urlTemplate = tcu_map, attribution = map_attr)  %>% addCircleMarkers( ~longitude, ~ latitude, radius = sqrt(ACLED_Data$fatalities))})
      
      
      shanEventsLast30 <- ggplot(data = filter(eventslastthirty, admin1 == "Shan"), aes(event_type)) + geom_bar(fill = "darkblue") + theme_classic() + ggtitle("Total Events Per State")
      
      output$shanEvents30 <- renderPlotly({shanEventsLast30})
      
      
      # ShanNetworkAnalysis <- ggplot(data = filter(eventslastthirty, admin1 == "Shan"), aes(from_id = actor1, to_id = actor2)) + geom_net(layout.alg = 'fruchtermanreingold', size = 6, labelon = TRUE, ggrepel = TRUE, vjust = -0.8, ecolour = "grey60", directed =FALSE, fontsize = 3, ealpha = 0.5) + theme_net() 
      
      # output$ShanNetwork30Day <- renderPlotly({ShanNetworkAnalysis})
      
      
      
  
  ####### STATE LEVEL EVENTS PER YEAR PAGE
  
  # fatalities_year <- aggregate(ACLED_Data['fatalities'], by=list(ACLED_Data['admin1'], ACLED_Data['year']), sum)
      eventsYear <-
        ggplot(ACLED_Data, aes(year, fill = admin1)) + geom_bar() + ggtitle("Events Per Year") + theme(plot.title = element_text(size =
                                                                                                                                   14, face = "bold")) + theme_classic()
      
      output$eventsPerYear <- renderPlotly({
        ggplotly(eventsYear)
      })
      
      fatalitiesYear <-
        ggplot(ACLED_Data, aes(x = year, y = fatalities, fill = admin1)) + geom_bar(stat =
                                                                                      "identity") + ggtitle("Fatalities Per Year") + theme(plot.title = element_text(size =
                                                                                                                                                                       14, face = "bold")) + theme_classic()
      
      output$fatalitiesPerYear <-
        renderPlotly({
          ggplotly(fatalitiesYear)
        })
      
      
      
  # Key Actor Activity Over Time
      
      dateInput <- reactive({
        ACLED_Data_Monadic %>% filter(event_date >= input$dates[1] & event_date <= input$dates[2])
      })
      
      output$eventTable <- renderDataTable({dateInput()})
      
      
      output$KeyActorActivityMap <- renderLeaflet({
        leaflet(data = dateInput()) %>% addTiles(urlTemplate = tcu_map, attribution = map_attr)  %>% addCircleMarkers( ~longitude, ~ latitude, radius = sqrt(dateInput()$fatalities))})
      
      
      
      # output$EventsPerActor <-renderPlot({
      #   ggplot(data = as.data.frame(table(dateInput()$actor1)), aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity", fill = "darkblue") + theme_classic() + ggtitle("Total Events Per Group") + coord_flip()
      #   
      # })    
      # 
      # ggplot(data = as.data.frame(table(ACLED_Data_Monadic$actor1)), aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity", fill = "darkblue") + theme_classic() + ggtitle("Total Events Per Group") + coord_flip()
      
      # EventsPerActor <- ggplot(data = as.data.frame(table(dateInput$actor1)), aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity", fill = "darkblue") + theme_classic() + ggtitle("Total Events Per Group") + coord_flip()
      # 
      # output$eventsPerActor <- renderPlotly({
      #   ggplotly(EventsPerActor)
      # })
      
      # output$EventsPerActor <- renderPlot({ggplot(data = as.data.frame(table(ACLED_API_Query_Monadic$actor1)), aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity", fill = "darkblue") + theme_classic() + ggtitle("Total Events Per Group") + coord_flip()
      #     })
      
      
    }

# Run the application
shinyApp(ui = ui, server = server)
