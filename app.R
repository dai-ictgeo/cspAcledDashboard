library(shiny)
library(shinythemes)
library(jsonlite)
#library(ggplot2)
library(plotly)
library(RColorBrewer)
library(leaflet)
library(dplyr)
library(lubridate)
library(shinydashboard)


#IMPORT ACLED DATA. DON"T USE THIS AT THE BEGINNING IN PRODUCTION
# ACLED_API_Query <- stream_in(file("https://api.acleddata.com/acled/read?country=myanmar&limit=1000000&terms=accept"))
# ACLED_Data <- as.data.frame(ACLED_API_Query$data)
# ACLED_Data$event_date <- as.Date(ACLED_Data$event_date)
# ACLED_Data$fatalities <- as.integer(ACLED_Data$fatalities)
# ACLED_Data$longitude <- as.double(ACLED_Data$longitude)
# ACLED_Data$latitude <- as.double(ACLED_Data$latitude)
# ACLED_Data$data_id <- as.integer(ACLED_Data$data_id)
# save(ACLED_Data, file = "data/ACLED_Data.rdata")

load(file = "data/ACLED_Data.rdata")

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
                        hr(),
                        plotlyOutput("eventsPerAdmin1")
                        )
                 
               )
             )),
    navbarMenu(
      "Regional Snapshots",
      tabPanel("Shan"),
      tabPanel("Rakhine"),
      tabPanel("Kachin")
    ),
    tabPanel(
      "Events over Time",
      h2("Total State Level Events Per Year"),
      plotlyOutput("eventsPerYear"),
      plotlyOutput("fatalitiesPerYear")
    ),
    tabPanel("Key Actors"),
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
      
      
      
      
      
    }

# Run the application
shinyApp(ui = ui, server = server)
