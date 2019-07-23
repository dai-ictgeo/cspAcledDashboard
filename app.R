library(shiny)
library(shinythemes)
library(jsonlite)
#library(ggplot2)
library(plotly)
library(RColorBrewer)

# IMPORT ACLED DATA. DON"T USE THIS AT THE BEGINNING IN PRODUCTION
# ACLED_API_Query <- stream_in(file("https://api.acleddata.com/acled/read?country=myanmar&limit=1000000&terms=accept"))
# ACLED_Data <- as.data.frame(ACLED_API_Query$data)
# ACLED_Data$event_date <- as.Date(ACLED_Data$event_date)
# ACLED_Data$fatalities <- as.integer(ACLED_Data$fatalities)




# Define UI for application that draws a histogram
ui <- navbarPage("Conflict Events Dashboard", theme = shinytheme("paper"),
                           tabPanel("Overview",
                                    h2("Country Overview"),
                                    plotlyOutput("eventsPerYear"),
                                    plotlyOutput("fatalitiesPerYear")
                                    ),
                           navbarMenu("Regional Snapshots",
                                      tabPanel("Shan"),
                                      tabPanel("Rakhine"),
                                      tabPanel("Kachin")
                                       ),
                           tabPanel("Events over Time"),
                           tabPanel("Key Actors"),
                           tabPanel("About",
                                    h1("About"),
                                    p("This dashboard shows Myanmar conflict event data produced by the Armed Conflict Location and Event Dataset (ACLED). Data is generally updated weekly. Read about the Data Methodology to learn more about the dataset."),
                                    p("Source: Armed Conflict Location & Event Project (ACLED) Codebook, 2017, Version 8")
                                    
                                    )
                 
                           )
                

# Define server logic required to draw a histogram
server <- function(input, output) {

  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  
  # fatalities_year <- aggregate(ACLED_Data['fatalities'], by=list(ACLED_Data['admin1'], ACLED_Data['year']), sum)
  
  eventsYear <-  ggplot(ACLED_Data, aes(year, fill=admin1)) + geom_bar() + ggtitle("Events Per Year") + theme(plot.title = element_text(size=14, face="bold"))
  
   output$eventsPerYear <- renderPlotly({ggplotly(eventsYear)})
   
   fatalitiesYear <- ggplot(ACLED_Data, aes(x=year, y=fatalities, fill=admin1)) + geom_bar(stat="identity") + ggtitle("Fatalities Per Year") + theme(plot.title = element_text(size=14, face="bold"))
   
   output$fatalitiesPerYear <- renderPlotly({ggplotly(fatalitiesYear)})
   
}

# Run the application 
shinyApp(ui = ui, server = server)
