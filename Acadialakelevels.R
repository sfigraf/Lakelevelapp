library(tidyverse)
library(stringr)
library(data.table)
library(lubridate)
library(rvest)
library(plotly)
library(leaflet)
library(readxl)
library(geojsonio)
library(rgdal)
library(ggrepel)
library(ggthemes)
library(shiny)

hadlock <- read_csv("hadlock.csv")
jordan <- read_csv("jordan.csv")
sealcove <- read_csv("sealcove.csv")
tarn <- read_csv("tarn.csv")
echo <- read_csv("echo.csv")
witchhole <- read_csv("witchhole.csv")
upbreakneck <- read_csv("upbreakneck.csv")
seawall <- read_csv("seawall.csv")
roundpond <- read_csv("roundpond.csv")
lowhadlock <- read_csv("lowhadlock.csv")
lowbreakneck <- read_csv("lowbreakneck.csv")
longpond <- read_csv("longpond.csv")
auntbetty <- read_csv("auntbetty.csv")
beaverdam <- read_csv("beaverdam.csv")
hodgdon <- read_csv("hodgdon.csv")
eagle <- read_csv("eagle.csv")
bubble <- read_csv("bubble.csv")
lakewoodsd3 <- read_csv("lakewoodsd3.csv")

ldf <- list(hadlock,jordan,sealcove,tarn,echo,witchhole,upbreakneck,seawall,roundpond,lowhadlock,lowbreakneck,longpond,auntbetty,beaverdam,hodgdon,eagle,bubble,lakewoodsd3)
names1 <- c("Upper.Hadlock","Jordan.Pond", "Seal.Cove","The.Tarn","Echo.Lake","Witch.Hole","Upper.Breakneck","Seawall.Pond","Round.Pond","Lower.Hadlock","Lower.Breakneck","Long.Pond","Aunt.Betty.Pond","Beaver.Dam","Hodgdon.Pond","Eagle.Lake","Bubble.Pond","Lakewood.sd3")
names(ldf) <- names1


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Acadia Lake Levels"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("slider1",
                     "Emphasize a Year (if data available at site)",
                     min = 1999,
                     max = 2019,
                     value = 2017,
                     sep = ""),
         selectInput(inputId = "dropdown1",
                     label = "Which dataset",
                     choices = names1),
         dateRangeInput(inputId = "date1", strong("Select Date Range for all sites"), start = "2019-01-01", end = "2019-12-31",
                        min = "1999-01-01", max = "2019-12-31")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("plot1"),
         plotlyOutput("plot2")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataset <- reactive(ldf[[as.character(input$dropdown1)]] %>%
                        mutate(
                          year = factor(year(Date)),     # use year to define separate curves
                          date = update(Date, year = 1)))
   
   output$plot1 <- renderPlotly({
      # generate bins based on input$bins from ui.R
      dataset() %>%
       ggplot(aes(date, `Distance From Datum`, color = year)) +
       scale_x_date(date_breaks = "1 month", date_labels = "%b") +
       labs(title = input$dropdown1) +
       geom_line(aes(group = year), color = "black", alpha = 0.1) +
       geom_line(data = function(x) filter(x, year == input$slider1), size = 1)
   })
   
   alllakes <- reactive(
     lapply(ldf, function(x) {
       filter(x, Date > as.POSIXct(input$date1[1]) & Date < as.POSIXct(input$date1[2])
       )
     })
   )
   
   output$plot2 <- renderPlotly({
     # generate bins based on input$bins from ui.R
     p <- plot_ly() 
     
     #for each dataframe in the list of dataframes, add a trace to the graph with their stage and date data 
     for(site in alllakes()) {  p <- add_trace(p, y=site[["Distance From Datum"]], x=site[["Date"]],
                                               name = site[["name"]],
                                               type = "scatter",
                                               mode = "lines",
                                               connectgaps = FALSE)
     }
     p
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

