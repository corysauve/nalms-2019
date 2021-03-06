library(shiny)
library(leaflet)
library(tidyverse)
library(shinythemes)

data <- readr::read_csv("lwqa_data_clean.csv")
data1 <- data %>% 
  group_by(lake_name) %>% 
  tally() 
lake_names <- data1[,1]
lake_list <- as.list(lake_names)
data$year <- as.factor(data$year)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  # Adding overall theme 
  theme = shinytheme("spacelab"),
  
  # Adding CLP logo & headings 
  tags$img(src = "clp_logo.jpg", height = 120, width = 85, align = "right"),
  h1("Indiana Clean Lakes Program", align = "left"),
  tags$i(h2("Lake Water Quality Assessment: 2015-2018")),

  sidebarPanel(
    selectInput(inputId = "year",
                label = "Year",
                choices = list(2015, 2016, 2017, 2018),
                selected = 2015
    
    )),
  mainPanel(leafletOutput("mymap"))
)

server <- function(input, output, session) {
  
  map_reactive <- function(){
    data %>% 
      filter(year %in% input$year & tsi_int %in% input$tsi) %>% 
      na.omit()
  }
  
  output$mymap <- renderLeaflet({
    leaflet(data = map_reactive()) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(~long, ~lat, popup = ~as.character(lake_name), label = ~as.character(lake_name))
  })
}

shinyApp(ui, server)