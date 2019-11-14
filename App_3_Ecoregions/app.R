# Shiny App 3 - ecoregions 

# Libraries 
library(shiny)
library(shinythemes)
library(tidyverse)
library(data.table)

# Input/clean data
data <- readr::read_csv("app_3_data.csv")
data <- na.omit(data)
data$year <- as.factor(data$year)

# Variable lists 
parameter_names <- as.list(unique(data$parameter))
eco_names <- as.list(unique(data$ecoregion_3))

# ggplot2 theme 
theme_custom <- function(base_size = 15, base_family = "sans"){
  base_size <-  base_size
  base_family <- base_family
  theme(
    line = element_line(colour = "black"),
    rect = element_rect(fill = "grey90", linetype = 0, color = NA),
    text = element_text(color = "grey50"),
    axis.title = element_text(color = "grey50", size = 15, family = "sans"),
    axis.text = element_text(color = "grey50", size = 15, family = "sans"),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.background = element_rect(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    panel.grid = element_line(color = NULL),
    panel.grid.major = element_line(color = "grey60"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    plot.background = element_rect(fill = "gray95", color = NA),
    panel.background = element_rect(fill = "gray95", color = NA)
  )
}

# Front end 
ui <- fluidPage(
  
  # Adding overall theme 
  theme = shinytheme("spacelab"),
  
  # Adding CLP logo & headings 
  tags$img(src = "clp_logo.jpg", height = 120, width = 85, align = "right"),
  h1("Indiana Clean Lakes Program", align = "left"),
  tags$i(h2("Lake Water Quality Assessment: 2015-2018")),

  # Adding side bar 
  sidebarLayout(
    
    # Parameter selection 
    sidebarPanel(
      selectInput(inputId = "parameter",
                  label = "Parameter:",
                  choices = parameter_names,
                  selected = "Secchi depth (m)"
      ),
      
      # Ecoregion boxes 
      checkboxGroupInput("ecoregion_3", label = "Ecoregion", 
                         choices = eco_names,
                         selected = eco_names
      ),
      
      br(), 
      # Download plot button
      downloadButton('downloadPlot', label = 'Download Plot'),
      br(), 
      br(),
      # Download data button 
      downloadButton("downloadData", "Download Data")
    ),
    
    # Creating tabs for plot, summary, table 
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "plot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

# Back end 
server <- function(input, output) {
  
  pal <- c("#769370", "#EAAE37", "#476F84", "#747669", "#43200E") 
  
  # Reactive plot function 
  reactive_plot <- function(){
    data %>% 
      filter(parameter %in% input$parameter & ecoregion_3 %in% input$ecoregion_3) %>% 
      ggplot() +
      geom_boxplot(aes(x = ecoregion_3, y = value,  fill = ecoregion_3), lwd = 1, outlier.alpha = 0, alpha = 0.25, width = 0.5) +
      geom_jitter(aes(x = ecoregion_3, y = value, color = ecoregion_3), position = position_jitter(.15), size = 2) +
      scale_color_manual(guide = FALSE, values = pal) +
      scale_fill_manual(guide = FALSE, values = pal) +
      scale_x_discrete(name = "") +
      scale_y_continuous(name = input$parameter) +
      coord_flip() +
      labs(caption = "Indiana Clean Lakes Program \n Indiana Lake Water Quality Assessment Report for 2015-2019") +
      theme_custom()
  }
  
  # Rendering plot 
  output$plot <- renderPlot({
    print(reactive_plot())
  })

  # Reactive summary function 
  reactive_sum <- reactive({
    data <- data %>% 
      filter(parameter %in% input$parameter & ecoregion_3 %in% input$ecoregion_3)
    
    Mean <- round(mean(data$value, na.rm = TRUE), 3)
    Median <- round(median(data$value, na.rm = TRUE), 3)
    Min <- round(min(data$value, na.rm = TRUE), 3)
    Max <- round(max(data$value, na.rm = TRUE), 3)
    
    table <- data.table::data.table(Min, Mean, Median, Max)
    
    print(table)
    
  })
  
  # Summary output 
  output$summary <- renderPrint({
    reactive_sum()
  })
  
  # Reactive table function 
  reactive_tbl <- reactive({
    data %>% 
      filter(parameter %in% input$parameter & ecoregion_3 %in% input$ecoregion_3) %>% 
      select(-sample_id, -parameter,-sample_location) %>% 
      arrange(lake_name, year) %>% 
      rename("Lake" = lake_name, "Year" = year, "Date Sampled" = date_sampled, "Value" = value, "Ecoregion 3" = ecoregion_3)
  })
  
  # Render table 
  output$table <- renderTable({
    reactive_tbl()
  })
  
  # Plot download 
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$parameter,'.png',sep='')
    },
    content = function(file){
      ggsave(file, reactive_plot())
      while (!is.null(dev.list())) dev.off()
    }
  )
  
  # Table download 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$parameter, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_tbl(), file, row.names = FALSE)
    }
  )
}

# Run app 
shinyApp(ui = ui, server = server) 





