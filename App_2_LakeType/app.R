# Shiny App 2 - Lake Type 

# Libraries 
library(shiny)
library(shinythemes)
library(tidyverse)
library(data.table)

# Input/clean data
data <- readr::read_csv("app_2_data.csv")
data <- na.omit(data)
data$year <- as.factor(data$year)

# Variable lists 
parameter_names <- as.list(unique(data$parameter))
lake_type_names <- as.list(unique(data$lake_type))

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
  
  # Overall theme 
  theme = shinytheme("spacelab"),
  
  # Adding CLP logo & headings 
  tags$img(src = "clp_logo.jpg", height = 120, width = 85, align = "right"),
  h1("Indiana Clean Lakes Program", align = "left"),
  tags$i(h2("Lake Water Quality Assessment: 2015-2018")),
  
  # Adding side bar 
  sidebarLayout(
    
    sidebarPanel(
      # Parameter dropdown
      selectInput(inputId = "parameter",
                  label = "Parameter:",
                  choices = parameter_names,
                  selected = "Secchi depth (m)"
      ),
      # Lake type boxes 
      checkboxGroupInput("lake_type", 
                         label = "Lake Type", 
                         choices = lake_type_names,
                         selected = c("Natural Lake", "Impoundment", "Surface Mine Lake")
      ),
      
      br(), 
    
      # Download plot button
      downloadButton('downloadPlot', label = 'Download Plot'),
      
      br(), 
      br(),
    
      # Download data button 
      downloadButton("downloadData", "Download Data")
  ),
    # Adding tabs for plot, summary, table
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
  
  # Reactive plot function 
  pal <- c("#769370", "#476F84", "#F5AF4D") 
  
  reactive_plot <- function(){
    data %>% 
      filter(parameter %in% input$parameter & lake_type %in% input$lake_type) %>% 
      ggplot() +
      geom_boxplot(aes(x = lake_type, y = value,  fill = lake_type), lwd = 1, outlier.alpha = 0, alpha = 0.25, width = 0.5) +
      geom_jitter(aes(x = lake_type, y = value, color = lake_type), position = position_jitter(.15), size = 2) +
      scale_color_manual(guide = FALSE, values = pal) +
      scale_fill_manual(guide = FALSE, values = pal) +
      scale_x_discrete(name = "") +
      scale_y_continuous(name = input$parameter) +
      coord_flip() +
      labs(caption = "Indiana Clean Lakes Program \n Indiana Lake Water Quality Assessment Report for 2015-2019") +
      theme_custom()
  }
  
  # Generate plot 
  output$plot <- renderPlot({
    print(reactive_plot())
  })
  
  # Reactive summary expression 
  reactive_sum <- reactive({
    data <- data %>% 
      filter(parameter %in% input$parameter & lake_type %in% input$lake_type)
    
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
  
  # Reactive table expression
  reactive_tbl <- reactive({
    data %>% 
      filter(parameter %in% input$parameter & lake_type %in% input$lake_type) %>% 
      select(-sample_id, -parameter,-sample_location) %>% 
      arrange(lake_name, year) %>% 
      rename("Lake" = lake_name, "Year" = year, "Date Sampled" = date_sampled, "Value" = value, "Lake Type" = lake_type)
  })

  # Table output 
  output$table <- renderTable({
    reactive_tbl()
  })
  
  # Plot download 
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$parameter,'.png',sep='')
    },
    content = function(file){
      ggsave(file, fun_plot())
      while (!is.null(dev.list())) dev.off()
    }
  )
  
  # Table download 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$parameter, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fun_table(), file, row.names = FALSE)
    }
  )
}

# Run app 
shinyApp(ui = ui, server = server) 
