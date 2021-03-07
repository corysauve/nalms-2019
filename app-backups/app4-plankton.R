library(shiny)
library(shinythemes)
library(tidyverse)
library(data.table)
library(nationalparkcolors)

data <- readr::read_csv("app_4_data.csv")
data <- na.omit(data)
data$year <- as.factor(data$year)
data1 <- data %>% 
  group_by(lake_type, parameter) %>% 
  summarize(mean = round(mean(value, na.rm = TRUE), 2))
data2 <- data %>% 
  group_by(tsi_int, parameter) %>% 
  summarize(mean = round(mean(value, na.rm = TRUE), 2))

ui <- fluidPage(
  
  theme = shinytheme("lumen"),
  
  titlePanel("Lake Water Quality Assessment: 2015-2018 - Plankton"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "parameter",
                  label = "Parameter:",
                  list("Chlorophyll-a (ug/L)", "Blue-green Density (cells/mL)", "Blue-green Density (NU/L)", "Blue-green Dominance (% cells)",
                       "Total Plankton (cells/mL)", "Total Plankton (NU/L)"
                  )),
      
      checkboxGroupInput("lake_type", label = "Lake Type", 
                         choices = list("Natural Lake" = "Natural Lake" , "Impoundment" = "Impoundment", "Surface Mine Lake" = "Surface Mine Lake"),
                         selected = c("Natural Lake", "Impoundment", "Surface Mine Lake")),
      
      checkboxGroupInput("tsi_int", label = "Trophic State", 
                         choices = list("Oligotrophic" = "Oligotrophic" , "Mesotrophic" = "Mesotrophic", "Eutrophic" = "Eutrophic",
                                        "Hypereutrophic" = "Hypereutrophic"),
                         selected = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypereutrophic"))
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2"))
                 )
        ),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression for above inputs 
  fun_plot <- reactive({
    
    data1 %>% 
      filter(parameter %in% input$parameter & lake_type %in% input$lake_type)
  })
  
  fun_plot2 <- reactive({
    
    data2 %>% 
      filter(parameter %in% input$parameter & tsi_int %in% input$tsi_int)
  })
  
  fun_summary <- reactive({
    data <- data %>% 
      filter(parameter %in% input$parameter & lake_type %in% input$lake_type)
    
    Mean <- round(mean(data$value, na.rm = TRUE), 3)
    Median <- round(median(data$value, na.rm = TRUE), 3)
    Min <- round(min(data$value, na.rm = TRUE), 3)
    Max <- round(max(data$value, na.rm = TRUE), 3)
    
    table <- data.table::data.table(Min, Mean, Median, Max)
    
    print(table)
    
  })
  
  fun_table <- reactive({
    data %>% 
      filter(tsi_int %in% tsi_int) %>% 
      select(-sample_id, -parameter) %>% 
      arrange(lake_name, year) %>% 
      rename("Lake" = lake_name, "Year" = year, "Date Sampled" = date_sampled, "Value" = value)
  })
  
  # Generate histogram in Plot tab
  output$plot1 <- renderPlot({
    
    pal <- park_palette("SmokyMountains", 3)
    
    p <- ggplot(fun_plot()) +
      geom_bar(mapping = aes(x = lake_type, y = mean, fill = lake_type), stat = "identity", width = 0.25, color = "black") + 
      scale_fill_manual(values = pal) + 
      coord_flip() + 
      labs(fill = "Trophic State", y = input$parameter, x = NULL) + 
      theme_classic(base_size = 13) + 
      scale_y_continuous(labels = scales::comma) + 
      scale_x_discrete(labels = c("Impoundment", "Natural Lake", "Surface Mine Lake")) + 
      guides(fill=FALSE) +
      ggtitle("Lake Type") +
      labs(caption = "Indiana Clean Lakes Program \n Indiana Lake Water Quality Assessment Report for 2015-2019") +
      theme(
        plot.background = element_rect(fill = "gray100", color = NA),
        panel.background = element_rect(fill = "gray96", color = NA),
        panel.border = element_rect(fill = NA, color = "grey90"),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "grey25", size = 1),
        axis.line.y = element_blank(),
        axis.text = element_text(color = "grey60", size = 12, family = "Helvetica"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(color = "grey25", size = 15),
        axis.ticks.x = element_line(size = 0.5, color = "grey25"),
        axis.ticks.y = element_line(size = 0.5, color = "grey90"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.caption = element_text(color = "grey60", size = 8, family = "Helvetica")
      )
    
    print(p)
  })
  
  output$plot2 <- renderPlot({
    
    pal2 <- park_palette("SmokyMountains", 4)
    
    p <- ggplot(fun_plot2()) +
      geom_bar(mapping = aes(x = tsi_int, y = mean, fill = tsi_int), stat = "identity", width = 0.25, color = "black") + 
      scale_fill_manual(values = pal2) + 
      coord_flip() + 
      labs(fill = "Trophic State", y = input$parameter, x = NULL) + 
      theme_classic(base_size = 13) + 
      scale_y_continuous(labels = scales::comma) + 
      scale_x_discrete() + 
      guides(fill=FALSE) +
      ggtitle("Trophic State") +
      labs(caption = "Indiana Clean Lakes Program \n Indiana Lake Water Quality Assessment Report for 2015-2019") +
      theme(
        plot.background = element_rect(fill = "gray100", color = NA),
        panel.background = element_rect(fill = "gray96", color = NA),
        panel.border = element_rect(fill = NA, color = "grey90"),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(color = "grey25", size = 1),
        axis.line.y = element_blank(),
        axis.text = element_text(color = "grey60", size = 12, family = "Helvetica"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(color = "grey25", size = 15),
        axis.ticks.x = element_line(size = 0.5, color = "grey25"),
        axis.ticks.y = element_line(size = 0.5, color = "grey90"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.caption = element_text(color = "grey60", size = 8, family = "Helvetica")
      )
    
    print(p)
  })
  
  output$summary <- renderPrint({
    fun_summary()
  })
  
  output$table <- renderTable({
    fun_table()
  })
}

shinyApp(ui = ui, server = server) 





