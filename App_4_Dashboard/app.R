# Shiny App 5 - Dashboard 

# Libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(DT)

# Data Input/Manipulation 

# Distribtion data 
dist_data <- read_csv("app_1_data.csv")
dist_data <- na.omit(dist_data)
dist_data$year <- as.factor(dist_data$year)
dist_data <- dist_data %>% filter(!parameter == "tkn")

# Lake Type data 

lake_data <- readr::read_csv("app_2_data.csv")
lake_data <- na.omit(lake_data)
lake_data$year <- as.factor(lake_data$year)
lake_data <- lake_data %>% filter(!parameter == "tkn")

# Ecoregion data 

eco_data <- readr::read_csv("app_3_data.csv")
eco_data <- na.omit(eco_data)
eco_data$year <- as.factor(eco_data$year)
eco_data <- eco_data %>% filter(!parameter == "tkn")

# All data 

all_data <- readr::read_csv("tbl_data.csv")
all_data <- na.omit(all_data)
all_data$year <- as.factor(all_data$year)
all_data <- all_data %>% 
  filter(!parameter == "tkn") %>% 
  mutate(lake_type = ifelse(lake_type == "natural lake", "Natural lake", lake_type),
         lake_type = ifelse(lake_type == "impoundment", "Impoundment", lake_type))

# Variable lists 

lake_names <- as.list(unique(dist_data$lake_name))
parameter_names <- as.list(unique(dist_data$parameter))
laketype_names <- as.list(unique(lake_data$lake_type))
ecoreg_names <- as.list(unique(eco_data$ecoregion_3))

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

###########
# Front-end 

# Top header
header <- dashboardHeader(title = "LWQA 2015-2018")

# Sidebar options 
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Background Information", tabName = "background", icon = icon("th")),
    menuItem("Tables", tabName = "tables", icon = icon("th")),
    menuItem("Lake Report", tabName = "report", icon = icon("th")),
    br(),
    selectInput(inputId = "parameter",
               label = "Parameter:",
               choices = parameter_names,
               selected = "Secchi depth (m)"
    ),
    checkboxGroupInput("lake_type", 
                       label = "Lake Type", 
                       choices = laketype_names,
                       selected = laketype_names
    ),
    checkboxGroupInput("ecoregion_3", 
                       label = "Ecoregion", 
                       choices = ecoreg_names,
                       selected = ecoreg_names
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard", 
            fluidRow(
                box(
                  width = 5,
                  title = "Distribution",
                  plotOutput("dist_plot", height = 300),
                  solidHeader = TRUE,
                  background = NULL
                ),
                box(
                  width = 7,
                  title = "Lake Type",
                  plotOutput("lake_plot", height = 300),
                  solidHeader = TRUE,
                  background = NULL
                )
              ),

            fluidRow(
              box(
                width = 12,
                title = "Ecoregion",
                plotOutput("eco_plot", height = 300),
                solidHeader = TRUE,
                background = NULL
              )
            )
    ),
    tabItem("background", includeMarkdown("lwqa_background.Rmd")),
    tabItem("tables", 
            fluidPage(
                DTOutput("tbl")
            )
    ),
    tabItem("report", 
            fluidRow(
                box(
                width = 12,
                title = "Build a Lake Report",
                "Welcome to the lake bulder tab! This will actually include instructions in the final draft"
              )),
            fluidRow(
                 box(
                   width = 4,
                   title = "Filters:",
                   selectInput(
                     inputId = "lake_name",
                     label = "Lake:",
                     choices = lake_names,
                     selected = "Airline"
                   ),
                   br(),
                   downloadButton(
                     "lake_report",
                     label = "Download report"
                   )
                 ),
                 box(
                   width = 8,
                   title = NULL,
                   height = 550,
                   "This will contain the real-time example of what the downloadable report will look like"
                )
            )
        )
    )
)

ui <- dashboardPage(header, sidebar, body)

###########
# Back-end

server <- function(input, output){
  
  # Dashboard Tab 
  
  # Distribution box 
  
  # Reactive plot function 
  dist_reactive <- function(){
    dist_data %>% 
      filter(parameter %in% input$parameter & sample_location == "Average") %>% 
      ggplot() +
      geom_histogram(aes(x = value), binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)), color = "black", fill = "#769370") +
      scale_x_continuous(name = input$parameter) +
      scale_y_continuous(name = "Frequency") +
      labs(caption = "Indiana Clean Lakes Program \n Lake Water Quality Assessment Report for 2015-2019") +
      theme_custom()
  }
  
  # Rendering plot 
  output$dist_plot <- renderPlot({
    print(dist_reactive())
  })
  
  # Lake Type Box 
  
  # Reactive plot function 
  
  lake_pal <- c("#769370", "#476F84", "#F5AF4D") 
  
  lake_reactive <- function(){
    lake_data %>% 
      filter(parameter %in% input$parameter & lake_type %in% input$lake_type) %>% 
      ggplot() +
      geom_boxplot(aes(x = lake_type, y = value,  fill = lake_type), lwd = 1, outlier.alpha = 0, alpha = 0.25, width = 0.5) +
      geom_jitter(aes(x = lake_type, y = value, color = lake_type), position = position_jitter(.15), size = 2) +
      scale_color_manual(guide = FALSE, values = lake_pal) +
      scale_fill_manual(guide = FALSE, values = lake_pal) +
      scale_x_discrete(name = "") +
      scale_y_continuous(name = input$parameter) +
      coord_flip() +
      labs(caption = "Indiana Clean Lakes Program \n Lake Water Quality Assessment Report for 2015-2019") +
      theme_custom()
  }
  
  # Rendering plot 
  
  output$lake_plot <- renderPlot({
    print(lake_reactive())
  })
  
  # Ecoregion Box 
  
  # Reactive plot function 
  
  eco_pal <- c("#769370", "#EAAE37", "#476F84", "#747669", "#43200E") 
  
  eco_reactive <- function(){
    eco_data %>% 
      filter(parameter %in% input$parameter & ecoregion_3 %in% input$ecoregion_3) %>% 
      ggplot() +
      geom_boxplot(aes(x = ecoregion_3, y = value,  fill = ecoregion_3), lwd = 1, outlier.alpha = 0, alpha = 0.25, width = 0.5) +
      geom_jitter(aes(x = ecoregion_3, y = value, color = ecoregion_3), position = position_jitter(.15), size = 2) +
      scale_color_manual(guide = FALSE, values = eco_pal) +
      scale_fill_manual(guide = FALSE, values = eco_pal) +
      scale_x_discrete(name = "") +
      scale_y_continuous(name = input$parameter) +
      coord_flip() +
      labs(caption = "Indiana Clean Lakes Program \n Lake Water Quality Assessment Report for 2015-2019") +
      theme_custom()
  }
  
  # Rendering plot 
  
  output$eco_plot <- renderPlot({
    print(eco_reactive())
  })
  
  # Table tab 
  
  # Reactive table function 
  
  tbl_reactive <- function(){
    all_data %>% 
      rename(
        "Lake Name" = lake_name, Year = year, "Date Sampled" = date_sampled, 
        Parameter = parameter, "Sample Location" = sample_location, Value = value,
        "Lake Type" = lake_type, "Ecoregion 3" = ecoregion_3
      ) 
  }
  
  # Rendering table 
  output$tbl <- renderDT({
    datatable(tbl_reactive(), 
              options = list(orderClasses = TRUE),
              height = 650,
              rownames = FALSE)
  })
  
  
  # Report generation - UNDER DEVELOPMENT
  
#  output$lake_report <- downloadHandler(
#    filename = "report.html",
#    content = function(file){
#      tempReport <- file.path(tempdir(), "report.Rmd")
#      file.copy("report.Rmd", tempReport, overwrite = TRUE)
#      
#      params <- list(lake_name <- input$lake_name,
#                     Parameter <- input$parameter,
#                     lake_type <- input$lake_type,
#                    ecoregion3 <- input$ecoregion_3
#                     )
#      
#      rmarkdown::render(tempReport, output_file = file,
#                        params = params, 
#                        envir = new.env(parent = globalenv()))
#    }
#  )
  
  
}

###########
# Run app 

shinyApp(ui, server)




