#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)

#read in data
LFD_data = read.csv("data/GRSC_Data_for_LFD.csv")

# Helper: Filter or include all
filter_or_all <- function(data, column, value) {
  if (value == "All") data else data %>% filter(!!sym(column) == value)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  headerPanel("Red Snapper Count LFD Comparison"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tags$div(style = "font-size:85%;",
      h4("Filter Group 1"),
      selectInput(inputId = "year1",
                  label = "Year of sampeling",
                  choices = c("All", sort(unique(LFD_data$year))), 
                  selected = "All"),
      
      selectInput(inputId = "Region1",
                  label = "State of sampeling",
                  choices =  c("All", sort(unique(LFD_data$Region))),
                  selected = "All"),
      
      selectInput(inputId = "Gear1",
                  label = "Sampeling Gear",
                  choices =  c("All", sort(unique(LFD_data$Gear))),
                  selected = "All"),
      
      selectInput(inputId = "Habitat1",
                  label = "Sampeling Habitat",
                  choices =  c("All", sort(unique(LFD_data$HabitatType))),
                  selected = "All"),
      
      selectInput(inputId = "SID1",
                  label = "Stock ID Region",
                  choices =  c("All", sort(unique(LFD_data$SID))),
                  selected = "All"),
      
      selectInput(inputId = "Source1",
                  label = "Data Source",
                  choices =  c("All", sort(unique(LFD_data$DataSource))),
                  selected = "All"),
      
      selectInput(inputId = "Depth1",
                  label = "Depth Zone of Sampeling",
                  choices =  c("All", sort(unique(LFD_data$DepthZone))),
                  selected = "All"),
      
      h4("Filter Group 2"),
      selectInput(inputId = "year2",
                  label = "Year of sampeling",
                  choices = c("All", sort(unique(LFD_data$year))), 
                  selected = "All"),
      
      selectInput(inputId = "Region2",
                  label = "State of sampeling",
                  choices =  c("All", sort(unique(LFD_data$Region))),
                  selected = "All"),
      
      selectInput(inputId = "Gear2",
                  label = "Sampeling Gear",
                  choices =  c("All", sort(unique(LFD_data$Gear))),
                  selected = "All"),
      
      selectInput(inputId = "Habitat2",
                  label = "Sampeling Habitat",
                  choices =  c("All", sort(unique(LFD_data$HabitatType))),
                  selected = "All"),
      
      selectInput(inputId = "SID2",
                  label = "Stock ID Region",
                  choices =  c("All", sort(unique(LFD_data$SID))),
                  selected = "All"),
      
      selectInput(inputId = "Source2",
                  label = "Data Source",
                  choices =  c("All", sort(unique(LFD_data$DataSource))),
                  selected = "All"),
      
      selectInput(inputId = "Depth2",
                  label = "Depth Zone of Sampeling",
                  choices =  c("All", sort(unique(LFD_data$DepthZone))),
                  selected = "All"),
      
      actionButton("update", "Update Plot")
      )),
                  
      # Show a plot of the generated distribution
      # Output: Histogram ----
      mainPanel(
        plotOutput("combinedHist"),
        br(),
        verbatimTextOutput("filterSummary"),
        br(),
        tags$hr(),
        tags$p(
          "Created by LaTreese Denson", format(Sys.Date(),"%Y"),
          style = "font-size: 80%; color: #888888; text-aligh: center;"
        )
      )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  filtered_data <- eventReactive(input$update, {
    # Group 1
    df1 <- LFD_data %>%
      filter_or_all("year", input$year1) %>%
      filter_or_all("Region", input$Region1) %>%
      filter_or_all("Gear", input$Gear1) %>%
      filter_or_all("SID", input$SID1) %>%
      filter_or_all("HabitatType", input$Habitat1) %>%
      filter_or_all("DataSource", input$Source1) %>%
      filter_or_all("DepthZone", input$Depth1) %>%
      mutate(group = "Filter 1")
    
    # Group 2
    df2 <- LFD_data %>%
      filter_or_all("year", input$year2) %>%
      filter_or_all("Region", input$Region2) %>%
      filter_or_all("Gear", input$Gear2) %>%
      filter_or_all("SID", input$SID2) %>%
      filter_or_all("HabitatType", input$Habitat2) %>%
      filter_or_all("DataSource", input$Source2) %>%
      filter_or_all("DepthZone", input$Depth2) %>%
      mutate(group = "Filter 2")
    
    list(combined = bind_rows(df1, df2), df1 = df1, df2 = df2)
  })
  
  output$combinedHist <- renderPlot({
    data <- filtered_data()$combined
    req(nrow(data) > 0)
    
    ggplot(data, aes(x = FL_cm, ,y=after_stat(width * density),fill = group)) +
      geom_histogram(alpha = 0.5, position = "identity", breaks =seq(10,115, by =5)) +
      labs(title = "Comparison of Distributions",y="frequency",
           x = "FL (cm)", fill = "Filter Group") +
      theme_minimal()
     })
  
  output$filterSummary <- renderText({
    req(input$update)
    data <- filtered_data()
    df1 <- data$df1
    df2 <- data$df2

    f1 <- paste0(
      "Group 1:\n",
      "  Year = ", input$year1, ", SID = ", input$SID1,", State = ", input$Region1,
      ", Gear = ", input$Gear1,", Habitat = ", input$Habitat1,", Depth zone = ", input$Depth1,
      ", Data Source = ", input$Source1, "\n",
      "  Count = ", nrow(df1), ", Mean FL(cm) = ", round(mean(df1$FL_cm), 2)
    )

    f2 <- paste0(
      "Group 2:\n",
      "  Year = ", input$year2, ", SID = ", input$SID2,", State = ", input$Region2,
      ", Gear = ", input$Gear2,", Habitat = ", input$Habitat2,", Depth zone = ", input$Depth2,
      ", Data Source = ", input$Source2, "\n",
      "  Count = ", nrow(df2), ", Mean FL(cm) = ", round(mean(df2$FL_cm), 2)
    )

    paste(f1, "\n\n", f2)
  })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
