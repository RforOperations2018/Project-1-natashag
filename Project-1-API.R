library(httr)
library(readr)
library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

#I am going to download the API from Kaggle (it needs credentials from Kaggle)
kaggle.api <- "https://www.kaggle.com/api/v1/datasets/download/zusmani/us-mass-shootings-last-50-years/Mass%20Shootings%20Dataset%20Ver%205.csv"
kaggle.auth <- function() {
  source("credentials.R")
  httr::authenticate(username, key)
}
response <- httr::GET(kaggle.api, kaggle.auth())
masshooting<- read_csv(response$content) %>%
   select(c("Title", "Date", "Incident Area","Gender" ,
            "Target", "Summary", "Fatalities", "Injured","Total victims",
           "Mental Health Issues","Race")) %>%
   na.omit() #This is filtering the missing values

# I am going to transform the Date column to date form to use Date Range
masshooting <- masshooting %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  mutate(Year= format(as.Date(Date, format = "%m/%d/%Y"),"%Y"))

#I am going to subset the data in order to clean the gender column
masshooting<-subset(masshooting, 
                    Gender== "Male" | Gender== "Female" | Gender== "M" | Gender== "Unknown")

masshooting <- masshooting %>%
  mutate(Gender= case_when(
    Gender=="Male"~"Male",
    Gender=="M"~"Male",
    Gender=="Female"~"Female",
    Gender=="Unknown"~"Unknown")
    )

pdf(NULL)

#I am going to create a header for my Dashboard
header <- dashboardHeader(title = "Mass Shooting Dashboard",
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "New data about mass shootings is appearing", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 110, color = "green",
                                                "Change data")
                          )
)
# I am assigning the elements to the sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Plot General Victims", icon = icon("bar-chart"), tabName = "plot2"),
    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    #Gender Slect
    selectInput("GenderSelect",
                "Gender of the Shooter:",
                choices = sort(unique(masshooting$Gender)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Male")),
    # Race Selection
    selectInput("RaceSelect",
                "Race of the Shooter:",
                choices = sort(unique(masshooting$Race)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Asian")),
    #Target Select
    selectInput("TargetSelect",
                "Target of the Shooter:",
                choices = sort(unique(masshooting$Target)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("random")),
    #Date Range
    dateRangeInput("DateSelect",
                   "Select Dates",
                   start = "8/10/1966",
                   end = "11/5/2017")
  )
)
#I am going to put the necessary information for the body 
body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            infoBoxOutput("Fatalities"),
            valueBoxOutput("Injured")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Injured", plotlyOutput("plot_Injured")),
                   tabPanel("Fatalities", plotlyOutput("plot_Fatalities")))
          )
  ),
  tabItem("plot2",
          fluidRow(
            tabBox(title = "Plot Total Victims",
                   width = 12,
                   tabPanel("Total Victims", plotlyOutput("plot_mentalh")))
          )
  ),
  tabItem("table",
          fluidPage(
            box(title = "Mass Shooting Stats", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  msInput <- reactive({
    masshooting<- masshooting %>%
    # Data Filter
    filter(Date >= input$DateSelect[1] & Date <= input$DateSelect[2])
    # Homeworld Filter
    if (length(input$RaceSelect) > 0) {
      masshooting <- subset(masshooting, Race %in% input$RaceSelect)
    }
    if (length(input$GenderSelect) > 0) {
      masshooting <- subset(masshooting, Gender %in% input$GenderSelect)
    }
    if (length(input$TargetSelect) > 0) { 
      masshooting <- subset(masshooting, Target %in% input$TargetSelect)
    }
    return(masshooting)
  })
  
  #A plot showing the massshootings per year and mental health condition 
  output$plot_mentalh <- renderPlotly({
    masshooting <- msInput()
    ggplot(data =  masshooting, aes(x =Year, y = get("Total victims"), fill =get("Mental Health Issues"))) + 
      geom_bar(stat = "identity") + 
      labs(title= "Total Victims of Mass Shootings per Year and Mental Health Condition",
           x= "Year", y= " Total number of Victims", Legend= "Mental Health Issues:")
  })
  # A plot showing the number of injured persons per Year
  output$plot_Injured <- renderPlotly({
    masshooting <- msInput()
    ggplot(data =  masshooting, aes(x =Year, y = Injured)) + geom_bar(stat = "identity") + 
      labs(title= "Total Number of Injured people of Mass Shootings per Year ",
           x= "Year", y= " Total number of Injured")
  })
  
  # A plot showing the number of fatalities
  output$plot_Fatalities <- renderPlotly({
    masshooting <- msInput()
    ggplot(data =  masshooting, aes(x = Year, y = Fatalities)) + geom_bar(stat = "identity") + 
      labs(title= "Total Number of Fatalities of Mass Shootings per Year ",
           x= "Year", y= " Total number of Fatalities")
  })
  
  # Data table of characters
  output$table <- DT::renderDataTable({
    subset(msInput(), select = c("Title", "Date", "Incident Area", "Target", "Fatalities", "Injured","Total victims",
                                 "Mental Health Issues","Race", "Gender"))
  })
  # Fatalities mean info box
  output$Fatalities <- renderInfoBox({
    ms <- msInput()
    num <- round(mean(ms$Fatalities, na.rm = T), 2)
    infoBox("Avg number of Fatalities", value = num, subtitle = paste(nrow(ms), "Fatalities"), 
            icon = icon("balance-scale"), color = "purple")
  })
  # Injured mean value box
  output$Injured <- renderValueBox({
    ms <- msInput()
    num <- round(mean(ms$Injured, na.rm = T), 2)
    
    valueBox(subtitle = "Avg number of Injured", value = num, 
             icon = icon("sort-numeric-asc"), color = "blue")
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)









