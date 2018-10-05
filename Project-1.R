library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

masshooting<-read.csv("MassShootings.csv") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

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
   # You might want an update Input or renderUI so users cannot select things that are not possible
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
    #Year Selection
    sliderInput("YearSelect",
                "Year of Shooting:",
                min = min(masshooting$Year, na.rm = T),
                max = max(masshooting$Year, na.rm = T),
                value = c(min(masshooting$Year, na.rm = T), max(masshooting$Year, na.rm = T)),
                step = 1,
                round= TRUE)
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
    masshooting <- masshooting %>%
      # Slider Filter
      filter(Year >= input$YearSelect[1] & Year <= input$YearSelect[2])
    # Homeworld Filter
    if (length(input$RaceSelect) > 0 | length(input$GenderSelect) > 0 | length(input$TargetSelect) > 0) {
      masshooting <- subset(masshooting, Race %in% input$RaceSelect)
      masshooting <- subset(masshooting, Gender %in% input$GenderSelect)
      masshooting <- subset(masshooting, Target %in% input$TargetSelect)
    }
    return(masshooting)
  })

#A plot showing the massshootings per year and mental health condition
 output$plot_mentalh <- renderPlotly({
   masshooting <- msInput()
   ggplot(data =  masshooting, aes(x = Year, y = Total.victims, fill = Mental.Health.Issues)) + 
     geom_bar(stat = "identity") + 
     labs(title= "Total Victims of Mass Shootings per Year and Mental Health Condition",
          x= "Year", y= " Total number of Victims", Legend= "Mental Health Condition")
})
# A plot showing the number of injured persons per Year
output$plot_Injured <- renderPlotly({
  masshooting <- msInput()
  ggplot(data =  masshooting, aes(x = Year, y = Injured)) + geom_bar(stat = "identity") + 
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
  subset(msInput(), select = c(Title, Date, Incident.Area, Target, Summary, Fatalities, Injured,Total.victims,
                               Mental.Health.Issues,Race))
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

  
  