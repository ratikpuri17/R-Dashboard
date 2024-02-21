library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)

# Read the dataset
air_quality <- read.csv("/Users/ratikpuri/Downloads/city_day.csv")
air_quality$Date <- as.Date(air_quality$Date)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Air Quality Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(8,
                       dateRangeInput("date_range", "Select Date Range",
                                      start = min(air_quality$Date),
                                      end = max(air_quality$Date),
                                      min = min(air_quality$Date),
                                      max = max(air_quality$Date)),
                       tags$style(type='text/css', ".input-group.date {padding: 5px;}"),
                       selectInput("city", "Select City",
                                   choices = c( "Ahmedabad", "Aizawl", "Amaravati", "Amritsar", "Bengaluru",
                                               "Bhopal", "Brajrajnagar", "Chandigarh", "Chennai", "Coimbatore",
                                               "Delhi", "Ernakulam", "Gurugram", "Guwahati", "Hyderabad", "Jaipur",
                                               "Jorapokhar", "Kochi", "Kolkata", "Lucknow", "Mumbai", "Patna",
                                               "Shillong", "Talcher", "Thiruvananthapuram", "Visakhapatnam"),
                                   multiple = TRUE,
                                   selected = "Delhi"
                                   ),
                       tags$style(type='text/css', ".selectize-input {padding: 5px;}")
                ),
                column(4)
              ),
              fluidRow(
                column(8,
                       box(title = "Air Quality Summary",
                           tableOutput("summary_table"),
                           style = "overflow-y: scroll; max-height: 800px; width: 100%;"
                       )
                ),
                column(4)
              ),
              fluidRow(
                column(8,
                       box(title = "PM2.5 Trend",
                           plotlyOutput("pm25_plot", height = 500)
                       )
                ),
                column(4)
              ),
              fluidRow(
                column(8,
                       box(title = "PM10 Trend",
                           plotlyOutput("pm10_plot", height = 500)
                       )
                ),
                column(4)
              ),
              fluidRow(
                column(12,
                       box(title = "AQI Trend",
                           plotlyOutput("aqi_plot", height = 500)
                       )
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Summary table
  output$summary_table <- renderTable({
    filtered_data <- filter_data(input$city, input$date_range)
    summary_data <- filtered_data %>%
      group_by(City) %>%
      summarise(Avg_PM25 = mean(PM2.5, na.rm = TRUE),
                Avg_PM10 = mean(PM10, na.rm = TRUE),
                Avg_AQI = mean(AQI, na.rm = TRUE))
    summary_data
  })
  
  # PM2.5 Trend
  output$pm25_plot <- renderPlotly({
    filtered_data <- filter_data(input$city, input$date_range)
    plot_ly(filtered_data, x = ~Date, y = ~PM2.5, color = ~City, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "PM2.5 Trend",
             xaxis = list(title = "Date"),
             yaxis = list(title = "PM2.5"))
  })
  
  # PM10 Trend
  output$pm10_plot <- renderPlotly({
    filtered_data <- filter_data(input$city, input$date_range)
    plot_ly(filtered_data, x = ~Date, y = ~PM10, color = ~City, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "PM10 Trend",
             xaxis = list(title = "Date"),
             yaxis = list(title = "PM10"))
  })
  
  # AQI Trend
  output$aqi_plot <- renderPlotly({
    filtered_data <- filter_data(input$city, input$date_range)
    plot_ly(filtered_data, x = ~Date, y = ~AQI, color = ~City, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "AQI Trend",
             xaxis = list(title = "Date"),
             yaxis = list(title = "AQI"))
  })
  
  # Function to filter data based on selected city and date range
  filter_data <- function(city, date_range) {
    data <- air_quality
    if (!"All" %in% city) {
      data <- data[data$City %in% city, ]
    }
    data <- data[data$Date >= date_range[1] & data$Date <= date_range[2], ]
    return(data)
  }
}

# Run the application
shinyApp(ui = ui, server = server)
