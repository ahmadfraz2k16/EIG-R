library(shiny)
library(dplyr)
library(DT)
library(lubridate)

# Assuming your data frame is named 'mw_data' and it has a column 'Time' of class 'POSIXct'
# Load your CSV mw_data
# Specify the file paths for both datasets
mw_file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/mw_new.csv"
# Read the CSV files
mw_data <- read.csv(mw_file_path)

# Convert the "Time" column to POSIXct format
mw_data$Time <- as.POSIXct(mw_data$Time, format = "%Y-%m-%d %H:%M", tz = "UTC")
str(mw_data$Time)

# Define UI
ui <- fluidPage(
  titlePanel("Data Filtering and Download"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Select Date Range", start = min(mw_data$Time), end = max(mw_data$Time), min = min(mw_data$Time), max = max(mw_data$Time)),
      downloadButton("download_data", "Download Data")
    ),
    
    mainPanel(
      DTOutput("filtered_data_table")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # # Filter data based on date selection
  # filtered_data <- reactive({
  #   req(input$date_range)
  #   filter(mw_data, between(Time, input$date_range[1], input$date_range[2]))
  # })
  # Filter data based on date selection
  filtered_data <- reactive({
    req(input$date_range)
    # Add one day and subtract one second to the end date
    end_date <- input$date_range[2] + days(1) - seconds(1)
    filter(mw_data, between(Time, input$date_range[1], end_date))
  })
  
  
  # Show filtered data in a paginated DataTable
  output$filtered_data_table <- renderDT({
    formatted_data <- mutate(filtered_data(), Time = format(Time, "%Y-%m-%d %H:%M:%S"))
    datatable(formatted_data, options = list(pageLength = 20))  # You can adjust 'pageLength' as needed
  })
  
  # Download filtered data as CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste("download_filtered_data.csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui, server)
