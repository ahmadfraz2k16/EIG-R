library(shiny)

# Specify the file path
file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/max_min_avg.csv"

# Read the CSV file
data <- read.csv(file_path)

## Convert the "Time" column to Date format
# data$Time <- as.Date(data$Time, format = "%Y-%m-%d")
# Convert the Time column to Date format using as.POSIXct
data$Time <- as.POSIXct(data$Time, format = "%Y-%m-%d")

# Check the structure of the data again to confirm the change
str(data)
uniqueDates <- unique(data$Time)
# Define UI
ui <- fluidPage(
  titlePanel("Date and Name Filter"),
  
  # Date filter
  sidebarLayout(
    sidebarPanel(
      selectInput("date", "Select Date", choices = uniqueDates, selected = uniqueDates[1]),
      selectInput("name", "Select Name", choices = NULL)
    ),
    mainPanel(
      # Display the table output
      tableOutput("table")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Update the name filter choices based on the selected date
  observeEvent(input$date, {
    names_for_selected_date <- data$name[data$Time == input$date]
    updateSelectInput(session, "name", choices = names_for_selected_date)
  })
  
  output$table <- renderTable({
    # Subset the data by the selected date and name
    filtered_data <- data[data$Time == input$date & data$name == input$name, ]
    # Convert the "Time" column to a character with the desired format
    filtered_data$Time <- format(filtered_data$Time, format = "%Y-%m-%d")
    # Return the filtered data as a table
    return(filtered_data)
  })
}

# Run the Shiny app
shinyApp(ui, server)