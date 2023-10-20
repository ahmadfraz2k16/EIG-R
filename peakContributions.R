library(shiny)

# Specify the file paths for both datasets
file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/max_min_avg.csv"
peakhours_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/peakhours.csv"  # Update with the correct path

# Read the CSV files
data <- read.csv(file_path)
peakhours_data <- read.csv(peakhours_path)

# Convert the "Time" column to Date format using as.POSIXct
data$Time <- as.POSIXct(data$Time, format = "%Y-%m-%d")
peakhours_data$Time <- as.POSIXct(peakhours_data$Time, format = "%m/%d/%Y %H:%M")

# Reformat the "Time" column in peakhours_data to match the filter's date format
peakhours_data$Time <- as.Date(peakhours_data$Time)

# Check the structure of the data again to confirm the change
str(data)
str(peakhours_data)

uniqueDates <- unique(data$Time)

# Define UI
ui <- fluidPage(
  titlePanel("Date and Name Filter"),
  
  # Date filter
  sidebarLayout(
    sidebarPanel(
      selectInput("date", "Select Date", choices = uniqueDates, selected = uniqueDates[1]),
      selectInput("name", "Select Name", choices = NULL),
    ),
    mainPanel(
      # Display the table output for the filtered data
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
    # Subset the data by the selected date and name from "peakhours_data"
    filtered_data <- peakhours_data[peakhours_data$Time == input$date & peakhours_data$name == input$name, ]
    # Convert the "Time" column to a character with the desired format
    filtered_data$Time <- format(filtered_data$Time, format = "%Y-%m-%d")
    # Return the filtered data as a table
    return(filtered_data)
  })
}

# Run the Shiny app
shinyApp(ui, server)
