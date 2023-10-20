library(shiny)

# Specify the file paths for both datasets
file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/max_min_avg.csv"
peakhours_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/peakhours.csv"  # Update with the correct path

# Read the CSV files
data <- read.csv(file_path)
peakhours_data <- read.csv(peakhours_path)

# Convert the "Time" column to POSIXct format
data$Time <- as.POSIXct(data$Time, format = "%Y-%m-%d")
peakhours_data$Time <- as.POSIXct(peakhours_data$Time, format = "%m/%d/%Y %H:%M")

# Define UI
ui <- fluidPage(
  titlePanel("Date and Name Filter"),
  
  # Date filter
  sidebarLayout(
    sidebarPanel(
      selectInput("date", "Select Date", choices = unique(data$Time), selected = unique(data$Time)[1]),
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
    # Extract the date part from the selected date
    selected_date <- as.Date(input$date)
    
    # Subset the data by the selected date and name from "peakhours_data"
    filtered_data <- peakhours_data[as.Date(peakhours_data$Time) == selected_date & peakhours_data$name == input$name, ]
    
    # Format the "Time" column as datetime (adjust format as needed)
    filtered_data$Time <- format(filtered_data$Time, "%Y-%m-%d %H:%M:%S")
    
    # Return the filtered data as a table
    return(filtered_data)
  })
}

# Run the Shiny app
shinyApp(ui, server)
