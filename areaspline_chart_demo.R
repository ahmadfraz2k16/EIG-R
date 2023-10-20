# library(highcharter)
# 
# # Sample data
# datetime <- seq(as.POSIXct("2022-01-01"), as.POSIXct("2022-01-09"), by = "day")
# peak_values <- c(10, 15, 12, 18, 20, 16, 14, 11, 13)
# 
# # Convert datetime to timestamps
# timestamps <- highcharter::datetime_to_timestamp(datetime)
# 
# # Create the chart
# highchart() %>%
#   hc_chart(type = "areaspline") %>%
#   hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = "%e. %b")) %>%
#   hc_yAxis(title = list(text = "Peak Values")) %>%
#   hc_add_series(data = peak_values, name = "Peak Values", pointStart = timestamps[1], pointInterval = 15 * 60 * 1000) %>%
#   hc_add_theme(hc_theme_smpl())  # Optional: Apply a theme

# library(shiny)
# 
# # Specify the file paths for both datasets
# mw_file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/mw_new.csv"
# file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/max_min_avg.csv"
# peakhours_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/peakhours.csv"  # Update with the correct path
# 
# # Read the CSV files
# mw_data <- read.csv(mw_file_path)
# data <- read.csv(file_path)
# peakhours_data <- read.csv(peakhours_path)
# 
# # Convert the "Time" column to POSIXct format
# data$Time <- as.POSIXct(data$Time, format = "%Y-%m-%d")
# peakhours_data$Time <- as.POSIXct(peakhours_data$Time, format = "%m/%d/%Y %H:%M")
# mw_data$Time <- as.POSIXct(mw_data$Time, format = "%m/%d/%Y %H:%M")
# # Define UI
# ui <- fluidPage(
#   titlePanel("Date and Name Filter"),
#   
#   # Date filter
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("date", "Select Date", choices = unique(data$Time), selected = unique(data$Time)[1]),
#       selectInput("name", "Select Name", choices = NULL),
#     ),
#     mainPanel(
#       # Display the table output for the filtered data
#       tableOutput("table"),
#       # Display the 24-hour data table
#       # tableOutput("hourlyData")
#     )
#   )
# )
# 
# # Define server
# server <- function(input, output, session) {
#   
#   # Update the name filter choices based on the selected date
#   observeEvent(input$date, {
#     names_for_selected_date <- data$Name[data$Time == input$date]
#     updateSelectInput(session, "name", choices = names_for_selected_date)
#   })
#   
#   output$table <- renderTable({
#     # Extract the date part from the selected date
#     selected_date <- as.Date(input$date)
#     
#     # Subset the data by the selected date and name from "peakhours_data"
#     filtered_data <- peakhours_data[as.Date(peakhours_data$Time) == selected_date & peakhours_data$Name == input$name, ]
#     
#     # Format the "Time" column as datetime (adjust format as needed)
#     filtered_data$Time <- format(filtered_data$Time, "%Y-%m-%d %H:%M:%S")
#     
#     # Return the filtered data as a table
#     return(filtered_data)
#   })
#   
#   # output$hourlyData <- renderTable({
#   #   # Extract the date part from the selected date
#   #   selected_date <- as.Date(input$date)
#   #   
#   #   # Subset the mw_data by the selected date and name
#   #   hourly_data <- mw_data[mw_data$Time == selected_date & mw_data$Name == input$name, c("Time", "Name", "Energy_MWh")]
#   #   
#   #   # Format the "Time" column as datetime (adjust format as needed)
#   #   hourly_data$Time <- format(hourly_data$Time, "%Y-%m-%d %H:%M:%S")
#   #   
#   #   # Return the 24-hour data as a table
#   #   return(hourly_data)
#   # })
# }
# 
# # Run the Shiny app
# shinyApp(ui, server)


library(shiny)
library(highcharter)
library(dplyr)
library(lubridate)

# Specify the file paths for both datasets
mw_file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/mw_new.csv"
file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/max_min_avg.csv"
peakhours_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/peakhours.csv"  # Update with the correct path

# Read the CSV files
mw_data <- read.csv(mw_file_path)
data <- read.csv(file_path)
peakhours_data <- read.csv(peakhours_path)

# Convert the "Time" column to POSIXct format
data$Time <- as.POSIXct(data$Time, format = "%Y-%m-%d")
peakhours_data$Time <- as.POSIXct(peakhours_data$Time, format = "%m/%d/%Y %H:%M")
mw_data$Time <- as.POSIXct(mw_data$Time, format = "%Y-%m-%d %H:%M")

# Initialize an empty global_df dataframe
global_df <- data.frame(Time = as.POSIXct(character()), Name = character(), Energy = numeric())
# Initialize an empty global_df dataframe
global_df_hourly_data <- data.frame(Time = as.POSIXct(character()), Name = character(), Energy = numeric())
# Create a combined data frame
combined_data <- bind_rows(
  select(peakhours_data, Time, peak_values, name),
  select(mw_data, Time, Energy_MWh, Name)
) %>%
  arrange(Time)

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
      tableOutput("table"),
      tableOutput("hourlyData"),
      highchartOutput("areaspline_chart"),
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
  
  # Append rows from peakhours_data to global_df
  observe({
    global_df <- global_df[0, ]
    selected_date <- as.Date(input$date)
    filtered_data <- peakhours_data[as.Date(peakhours_data$Time) == selected_date & peakhours_data$name == input$name, ]
    global_df <<- rbind(global_df, data.frame(Time = as.POSIXct(filtered_data$Time, format = "%Y-%m-%d %H:%M:%S"), Name = filtered_data$name, Energy = filtered_data$peak_values))
  })
  
  # Append rows from hourly_data to global_df
  observe({
    selected_date <- as.Date(input$date)
    hourly_data <- mw_data[mw_data$Time >= selected_date & mw_data$Time < selected_date + lubridate::dhours(24) & mw_data$Name == input$name, c("Time", "Name", "Energy_MWh")]
    global_df <<- rbind(global_df, data.frame(Time = as.POSIXct(hourly_data$Time, format = "%Y-%m-%d %H:%M:%S"), Name = hourly_data$Name, Energy = hourly_data$Energy_MWh))
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
  
  output$hourlyData <- renderTable({
    # Extract the date part from the selected date
    selected_date <- as.Date(input$date)
    
    # # Print some debugging information
    # print(input$date) # Check the selected date
    # print(input$name) # Check the selected name
    
    # Subset the mw_data by the selected date and name
    hourly_data <- mw_data[mw_data$Time >= selected_date & mw_data$Time < selected_date + lubridate::dhours(24) & mw_data$Name == input$name, c("Time", "Name", "Energy_MWh")]
    
    # # Print some debugging information
    # print(names(hourly_data)) # Check the column names
    # print(unique(hourly_data$Name)) # Check unique "Name" values
    
    # Format the "Time" column as datetime (adjust format as needed)
    hourly_data$Time <- format(hourly_data$Time, "%Y-%m-%d %H:%M:%S")
    
    # Return the 24-hour data as a table
    return(hourly_data)
  })
  
  output$areaspline_chart <- renderHighchart({
    # Filter the combined data based on user input
    selected_date <- as.Date(input$date)
    selected_name <- input$name
    # filtered_data <- combined_data %>%
    #   filter(as.Date(Time) == selected_date, Name == selected_name)
    # Extract the date from the first row of global_df
    date_variable <- as.Date(global_df$Time[1])
    print(date_variable)
    # Remove the date part from the Time column
    global_df$Time <- format(global_df$Time, format = "%H:%M")
    # Sort the records in global_df by the Time column
    global_df <- global_df %>%
      arrange(Time)
    # print(filtered_data)
    print(global_df)
    
    # Create the Highcharter chart
    hc <- highchart() %>%
      hc_title(text = "Area Spline Chart") %>%
      hc_xAxis(type = "category", title = list(text = date_variable)) %>%
      hc_yAxis(title = list(text = "Energy (MWh)")) %>%
      hc_add_series(
        data = global_df,
        type = "areaspline", # Specify the chart type
        hcaes(x = Time, y = Energy),
        # hcaes(x = format(Time, "%Y-%m-%d %H:%M:%S"), y = Energy),
        name = selected_name
      )
    # # Create the Highcharter chart
    # hc <- highchart() %>%
    #   hc_title(text = "Area Spline Chart") %>%
    #   hc_xAxis(type = "datetime", title = list(text = "Time")) %>%
    #   hc_yAxis(title = list(text = "Energy (MWh)")) %>%
    #   hc_add_series(
    #     data = filtered_data,
    #     type = "areaspline", # Specify the chart type
    #     hcaes(x = format(Time, "%Y-%m-%d %H:%M:%S"), y = Energy_MWh),
    #     name = selected_name
    #   )
    
    return(hc)
  })
}

# Run the Shiny app
shinyApp(ui, server)
