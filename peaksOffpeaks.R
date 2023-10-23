library(shiny)
library(shinyWidgets) # Load the shinyWidgets library for icons
library(bslib)

int <- function(x) {
  as.integer(floor(x))
}
# Specify the file path
file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/max_min_avg.csv"

# Read the CSV file
data_min_max_avg <- read.csv(file_path)

## Convert the "Time" column to Date format
# data_min_max_avg$Time <- as.Date(data_min_max_avg$Time, format = "%Y-%m-%d")
# Convert the Time column to Date format using as.POSIXct
data_min_max_avg$Time <- as.POSIXct(data_min_max_avg$Time, format = "%Y-%m-%d")

# Check the structure of the data_min_max_avg again to confirm the change
str(data_min_max_avg)
uniqueDates <- unique(data_min_max_avg$Time)
# Define UI
ui <- fluidPage(
  titlePanel("Date and Name Filter"),
  
  # Date filter
  sidebarLayout(
    sidebarPanel(
      selectInput("date_min_max_avg", "Select Date", choices = uniqueDates, selected = uniqueDates[1]),
      selectInput("name_min_max_avg", "Select Name", choices = NULL)
    ),
    mainPanel(
      uiOutput("valueBoxes_min_max_avg")
    )
  )
)

# Define server
server <- function(input, output, session) {
  observeEvent(input$date_min_max_avg, {
    names_for_selected_date <- data_min_max_avg$name[data_min_max_avg$Time == input$date_min_max_avg]
    updateSelectInput(session, "name_min_max_avg", choices = names_for_selected_date)
  })
  
  output$valueBoxes_min_max_avg <- renderUI({
    # Subset the data_min_max_avg by the selected date and name
    filtered_data <- data_min_max_avg[data_min_max_avg$Time == input$date_min_max_avg & data_min_max_avg$name == input$name_min_max_avg, ]
    # Convert the "Time" column to a character with the desired format
    filtered_data$Time <- format(filtered_data$Time, format = "%Y-%m-%d")
    
    # Define value boxes
    value_boxes <- list(
      fluidRow(
        h3(paste0("ALL DAY : ", as.character(filtered_data$time_range_one))),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$max_1,
            subtitle = "MAX",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MAX")
          )
        ),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$min_1,
            subtitle = "MIN",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MIN")
          )
        ),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = int(filtered_data$average),
            subtitle = "AVERAGE",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("AVERAGE")
          )
        )
      ),
      fluidRow(
        h3(paste0("OFF-PEAK DURATION : ", as.character(filtered_data$time_range_two))),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$max_2,
            subtitle = "MAX",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MAX")
          )
        ),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$min_2,
            subtitle = "MIN",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MIN")
          )
        )
      ),
      fluidRow(
        h3(paste0("PEAK DURATION : ", as.character(filtered_data$time_range_three))),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$max_3,
            subtitle = "MAX",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MAX")
          )
        ),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$min_3,
            subtitle = "MIN",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MIN")
          )
        )
      )
    )
    
    tagList(value_boxes)
  })
  
}

# Run the Shiny app
shinyApp(ui, server)