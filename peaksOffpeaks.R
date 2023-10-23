library(shiny)
library(shinyWidgets) # Load the shinyWidgets library for icons
library(bslib)

int <- function(x) {
  as.integer(floor(x))
}
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
      # tableOutput("table"),
      # uiOutput("cards"),
      uiOutput("valueBoxes")
    )
  )
)

# Define server
server <- function(input, output, session) {
  observeEvent(input$date, {
    names_for_selected_date <- data$name[data$Time == input$date]
    updateSelectInput(session, "name", choices = names_for_selected_date)
  })
  
  output$valueBoxes <- renderUI({
    selected_date <- as.Date(input$date)
    filtered_data <- data[as.Date(data$Time) == selected_date & data$name == input$name, ]
    
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
  # # Update the name filter choices based on the selected date
  # observeEvent(input$date, {
  #   names_for_selected_date <- data$name[data$Time == input$date]
  #   updateSelectInput(session, "name", choices = names_for_selected_date)
  # })
  # 
  # # output$table <- renderTable({
  # #   # Subset the data by the selected date and name
  # #   filtered_data <- data[data$Time == input$date & data$name == input$name, ]
  # #   # Convert the "Time" column to a character with the desired format
  # #   filtered_data$Time <- format(filtered_data$Time, format = "%Y-%m-%d")
  # #   # Return the filtered data as a table
  # #   return(filtered_data)
  # # })
  # # Create a function to generate a Bootstrap card
  # generate_card <- function(title, icon, value, unit, name) {
  #   card_div <- tags$div(
  #     class = "col-sm-12 col-md-4",
  #     div(
  #       class = "card",
  #       style = "border: 1px solid #ccc;",
  #       div(
  #         class = "card-body",
  #         div(
  #           class = "d-flex flex-row",
  #           div(
  #             div(
  #               class = "m-l-10 align-self-center",
  #               h4(class = "m-b-0", title)
  #             ),
  #             div(
  #               class = "ml-auto align-self-center",
  #               h2(class = "font-medium m-b-0", value, span(class = "lead h6", unit),
  #                  h5(class = "font-medium m-b-0", name)
  #               )
  #             )
  #           )
  #         )
  #       )
  #     )
  #   )
  #   return(card_div)
  # }
  # 
  # # Render the cards
  # output$cards <- renderUI({
  #   # Extract the date part from the selected date
  #   selected_date <- as.Date(input$date)
  #   
  #   # Subset the data by the selected date and name from "data"
  #   filtered_data <- data[as.Date(data$Time) == selected_date & data$name == input$name, ]
  #   
  #   # Create a list of cards
  #   cards <- list(
  #     div(class = "container-fluid", id = "max_min",
  #         div(
  #           div(id = "cardContainer", class = "row justify-content-between",
  #               div(class = "text-center", h4(class = "text-danger", paste0("ALL DAY : ", as.character(filtered_data$time_range_one)))),
  #               generate_card("MAX", "ti-bolt", filtered_data$max_1, "MW", input$name),
  #               generate_card("MIN", "ti-bolt", filtered_data$min_1, "MW", input$name),
  #               generate_card("AVERAGE", "ti-bolt", int(filtered_data$average), "MW", input$name)
  #           ),
  #           div(id = "cardContainer", class = "row justify-content-between",
  #               div(class = "text-center", h4(class = "text-danger", paste0("OFF-PEAK DURATION: ", as.character(filtered_data$time_range_two)))),
  #               generate_card("MAX", "ti-bolt", filtered_data$max_2, "MW", input$name),
  #               generate_card("MIN", "ti-bolt", filtered_data$min_2, "MW", input$name)
  #           ),
  #           div(id = "cardContainer", class = "row justify-content-between",
  #               div(class = "text-center", h4(class = "text-danger", paste0("PEAK DURATION: ", as.character(filtered_data$time_range_three)))),
  #               generate_card("MAX", "ti-bolt", filtered_data$max_3, "MW", input$name),
  #               generate_card("MIN", "ti-bolt", filtered_data$min_3, "MW", input$name)
  #           )
  #         )
  #     )
  #   )
  #   
  #   # Return the list of cards
  #   tagList(cards)
  # })
  
}

# Run the Shiny app
shinyApp(ui, server)