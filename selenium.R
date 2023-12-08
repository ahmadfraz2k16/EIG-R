# Load libraries
library(shiny)
library(shinydashboard)
library(bslib)
library(dplyr)
library(tidyr)
library(highcharter)
library(lubridate)
library(pdftools)
library(stringr)
library(tidyverse)
# Read the CSV data
df <- read.csv("log.csv", stringsAsFactors = FALSE)
df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M")
unique(df$Tab)


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Bills Dashboard"),
  dashboardSidebar(
    width = 250,
    selectInput("username", "Select Username", choices = unique(df$Username)),
    dateInput("billingMonth", "Select Billing Month", format = "yyyy-mm", startview = "year", value = Sys.Date()),
    dateInput("meterReadingDate", "Select Meter Reading Date", format = "dd-mm-yyyy", value = Sys.Date())
  ),
  dashboardBody(
      div(
        class = "content",
        div(
          fileInput("pdfFile", "Upload PDF Bill", accept = ".pdf"),
          actionButton("uploadBtn", "Upload"),
          style = "padding-top: 27px;",
          verbatimTextOutput("status")
        )
      )
  )
)


# ui <- fluidPage(
#   theme = bs_theme(base_color = "#f5f5f5", bg = "white", fg = "black"),
#   page_navbar(
#     nav_panel("Bills Dashboard",
#               card(
#                 card_header(
#                   class = "bg-warning",
#                   "Bill Uploader"
#                 ),
#                 # Set minimum height of the card
#                 style = "min-height: 800px;",
#                 fluidRow(
#                   # Username dropdown
#                   column(4, selectInput("username", "Select Username", choices = unique(df$Username))),
#                   
#                   # Billing month dropdown (calendar type)
#                   column(4, dateInput("billingMonth", "Select Billing Month", format = "yyyy-mm", startview = "year", value = Sys.Date())),
#                   
#                   # Meter reading date input
#                   column(4, dateInput("meterReadingDate", "Select Meter Reading Date", format = "dd-mm-yyyy", value = Sys.Date())),
#                   column(4),
#                   # PDF file uploader and Upload button (spanned)
#                   column(3, 
#                          div(
#                            fileInput("pdfFile", "Upload PDF Bill", accept = ".pdf"),
#                            actionButton("uploadBtn", "Upload"),
#                            style = "padding-top: 27px;"
#                          )
#                   ),
#                   
#                   
#                   # Status message for successful upload
#                   verbatimTextOutput("status")
#                 )
#               )
#     )
#   )
# )

# Server version 3
server <- function(input, output, session) {
  filtered_data <- reactive({
    # Extracting username, billing month, and meter reading date from input
    username <- input$username
    billing_month <- format(input$billingMonth, "%Y-%m") # Format the billing_month
    meter_reading_date <- format(input$meterReadingDate, "%Y-%m-%d") # Format the 
    
    
    
    cat("Meter Reading Date:",meter_reading_date)
    # Check if a file is selected
    if (is.null(input$pdfFile)) {
      return(NULL)
    }
    
    # Get the selected file info
    pdf_file <- input$pdfFile
    # print(pdf_file)
    # Validate file size
    if (pdf_file$size > 2 * 1024^2) {
      showModal(modalDialog("Error: File size should not exceed 2 MB.", easyClose = TRUE))
      return(NULL)
    }
    
    # Read the contents of the PDF file
    pdf_text <- pdftools::pdf_text(pdf_file$datapath)
    # print(pdf_text)
    pattern <- "\\b([A-Z]{3} \\d{2})\\b"
    billing_month_pdf <- str_extract(pdf_text, pattern)
    billing_month_pdf <- as.character(billing_month_pdf)
    cat("Bill Month Pdf:", billing_month_pdf, "\n")
    # Convert the billing_month to a date object
    billing_month_date <- ym(billing_month)
    
    # Format the date as "MON YY"
    formatted_billing_month <- format(billing_month_date, "%b %y")
    formatted_billing_month <- as.character(formatted_billing_month)
    cat("Bill Month Filter:", toupper(formatted_billing_month), "\n")
    # Perform checks on the PDF content (replace this with your own logic)
    if (billing_month_pdf != toupper(formatted_billing_month)) {
      showModal(modalDialog(paste("Error: Billing Month doesnt match, your billing month, it should be (", billing_month_pdf, ")."), easyClose = TRUE))
      return(NULL)
    }
    
    replace_spaces_with_underscores <- function(input_string) {
      result <- gsub(" ", "_", input_string)
      return(result)
    }
    modified_string <- replace_spaces_with_underscores(toupper(billing_month_pdf))
    # Move the file to a specific directory (optional)
    destination_base <- "C:/Users/python/Documents/projR/MAR-2022/Bills_Check_Post"
    # Create a directory with the username if it doesn't exist
    user_directory <- file.path(destination_base, input$username)
    if (!dir.exists(user_directory)) {
      dir.create(user_directory)
    }
    # Create the new file name
    new_file_name <- paste0(input$username, "_", modified_string, ".pdf")
    # Create the full destination path
    destination_path <- file.path(user_directory, new_file_name)
    # Copy the file to the destination directory with the new name
    file.copy(pdf_file$datapath, destination_path, overwrite = TRUE)
    # data frame for csv file
    # Create a DataFrame
    meterReading_dataframe <- data.frame(username = username, billing_month = billing_month_pdf, reading_date = meter_reading_date )
    
    # csv file name
    csv_file_name <- paste0(input$username, "_", modified_string, ".csv")
    # Save the DataFrame to a CSV file with the specified name
    destination_path_csv <- file.path(user_directory, csv_file_name)
    # Save the DataFrame to a CSV file
    write_csv(meterReading_dataframe, destination_path_csv)
    # 
    # # Move the file to a specific directory (optional)
    # destination_directory <- "C:/Users/python/Documents/projR/MAR-2022/Bills_Check_Post"
    # destination_path <- file.path(destination_directory, pdf_file$name)
    # file.copy(pdf_file$datapath, destination_path, overwrite = TRUE)
    
    # Display a success message
    output$status <- renderText({
      paste("Pdf uploaded & csv created successfully. Destination:", destination_path)
    })
    
    # You can perform additional actions here
    
    # Return any result or NULL
    return(NULL)
  })
  
  # Additional action on upload button click (optional)
  observeEvent(input$uploadBtn, {
    filtered_data()
    # Additional actions you want to perform when the Upload button is clicked
  })
}

shinyApp(ui, server)
# Server version 2
# server <- function(input, output, session) {
#   filtered_data <- reactive({
#     # Extracting username, billing month, and meter reading date from input
#     username <- input$username
#     billing_month <- format(input$billingMonth, "%Y-%m") # Format the billing_month
#     meter_reading_date <- format(input$meterReadingDate, "%Y-%m-%d") # Format the 
#     
#     # Check if a file is selected
#     if (is.null(input$pdfFile)) {
#       return(NULL)
#     }
#     
#     # Get the selected file info
#     pdf_file <- input$pdfFile
#     
#     # Validate file size
#     if (pdf_file$size > 2 * 1024^2) {
#       showModal(modalDialog("Error: File size should not exceed 2 MB.", easyClose = TRUE))
#       return(NULL)
#     }
#     
#     # Move the file to a specific directory (optional)
#     destination_directory <- "C:/Users/python/Documents/projR/MAR-2022/Bills_Check_Post"
#     destination_path <- file.path(destination_directory, pdf_file$name)
#     file.copy(pdf_file$datapath, destination_path, overwrite = TRUE)
#     
#     # Display a success message
#     output$status <- renderText({
#       paste("File uploaded successfully. Destination:", destination_path)
#     })
#     
#     # You can perform additional actions here
#     
#     # Return any result or NULL
#     return(NULL)
#   })
#   
#   # Additional action on upload button click (optional)
#   observeEvent(input$uploadBtn, {
#     filtered_data()
#     # Additional actions you want to perform when the Upload button is clicked
#   })
# }


# Server version 1
# server <- function(input, output, session) {
#   filtered_data <- reactive({
#     # Extracting username, billing month, and meter reading date from input
#     username <- input$username
#     billing_month <- format(input$billingMonth, "%Y-%m") # Format the billing_month
#     meter_reading_date <- format(input$meterReadingDate, "%Y-%m-%d") # Format the 
#     
#     # Check if a file is selected
#     if (is.null(input$pdfFile)) {
#       return(NULL)
#     }
#     
#     # Get the selected file info
#     pdf_file <- input$pdfFile
#     
#     # Validate file size
#     if (pdf_file$size > 2 * 1024^2) {
#       showModal(modalDialog("Error: File size should not exceed 2 MB.", easyClose = TRUE))
#       return(NULL)
#     }
#     
#     # Move the file to a specific directory (optional)
#     destination_directory <- "C:/Users/python/Documents/projR/MAR-2022/Bills_Check_Post"
#     destination_path <- file.path(destination_directory, pdf_file$name)
#     file.copy(pdf_file$datapath, destination_path, overwrite = TRUE)
#     
#     # Modify filter accordingly
#     df_filtered <- df %>%
#       mutate(Time = as.POSIXct(Time)) %>%
#       filter(Username == username)
#     
#     # Filter by billing_month if it's not NULL
#     if (!is.null(billing_month)) {
#       df_filtered <- df_filtered %>%
#         filter(format(Time, "%Y-%m") == billing_month)
#     }
#     
#     # Filter by meter_reading_date if it's not NULL
#     if (!is.null(meter_reading_date)) {
#       df_filtered <- df_filtered %>%
#         filter(format(Time, "%Y-%m-%d") == meter_reading_date)
#     }
#     
#     # Display a success message
#     output$status <- renderText({
#       paste("File uploaded successfully. Destination:", destination_path)
#     })
#     
#     print(str(df_filtered))
#     return(df_filtered)
#   })
#   
#   # Additional action on upload button click (optional)
#   observeEvent(input$uploadBtn, {
#     filtered_data()
#     # Additional actions you want to perform when the Upload button is clicked
#   })
# }



