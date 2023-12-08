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
library(pdftools)
library(rvest)
library(dplyr)
library (xml2)
# Load the stringr package
library(stringr)
library(base)
library(httr)
# Read the CSV data
df <- read.csv("log.csv", stringsAsFactors = FALSE)
df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M")
unique(df$Tab)
generate_billing_history <- function(first_customer_id, nBatchNo, nSubDiv, nRefNo, strRU, destination_path_csv_billing_history) {
  
  # Define the URL
  url <- "http://www.lesco.gov.pk:36269/Modules/CustomerBill/CustomerMenu.asp"
  
  # Define the payload
  payload <- list(
    # txtCustID = 7630316,
    txtCustID = first_customer_id,
    btnViewMenu = "Customer Menu"
  )
  
  # Define the headers as a named list
  headers <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "en-US,en;q=0.9",
    "Cache-Control" = "max-age=0",
    "Connection" = "keep-alive",
    "Content-Type" = "application/x-www-form-urlencoded",
    # "Cookie" = "ASPSESSIONIDQSBSDCST=PBEILDAABJONLDEPJNGIHBBJ; ASPSESSIONIDCCRTBCRR=IBDIMPFAIJDLCANNDLGAFDOG; ASP.NET_SessionId=aop5mbhupeo1ff4sfxw4pmd0",
    "Host" = "www.lesco.gov.pk:36269",
    "Origin" = "http://www.lesco.gov.pk:36269",
    "Referer" = "http://www.lesco.gov.pk:36269/Modules/CustomerBill/CustomerMenu.asp",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36 Edg/119.0.0.0"
  )
  
  # Make the POST request with headers
  response <- httr::POST(
    url,
    body = payload,
    encode = "form",
    add_headers(.headers = headers)
  )
  
  # Get the content of the response
  content <- httr::content(response, "text")
  
  # # Print the response content
  # cat(content)
  doc <- read_html (content)
  
  # Assuming you have already read the HTML content into 'doc'
  strong_text <- html_text(xml_find_first(doc, xpath = '//*[@id="ContentPane"]/font[1]/strong'))
  
  # Print the extracted text
  cat("Text inside strong tag:", strong_text, "\n")
  # Remove "Ref No :" from the text
  cleaned_text <- sub("Ref No : ", "", strong_text)
  
  # # Print the cleaned text
  # cat("Text inside strong tag (cleaned):", cleaned_text, "\n")
  # Split the cleaned text by spaces
  parts <- strsplit(cleaned_text, " ")[[1]]
  
  # Assign parts to variables
  nBatchNo <- parts[1]
  nSubDiv <- parts[2]
  dummy <- parts[3]
  
  # Extract digits from the dummy variable
  nRefNo <- gsub("\\D", "", dummy)
  
  # Extract the last character from the dummy variable
  strRU <- substr(dummy, nchar(dummy), nchar(dummy))
  
  # Print the variables
  cat("Ref No =", cleaned_text, "\n")
  cat("nBatchNo =", nBatchNo, "\n")
  cat("nSubDiv =", nSubDiv, "\n")
  cat("nRefNo =", nRefNo, "\n")
  cat("strRU =", strRU, "\n")
  
  
  
  # Define the URL
  url <- "http://www.lesco.gov.pk:36269/Modules/CustomerBill/History.asp"
  
  # Define the payload
  payload <- list(
    nBatchNo = nBatchNo,
    nSubDiv = nSubDiv,
    nRefNo = nRefNo,
    strRU = strRU,
    submit_param = "submit_value"
  )
  
  
  # Make the POST request with headers
  response <- httr::POST(
    url,
    body = payload,
    encode = "form",
    add_headers(.headers = headers)
  )
  
  # Get the content of the response
  content <- httr::content(response, "text")
  
  # # Print the response content
  # cat(content)
  
  
  
  doc <- read_html (content)
  
  table_node <- xml_find_first (doc, xpath = '//*[@id="ContentPane"]/table')
  
  table_string <- as.character (table_node)
  
  # print (table_string)
  
  # now using rvest and dplyr
  # Parse HTML
  html_table <- read_html(table_string) %>%
    html_node("table") %>%
    html_table()
  
  # # Print the data frame
  # print(html_table)
  # Remove the existing header
  names(html_table) <- NULL
  
  # Set the first row as the header
  header <- as.character(html_table[1, ])
  html_table <- html_table[-1, ]
  
  # Assign the header to the data frame
  names(html_table) <- header
  
  # Print the modified data frame
  print(html_table)
  # Save the DataFrame to a CSV file
  write_csv(html_table, destination_path_csv_billing_history)
  
}


generate_billing_summary <- function(test, meterReading_dataframe, destination_path_csv_billing_summary, destination_path_csv_billing_history){
  # Example customer ID pattern
  # Define the pattern for a 7-digit customer ID
  pattern_custID <- "\\b\\d{7}\\b"
  
  # Extract all occurrences of 7-digit customer IDs
  customer_ids <- regmatches(test, gregexpr(pattern_custID, test))
  
  # # Flatten the list and print the customer IDs
  # cat("Customer IDs:", unlist(customer_ids), "\n")
  # Grab the first occurrence
  first_customer_id <- customer_ids[[1]][1]
  
  # Print the first customer ID
  cat("First Customer ID:", first_customer_id, "\n")
  
  
  late_payment <- sub(".*LATE PAYMENT\\s+([0-9,.]+).*", "\\1", test)
  total_payable <- sub(".*TOTAL PAYABLE\\s+Rs\\.\\s+([0-9,.]+).*", "\\1", test)
  last_date <- sub(".*LAST DATE:\\s+(\\d+\\s+[a-zA-Z]+\\s+\\d+).*", "\\1", test)
  # Define a regular expression pattern to match the billing month
  pattern <- "\\b([A-Z]{3} \\d{2})\\b"
  billing_month <- str_extract(test, pattern)
  
  
  cat("Late Payment:", late_payment, "\n")
  cat("Total Payable:", total_payable, "\n")
  cat("Last Date:", last_date, "\n")
  cat("Bill Month:", billing_month, "\n")
  # cat("Bill History:", bill_history, "\n")
  
  
  
  extract_meter_reading <- function(test) {
    # Split the text into lines
    lines <- strsplit(test, "\n")[[1]]
    
    return(trimws(lines))
  }
  
  # Example usage
  extracted_text <- extract_meter_reading(test)
  # cat("Extracted Text:\n", extracted_text, "\n")
  
  
  
  # Define the function
  extract_and_remove_before_keyword <- function(test, keyword) {
    # Split the text into lines
    lines <- strsplit(test, "\n")[[1]]
    
    # Find the index of the keyword
    keyword_index <- grep(keyword, lines, fixed = TRUE)
    
    # Initialize variables for removed and remaining text
    removed_text <- ""
    remaining_text <- ""
    
    # If the keyword is found, extract lines after the keyword
    if (length(keyword_index) > 0) {
      removed_text <- paste(lines[1:(keyword_index[1] - 1)], collapse = "\n")
      remaining_text <- paste(lines[keyword_index[1]:length(lines)], collapse = "\n")
    } else {
      remaining_text <- paste(lines, collapse = "\n")
    }
    
    return(list(trimws(remaining_text), trimws(removed_text)))
  }
  
  # Example usage
  result <- extract_and_remove_before_keyword(test, "CURRENT BILL")
  # cat("Modified Text:\n", result[[1]], "\n")
  # cat("Removed Text:\n", result[[2]], "\n")
  
  
  
  PEAKS <- result[[2]]
  PEAKS_REFINED <- extract_and_remove_before_keyword(PEAKS, "METER READING")
  # cat("peaks & off peaks and tax vs cost:\n", PEAKS_REFINED[[1]], "\n")
  # cat("Removed Text:\n", PEAKS_REFINED[[2]], "\n")
  # Regular expressions to capture values
  total_cost_pattern <- "Total\\s*=\\s*([\\d,.]+)"
  total_tax_pattern <- "TOTAL\\s*=\\s*([\\d,.]+)"
  off_peak_pattern <- "Off Peak\\s*([0-9,\\s-]+)\\s*-\\s*([0-9,\\s-]+)"
  peak_pattern <- "Peak\\s*([0-9,\\s-]+)\\s*-\\s*([0-9,\\s-]+)"
  match <- grepl (off_peak_pattern, PEAKS_REFINED[[1]])
  cat("Match off peak: ", match, "\n")
  peak_matched <- grepl (peak_pattern, PEAKS_REFINED[[1]])
  cat("Match off peak: ", peak_matched, "\n")
  #print the result cat ("Match: ", match, “\n”)
  
  
  # Function to extract values using regex
  extract_value <- function(pattern, text) {
    match_result <- regmatches(text, regexpr(pattern, text, perl = TRUE))
    if (length(match_result) > 0) {
      return(sub(pattern, "\\1", match_result))
    } else {
      return(NA)
    }
  }
  
  # Extract values
  total_cost_value <- extract_value(total_cost_pattern, PEAKS_REFINED[[1]])
  total_tax_value <- extract_value(total_tax_pattern, PEAKS_REFINED[[1]])
  off_peak_value <- extract_value(off_peak_pattern, PEAKS_REFINED[[1]])
  # Remove "Off Peak" from the text
  PEAKS_REFINED[[1]] <- gsub("Off Peak", "", PEAKS_REFINED[[1]])
  peak_value <- extract_value(peak_pattern, PEAKS_REFINED[[1]])
  
  # Print the extracted values
  cat("Total Cost without tax:", total_cost_value, "\n")
  cat("Total tax:", total_tax_value, "\n")
  cat("Off Peak:", off_peak_value, "\n")
  cat("Peak:", peak_value, "\n")
  
  # Function to extract numeric value from a string
  extract_numeric <- function(string) {
    numeric_part <- gsub("[^0-9.]", "", string)
    as.numeric(numeric_part)
  }
  
  # Extract numeric values
  total_cost_value <- extract_numeric(total_cost_value)
  total_tax_value <- extract_numeric(total_tax_value)
  
  # Print the extracted values
  cat("Total Cost without tax:", total_cost_value, "\n")
  cat("Total tax:", total_tax_value, "\n")
  
  
  # Create a data frame
  billing_summary <- data.frame(
    Late_Payment = late_payment,
    Total_Payable = total_payable,
    Last_Date = last_date,
    Bill_Month = billing_month,
    Total_Cost_Without_Tax = total_cost_value,
    Total_Tax = total_tax_value,
    Off_Peak = off_peak_value,
    Peak = peak_value
  )
  # Print the data frame
  print(billing_summary)
  # Combine the two data frames by column
  billing_summary_final <- cbind(billing_summary, meterReading_dataframe)
  # Save the DataFrame to a CSV file
  write_csv(billing_summary_final, destination_path_csv_billing_summary)
  path_billing_history <- destination_path_csv_billing_history
  print(path_billing_history)
  generate_billing_history(first_customer_id, nBatchNo, nSubDiv, nRefNo, strRU, path_billing_history)
}

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
    
    # # csv file name
    # csv_file_name <- paste0(input$username, "_", modified_string, ".csv")
    # # Save the DataFrame to a CSV file with the specified name
    # destination_path_csv <- file.path(user_directory, csv_file_name)
    # # Save the DataFrame to a CSV file
    # write_csv(meterReading_dataframe, destination_path_csv)
    
    
    
    # csv file name
    csv_file_name_billing_summary <- paste0(input$username, "_", modified_string,"_billing_summary", ".csv")
    csv_file_name_billing_history <- paste0(input$username, "_", modified_string,"_billing_history", ".csv")
    # Save the DataFrame to a CSV file with the specified name
    destination_path_csv_billing_summary <- file.path(user_directory, csv_file_name_billing_summary)
    # Save the DataFrame to a CSV file with the specified name
    destination_path_csv_billing_history <- file.path(user_directory, csv_file_name_billing_history)
    # Save the DataFrame to a CSV file
    write_csv(meterReading_dataframe, destination_path_csv_billing_summary)
    # Pass the content to the generate_billing_summary function
    generate_billing_summary(pdf_text, meterReading_dataframe, destination_path_csv_billing_summary, destination_path_csv_billing_history)
    # 
    # # Move the file to a specific directory (optional)
    # destination_directory <- "C:/Users/python/Documents/projR/MAR-2022/Bills_Check_Post"
    # destination_path <- file.path(destination_directory, pdf_file$name)
    # file.copy(pdf_file$datapath, destination_path, overwrite = TRUE)
    
    # Display a success message
    output$status <- renderText({
      paste("Pdf uploaded & csv created successfully. Destination:", user_directory)
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



