library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(tidyr)

process_total_cost_without_tax <- function(dataframe_index) {
  # Assuming df7 is the seventh data frame
  df7 <- dataframes[dataframe_index]
  # # Print or inspect the result
  # print(df7[[1]]$dataframe[, c("X1")], n = Inf)
  # print(df7[[1]]$dataframe[, c("X2")], n = Inf)
  # Assuming df7 is the seventh data frame
  total_row <- df7[[1]]$dataframe[df7[[1]]$dataframe$X1 == "TOTAL", "X2"]
  # Assuming total_row is your data frame
  colnames(total_row)[colnames(total_row) == "X2"] <- "Total_Cost_Without_Tax"
  
  # Print the result
  print(total_row)
  return(total_row)
}
process_reading_date <- function(dataframe_index) {
  # Access the first data frame
  df <- dataframes[[dataframe_index]]$dataframe
  # Access columns X5
  column_X5_reading_date <- df$X5
  # # Print the values
  # cat("Reading Date:", column_X5_reading_date, "\n")
  # return(column_X5_reading_date)
  # Remove the prefix "READING DATE "
  cleaned_date <- gsub("^READING DATE ", "", column_X5_reading_date)
  # Print the cleaned date
  print(cleaned_date[2])
  return(cleaned_date[2])
}
process_peak_offpeak <- function(dataframe_index) {
  # Assuming df7 is the seventh data frame
  df7 <- dataframes[dataframe_index]
  peakOffpeak <- df7[[1]]$nested_tables[[2]]$dataframe
  # Make the first row as header
  colnames(peakOffpeak) <- peakOffpeak[1, ]
  # Remove the first row
  peakOffpeak <- peakOffpeak[-1, , drop = FALSE]
  # Remove the first column
  peakOffpeak <- peakOffpeak[, -1, drop = FALSE]
  # Remove the first two rows
  peakOffpeak <- peakOffpeak[-c(1, 2), , drop = FALSE]
  # Print or inspect the result
  print(peakOffpeak)
  return(peakOffpeak)
}
# process_billing_summary_v2 <- function(dataframe_index) {
#   # Access the 15th data frame
#   df <- dataframes[[dataframe_index]]$dataframe
#   
#   # Convert X5 to character type
#   df$X5 <- as.character(df$X5)
#   # Create a new dataframe with X4 and X5
#   df_transposed <- as.data.frame(t(df[, c("X4", "X5")]), row.names = FALSE)
#   
#   # Assign the desired column names
#   colnames(df_transposed) <- c("X4", "X5")
#   # Assuming df is your dataframe
#   df <- df %>% select(-X4, -X5)
#   # Combine dataframes by column
#   merged_df <- cbind(df, df_transposed)
#   # # Print the merged dataframe
#   # print(merged_df$X3[2])
#   # print(merged_df$X4[2])
#   # print(merged_df$X5[2])
#   
#   # Create an empty dataframe with specific columns
#   empty_dataframe <- data.frame(
#     Late_Payment = character(),
#     Total_Payable = character(),
#     Last_Date = character(),
#     Bill_Month = character(),
#     stringsAsFactors = FALSE
#   )
#   
#   # Set explicit column names
#   # Initialize the dataframe with some data
#   empty_dataframe <- data.frame(matrix(ncol = 4, nrow = 1))
#   colnames(empty_dataframe) <- c("Late_Payment", "Total_Payable", "Last_Date", "Bill_Month")
#   
#   # Assign values to the first row
#   empty_dataframe$Late_Payment[1] <- as.character(merged_df$X5[2])
#   empty_dataframe$Total_Payable[1] <- as.character(merged_df$X4[2])
#   empty_dataframe$Last_Date[1] <- as.character(merged_df$X2[2])
#   empty_dataframe$Bill_Month[1] <- as.character(merged_df$X1[2])
#   # Print or inspect the dataframe
#   print(empty_dataframe)
#   return(empty_dataframe)
# }
process_billing_summary <- function(dataframe_index, processed_peak_offpeak, processed_reading_date, processed_consumer_id, processed_total_cost_without_tax) {
  # Access the 15th data frame
  df <- dataframes[[dataframe_index]]$dataframe
  
  # Convert X5 to character type
  df$X5 <- as.character(df$X5)
  # Create a new dataframe with X4 and X5
  df_transposed <- as.data.frame(t(df[, c("X4", "X5")]), row.names = FALSE)
  
  # Assign the desired column names
  colnames(df_transposed) <- c("X4", "X5")
  # Assuming df is your dataframe
  df <- df %>% select(-X4, -X5)
  # Combine dataframes by column
  merged_df <- cbind(df, df_transposed)
  # # Print the merged dataframe
  # print(merged_df$X3[2])
  # print(merged_df$X4[2])
  # print(merged_df$X5[2])
  
  # Create an empty dataframe with specific columns
  empty_dataframe <- data.frame(
    Late_Payment = character(),
    Total_Payable = character(),
    Last_Date = character(),
    Bill_Month = character(),
    stringsAsFactors = FALSE
  )
  
  # Set explicit column names
  # Initialize the dataframe with some data
  empty_dataframe <- data.frame(matrix(ncol = 10, nrow = 1))
  colnames(empty_dataframe) <- c("Late_Payment", "Total_Payable", "Last_Date", "Bill_Month", "Total_Cost_Without_Tax",	"Total_Tax",	"Off_Peak",	"Peak", "reading_date", "consumer_id")
  # total payable - total cost without tax = total tax
  Total_Tax <- round(as.numeric(merged_df$X4[2])) - round(as.numeric(processed_total_cost_without_tax$Total_Cost_Without_Tax))
  # Assign values to the first row
  empty_dataframe$Late_Payment[1] <- as.character(merged_df$X5[2])
  empty_dataframe$Total_Payable[1] <- as.character(merged_df$X4[2])
  empty_dataframe$Last_Date[1] <- as.character(merged_df$X2[2])
  empty_dataframe$Bill_Month[1] <- as.character(merged_df$X1[2])
  empty_dataframe$Total_Cost_Without_Tax[1] <- as.character(round(as.numeric(processed_total_cost_without_tax$Total_Cost_Without_Tax)))
  empty_dataframe$Total_Tax[1] <- as.character(Total_Tax)
  empty_dataframe$Off_Peak[1] <- as.character(processed_peak_offpeak$`Off Peak`)
  empty_dataframe$Peak[1] <- as.character(processed_peak_offpeak$Peak)
  empty_dataframe$reading_date[1] <- as.character(processed_reading_date)
  empty_dataframe$consumer_id[1] <- as.character(processed_consumer_id)
  # Print or inspect the dataframe
  print(empty_dataframe)
  return(empty_dataframe)
}
process_billing_history <- function(dataframe_index, csv_file_path) {
  # Assuming 'your_dataframe' is the name of your data frame
  billing_history <- dataframes[[dataframe_index]]$dataframe

  # Make the first row the header
  new_header <- billing_history[1, ]
  billing_history <- billing_history[-1, ]

  # Set the new header
  colnames(billing_history) <- new_header
  print(billing_history)
  # Specify the file path where you want to save the CSV file
  csv_file_path <- "C:/Users/python/Documents/projR/MAR-2022/bill_history.csv"
  # Save the DataFrame as a CSV file
  write.csv(billing_history, file = csv_file_path, row.names = FALSE)
  return(billing_history)
}
process_consumer_id_dataframe <- function(dataframe_index) {
  # Assuming df1 is the specified data frame
  df <- t(dataframes[[dataframe_index]]$dataframe)  # Assign your actual data frame here
  # Assign the first row as column names
  colnames(df) <- as.character(unlist(df[1, ]))
  # Remove the first row
  df <- df[-1, , drop = FALSE]
  # Reset row names
  rownames(df) <- NULL
  # Convert to a data frame
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # Rename the column to "CONSUMER ID"
  df <- setNames(df, "CONSUMER ID")
  print(df)
  # Return the dataframe
  return(df)
}
# Specify the URL and parameters
url <- "https://bill.pitc.com.pk/iescobill/general"
appno <- 1141736793

# Set headers
headers <- c(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:120.0) Gecko/20100101 Firefox/120.0",
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
  "Accept-Language" = "en-US,en;q=0.5",
  "Accept-Encoding" = "gzip, deflate, br",
  "Referer" = "https://bill.pitc.com.pk/iescobill",
  "Connection" = "keep-alive",
  # "Cookie" = "ASP.NET_SessionId=abishlaha5551pwgpn2rugvb; __RequestVerificationToken=CY4KIbs_pYSn2eMox-cFj2WHAKtS-LoKmX66pxUvVCWcWoj8LDCNqEVFqa2glnPDh9yM82y_CWCNlt7_x7fJAcPQR-6JIx33XhhIFWfCYe41",
  "Upgrade-Insecure-Requests" = "1",
  "Sec-Fetch-Dest" = "document",
  "Sec-Fetch-Mode" = "navigate",
  "Sec-Fetch-Site" = "same-origin",
  "Sec-Fetch-User" = "?1",
  "TE" = "trailers"
)

# Make the GET request
response <- httr::GET(url, query = list(appno = appno), add_headers(headers), config = httr::config(ssl_verifypeer = 0L))

# Extract the HTML content from the response
html_content <- content(response, "text")
# cat(html_content)  # Print or use the HTML content as needed

# # Parse the HTML content
# html <- read_html(html_content)

# Function to recursively extract nested tables
extract_nested_tables <- function(table_node) {
  df <- html_table(table_node, fill = TRUE)
  nested_tables <- html_nodes(table_node, "table")
  
  if (length(nested_tables) > 0) {
    nested_dataframes <- lapply(nested_tables, extract_nested_tables)
    return(list(dataframe = df, nested_tables = nested_dataframes))
  } else {
    return(list(dataframe = df, nested_tables = NULL))
  }
}

# Parse the HTML content
html <- read_html(html_content)

# Extract tables
top_level_tables <- html_nodes(html, "table")

# Convert each top-level table to a data frame and handle nested tables
dataframes <- lapply(top_level_tables, extract_nested_tables)

# Function to display data frames and nested tables
display_dataframes <- function(dataframes_list, level = 1) {
  for (i in seq_along(dataframes_list)) {
    cat(rep("  ", level), "Data Frame", i, ":\n")
    print(dataframes_list[[i]]$dataframe)
    
    nested_tables <- dataframes_list[[i]]$nested_tables
    if (!is.null(nested_tables)) {
      display_dataframes(nested_tables, level + 1)
    }
    
    cat("\n")
  }
}

# # Display each top-level data frame and its nested tables
# display_dataframes(dataframes)


# # Assuming 'your_dataframe' is the name of your data frame
# billing_history <- dataframes[[6]]$dataframe
# 
# # Make the first row the header
# new_header <- billing_history[1, ]
# billing_history <- billing_history[-1, ]
# 
# # Set the new header
# colnames(billing_history) <- new_header
# print(billing_history)
# # Specify the file path where you want to save the CSV file
# csv_file_path <- "C:/Users/python/Documents/projR/MAR-2022/bill_history.csv"
# # Save the DataFrame as a CSV file
# write.csv(billing_history, file = csv_file_path, row.names = FALSE)






# print(dataframes[[1]]$dataframe)
# # Access the first data frame
# df <- dataframes[[1]]$dataframe
# # Access columns X5
# column_X5_reading_date <- df$X5
# # Print the values
# cat("X5 Reading Date:", column_X5_reading_date, "\n")






# 
# # Assuming df1 is the second data frame
# df1 <- t(dataframes[[14]]$dataframe)  # Assign your actual data frame here
# print(df1, row.names = FALSE)
# colnames(df1) <- as.character(unlist(df1[1,])) # assign first row as column names
# df1 <- df1[-1, ] # remove first row
# rownames(df1) <- NULL # remove row names
# df1 <- as.data.frame(df1) # convert vector to dataframe
# df1 <- setNames(df1, "CONSUMER ID") # rename column
# print(df1) # print updated dataframe
# # Example usage:






# # Assuming df7 is the seventh data frame
# df7 <- dataframes[7]
# # # Print or inspect the result
# # print(df7[[1]]$dataframe[, c("X1")], n = Inf)
# # print(df7[[1]]$dataframe[, c("X2")], n = Inf)
# # Assuming df7 is the seventh data frame
# total_row <- df7[[1]]$dataframe[df7[[1]]$dataframe$X1 == "TOTAL", "X2"]
# # Assuming total_row is your data frame
# colnames(total_row)[colnames(total_row) == "X2"] <- "Total_Cost_Without_Tax"
# 
# # Print the result
# print(total_row)








# peakOffpeak <- df7[[1]]$nested_tables[[2]]$dataframe
# # Print or inspect the result
# print(peakOffpeak)
# # Make the first row as header
# colnames(peakOffpeak) <- peakOffpeak[1, ]
# 
# # Remove the first row
# peakOffpeak <- peakOffpeak[-1, , drop = FALSE]
# 
# # Remove the first column
# peakOffpeak <- peakOffpeak[, -1, drop = FALSE]
# 
# # Remove the first two rows
# peakOffpeak <- peakOffpeak[-c(1, 2), , drop = FALSE]
# 
# # Print or inspect the result
# print(peakOffpeak)







# # Display each top-level data frame and its nested tables
# display_dataframes(dataframes[15])
# # Access the first data frame
# df15 <- dataframes[[15]]$dataframe
# 
# # Access the 15th data frame
# df <- dataframes[[15]]$dataframe
# 
# # Convert X5 to character type
# df$X5 <- as.character(df$X5)
# # Create a new dataframe with X4 and X5
# df_transposed <- as.data.frame(t(df[, c("X4", "X5")]), row.names = FALSE)
# 
# # Assign the desired column names
# colnames(df_transposed) <- c("X4", "X5")
# 
# # Print the rearranged data frame
# print(df_transposed)
# # Assuming df is your dataframe
# df <- df %>% select(-X4, -X5)
# 
# # Print the rearranged data frame
# print(df)
# # Combine dataframes by column
# merged_df <- cbind(df, df_transposed)
# print(merged_df)
# str(merged_df)
# # Print the merged dataframe
# print(merged_df$X3[2])
# print(merged_df$X4[2])
# print(merged_df$X5[2])
# 
# # Create an empty dataframe with specific columns
# empty_dataframe <- data.frame(
#   Late_Payment = character(),
#   Total_Payable = character(),
#   Last_Date = character(),
#   Bill_Month = character(),
#   stringsAsFactors = FALSE
# )
# 
# # Set explicit column names
# # Initialize the dataframe with some data
# empty_dataframe <- data.frame(matrix(ncol = 4, nrow = 1))
# colnames(empty_dataframe) <- c("Late_Payment", "Total_Payable", "Last_Date", "Bill_Month")
# 
# # Assign values to the first row
# empty_dataframe$Late_Payment[1] <- as.character(merged_df$X5[2])
# empty_dataframe$Total_Payable[1] <- as.character(merged_df$X4[2])
# empty_dataframe$Last_Date[1] <- as.character(merged_df$X2[2])
# empty_dataframe$Bill_Month[1] <- as.character(merged_df$X1[2])
# 
# # Print or inspect the dataframe
# print(empty_dataframe)



# Example usage with dataframe 6
csv_file_path <- "C:/Users/python/Documents/projR/MAR-2022/bill_history.csv"
processed_billing_history <- process_billing_history(6, csv_file_path)

processed_peak_offpeak <- process_peak_offpeak(7)
processed_reading_date <- process_reading_date(1)
processed_consumer_id <- process_consumer_id_dataframe(14)
processed_total_cost_without_tax <- process_total_cost_without_tax(7)
processed_billing_summary <- process_billing_summary(15, processed_peak_offpeak, processed_reading_date, processed_consumer_id, processed_total_cost_without_tax)
# Late_Payment	Total_Payable	 Last_Date	  Bill_Month	Total_Cost_Without_Tax	Total_Tax	  
# 27,116	        27,116	      15-Nov-23  	23-Oct	       2265.82	           62	         
# Off_Peak  	Peak	 username	 billing_month	reading_date
# 13455	    3377	  ahmad	    23-Oct	       11/1/2023
# Total_Tax	     Off_Peak  	    Peak	      username	   billing_month	        reading_date
# 62	            13455	        3377	        ahmad	       23-Oct	               11/1/2023




# # Extract tables
# tables <- html_nodes(html, "table")
# 
# # Convert each table to a data frame
# dataframes <- lapply(tables, function(table_node) {
#   df <- html_table(table_node, fill = TRUE)
#   return(df)
# })
# 
# # Display each data frame
# for (i in seq_along(dataframes)) {
#   cat("Data Frame", i, ":\n")
#   print(dataframes[[i]])
#   cat("\n\n")
# }