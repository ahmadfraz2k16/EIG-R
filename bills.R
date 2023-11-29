library(pdftools)
# Load the stringr package
library(stringr)
library(base)

test <- pdf_text("C:/Users/python/Documents/projR/MAR-2022/bills/sampleBillLESCO.pdf")
cat(test)
# Extracting information
late_payment <- sub(".*LATE PAYMENT\\s+([0-9,.]+).*", "\\1", test)
total_payable <- sub(".*TOTAL PAYABLE\\s+Rs\\.\\s+([0-9,.]+).*", "\\1", test)
last_date <- sub(".*LAST DATE:\\s+(\\d+\\s+[a-zA-Z]+\\s+\\d+).*", "\\1", test)
# Define a regular expression pattern to match the billing month
pattern <- "\\b([A-Z]{3} \\d{2})\\b"
billing_month <- str_extract(test, pattern)

# Print the extracted billing month
# print(billing_month)
# 
# bill_month <- sub(".*BILL MONTH:\\s+([a-zA-Z]+\\s+\\d+).*", "\\1", test)
# # Extracting information from BILL HISTORY
# bill_history <- sub("BILL HISTORY(.*?)REFERENCE NO:", "\\1", test, perl = TRUE)

# Print the extracted information
cat("Late Payment:", late_payment, "\n")
cat("Total Payable:", total_payable, "\n")
cat("Last Date:", last_date, "\n")
cat("Bill Month:", billing_month, "\n")
# cat("Bill History:", bill_history, "\n")

process_billing_history <- function(test) {
  # Split the text into lines
  lines <- strsplit(test, "\n")[[1]]
  
  # Find the index where "BILL HISTORY" starts
  start_index <- grep("MONTH", lines)
  
  # Initialize the end_index
  end_index <- NULL
  
  # Iterate through the lines to find the end of the bill history section
  for (i in (start_index + 1):length(lines)) {
    if (grepl("^REFERENCE NO:", lines[i])) {
      end_index <- i - 1
      break
    }
  }
  
  # If end_index is still NULL, use the length of lines
  if (is.null(end_index)) {
    end_index <- length(lines)
  }
  
  # Extract the lines corresponding to bill history
  bill_history_lines <- lines[start_index:end_index]
  
  # Combine the lines into a single string
  bill_history <- paste(bill_history_lines, collapse = "\n")
  
  # Print the extracted bill history
  # cat("Bill History:\n", bill_history, "\n")
  
  # Remove the text after "REFERENCE NO:"
  text_clean <- sub("REFERENCE NO:.*", "", bill_history)
  
  # Print the cleaned bill history
  # cat("Latest Bill History:\n", text_clean, "\n")
  
  # Split the cleaned text into lines
  lines_clean <- strsplit(text_clean, "\n")[[1]]
  
  # Extract the first 79 characters from each line
  extracted_text <- sapply(lines_clean, function(line) substr(line, 1, 79))
  
  # Combine the extracted text into a single string
  billing_history <- paste(extracted_text, collapse = "\n")
  
  # Print the result
  # cat("Extracted Text:\n", billing_history, "\n")
  
  # Return the final billing history
  return(billing_history)
}

result <- process_billing_history(test)
cat("Billing History:\n", result, "\n")
text <- readLines(textConnection(result))
text <- text[!grepl("^\\s*$", text) & !grepl("MONTH", text)]
text <- strsplit(text, "\\s+")
mat <- do.call(rbind, text)
df <- as.data.frame(mat)
colnames(df) <- c("st", "Month", "Units", "Bill", "Payment", "Adj", "dummy")
print(df)
# Check if "Adj" column contains "/"
has_slash <- grepl("/", df$Adj)
# If "Adj" column contains "/", remove specified columns
if (any(has_slash)) {
  # Create a new column with Adjustment values
  df$Adjustment <- ifelse(has_slash, paste(tail(df$Payment[has_slash], 1), "/", tail(df$dummy[has_slash], 1)), "")
  # Specify the columns to be removed
  columns_to_remove <- c("Adj", "dummy")
  
  # Remove the specified columns
  df <- df[, !names(df) %in% columns_to_remove]
  # Get the column names
  col_names <- names(df)
  
  # Identify the indices of the last two columns
  last_two_columns <- tail(seq_along(col_names), 2)
  
  # Swap the last two columns
  df <- df[, c(setdiff(seq_along(col_names), last_two_columns), rev(last_two_columns))]
}

# Save the DataFrame to a CSV file
write.csv(df, file = "bill_history.csv", row.names = FALSE)

# Print a message indicating the file has been saved
cat("DataFrame saved to 'bill_history.csv'\n")

# Print the updated DataFrame
print(df)






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
cat("peaks & off peaks and tax vs cost:\n", PEAKS_REFINED[[1]], "\n")
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
  # if (length(match_result) > 0) {
  #   cleaned_value <- gsub("[^0-9.-]", "", match_result)
  #   return(cleaned_value)
  # } else {
  #   return(NA)
  # }
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
# Save the DataFrame to a CSV file
write.csv(billing_summary, file = "billing_summary.csv", row.names = FALSE)

# Print a message indicating the file has been saved
cat("DataFrame saved to 'billing_summary.csv'\n")


# # Split the text into lines
# lines <- strsplit(test, "\n")[[1]]
# 
# # Find the index where "BILL HISTORY" starts
# start_index <- grep("MONTH", lines)
# 
# # Initialize the end_index
# end_index <- NULL
# 
# # Iterate through the lines to find the end of the bill history section
# for (i in (start_index + 1):length(lines)) {
#   if (grepl("^REFERENCE NO:", lines[i])) {
#     end_index <- i - 1
#     break
#   }
# }
# 
# # If end_index is still NULL, use the length of lines
# if (is.null(end_index)) {
#   end_index <- length(lines)
# }
# 
# # Extract the lines corresponding to bill history
# bill_history_lines <- lines[start_index:end_index]
# 
# # Combine the lines into a single string
# bill_history <- paste(bill_history_lines, collapse = "\n")
# 
# # Print the extracted bill history
# cat("Bill History:\n", bill_history, "\n")
# 
# text_clean <- sub("REFERENCE NO:.*", "", bill_history)
# # print(text_clean)
# cat("Latest Bill History:\n", text_clean, "\n")
# 
# # Split the text into lines
# lines <- strsplit(text_clean, "\n")[[1]]
# 
# # Extract the first line
# # first_line <- trimws(lines[1])  # trimws removes leading and trailing whitespaces
# first_line <- lines[1]
# # Find the length of the first line up to the first newline character
# first_line_length <- nchar(sub("\\n.*", "", first_line))
# 
# # Print the first line and its length
# cat("First Line:", first_line, "\n")
# cat("Length of First Line up to \\n:", first_line_length, "\n")
# # Split the text_clean into lines
# lines_clean <- strsplit(text_clean, "\n")[[1]]
# 
# # Extract the first 79 characters from each line
# extracted_text <- sapply(lines_clean, function(line) substr(line, 1, 79))
# 
# # Combine the extracted text into a single string
# billing_history <- paste(extracted_text, collapse = "\n")
# 
# # Print the result
# cat("Extracted Text:\n", billing_history, "\n")
# 
# # Remove leading and trailing spaces for all lines
# # trimmed_lines <- trimws(lines)



