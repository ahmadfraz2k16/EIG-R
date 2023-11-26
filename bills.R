library(pdftools)
test <- pdf_text("C:/Users/python/Documents/projR/MAR-2022/bills/sampleBillLESCO.pdf")
# cat(test)
# Extracting information
late_payment <- sub(".*LATE PAYMENT\\s+([0-9,.]+).*", "\\1", test)
total_payable <- sub(".*TOTAL PAYABLE\\s+Rs\\.\\s+([0-9,.]+).*", "\\1", test)
last_date <- sub(".*LAST DATE:\\s+(\\d+\\s+[a-zA-Z]+\\s+\\d+).*", "\\1", test)

# 
# bill_month <- sub(".*BILL MONTH:\\s+([a-zA-Z]+\\s+\\d+).*", "\\1", test)
# # Extracting information from BILL HISTORY
# bill_history <- sub("BILL HISTORY(.*?)REFERENCE NO:", "\\1", test, perl = TRUE)

# Print the extracted information
cat("Late Payment:", late_payment, "\n")
cat("Total Payable:", total_payable, "\n")
cat("Last Date:", last_date, "\n")
# cat("Bill Month:", bill_month, "\n")
# cat("Bill History:", bill_history, "\n")


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
cat("Bill History:\n", bill_history, "\n")

text_clean <- sub("REFERENCE NO:.*", "", bill_history)
# print(text_clean)
cat("Latest Bill History:\n", text_clean, "\n")

# Split the text into lines
lines <- strsplit(text_clean, "\n")[[1]]

# Extract the first line
# first_line <- trimws(lines[1])  # trimws removes leading and trailing whitespaces
first_line <- lines[1]
# Find the length of the first line up to the first newline character
first_line_length <- nchar(sub("\\n.*", "", first_line))

# Print the first line and its length
cat("First Line:", first_line, "\n")
cat("Length of First Line up to \\n:", first_line_length, "\n")
# Split the text_clean into lines
lines_clean <- strsplit(text_clean, "\n")[[1]]

# Extract the first 79 characters from each line
extracted_text <- sapply(lines_clean, function(line) substr(line, 1, 79))

# Combine the extracted text into a single string
billing_history <- paste(extracted_text, collapse = "\n")

# Print the result
cat("Extracted Text:\n", billing_history, "\n")

# Remove leading and trailing spaces for all lines
# trimmed_lines <- trimws(lines)


