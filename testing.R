# IMPORTING 
library(readxl)
library(tidyverse)
# load the data.table package
library(data.table)
# load the dplyr package
library(dplyr)
# load magrittr or dplyr package
library(magrittr)
# for picking dates from file names
library(stringr)
library(lubridate)


process_mw_sheets <- function(fetched_file_name, fetched_date) {
  # READING EXCEL FILE
  file_date<-fetched_date
  filename<-fetched_file_name
  # filename<-"02 MARCH 2K22.xlsm"
  # Read the Excel file
  file_path <-"C:/Users/python/Documents/projR/MAR-2022/"
  df <- read_excel(file.path(file_path, filename), sheet = "MW", range = "A4:Y311")
  # df <- read_excel(filename, sheet = "MW", range = "A4:Y311")
  # Applying Basic Cleaning
  #remove rows containing FUT
  df<-df[-grep("FUT", df$HYDEL.), ]
  # before removing spaces
  #count the number of characters in the first column before trimming
  nchar(df$HYDEL.)
  # removing rows which are other than power plants
  trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  }
  # removing leading & trailing spaces from all columns
  df_trimmed <- df %>% mutate(across(everything(), trim))
  # Exporting data to csv to see
  # Remove rows with NA in the first column
  df_trimmed <- df_trimmed[!is.na(df_trimmed[, 1]), ]
  # excluding some irrelevant columns
  # create a vector of numbers using c ()
  keywords <- c ("HYDEL", "SMALL HYDEL", "WAPDA HYDEL.", "TOTAL HYDEL.", "GENCO-I", "GENCO-II", "GENCO-III", "TOTAL GENCOS", "TOTAL (R.E)", "TOTAL IPPs THERMAL", "TOTAL THERMAL", "IMP.FROM K-ELECTRIC", "TOTAL GENERATION", "NPCC LOAD MNGMT.", "REG. LOAD MNGMT.", "TOTAL SYS.DEMAND", "EXP. TO K-ELECTRIC", "NTDC DEMAND.", "EXP. JMS-KDA-1", "EXP. JMS-KDA-1", "EXP. NKI-KDA-33", "EXP. NKI-BALDIA", "PRIVATE POWER", "TIME", "IPP's  HYDEL", "TOTAL IPPs  HYDEL", "IPPs FOSSIL FUEL", "TOTAL IPPs FOSSIL FUEL", "TIME (FOR CALCULATION)", "IPP'S  BAGASSE", "TOTAL BAGASSE", "TIME (FOR CALCULATION)", "IPP'S  NUCLEAR", "TOTAL NUCLEAR", "TOTAL IPPs THERMAL.", "RENEWABLE  ENERGY", "SOLAR POWER", "TOTAL SOLAR", "WIND POWER", "TOTAL WINDS", "TOTAL (RENEWABLE  ENERGY)", "TIME (FOR CALCULATION)", "IPPs (ALL TYPES)", "NET EXPORT TO KESC.", "EXPORT+KESC")
  
  # removing columns that are in keyword list
  df_updated <- df_trimmed %>% filter(!HYDEL. %in% keywords)
  # Change the name of the first column
  colnames(df_updated)[1] <- "name"
  # making names similar with given file
  # each space and hyphen will become period
  # Replace spaces and hyphens with periods, ignoring leading and trailing spaces
  df_updated$name <- gsub(" ", ".", df_updated$name)  # Replace spaces with periods
  df_updated$name <- gsub("-", ".", df_updated$name)      # Replace hyphens with periods
  # reshaping data of csv, as desired by supervisor
  # Create a new data frame for the transformed data
  transformed_df <- data.frame(Time = character(),
                               Name = character(),
                               Energy_MWh = numeric(),
                               stringsAsFactors = FALSE)
  
  # Loop through each hour from 1 to 24
  for (hour in 1:24) {
    # Get the column name for the hour
    col_name <- sprintf("%02d00", hour)
    
    # Extract the data for the current hour
    hour_data <- df_updated[c("name", col_name)]
    
    # Rename the columns
    colnames(hour_data) <- c("Name", "Energy_MWh")
    
    # Add the time column
    hour_data$Time <- sprintf("%02d:00", hour)
    
    # Append the hour's data to the transformed dataframe
    transformed_df <- rbind(transformed_df, hour_data)
  }
  
  # Reorder the columns
  transformed_df <- transformed_df[, c("Time", "Name", "Energy_MWh")]
  # making 24:00 to 00:00
  # Create a new data frame for the transformed data
  transformed_df <- data.frame(Time = character(),
                               Name = character(),
                               Energy_MWh = numeric(),
                               stringsAsFactors = FALSE)
  
  # Loop through each hour from 1 to 24
  for (hour in 1:24) {
    # Get the column name for the hour
    col_name <- sprintf("%02d00", hour)
    
    # Extract the data for the current hour
    hour_data <- df_updated[c("name", col_name)]
    
    # Rename the columns
    colnames(hour_data) <- c("Name", "Energy_MWh")
    
    # Add the time column
    hour_data$Time <- sprintf("%02d:00", hour)
    
    # Append the hour's data to the transformed data frame
    transformed_df <- rbind(transformed_df, hour_data)
  }
  
  # Modify the time format for 2400 to 0000
  transformed_df$Time[transformed_df$Time == "24:00"] <- "00:00"
  
  # Reorder the columns
  transformed_df <- transformed_df[, c("Time", "Name", "Energy_MWh")]
  # making time 00:00 to appear in start
  # Create a temporary data frame to store records with time "00:00"
  temp_df <- transformed_df[transformed_df$Time == "00:00", ]
  
  # Remove records with time "00:00" from transformed_df
  transformed_df <- transformed_df[transformed_df$Time != "00:00", ]
  
  # Append the temporary data frame to transformed_df
  transformed_df <- rbind(temp_df, transformed_df)
  # Save the updated transformed data frame to CSV
  # removing file extension = .xlsm
  filename <- tools::file_path_sans_ext(filename)
  path <- "C:/Users/python/Documents/projR/processed_mw_sheets"
  write.csv(transformed_df, file.path(path, paste0(filename, ".csv")), row.names = FALSE, quote = FALSE)
  # write.csv(transformed_df, "transformed_data.csv", row.names = FALSE, quote = FALSE)
  
}


# Define the directory path
dir_path <- "C:/Users/python/Documents/projR/MAR-2022/"

# List all the files in the directory
files <- list.files(dir_path)

# Define a regular expression to match the date format
date_pattern <- "\\d{2} [A-Z]+ 2K\\d{2}"

# Define a date format to parse the date string
date_format <- "%d %B %Y"

# Create a list to store the newest file for each date
newest_files <- list()

# Loop through the files and filter by the extension and extract and transform the date
for (file in files) {
  # Skip temporary files starting with "~$"
  if (grepl("^~\\$", file)) {
    next
  }
  # Get the file name and extension
  name <- tools::file_path_sans_ext(file)
  ext <- tools::file_ext(file)
  
  # Check if the extension is xlsm
  if (ext == "xlsm") {
    # Search for the date pattern in the file name
    match <- str_match(name, date_pattern)
    if (!is.na(match)) {
      # If a match is found, parse the date string
      date <- match[1]
      # Remove any additional text within parentheses if present
      date <- str_remove(date, "\\(.*\\)")
      # Replace "2K" with "20" in the date string
      date <- str_replace(date, "2K", "20")
      date_obj <- parse_date_time(date, orders = date_format)
      
      # Format the date object as per the database format
      db_date <- paste(month(date_obj), day(date_obj), year(date_obj), sep = "/")
      
      # Check if the current file is newer than the previously stored file for the same date
      if (is.null(newest_files[[db_date]]) || 
          (is.numeric(file.mtime(file)) && 
           is.numeric(file.mtime(newest_files[[db_date]])) && 
           file.mtime(file) > file.mtime(newest_files[[db_date]]))) {
        newest_files[[db_date]] <- file
      }
    } else {
      # If no match is found, print an error message
      print(paste("No date found in", name))
    }
  }
}

# Print the newest files for each date
for (date in names(newest_files)) {
  process_mw_sheets(newest_files[[date]], date)
  print(date)
  print(newest_files[[date]])
}



