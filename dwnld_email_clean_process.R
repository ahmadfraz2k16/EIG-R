library(readxl)
library(tidyverse)
# load the data.table package
library(data.table)
# load the dplyr package
library(dplyr)
# load magrittr or dplyr package
library(magrittr)
# Load the lubridate package
library(lubridate)
library(here)
# Load the tidyr package if it's not already loaded
library(tidyr)
# Load the gmailr package
library(gmailr)

# Set the path to your OAuth JSON file
json_path <- "C:/Users/python/AppData/Local/gmailr/gmailr/client_secret_569058314845-3jjikvp2k3f0ao7v8dkfg36botomlln3.apps.googleusercontent.com.json"

# Configure your OAuth client
gm_auth_configure(path = json_path)

# Manually trigger the OAuth2 process
gm_auth(email = "ahmadfraz2k16@gmail.com")
gm_profile()
messagez <- gm_messages(search = 'from:ahmad.fraz@lums.edu.pk', num_results = 15)
number_of_messagez <- length(messagez[[1]]$messages)
print(number_of_messagez)
for (i in 1:number_of_messagez) {
  latest_message <- gm_message(gm_id(messagez)[[i]])
  filenameMsg <- latest_message$payload$parts[[2]]$filename
  print(filenameMsg)
  gm_save_attachments(latest_message, path = "C:/Users/python/Documents/projR/MAR-2022")
}

# # for latest email only
# latest_message <- gm_message(gm_id(messagez)[[1]])
# filenameMsg <- latest_message$payload$parts[[2]]$filename
# print(filenameMsg)
# gm_save_attachments(latest_message, path = "C:/Users/python/Documents/projR/MAR-2022")
# Log out
gm_deauth()
# Set your directory path
directory_path <- "C:/Users/python/Documents/projR/MAR-2022"

# List all files in the directory
all_files <- list.files(directory_path)

# Check if there are any .xlsm files
if (any(grepl("\\.xlsm$", all_files, ignore.case = TRUE))) {
  
  
  # Create an empty global DataFrame with the same column structure
  global_max_min_avg_df <- data.frame(
    Time = as.POSIXct(character(0), format="%Y-%m-%d"),
    name = character(0),
    time_range_one = character(0),
    max_1 = numeric(0),
    min_1 = numeric(0),
    average = numeric(0),
    time_range_two = character(0),
    max_2 = numeric(0),
    min_2 = numeric(0),
    time_range_three = character(0),
    max_3 = numeric(0),
    min_3 = numeric(0)
  )
  
  
  # Initialize an empty dataframe for global_peak_hour_df
  global_peak_hour_df <- data.frame(
    name = character(0),  # Replace 'character' with the appropriate data type
    peak_no = character(0),  # Replace 'character' with the appropriate data type
    peak_values = numeric(0),  # Replace 'numeric' with the appropriate data type
    Time = as.POSIXct(character(0), format="%Y-%m-%d %H:%M")  # Initialize Time as POSIXct
  )
  
  
  extract_max_min_avg <- function(file_name, file_path, file_date) {
    # Convert file_date to Date format
    file_date <- as.Date(file_date, format = "%m/%d/%Y")
    
    # Format the date as "yyyy-mm-dd"
    formatted_date <- format(file_date, format = "%Y-%m-%d")
    # Construct the full file path
    full_file_path <- file.path(file_path, file_name)
    
    # Read the Excel file
    df_peakhour <- read_excel(full_file_path, sheet = "MW", range = "A2:AW311")
    
    # Select columns A, Z1 to AW1
    df_peakhourV2 <- df_peakhour %>% select(1, 26:49)
    # Save the first row as "row1" and the second row as "row2"
    row1 <- df_peakhourV2[1, ]
    row2 <- df_peakhourV2[2, ]
    # combining using rbind row1 and row2
    row_merged <- rbind(row1, row2)
    # Assuming row_merged is your dataframe
    row_merged <- row_merged %>% mutate(across(everything(), as.character))
    # Assuming row_merged is your dataframe
    row_merged[is.na(row_merged)] <- ""
    # Assuming row_merged is your dataframe
    new_header_final <- c("name", paste0("p", 1:9), "calc_enrg", "act_enrg", "diff_percentage", "a_load", "max_1", "min_1", "max_2", "min_2", "max_3", "min_3", "average", "energy_generation", "L_one_true_1", "L_two_false_90", "L_three_1")
    # Assign the new header to the dataframe
    colnames(row_merged) <- new_header_final
    
    # Use row 2 as the new header
    new_header <- df_peakhourV2[2, ]
    
    # Remove the first two rows which were used for the header
    df_peakhourV2 <- df_peakhourV2[-c(1, 2), ]
    # # Merge row 1 and 2 and use it as the new header
    # new_header <- paste(df_peakhourV2[1, ], df_peakhourV2[2, ], sep = "_")
    # colnames(df_peakhourV2) <- new_header
    
    # # Remove the first two rows which were merged
    # df_peakhourV2 <- df_peakhourV2[-c(1, 2), ]
    # create a vector of numbers using c ()
    keywords <- c ("HYDEL", "SMALL HYDEL", "WAPDA HYDEL.", "TOTAL HYDEL.", "GENCO-I", "GENCO-II", "GENCO-III", "TOTAL GENCOS", "TOTAL (R.E)", "TOTAL IPPs THERMAL", "TOTAL THERMAL", "IMP.FROM K-ELECTRIC", "TOTAL GENERATION", "NPCC LOAD MNGMT.", "REG. LOAD MNGMT.", "TOTAL SYS.DEMAND", "EXP. TO K-ELECTRIC", "NTDC DEMAND.", "EXP. JMS-KDA-1", "EXP. JMS-KDA-1", "EXP. NKI-KDA-33", "EXP. NKI-BALDIA", "PRIVATE POWER", "TIME", "IPP's  HYDEL", "TOTAL IPPs  HYDEL", "IPPs FOSSIL FUEL", "TOTAL IPPs FOSSIL FUEL", "TIME (FOR CALCULATION)", "IPP'S  BAGASSE", "TOTAL BAGASSE", "TIME (FOR CALCULATION)", "IPP'S  NUCLEAR", "TOTAL NUCLEAR", "TOTAL IPPs THERMAL.", "RENEWABLE  ENERGY", "SOLAR POWER", "TOTAL SOLAR", "WIND POWER", "TOTAL WINDS", "TOTAL (RENEWABLE  ENERGY)", "TIME (FOR CALCULATION)", "IPPs (ALL TYPES)", "NET EXPORT TO KESC.", "EXPORT+KESC")
    # Change the name of the first column
    colnames(df_peakhourV2)[1] <- "name"
    #remove rows containing FUT
    df_peakhourV2<-df_peakhourV2[-grep("FUT", df_peakhourV2$name), ]
    # before removing spaces
    # removing rows which are other than power plants
    trim <- function( x ) {
      gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    }
    # removing leading & trailing spaces from all columns
    df_peakhourV2 <- df_peakhourV2 %>% mutate(across(everything(), trim))
    # Exporting data to csv to see
    # Remove rows with NA in the first column
    df_peakhourV2 <- df_peakhourV2[!is.na(df_peakhourV2[, 1]), ]
    # Remove rows where the value in the first column matches any of the keywords
    # removing columns that are in keyword list
    df_peakhourV2 <- df_peakhourV2 %>% filter(!name %in% keywords)
    
    # making names similar with given file
    # each space and hyphen will become period
    # Replace spaces and hyphens with periods, ignoring leading and trailing spaces
    df_peakhourV2$name <- gsub(" ", ".", df_peakhourV2$name)  # Replace spaces with periods
    df_peakhourV2$name <- gsub("-", ".", df_peakhourV2$name)      # Replace hyphens with periods
    df_peakhourV2$name <- gsub("\\(|\\)", ".", df_peakhourV2$name)      # Replace small brackets ( or ) with periods
    
    
    # renaming Neelam to Neelum, to make it consistent
    if ("NEELAM.JEHLAM" %in% df_peakhourV2$name) {
      df_peakhourV2$name <- ifelse(df_peakhourV2$name == "NEELAM.JEHLAM", "NEELUM.JEHLUM", df_peakhourV2$name)
    }
    # Assign the new header to the dataframe
    colnames(df_peakhourV2) <- new_header_final
    
    # Define the column names you want to convert to numeric
    numeric_columns <- c(paste0("p", 1:9), "calc_enrg", "act_enrg", "diff_percentage", "a_load", "max_1", "min_1", "max_2", "min_2", "max_3", "min_3", "average", "L_one_true_1", "L_two_false_90", "L_three_1")
    
    # changing types of columns, name and energy_generation will be character, remaining cols will be numeric
    df_peakhourV2 <- df_peakhourV2 %>%
      mutate(across(all_of(numeric_columns), as.numeric))
    Time <- paste("2022-03-17",row_merged[2,2:10])
    Time= as.POSIXct(Time,format="%Y-%m-%d %H%M")
    
    # Extract the values from row 1 for max_1, max_2, and max_3
    max_1_value <- row_merged[1, "max_1"]
    max_2_value <- row_merged[1, "max_2"]
    max_3_value <- row_merged[1, "max_3"]
    # Update row 2 of row_merged DataFrame
    # Update row 2 of row_merged DataFrame for max and min columns
    max_min_columns <- c("max_1", "max_2", "max_3", "min_1", "min_2", "min_3")
    
    # Define corresponding values for each set of columns
    value_map <- c(max_1_value, max_1_value, max_2_value, max_2_value, max_3_value, max_3_value)
    
    # Update row 2 of row_merged DataFrame for max and min columns
    for (i in 1:length(max_min_columns)) {
      col_name <- max_min_columns[i]
      value <- value_map[i]
      row_merged[2, col_name] <- paste(value, row_merged[2, col_name], sep = " - ")
    }
    # Define the values for the last three columns
    L_one_true_1_value <- "L_one_true_1"
    L_two_false_90_value <- "L_two_false_90"
    L_three_1_value <- "L_three_1"
    
    # Update row 2 of row_merged DataFrame for each of the last three columns
    row_merged[2, "L_one_true_1"] <- L_one_true_1_value
    row_merged[2, "L_two_false_90"] <- L_two_false_90_value
    row_merged[2, "L_three_1"] <- L_three_1_value
    
    
    df_max_min <- df_peakhourV2[, c(1, 15:21)]
    # Create a new column "Time" with the same value for all rows
    df_max_min$Time <- as.POSIXct(formatted_date, format = "%Y-%m-%d")
    
    # Add three new columns with the same values for all rows
    df_max_min$time_range_one <- max_1_value$max_1
    df_max_min$time_range_two <- max_2_value$max_2
    df_max_min$time_range_three <- max_3_value$max_3
    # Define the desired order of column names
    desired_order <- c("Time", "name", "time_range_one", "max_1", "min_1", "average", "time_range_two", "max_2", "min_2", "time_range_three", "max_3", "min_3")
    
    # Reorder the columns according to the desired order
    df_max_min <- df_max_min %>%
      select(desired_order)
    return(df_max_min)
  }
  
  extract_peakhours <- function(file_name, file_path, file_date) {
    # Convert file_date to Date format
    file_date <- as.Date(file_date, format = "%m/%d/%Y")
    
    # Format the date as "yyyy-mm-dd"
    formatted_date <- format(file_date, format = "%Y-%m-%d")
    # Construct the full file path
    full_file_path <- file.path(file_path, file_name)
    
    # Read the Excel file
    df_peakhour <- read_excel(full_file_path, sheet = "MW", range = "A2:AW311")
    
    # Select columns A, Z1 to AW1
    df_peakhourV2 <- df_peakhour %>% select(1, 26:49)
    # Save the first row as "row1" and the second row as "row2"
    row1 <- df_peakhourV2[1, ]
    row2 <- df_peakhourV2[2, ]
    # # Print the saved rows
    # print(row1)
    # print(row2)
    # combining using rbind row1 and row2
    row_merged <- rbind(row1, row2)
    # head(row_merged)
    # Assuming row_merged is your dataframe
    row_merged <- row_merged %>% mutate(across(everything(), as.character))
    # Assuming row_merged is your dataframe
    row_merged[is.na(row_merged)] <- ""
    # Assuming row_merged is your dataframe
    new_header_final <- c("name", paste0("p", 1:9), "calc_enrg", "act_enrg", "diff_percentage", "a_load", "max_1", "min_1", "max_2", "min_2", "max_3", "min_3", "average", "energy_generation", "L_one_true_1", "L_two_false_90", "L_three_1")
    # Assign the new header to the dataframe
    colnames(row_merged) <- new_header_final
    # head(row_merged)
    # Use row 2 as the new header
    new_header <- df_peakhourV2[2, ]
    # Remove the first two rows which were used for the header
    df_peakhourV2 <- df_peakhourV2[-c(1, 2), ]
    # create a vector of numbers using c ()
    keywords <- c ("HYDEL", "SMALL HYDEL", "WAPDA HYDEL.", "TOTAL HYDEL.", "GENCO-I", "GENCO-II", "GENCO-III", "TOTAL GENCOS", "TOTAL (R.E)", "TOTAL IPPs THERMAL", "TOTAL THERMAL", "IMP.FROM K-ELECTRIC", "TOTAL GENERATION", "NPCC LOAD MNGMT.", "REG. LOAD MNGMT.", "TOTAL SYS.DEMAND", "EXP. TO K-ELECTRIC", "NTDC DEMAND.", "EXP. JMS-KDA-1", "EXP. JMS-KDA-1", "EXP. NKI-KDA-33", "EXP. NKI-BALDIA", "PRIVATE POWER", "TIME", "IPP's  HYDEL", "TOTAL IPPs  HYDEL", "IPPs FOSSIL FUEL", "TOTAL IPPs FOSSIL FUEL", "TIME (FOR CALCULATION)", "IPP'S  BAGASSE", "TOTAL BAGASSE", "TIME (FOR CALCULATION)", "IPP'S  NUCLEAR", "TOTAL NUCLEAR", "TOTAL IPPs THERMAL.", "RENEWABLE  ENERGY", "SOLAR POWER", "TOTAL SOLAR", "WIND POWER", "TOTAL WINDS", "TOTAL (RENEWABLE  ENERGY)", "TIME (FOR CALCULATION)", "IPPs (ALL TYPES)", "NET EXPORT TO KESC.", "EXPORT+KESC")
    # Change the name of the first column
    colnames(df_peakhourV2)[1] <- "name"
    #remove rows containing FUT
    df_peakhourV2<-df_peakhourV2[-grep("FUT", df_peakhourV2$name), ]
    # before removing spaces
    # removing rows which are other than power plants
    trim <- function( x ) {
      gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
    }
    # removing leading & trailing spaces from all columns
    df_peakhourV2 <- df_peakhourV2 %>% mutate(across(everything(), trim))
    # Exporting data to csv to see
    # Remove rows with NA in the first column
    df_peakhourV2 <- df_peakhourV2[!is.na(df_peakhourV2[, 1]), ]
    # Remove rows where the value in the first column matches any of the keywords
    # removing columns that are in keyword list
    df_peakhourV2 <- df_peakhourV2 %>% filter(!name %in% keywords)
    # making names similar with given file
    # each space and hyphen will become period
    # Replace spaces and hyphens with periods, ignoring leading and trailing spaces
    df_peakhourV2$name <- gsub(" ", ".", df_peakhourV2$name)  # Replace spaces with periods
    df_peakhourV2$name <- gsub("-", ".", df_peakhourV2$name)      # Replace hyphens with periods
    df_peakhourV2$name <- gsub("\\(|\\)", ".", df_peakhourV2$name)      # Replace small brackets ( or ) with periods
    # renaming Neelam to Neelum, to make it consistent
    if ("NEELAM.JEHLAM" %in% df_peakhourV2$name) {
      df_peakhourV2$name <- ifelse(df_peakhourV2$name == "NEELAM.JEHLAM", "NEELUM.JEHLUM", df_peakhourV2$name)
    }
    # Assign the new header to the dataframe
    colnames(df_peakhourV2) <- new_header_final
    # Define the column names you want to convert to numeric
    numeric_columns <- c(paste0("p", 1:9), "calc_enrg", "act_enrg", "diff_percentage", "a_load", "max_1", "min_1", "max_2", "min_2", "max_3", "min_3", "average", "L_one_true_1", "L_two_false_90", "L_three_1")
    # changing types of columns, name and energy_generation will be character, remaining cols will be numeric
    df_peakhourV2 <- df_peakhourV2 %>%
      mutate(across(all_of(numeric_columns), as.numeric))
    Time <- paste(formatted_date,row_merged[2,2:10])
    # Time <- paste("2022-03-17",row_merged[2,2:10])
    Time= as.POSIXct(Time,format="%Y-%m-%d %H%M")
    # Select the first 10 columns of df_peakhourV2
    df_peakhourV2 <- df_peakhourV2[, 1:10]
    # Use pivot_longer to reshape the DataFrame
    df_long <- df_peakhourV2 %>%
      pivot_longer(cols = starts_with("p"),
                   names_to = "peak_no",
                   values_to = "peak_values")
    # Add the Time column by repeating the Time vector
    df_long$Time <- rep(Time, length.out = nrow(df_long))
    return(df_long)
  }
  
  # Set the folder paths
  input_folder <- "C:/Users/python/Documents/projR/MAR-2022/"  # Replace with the actual path to the input folder
  # output_folder <- here("C:/Users/python/Documents/projR/phpUploadsProcessed/")  # Replace with the actual path to the processed files folder
  
  # # Define a function to process Excel files
  # process_excel_file <- function(file_path) {
  #   # Get the file name without the path
  #   file_name <- basename(file_path)
  #
  #   # Process the file (you can add your processing logic here)
  #
  #   # After processing, move the file to the output folder
  #   new_path <- file.path(output_folder, file_name)
  #   file.rename(file_path, new_path)
  #
  #   cat("Processed:", file_name, "\n")
  # }
  
  
  # Function to extract a category based on start and end values
  extract_category <- function(dataframe, start_value, end_value = NULL, category_name) {
    # Find the index of the starting point
    start_index <- which(dataframe$name == start_value)[1]
    
    if (is.null(end_value)) {
      # For Wind category, select rows from start_index onwards
      category_df <- dataframe[start_index:nrow(dataframe), ]
    } else {
      # Find the index of the ending point
      end_index <- which(dataframe$name == end_value)[1]
      
      # Select the rows between start_index and end_index (inclusive)
      category_df <- dataframe[start_index:end_index, ]
      # removing last row
      category_df <- head(category_df, -1)
    }
    
    # Add a category column
    category_df$category <- category_name
    
    return(category_df)
  }
  # combining categories
  combine_categories <- function(df_updated){
    # new code added
    MW_worksheet <- df_updated
    # Example usage
    Hydel <- extract_category(MW_worksheet, "TARBELA", "JAMSHORO", "Hydel")
    Genco1 <- extract_category(MW_worksheet, "JAMSHORO", "GUDDU...5.13", "Genco1")
    Genco2 <- extract_category(MW_worksheet, "GUDDU...5.13", "MUZAFFARGARH", "Genco2")
    Genco3 <- extract_category(MW_worksheet, "MUZAFFARGARH", "JAGRAN", "Genco3")
    IPPs_Hydel <- extract_category(MW_worksheet, "JAGRAN", "KAPCO", "IPPs Hydel")
    IPPs_Fossil_Fuel <- extract_category(MW_worksheet, "KAPCO", "JDW.II..SADIQ.ABAD.", "IPPs Fossil Fuel")
    IPPs_Bagasse <- extract_category(MW_worksheet, "JDW.II..SADIQ.ABAD.", "CHASHNUPP...I", "IPPs Bagasse")
    IPPs_Nuclear <- extract_category(MW_worksheet, "CHASHNUPP...I", "QUAID.AZAM.SOLAR", "IPPs Nuclear")
    Solar <- extract_category(MW_worksheet, "QUAID.AZAM.SOLAR", "FFCEL.WIND", "Solar")
    # wind category has no end point
    Wind <- extract_category(MW_worksheet, "FFCEL.WIND", category_name = "Wind")
    
    # Combine all the category dataframes into one
    MW_worksheet_with_categories <- rbind(Hydel, Genco1, Genco2, Genco3, IPPs_Hydel, IPPs_Fossil_Fuel, IPPs_Bagasse, IPPs_Nuclear, Solar, Wind)
    # Reorganize columns to place "category" just after "name", re-arranged category column from end to start (2nd column)
    MW_worksheet_with_categories <- MW_worksheet_with_categories %>%
      select(name, category, everything())
    # Convert numeric columns (excluding "name" and "category") to numeric type
    numeric_cols <- names(MW_worksheet_with_categories)[!(names(MW_worksheet_with_categories) %in% c("name", "category"))]
    MW_worksheet_with_categories <- MW_worksheet_with_categories %>%
      mutate(across(all_of(numeric_cols), as.numeric))
    # Round off numeric columns to 2 decimal places without trailing zeros for whole numbers
    MW_worksheet_with_categories[numeric_cols] <- lapply(MW_worksheet_with_categories[numeric_cols], function(x) {
      ifelse(abs(x - round(x)) < 1e-6, round(x), round(x, 2))
    })
    # new code ended
    return(MW_worksheet_with_categories)
  }
  # adding sub categories by fuel type and re-arranging global_df
  rearrange_and_add_subcategories <- function(global_df, sub_categories_df_path) {
    # Read the CSV file and assign its data to sub_categories_df
    sub_categories_df <- read.csv(sub_categories_df_path)
    
    # Change the data types of all columns to character type
    sub_categories_df[] <- lapply(sub_categories_df, as.character)
    
    # matching names of both dataframes and adding new column PLANT.TYPE.WITH.FUEL in global_df and respective values from sub_categories_df
    global_df$sub_categories_by_fuel <- sub_categories_df$PLANT.TYPE.WITH.FUEL[match(global_df$Name, sub_categories_df$name)]
    
    # rearranging columns (Time, Name, Energy_MWh, category, sub_categories_by_fuel)
    global_df <- global_df %>%
      select(Time, Name, Energy_MWh, category, sub_categories_by_fuel)
    
    return(global_df)
  }
  
  
  # Start measuring time
  start_time <- Sys.time()
  
  # Create an empty global data frame
  global_df <- data.frame(Time = character(), Name = character(), category = character(), Energy_MWh = numeric())
  
  df_modified <- data.frame()
  
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
    df_updated$name <- gsub("\\(|\\)", ".", df_updated$name)      # Replace small brackets ( or ) with periods
    
    
    # renaming Neelam to Neelum, to make it consistent
    if ("NEELAM.JEHLAM" %in% df_updated$name) {
      df_updated$name <- ifelse(df_updated$name == "NEELAM.JEHLAM", "NEELUM.JEHLUM", df_updated$name)
    }
    df_updated <- combine_categories(df_updated)
    
    # reshaping data of csv, as desired by supervisor
    # Create a new data frame for the transformed data
    # Create a new data frame for the transformed data
    transformed_df <- data.frame(Time = character(),
                                 Name = character(),
                                 Category = character(),
                                 Energy_MWh = numeric(),
                                 stringsAsFactors = FALSE)
    
    # Loop through each hour from 1 to 24
    for (hour in 1:24) {
      col_name <- sprintf("%02d00", hour)
      
      # Extract the data for the current hour
      hour_data <- df_updated[c("name", "category", col_name)]
      
      # Rename the columns
      colnames(hour_data) <- c("Name", "category", "Energy_MWh")
      
      # Add the time column
      hour_data$Time <- sprintf("%02d:00", hour)
      
      
      # try catch
      tryCatch({
        # Append the hour's data to the transformed data frame
        transformed_df <- rbind(transformed_df, hour_data)
      }, error = function(e) {
        # Handle the error (you can choose to print a message or take other actions)
        cat("Error occurred while combining rows, transformed_df and hour_data. Continuing script execution...\n")
      })
    }
    
    
    
    
    # # Reorder the columns
    # transformed_df <- transformed_df[, c("Time", "Name", "category", "Energy_MWh")]
    # # making 24:00 to 00:00
    # # Create a new data frame for the transformed data
    # transformed_df <- data.frame(Time = character(),
    #                              Name = character(),
    #                              Category = character(),
    #                              Energy_MWh = numeric(),
    #                              stringsAsFactors = FALSE)
    #
    # # Loop through each hour from 1 to 24
    # for (hour in 1:24) {
    #   # Get the column name for the hour
    #   col_name <- sprintf("%02d00", hour)
    #
    #   # Extract the data for the current hour
    #   hour_data <- df_updated[c("name", "category", col_name)]
    #
    #   # Rename the columns
    #   colnames(hour_data) <- c("Name", "category", "Energy_MWh")
    #
    #   # Add the time column
    #   hour_data$Time <- sprintf("%02d:00", hour)
    #
    #   # Append the hour's data to the transformed data frame
    #   transformed_df <- rbind(transformed_df, hour_data)
    # }
    
    
    
    # Modify the time format for 2400 to 0000
    transformed_df$Time[transformed_df$Time == "24:00"] <- "00:00"
    
    # Reorder the columns
    transformed_df <- transformed_df[, c("Time", "Name", "category", "Energy_MWh")]
    # making time 00:00 to appear in start
    # Create a temporary data frame to store records with time "00:00"
    temp_df <- transformed_df[transformed_df$Time == "00:00", ]
    
    # Remove records with time "00:00" from transformed_df
    transformed_df <- transformed_df[transformed_df$Time != "00:00", ]
    
    
    # try catch
    tryCatch({
      # Append the temporary data frame to transformed_df
      transformed_df <- rbind(temp_df, transformed_df)
    }, error = function(e) {
      # Handle the error (you can choose to print a message or take other actions)
      cat("Error occurred while combining rows, temp_df and transformed_df. Continuing script execution...\n")
    })
    
    # Append file_date to the Time column in transformed_df
    transformed_df$Time <- paste(file_date, transformed_df$Time, sep = " ")
    
    # try catch
    tryCatch({
      # saving each data frame to global data frame to have collective data
      # Append transformed_df to global_df
      global_df <- rbind(global_df, transformed_df)
    }, error = function(e) {
      # Handle the error (you can choose to print a message or take other actions)
      cat("Error occurred while combining rows, global_df and transformed_df. Continuing script execution...\n")
    })
    # Update the global variable
    assign("global_df", global_df, envir = .GlobalEnv)
    
    
    
    # Call extract_peakhours function for the current date
    df_long <- extract_peakhours(filename, file_path, file_date)
    # Append the data to global_peak_hour_df
    global_peak_hour_df <- rbind(global_peak_hour_df, df_long)
    # Update the global variable
    assign("global_peak_hour_df", global_peak_hour_df, envir = .GlobalEnv)
    
    
    
    
    # Call extract_peakhours function for the current date
    df_max_mins <- extract_max_min_avg(filename, file_path, file_date)
    # Append the data to global_peak_hour_df
    global_max_min_avg_df <- rbind(global_max_min_avg_df, df_max_mins)
    # Update the global variable
    assign("global_max_min_avg_df", global_max_min_avg_df, envir = .GlobalEnv)
    
    
    # Save the updated transformed data frame to CSV
    # removing file extension = .xlsm from file names
    filename_without_extentions <- tools::file_path_sans_ext(filename)
    path <- "C:/Users/python/Documents/projR/processed_mw_sheets"
    write.csv(transformed_df, file.path(path, paste0(filename_without_extentions, ".csv")), row.names = FALSE, quote = FALSE)
    # write.csv(transformed_df, "transformed_data.csv", row.names = FALSE, quote = FALSE)
    df_modified <- transformed_df
    # Remove the processed file from the "MAR-2022" directory
    file.remove(file.path(file_path, filename))
  }
  
  
  # Define the directory path
  dir_path <- "C:/Users/python/Documents/projR/MAR-2022"
  
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
    # print(paste("processed", newest_files[[date]], sep = " "))
    print(date)
    # print(newest_files[[date]])
  }
  # Assuming global_df and sub_categories_df_path are provided
  global_df <- rearrange_and_add_subcategories(global_df, "C:/Users/python/Documents/projR/Power Plants Type.csv")
  # # Save the collective global data frame to CSV
  # filename <- "collective_data"
  # path <- "C:/Users/python/Documents/projR/processed_mw_sheets"
  # write.csv(global_df, file.path(path, paste0(filename, ".csv")), row.names = FALSE, quote = FALSE)
  
  
  
  # Path to the CSV file for collective_data
  csv_file_collective_data <- "C:/Users/python/Documents/projR/processed_mw_sheets/collective_data.csv"
  # Check if the CSV file exists
  if (file.exists(csv_file_collective_data)) {
    # Read existing data
    existing_data_collective_data <- read.csv(csv_file_collective_data)
    existing_data_collective_data$Time <- as.POSIXct(existing_data_collective_data$Time, format="%m/%d/%Y %H:%M")
    global_df$Time <- as.POSIXct(global_df$Time, format="%m/%d/%Y %H:%M")
    str(existing_data_collective_data)
    str(global_df)
    # Get unique dates
    unique_dates <- unique(format(global_df$Time, "%m/%d/%Y"))
    
    # Loop through unique dates
    for (date in unique_dates) {
      # Filter data for the current date
      filtered_data <- existing_data_collective_data %>% filter(format(Time, "%m/%d/%Y") != date)
      
      # Update your_data_frame with filtered_data
      existing_data_collective_data <- filtered_data
    }
    # Combine existing data with new data
    combined_data_collective_data <- bind_rows(existing_data_collective_data, global_df)
    
    # Sort the combined data by the Time column
    combined_data_collective_data <- combined_data_collective_data[order(combined_data_collective_data$Time), ]
    
    # Write the sorted combined data to the CSV file
    write.csv(combined_data_collective_data, csv_file_collective_data, row.names = FALSE, quote = FALSE)
  } else {
    # If the CSV file doesn't exist, use global_df as the starting point
    ## Save the global_df data frame to CSV named collective_data
    filename <- "collective_data"
    path <- "C:/Users/python/Documents/projR/processed_mw_sheets"
    # global_df$Time <- as.POSIXct(global_df$Time, format="%m/%d/%Y %H:%M")
    write.csv(global_df, file.path(path, paste0(filename, ".csv")), row.names = FALSE, quote = FALSE)
  }
  
  
  
  
  
  
  # Path to the CSV file
  csv_file_max_min_avg <- "C:/Users/python/Documents/projR/processed_mw_sheets/max_min_avg.csv"
  # Check if the CSV file exists
  if (file.exists(csv_file_max_min_avg)) {
    # Read existing data
    existing_data_max_min_avg <- read.csv(csv_file_max_min_avg)
    existing_data_max_min_avg$Time<-as.POSIXct(existing_data_max_min_avg$Time, format="%Y-%m-%d")
    str(existing_data_max_min_avg)
    # Get unique dates
    unique_dates <- unique(format(global_max_min_avg_df$Time, "%Y-%m-%d"))
    
    # Loop through unique dates
    for (date in unique_dates) {
      # Filter data for the current date
      filtered_data <- existing_data_max_min_avg %>% filter(format(Time, "%Y-%m-%d") != date)
      
      # Update your_data_frame with filtered_data
      existing_data_max_min_avg <- filtered_data
    }
    # Combine existing data with new data
    combined_data_max_min_avg <- bind_rows(existing_data_max_min_avg, global_max_min_avg_df)
    str(global_max_min_avg_df)
    # Sort the combined data by the Time column
    combined_data_max_min_avg <- combined_data_max_min_avg[order(combined_data_max_min_avg$Time), ]
    # Write the sorted combined data to the CSV file
    write.csv(combined_data_max_min_avg, csv_file_max_min_avg, row.names = FALSE, quote = FALSE)
  } else {
    # If the CSV file doesn't exist, use new_data_df as the starting point
    ## Save the global_peak_hour_df data frame to CSV name peakhours
    filename <- "max_min_avg"
    path <- "C:/Users/python/Documents/projR/processed_mw_sheets"
    write.csv(global_max_min_avg_df, file.path(path, paste0(filename, ".csv")), row.names = FALSE, quote = FALSE)
    str(global_max_min_avg_df)
  }
  
  
  
  # Path to the CSV file
  csv_file_peakhours <- "C:/Users/python/Documents/projR/processed_mw_sheets/peakhours.csv"
  # Check if the CSV file exists
  if (file.exists(csv_file_peakhours)) {
    # Read existing data
    existing_data_peakhours <- read.csv(csv_file_peakhours)
    existing_data_peakhours$Time<-as.POSIXct(existing_data_peakhours$Time, tz="Asia/Karachi", format="%Y-%m-%d %H:%M")
    # Get unique dates
    unique_dates <- unique(format(global_peak_hour_df$Time, "%Y-%m-%d"))
    
    # Loop through unique dates
    for (date in unique_dates) {
      # Filter data for the current date
      filtered_data <- existing_data_peakhours %>% filter(format(Time, "%Y-%m-%d") != date)
      
      # Update your_data_frame with filtered_data
      existing_data_peakhours <- filtered_data
    }
    # Combine existing data with new data
    combined_data_peakhours <- bind_rows(existing_data_peakhours, global_peak_hour_df)
    str(combined_data_peakhours)
    str(global_peak_hour_df)
    # Sort the combined data by the Time column
    combined_data_peakhours <- combined_data_peakhours[order(combined_data_peakhours$Time), ]
    # Write the sorted combined data to the CSV file
    write.csv(combined_data_peakhours, csv_file_peakhours, row.names = FALSE, quote = FALSE)
  } else {
    # If the CSV file doesn't exist, use new_data_df as the starting point
    # Save the global_peak_hour_df data frame to CSV name peakhours
    filename <- "peakhours"
    path <- "C:/Users/python/Documents/projR/processed_mw_sheets"
    write.csv(global_peak_hour_df, file.path(path, paste0(filename, ".csv")), row.names = FALSE, quote = FALSE)
    str(global_peak_hour_df)
  }
  
  
  
  
  
  # global_df$Time<-as.POSIXct(global_df$Time, tz="Asia/Karachi", format="%m/%d/%Y %H:%M")
  # global_df$Month_Year <- format(global_df$Time, "%Y %b")
  
  # reseting global_df
  # remove all records from global_df
  # global_df <- subset(global_df, )
  # global_df <- global_df[0, ]
  
  # Stop measuring time
  end_time <- Sys.time()
  
  # Calculate the elapsed time
  elapsed_time <- end_time - start_time
  
  # Print the elapsed time
  print(elapsed_time)
  
  
} else {
  cat("No .xlsm files found to clean.")
}

