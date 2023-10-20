 # Cleaning Main Worksheet  
  
# imports
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
library(tidyr)

 # reading Main worksheet from excel DLR file 
 # converting as a function to make it usable for multiple files 
  
# Function to read Excel file and clean data
clean_data1 <- function(file_path, filename) {
  # Read the Excel file
  df <- read_excel(file.path(file_path, filename), sheet = "MAIN", range = "A4:AY311")
  
  # Generate the column names with a loop or vectorized approach
  start_range <- 0
  end_range <- 2300
  suffixes <- c("MW", "MVAR")
  
  # Create an empty vector to store the column names
  column_names <- c("power_plant_names", "unit_no", "inst_cap")
  
  # Loop through the specified range and create the column names
  for (i in seq(start_range, end_range, by = 100)) {
    for (suffix in suffixes) {
      column_names <- c(column_names, sprintf("%04d_%s", i, suffix))
    }
  }
  
  # Assign the column names to the data frame
  colnames(df) <- column_names
  
  # Count spaces in character columns before applying trimws
  char_columns <- sapply(df, is.character)
  spaces_before <- sapply(df[, char_columns], function(x) sum(nchar(gsub("[^ ]", "", x))))
  
  # Remove leading and trailing spaces from character columns
  df[, char_columns] <- lapply(df[, char_columns], trimws)
  
  # Count spaces in character columns after applying trimws
  spaces_after <- sapply(df[, char_columns], function(x) sum(nchar(gsub("[^ ]", "", x))))
  
  # Create a data frame to compare spaces before and after
  space_comparison <- data.frame(
    Column = names(spaces_before),
    Spaces_Before = spaces_before,
    Spaces_After = spaces_after
  )
  
  # Remove rows containing "FUT" in the power_plant_names column
  df <- df[!grepl("FUT", df$power_plant_names), ]
  
  # Fill NA in the first column with the immediate power plant name above
  df <- df %>% 
    fill(power_plant_names, .direction = "down")
  
  # Convert specific columns to numeric type (assuming columns from 3rd to last are numeric)
  df[, 3:length(df)] <- lapply(df[, 3:length(df)], as.numeric)
  # Remove rows with NA values in the "unit_no" column
  df <- df %>%
    filter(!is.na(unit_no))
  # Using subset() function
  df <- subset(df, unit_no != "UNIT" & unit_no != "NO.")
  df <- subset(df, power_plant_names != "TOTAL")
  # Return the cleaned data frame
  return(cleaned_df = df)
}



 # CLEANING WORKSHEET ENRG 1 
# Function to read Excel file and clean data
clean_data2 <- function(file_path, filename) {
  # Read the Excel file
  df_ENRG1 <- read_excel(file.path(file_path, filename), sheet = "ENRG 1", range = "A2:K311")
  # Create the new header
  new_header <- c("power_stations", "present_year_daily", "previous_year_daily",
                  "previous_year_progressive_KWH_monthly", "present_year_progressive_KWH_monthly",
                  "present_year_progressive_KWH_yearly", "load_factor_percentage",
                  "Installed_capacity_MW", "peak_load_MW_1830", "peak_cap_MW_1830",
                  "max_load_of_the_day_MW")
  
  # Assign the new header to the dataframe
  colnames(df_ENRG1) <- new_header
  
  # Remove the rows A2, A3, and A4 as they have been used as the header
  df_ENRG1 <- df_ENRG1[-c(1:3), ]
  # Convert the first column to character type
  df_ENRG1$power_stations <- as.character(df_ENRG1$power_stations)
  
  # Convert the specific columns to numeric type
  df_ENRG1 <- df_ENRG1 %>% 
    mutate(across(c("present_year_daily", "previous_year_daily",
                    "previous_year_progressive_KWH_monthly", "present_year_progressive_KWH_monthly",
                    "present_year_progressive_KWH_yearly", "load_factor_percentage",
                    "Installed_capacity_MW", "peak_load_MW_1830", "peak_cap_MW_1830",
                    "max_load_of_the_day_MW"), as.numeric))
  # Remove rows containing "FUT" in the power_stations column
  df_ENRG1 <- df_ENRG1[!grepl("FUT", df_ENRG1$power_stations), ]
  # Remove rows containing all NA values in all columns
  df_ENRG1 <- df_ENRG1[rowSums(is.na(df_ENRG1)) < ncol(df_ENRG1), ]
  # Assuming your dataframe is df_ENRG1
  df_ENRG1 <- df_ENRG1[1:(nrow(df_ENRG1) - 3), ]
  
  # Assuming your dataframe is df_ENRG1
  # Vector of values to be removed
  values_to_remove <- c("SMALL HYDEL", "WAPDA HYDEL", "TOTAL HYDEL", "GENCO-I", "TOTAL GENCO-I",
                        "GENCO-II", "TOTAL GENCO-II", "GENCO-III", "TOTAL GENCO-III", "TOTAL GENCO'S:-",
                        "TOTAL (R.E)", "TOTAL IPPs THERMAL", "TOTAL THERMAL:-", "IMP. FROM KESC",
                        "TOTAL SYSTEM:-", "EXP. TO KESC", "IPP's  HYDEL", "TOTAL IPPs  HYDEL",
                        "IPPs FOSSIL FUEL", "TOTAL IPPs FOSSIL FUEL", "IPPs  BAGASSE", "TOTAL BAGASSE",
                        "NUCLEAR", "TOTAL NUCLEAR", "TOTAL IPPs THERMAL.", "RENEWABLE  ENERGY",
                        "SOLAR POWER", "TOTAL SOLAR", "WIND POWER")
  
  # Remove rows with specified values in the "power_stations" column
  df_ENRG1 <- subset(df_ENRG1, !(power_stations %in% values_to_remove))
  
  
  
  return(df_ENRG1)
}


  # cleaning worksheet DLR2 
  # these two rows are not removed TOTAL WIND:-   and TOTAL (RENEWABLE  ENERGY) 
  
# Function to read Excel file and clean data
clean_data3 <- function(file_path, filename) {
  # Read the Excel file
  df_dlr2 <- read_excel(file.path(file_path, filename), sheet = "DLR 2", range = "A4:K311")
  # Select the first column
  first_col <- df_dlr2[, 1]
  
  # Select the last 4 columns
  last_4_cols <- df_dlr2[, (ncol(df_dlr2) - 3):ncol(df_dlr2)]
  
  # Combine the first column with the last 4 columns
  combined_df <- cbind(first_col, last_4_cols)
  
  # Rename the columns based on your requirements
  colnames(combined_df) <- c("power_stations", "minimum_load_MW", "minimum_load_hours", "load_factor_percentage", "daily_energy_MKWH")
  
  # Convert the first column to character type
  combined_df$power_stations <- as.character(combined_df$power_stations)
  
  # Convert the specific columns to numeric type
  combined_df <- combined_df %>% 
    mutate(across(c("minimum_load_MW", "minimum_load_hours", "load_factor_percentage", "daily_energy_MKWH"), as.numeric))
  # Remove rows containing "FUT" in the power_stations column
  combined_df <- combined_df[!grepl("FUT", combined_df$power_stations), ]
  # Remove rows containing all NA values in all columns
  combined_df <- combined_df[rowSums(is.na(combined_df)) < ncol(combined_df), ]
  # Trim leading and trailing spaces from "power_stations" column
  combined_df$power_stations <- trimws(combined_df$power_stations)
  
  # # Filter rows based on specific range of values in the "power_stations" column
  # filtered_df <- combined_df %>%
  #   filter(between(power_stations, "HYDEL", "SMALL HYDEL"))
  
  
  # Filter rows based on the condition: "power_stations" is not NA and other columns are not all NA
  filtered_df <- combined_df %>%
    filter(!(!is.na(power_stations) & rowSums(is.na(.[2:5])) == 4))
  
  # columns to be removed from filtered_df
  values <- c("TOTAL IPPs FOSSIL FUEL"
              ,"IPPs (ALL TYPES)", "TOTAL GENERATION",
              "TOTAL NUCLEAR", "TOTAL IPPs THERMAL.", "TOTAL BAGASSE", "TOTAL IPPs HYDEL",
              "TOTAL GENCO-III", "TOTAL GENCO'S", "TOTAL (R.E)", "TOTAL IPPs THERMAL", "TOTAL THERMAL",
              "IMP.FROM K-ELECTRIC", "TOTAL GENERATION", "EXP. TO K-ELECTRIC", "NTDC DEMAND.",
              "TOTAL GENCO-I", "SMALL HYDEL", "WAPDA HYDEL:-", "TOTAL HYDEL:-", "TOTAL GENCO-II", "TOTAL  WIND:-
", "TOTAL (RENEWABLE  ENERGY)
" )
  
  filtered_df <- subset(filtered_df, !(power_stations %in% values))
  # temporary fix, because two last records are not removed for some reason, currently debugging the issue
  # Assuming your dataframe is named DLR2_worksheet
  filtered_df <- filtered_df %>% slice(1:(n() - 2))
  
  return(filtered_df)
}

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

process_mw_sheets <- function(fetched_path, fetched_file_name) {
  
  filename<-fetched_file_name
  # filename<-"02 MARCH 2K22.xlsm"
  # Read the Excel file
  file_path <- fetched_path
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
  
  return(MW_worksheet_with_categories)  
  # return(df_updated)
  
}



# Usage of the function with your directory and file-name
filename <- "07 MARCH 2K22.xlsm"
file_path <- "C:/Users/python/Documents/projR/MAR-2022"
main_worksheet <- clean_data1(file_path, filename)
ENRG1_worksheet <- clean_data2(file_path, filename)
DLR2_worksheet <- clean_data3(file_path, filename)
# its not final data frame for MW_worksheet, categories needs to be attached
MW_worksheet_with_categories <- process_mw_sheets(file_path, filename)





