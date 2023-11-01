library(jsonlite)

# Read the CSV file
csvData <- read.csv('C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/mw_new.csv', header = TRUE)

# Convert "Time" column to POSIXct with the appropriate time zone
csvData$Time <- as.POSIXct(csvData$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC+5")

# Define the function
hourlySum <- function(data, targetDate, subCategoryToCombine) {
  # Convert the target date to a POSIXct object in the Pakistan time zone (UTC+5)
  targetDate <- as.POSIXct(targetDate, tz = "UTC+5")
  
  # Filter the data by the time range for the target date and sub-category
  data <- data[data$Time >= targetDate & data$Time < targetDate + 86400 & data$sub_categories_by_fuel == subCategoryToCombine, ]
  
  # Create a sequence of hours (0-23) for the result
  hours <- format(seq(from = targetDate, by = "hour", length.out = 25), "%H")
  
  # Initialize a data frame to store the hourly sum
  result <- data.frame(Hour = hours[-25], Sum = rep(0, 24))
  
  # Calculate the hourly sum
  for (i in 1:24) {
    hour <- hours[i]
    result$Sum[i] <- sum(data$Energy_MWh[format(data$Time, "%H") == hour])
  }
  
  # Return the data frame
  return(result)
}

filter_date <- "2022-03-02"
# Test the function with your inputs
public_hydro <- hourlySum(csvData, filter_date, "HYDEL")
pvt_hydro <- hourlySum(csvData, filter_date, "IPPS HYDEL HYDEL")
# renewables
SOLAR <- hourlySum(csvData, filter_date, "SOLAR")
WIND <- hourlySum(csvData, filter_date, "WIND")
BAGASSE <- hourlySum(csvData, filter_date, "IPPS BAGASSE BAGASSE")
# NUCLEAR
NUCLEAR <- hourlySum(csvData, filter_date, "NUCLEAR")
# thermal Gencos
GENCOS_Gas <- hourlySum(csvData, filter_date, "GENCOS Gas")
GENCOS_Coal <- hourlySum(csvData, filter_date, "GENCOS Coal")
GENCOS_RLNG <- hourlySum(csvData, filter_date, "GENCOS RLNG")
# Thermal ipps
IPPS_Gas <- hourlySum(csvData, filter_date, "IPPS FOSSIL FUEL Gas")
IPPS_Coal <- hourlySum(csvData, filter_date, "IPPS FOSSIL FUEL Coal")
IPPS_FO <- hourlySum(csvData, filter_date, "IPPS FOSSIL FUEL FO")
IPPS_RLNG <- hourlySum(csvData, filter_date, "IPPS FOSSIL FUEL RLNG")
# Print the HYDRO result
print(public_hydro)
print(pvt_hydro)
# PRINT THE RENEWABLES RESULT
print(SOLAR)
print(WIND)
print(BAGASSE)
# PRINT THE RENEWABLES RESULT
print(NUCLEAR)
# PRINT THE GENCOS RESULT
print(GENCOS_Gas)
print(GENCOS_Coal)
print(GENCOS_RLNG)
# print the IPPS
print(IPPS_Gas)
print(IPPS_Coal)
print(IPPS_FO)
print(IPPS_RLNG)