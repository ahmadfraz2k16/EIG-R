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

# Test the function with your inputs
public_hydro <- hourlySum(csvData, "2022-03-02", "HYDEL")

pvt_hydro <- hourlySum(csvData, "2022-03-02", "IPPS HYDEL HYDEL")
# Print the result
print(public_hydro)
# Print the result
print(pvt_hydro)