library(jsonlite)

# Read the CSV file
csvData <- read.csv('C:/Users/python/Documents/projR/processed_mw_sheets/collective_data.csv', header = TRUE)
csvData$Time <- ifelse(grepl(" ", csvData$Time), csvData$Time, paste(csvData$Time, "00:00:00"))
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

# Load the lubridate package
library(lubridate)

# Define start_date and end_date
start_date <- ymd("2022-03-02")
end_date <- ymd("2022-03-05")

# Create a sequence of dates between start_date and end_date
date_sequence <- seq(start_date, end_date, by = "1 day")

# Print the dates using a for loop
for (date in date_sequence) {
  x <- structure(date, class = "Date")
  print(x)
}


# Create an empty list to store results for each date
result_list <- list()

# Loop through the date range
for (date in date_sequence) {
  filter_date <- structure(date, class = "Date")
  filter_date <- as.character(filter_date)
  print(filter_date)
  # Test the function with your inputs for the current date
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
  
  # Calculate the sums
  privateSum <- sum(pvt_hydro$Sum)
  publicSum <- sum(public_hydro$Sum)
  hydroSum <- privateSum + publicSum
  nuclearSum <- sum(NUCLEAR$Sum)
  bagasseSum <- sum(BAGASSE$Sum)
  windSum <- sum(WIND$Sum)
  solarSum <- sum(SOLAR$Sum)
  renewableSum <- solarSum + windSum + bagasseSum
  ippsrlngSum <- sum(IPPS_RLNG$Sum)
  ippsgasSum <- sum(IPPS_Gas$Sum)
  ippscoalSum <- sum(IPPS_Coal$Sum)
  ippsfoSum <- sum(IPPS_FO$Sum)
  ippsSum <- ippsrlngSum + ippsgasSum + ippscoalSum + ippsfoSum
  gencosrlngSum <- sum(GENCOS_RLNG$Sum)
  gencoscoalSum <- sum(GENCOS_Coal$Sum)
  gencosgasSum <- sum(GENCOS_Gas$Sum)
  gencosSum <- gencosrlngSum + gencoscoalSum + gencosgasSum
  thermalSum <- ippsSum + gencosSum
  

  # Create the desired output structure for Hydro for the current date
  result <- list(
    "Private" = list(
      "data" = as.matrix(cbind(pvt_hydro$Hour, pvt_hydro$Sum))
    ),
    "Public" = list(
      "data" = as.matrix(cbind(public_hydro$Hour, public_hydro$Sum))
    ),
    "Solar" = list(
      "data" = as.matrix(cbind(SOLAR$Hour, SOLAR$Sum))
    ),
    "Wind" = list(
      "data" = as.matrix(cbind(WIND$Hour, WIND$Sum))
    ),
    "Bagasse" = list(
      "data" = as.matrix(cbind(BAGASSE$Hour, BAGASSE$Sum))
    ),
    "Nuclear" = list(
      "data" = as.matrix(cbind(NUCLEAR$Hour, NUCLEAR$Sum))
    ),
    "GenGas" = list(
      "data" = as.matrix(cbind(GENCOS_Gas$Hour, GENCOS_Gas$Sum))
    ),
    "GenCoal" = list(
      "data" = as.matrix(cbind(GENCOS_Coal$Hour, GENCOS_Coal$Sum))
    ),
    "GenRlng" = list(
      "data" = as.matrix(cbind(GENCOS_RLNG$Hour, GENCOS_RLNG$Sum))
    ),
    "IppsGas" = list(
      "data" = as.matrix(cbind(IPPS_Gas$Hour, IPPS_Gas$Sum))
    ),
    "IppsCoal" = list(
      "data" = as.matrix(cbind(IPPS_Coal$Hour, IPPS_Coal$Sum))
    ),
    "IppsRlng" = list(
      "data" = as.matrix(cbind(IPPS_RLNG$Hour, IPPS_RLNG$Sum))
    ),
    "IppsFo" = list(
      "data" = as.matrix(cbind(IPPS_FO$Hour, IPPS_FO$Sum))
    ),
    "privateSum" = privateSum,
    "publicSum" = publicSum,
    "hydroSum" = hydroSum,
    "solarSum" = solarSum,
    "windSum" = windSum,
    "bagasseSum" = bagasseSum,
    "renewableSum" = renewableSum,
    "nuclearSum" = nuclearSum,
    "gencosSum" = gencosSum,
    "ippsSum" = ippsSum
  )
  
  # Add the result structure for the current date to the list
  result_list[[filter_date]] <- result
}

# Convert the list to JSON
result_json <- toJSON(result_list, pretty = TRUE, auto_unbox = TRUE)

# Print the JSON for all dates
cat(result_json)
# Write the JSON data to a file
write(result_json, "C:/Users/python/Documents/output.json")

# Load necessary library
library(jsonlite)

# Create a list with nested keys
data <- list(
  "2023-12-01" = list(
    key1 = "value1",
    key2 = list(
      nestedKey1 = "nestedValue1",
      nestedKey2 = "nestedValue2"
    )
  ),
  "2023-12-02" = list(
    key1 = "value3",
    key2 = list(
      nestedKey1 = "nestedValue3",
      nestedKey2 = "nestedValue4"
    )
  )
)

# Convert the list to JSON
json_data <- toJSON(data, pretty = TRUE, auto_unbox = TRUE)

# Print the JSON data
print(json_data)





# filter_date <- "2022-03-02"
# # Test the function with your inputs
# public_hydro <- hourlySum(csvData, filter_date, "HYDEL")
# pvt_hydro <- hourlySum(csvData, filter_date, "IPPS HYDEL HYDEL")
# # renewables
# SOLAR <- hourlySum(csvData, filter_date, "SOLAR")
# WIND <- hourlySum(csvData, filter_date, "WIND")
# BAGASSE <- hourlySum(csvData, filter_date, "IPPS BAGASSE BAGASSE")
# # NUCLEAR
# NUCLEAR <- hourlySum(csvData, filter_date, "NUCLEAR")
# # thermal Gencos
# GENCOS_Gas <- hourlySum(csvData, filter_date, "GENCOS Gas")
# GENCOS_Coal <- hourlySum(csvData, filter_date, "GENCOS Coal")
# GENCOS_RLNG <- hourlySum(csvData, filter_date, "GENCOS RLNG")
# # Thermal ipps
# IPPS_Gas <- hourlySum(csvData, filter_date, "IPPS FOSSIL FUEL Gas")
# IPPS_Coal <- hourlySum(csvData, filter_date, "IPPS FOSSIL FUEL Coal")
# IPPS_FO <- hourlySum(csvData, filter_date, "IPPS FOSSIL FUEL FO")
# IPPS_RLNG <- hourlySum(csvData, filter_date, "IPPS FOSSIL FUEL RLNG")
# 
# # Calculate the sums
# privateSum <- sum(pvt_hydro$Sum)
# publicSum <- sum(public_hydro$Sum)
# hydroSum <- privateSum + publicSum
# nuclearSum <- sum(NUCLEAR$Sum)
# bagasseSum <- sum(BAGASSE$Sum)
# windSum <- sum(WIND$Sum)
# solarSum <- sum(SOLAR$Sum)
# renewableSum <- solarSum + windSum + bagasseSum
# ippsrlngSum <- sum(IPPS_RLNG$Sum)
# ippsgasSum <- sum(IPPS_Gas$Sum)
# ippscoalSum <- sum(IPPS_Coal$Sum)
# ippsfoSum <- sum(IPPS_FO$Sum)
# ippsSum <- ippsrlngSum + ippsgasSum + ippscoalSum + ippsfoSum
# gencosrlngSum <- sum(GENCOS_RLNG$Sum)
# gencoscoalSum <- sum(GENCOS_Coal$Sum)
# gencosgasSum <- sum(GENCOS_Gas$Sum)
# gencosSum <- gencosrlngSum + gencoscoalSum + gencosgasSum
# thermalSum <- ippsSum + gencosSum
# 
# 
# # Create the desired output structure for Hydro
# result_hydro <- list(
#   "2022-03-02" = list(
#       "Private" = list(
#         "data" = as.matrix(cbind(pvt_hydro$Hour, pvt_hydro$Sum))
#       ),
#       "Public" = list(
#         "data" = as.matrix(cbind(public_hydro$Hour, public_hydro$Sum))
#       ),
#       "Solar" = list(
#         "data" = as.matrix(cbind(SOLAR$Hour, SOLAR$Sum))
#       ),
#       "Wind" = list(
#         "data" = as.matrix(cbind(WIND$Hour, WIND$Sum))
#       ),
#       "Bagasse" = list(
#         "data" = as.matrix(cbind(BAGASSE$Hour, BAGASSE$Sum))
#       ),
#       "Nuclear" = list(
#         "data" = as.matrix(cbind(NUCLEAR$Hour, NUCLEAR$Sum))
#       ),
#       "GenGas" = list(
#         "data" = as.matrix(cbind(GENCOS_Gas$Hour, GENCOS_Gas$Sum))
#       ),
#       "GenCoal" = list(
#         "data" = as.matrix(cbind(GENCOS_Coal$Hour, GENCOS_Coal$Sum))
#       ),
#       "GenRlng" = list(
#         "data" = as.matrix(cbind(GENCOS_RLNG$Hour, GENCOS_RLNG$Sum))
#       ),
#       "IppsGas" = list(
#         "data" = as.matrix(cbind(IPPS_Gas$Hour, IPPS_Gas$Sum))
#       ),
#       "IppsCoal" = list(
#         "data" = as.matrix(cbind(IPPS_Coal$Hour, IPPS_Coal$Sum))
#       ),
#       "IppsRlng" = list(
#         "data" = as.matrix(cbind(IPPS_RLNG$Hour, IPPS_RLNG$Sum))
#       ),
#       "IppsFo" = list(
#         "data" = as.matrix(cbind(IPPS_FO$Hour, IPPS_FO$Sum))
#       ),
#     "privateSum" = privateSum,
#     "publicSum" = publicSum,
#     "hydroSum" = hydroSum,
#     "solarSum" = solarSum,
#     "windSum" = windSum,
#     "bagasseSum" = bagasseSum,
#     "renewableSum" = renewableSum,
#     "nuclearSum" = nuclearSum,
#     "gencosSum" = gencosSum,
#     "ippsSum" = ippsSum
#   )
# )
# 
# 
# # Convert the list to JSON
# result_json_hydro <- toJSON(result_hydro, pretty = TRUE)
# 
# # Print the JSON for Hydro
# cat(result_json_hydro)
