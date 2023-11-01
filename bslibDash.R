library(shiny)
library(highcharter)
library(dplyr)
library(lubridate)
library(bslib)
library(bsicons)
library(jsonlite)

# Read the JSON file line by line
jsondata <- readLines("drilldownJSONdata.json")
# Paste the lines together as a string
jsondata <- paste(jsondata, collapse = "")
# Print the string
print(jsondata)

# Define the JavaScript code
js_code <- "function(e) {
    var javascriptArray = %s;
    var specificDate = javascriptArray[e.point.name];
    var chart = this,
    Hydro = specificDate.Hydro,

    Renewables = specificDate.Renewable,
    
    Thermal = {
      
      'IPPS': {
        name: 'IPPS',
        color: 'Brown',
        data: [
          {
            'name': '2022-03-02',
            'y': 172373,
            'drilldown': true
          },
          {
            'name': '2022-03-03',
            'y': 168726,
            'drilldown': true
          }
        ],
        stack: 'move'
        
      },
      
      
      'GENCOS': {
        
        name: 'GENCOS',
        color: 'DarkGrey',
        data: [
          {
            'name': '2022-03-02',
            'y': 16305,
            'drilldown': true
          },
          {
            'name': '2022-03-03',
            'y': 25527,
            'drilldown': true
          }
        ],
        stack: 'move'
        
      }
      
    },
    
    
    IPPS = {
      'Gas': {
        'name': 'Gas',
        'color': 'yellow',
        'data': [
          [
            '00:00',
            1091
          ],
          [
            '01:00',
            1080
          ],
          [
            '02:00',
            811
          ]
        ]
      },
      'Coal': {
        'name': 'Coal',
        'color': 'green',
        'data': [
          [
            '00:00',
            2798
          ],
          [
            '01:00',
            2605
          ],
          [
            '02:00',
            2371
          ]
        ]
      },
      'FO': {
        'name': 'FO',
        'color': 'red',
        'data': [
          [
            '00:00',
            510
          ],
          [
            '01:00',
            520
          ],
          [
            '02:00',
            530
          ]
        ]
      },
      'RLNG': {
        'name': 'RLNG',
        'color': 'blue',
        'data': [
          [
            '00:00',
            2171
          ],
          [
            '01:00',
            2013
          ],
          [
            '02:00',
            1894
          ]
        ]
      }
    },
    
    
    Gencos = {
      'Gas': {
        'name': 'Gas',
        'color': 'yellow',
        'data': [
          [
            '00:00',
            234
          ],
          [
            '01:00',
            233
          ],
          [
            '02:00',
            193
          ]
        ]
      },
      'Coal': {
        'name': 'Coal',
        'color': 'green',
        'data': [
          [
            '00:00',
            234
          ],
          [
            '01:00',
            233
          ],
          [
            '02:00',
            193
          ]
        ]
      },
      'RLNG': {
        'name': 'RLNG',
        'color': 'blue',
        'data': [
          [
            '00:00',
            234
          ],
          [
            '01:00',
            233
          ],
          [
            '02:00',
            193
          ]
        ]
      }
    },
    
    
    
    Nuclear = {
      'Nuclear': {
        'name': 'Nuclear',
        'color': 'blue',
        'data': [
          [
            '00:00',
            2015
          ],
          [
            '01:00',
            2221
          ],
          [
            '02:00',
            2221
          ]
        ]
      }
    }
    
    
    
    
    if (e.point.color == 'blue') {
      chart.addSingleSeriesAsDrilldown(e.point, Hydro.Private);
      chart.addSingleSeriesAsDrilldown(e.point, Hydro.Public);
    } else if (e.point.color == 'green') {
      chart.addSingleSeriesAsDrilldown(e.point, Renewables.Solar);
      chart.addSingleSeriesAsDrilldown(e.point, Renewables.Wind);
      chart.addSingleSeriesAsDrilldown(e.point, Renewables.Bagasse);
      
    } else if (e.point.color == 'red') {
      chart.addSingleSeriesAsDrilldown(e.point, Thermal.IPPS);
      chart.addSingleSeriesAsDrilldown(e.point, Thermal.GENCOS);
      
    } else if (e.point.color == 'yellow') {
      chart.addSingleSeriesAsDrilldown(e.point, Nuclear.Nuclear);
      
    }
    
    
    if (e.point.color == 'Brown') {
      chart.addSingleSeriesAsDrilldown(e.point, IPPS.Gas);
      chart.addSingleSeriesAsDrilldown(e.point, IPPS.Coal);
      chart.addSingleSeriesAsDrilldown(e.point, IPPS.RLNG);
      chart.addSingleSeriesAsDrilldown(e.point, IPPS.FO);
    } else if (e.point.color == 'DarkGrey') {
      chart.addSingleSeriesAsDrilldown(e.point, Gencos.Gas);
      chart.addSingleSeriesAsDrilldown(e.point, Gencos.RLNG);
      chart.addSingleSeriesAsDrilldown(e.point, Gencos.Coal);
    }
    
    
    chart.applyDrilldown();
  }"
# Replace %s in the JavaScript code with the actual JSON data
js_code <- sprintf(js_code, jsondata)



int <- function(x) {
  as.integer(floor(x))
}

# Load your CSV mw_data
# Specify the file paths for both datasets
mw_file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/mw_new.csv"
file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/max_min_avg.csv"
peakhours_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/peakhours.csv"  # Update with the correct path


# Read the CSV files
mw_data <- read.csv(mw_file_path)
data <- read.csv(file_path)
peakhours_data <- read.csv(peakhours_path)

# Convert the "Time" column to POSIXct format
data$Time <- as.POSIXct(data$Time, format = "%Y-%m-%d")
peakhours_data$Time <- as.POSIXct(peakhours_data$Time, format = "%m/%d/%Y %H:%M")
mw_data$Time <- as.POSIXct(mw_data$Time, format = "%Y-%m-%d %H:%M", tz = "UTC")

# Initialize an empty global_df dataframe
global_df <- data.frame(Time = as.POSIXct(character()), Name = character(), Energy = numeric())
# Determine the minimum and maximum dates in the CSV mw_data
min_date <- min(as.Date(mw_data$Time))
max_date <- max(as.Date(mw_data$Time))
# Get the last date
last_date <- max_date
# # Filter the mw_data initially with the default date range
# default_filtered_data <- mw_data %>%
#   filter(as.Date(Time) >= min_date, as.Date(Time) <= max_date)
# Define a function to filter mw_data and calculate total energy
calculateCategoryEnergy <- function(mw_data, category) {
  filtered_data <- mw_data %>%
    filter(sub_categories_by_fuel %in% category)
  total_energy <- filtered_data %>%
    summarise(total_energy = sum(Energy_MWh))
  return(total_energy$total_energy)
}

# copied min-max-avg csv data, for separate filter
data_min_max_avg <- data.frame(data)

## Convert the "Time" column to Date format
# data_min_max_avg$Time <- as.Date(data_min_max_avg$Time, format = "%Y-%m-%d")
# Convert the Time column to Date format using as.POSIXct
data_min_max_avg$Time <- as.POSIXct(data_min_max_avg$Time, format = "%Y-%m-%d")

# Check the structure of the data_min_max_avg again to confirm the change
str(data_min_max_avg)
uniqueDates <- unique(data_min_max_avg$Time)


vbs <- list(
  value_box(
    title = "1st value",
    value = "123",
    showcase = bs_icon("bar-chart"),
    theme = "bg-purple",
    p("The 1st detail")
  ),
  value_box(
    title = "2nd value",
    value = "456",
    showcase = bs_icon("graph-up"),
    theme = "teal",
    p("The 2nd detail"),
    p("The 3rd detail")
  ),
  value_box(
    title = "3rd value",
    value = "789",
    showcase = bs_icon("pie-chart"),
    theme = "pink",
    p("The 4th detail"),
    p("The 5th detail"),
    p("The 6th detail")
  )
)

value_boxes_lasthour <- list(
  value_box(
    title = "Hydro",
    value = textOutput("totalHydroEnergylasthour"),
    showcase = bs_icon("water"),
    p(textOutput("totalPublicHydroEnergylasthour")),
    p(textOutput("totalPrivateHydroEnergylasthour"))
  ),
  value_box(
    title = "Renewable",
    value = textOutput("totalRenewableEnergylasthour"),
    showcase = bs_icon("recycle"),
    p(textOutput("totalSolarEnergylasthour")),
    p(textOutput("totalWindEnergylasthour")),
    p(textOutput("totalBagasseEnergylasthour"))
  ),
  value_box(
    title = "Nuclear",
    value = textOutput("totalNuclearEnergylasthour"),
    showcase = bs_icon("radioactive"),
  ),
  value_box(
    title = "Thermal",
    value = textOutput("totalThermalEnergylasthour"),
    showcase = bs_icon("fire"),
    fluidRow(
      column(6, actionButton(
        "btn_pop", 
        textOutput("totalIppsThermalEnergylasthour")
      ) |>
        popover(
          title = "Ipps",
          textOutput("totalIppsCoalThermalEnergylasthour"),
          textOutput("totalIppsFoThermalEnergylasthour"),
          textOutput("totalIppsRlngThermalEnergylasthour"),
          textOutput("totalIppsGasThermalEnergylasthour"),
        )),
      column(6, actionButton(
        "btn_pop", 
        textOutput("totalGencosThermalEnergylasthour")
      ) |>
        popover(
          title = "Genocs",
          textOutput("totalGencosCoalThermalEnergylasthour"),
          textOutput("totalGencosGasThermalEnergylasthour"),
          textOutput("totalGencosRlngThermalEnergylasthour"),
        ))
    )
  )
  
)

value_boxes_major_categories <- list(
  value_box(
    title = "Hydro",
    value = textOutput("totalHydroEnergy"),
    showcase = bs_icon("water"),
    p(textOutput("totalPublicHydroEnergy")),
    p(textOutput("totalPrivateHydroEnergy"))
  ),
  value_box(
    title = "Renewable",
    value = textOutput("totalRenewableEnergy"),
    showcase = bs_icon("recycle"),
    p(textOutput("totalSolarEnergy")),
    p(textOutput("totalWindEnergy")),
    p(textOutput("totalBagasseEnergy"))
  ),
  value_box(
    title = "Nuclear",
    value = textOutput("totalNuclearEnergy"),
    showcase = bs_icon("radioactive"),
  ),
  value_box(
    title = "Thermal",
    value = textOutput("totalThermalEnergy"),
    showcase = bs_icon("fire"),
    fluidRow(
      column(6, actionButton(
        "btn_pop", 
        textOutput("totalIppsThermalEnergy")
      ) |>
        popover(
          title = "Ipps",
          textOutput("totalIppsCoalThermalEnergy"),
          textOutput("totalIppsFoThermalEnergy"),
          textOutput("totalIppsRlngThermalEnergy"),
          textOutput("totalIppsGasThermalEnergy"),
        )),
      column(6, actionButton(
        "btn_pop", 
        textOutput("totalGencosThermalEnergy")
      ) |>
        popover(
          title = "Genocs",
          textOutput("totalGencosCoalThermalEnergy"),
          textOutput("totalGencosGasThermalEnergy"),
          textOutput("totalGencosRlngThermalEnergy"),
        ))
    )
  )
  
)

ui <- fluidPage(page_navbar(
  # navset_card_tab(
    # sidebar = sidebar("sidebar"),
    nav_panel("MW",
              card(
                card_header(
                  class = "bg-dark",
                  paste("Last File Date:", as.character(last_date))
                ),
                selectInput("timeRangelasthour", "Select Time Range", choices = NULL, selected = "23:00"),  # We set initial choices to NULL
                fluidRow(
                  layout_column_wrap(
                    width = "150px",
                    !!!value_boxes_lasthour
                  )
                  
                )
              ),
              card(
                card_header(
                  class = "bg-dark",
                  "Major Categories"
                ),
                dateRangeInput("dateRange", "Select Date Range", start = min_date, end = max_date, min = min_date, max = max_date),
                # actionButton("calculateButton", "Filter"),
                fluidRow(
                  layout_column_wrap(
                    width = "150px",
                    !!!value_boxes_major_categories
                  )
                  
                )
              ),
              card(
                card_header(
                  class = "bg-dark",
                  "Daily & Hourly breakdown"
                ),
                # actionButton("calculateButton", "Filter"),
                fluidRow(
                  
                  highchartOutput("drilldown_Chart"),
                  
                  
                )
              ),
              card(
                card_header(
                  class = "bg-dark",
                  "Peak Contribution By Individual Powerplant"
                ),
                # actionButton("calculateButton", "Filter"),
                fluidRow(
                  
                  column(3, selectInput("date", "Select Date", choices = unique(data$Time), selected = unique(data$Time)[1])),
                  column(4, selectInput("name", "Select Name", choices = NULL)),
                  column(6),
                  uiOutput("cards"),
                    # Display the table output for the filtered data
                    # tableOutput("table"),
                    # tableOutput("hourlyData"),
                    highchartOutput("areaspline_chart"),
                 
                  
                )
              ), 
              card(
                card_header(
                  class = "bg-dark",
                  "Min Max Average"
                ),
                fluidRow(
                  column(3, selectInput("date_min_max_avg", "Select Date", choices = uniqueDates, selected = uniqueDates[1])),
                  column(4, selectInput("name_min_max_avg", "Select Name", choices = NULL)),
                  column(6),
                  uiOutput("valueBoxes_min_max_avg")
                  
                )
              )
              ),
    nav_panel("Main", 
              layout_column_wrap(
                width = "250px",
                !!!vbs
              ))
  # )
))

server <- function(input, output, session) {
  # last hour first section starts
  # Determine the unique times for the last date
  last_date_times <- unique(format(subset(mw_data, as.Date(Time) == last_date)$Time, "%H:%M"))
  
  # Set the choices for the selectInput
  updateSelectInput(session, "timeRangelasthour", choices = last_date_times, selected = "23:00")
  
  # Define a reactive expression to filter data based on the time range selected
  filtered_datalasthour <- reactive({
    # Filter for the selected time range and the last date
    filtered_data <- mw_data %>%
      filter(format(Time, "%H:%M") == input$timeRangelasthour, as.Date(Time) == last_date)
    return(filtered_data)
  })
  
  observe({
    filtered_data_val <- filtered_datalasthour()
    
    total_Public_hydro_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "HYDEL") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    
    total_Private_hydro_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS HYDEL HYDEL") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    
    total_hydro_energylasthour <- sum(total_Public_hydro_energylasthour$total_energylasthour, total_Private_hydro_energylasthour$total_energylasthour)
    
    total_solar_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "SOLAR") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    total_wind_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "WIND") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    total_bagasse_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS BAGASSE BAGASSE") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    total_renewable_energylasthour <- sum(total_solar_energylasthour$total_energylasthour, total_wind_energylasthour$total_energylasthour, total_bagasse_energylasthour$total_energylasthour)
    
    total_nuclear_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "NUCLEAR") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    
    total_ipps_coal_thermal_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS FOSSIL FUEL Coal") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    
    total_ipps_rlng_thermal_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS FOSSIL FUEL RLNG") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    
    total_ipps_fo_thermal_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS FOSSIL FUEL FO") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    
    total_ipps_gas_thermal_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS FOSSIL FUEL Gas") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    
    total_ipps_thermal_energylasthour <- sum(total_ipps_coal_thermal_energylasthour$total_energylasthour, total_ipps_rlng_thermal_energylasthour$total_energylasthour, total_ipps_fo_thermal_energylasthour$total_energylasthour, total_ipps_gas_thermal_energylasthour$total_energylasthour)
    
    total_gencos_gas_thermal_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "GENCOS Gas") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    
    total_gencos_rlng_thermal_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "GENCOS RLNG") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    
    total_gencos_coal_thermal_energylasthour <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "GENCOS Coal") %>%
      summarise(total_energylasthour = sum(Energy_MWh))
    
    total_gencos_thermal_energylasthour <- sum(total_gencos_gas_thermal_energylasthour$total_energylasthour, total_gencos_rlng_thermal_energylasthour$total_energylasthour, total_gencos_coal_thermal_energylasthour$total_energylasthour)
    
    total_thermal_energylasthour <- total_ipps_thermal_energylasthour + total_gencos_thermal_energylasthour
    
    output$totalPublicHydroEnergylasthour <- renderText({
      paste(total_Public_hydro_energylasthour$total_energylasthour, "Public")
    })
    output$totalPrivateHydroEnergylasthour <- renderText({
      paste(total_Private_hydro_energylasthour$total_energylasthour, "Private")
    })
    output$totalHydroEnergylasthour <- renderText({
      paste(total_hydro_energylasthour, "MWh")
    })
    
    output$totalSolarEnergylasthour <- renderText({
      paste(total_solar_energylasthour$total_energylasthour, "Solar")
    })
    output$totalWindEnergylasthour <- renderText({
      paste(total_wind_energylasthour$total_energylasthour, "Wind")
    })
    output$totalBagasseEnergylasthour <- renderText({
      paste(total_bagasse_energylasthour$total_energylasthour, "Bagasse")
    })
    output$totalRenewableEnergylasthour <- renderText({
      paste(total_renewable_energylasthour, "MWh")
    })
    
    output$totalNuclearEnergylasthour <- renderText({
      paste(total_nuclear_energylasthour$total_energylasthour, "MWh")
    })
    
    output$totalIppsGasThermalEnergylasthour <- renderText({
      paste(int(total_ipps_gas_thermal_energylasthour$total_energylasthour), "Gas")
    })
    output$totalIppsCoalThermalEnergylasthour <- renderText({
      paste(int(total_ipps_coal_thermal_energylasthour$total_energylasthour), "Coal")
    })
    output$totalIppsFoThermalEnergylasthour <- renderText({
      paste(int(total_ipps_fo_thermal_energylasthour$total_energylasthour), "Fo")
    })
    output$totalIppsRlngThermalEnergylasthour <- renderText({
      paste(int(total_ipps_rlng_thermal_energylasthour$total_energylasthour), "Rlng")
    })
    output$totalIppsThermalEnergylasthour <- renderText({
      paste(int(total_ipps_thermal_energylasthour), "Ipps")
    })
    output$totalGencosGasThermalEnergylasthour <- renderText({
      paste(int(total_gencos_gas_thermal_energylasthour$total_energylasthour), "Gas")
    })
    output$totalGencosCoalThermalEnergylasthour <- renderText({
      paste(int(total_gencos_coal_thermal_energylasthour$total_energylasthour), "Coal")
    })
    output$totalGencosRlngThermalEnergylasthour <- renderText({
      paste(int(total_gencos_rlng_thermal_energylasthour$total_energylasthour), "Rlng")
    })
    output$totalGencosThermalEnergylasthour <- renderText({
      paste(int(total_gencos_thermal_energylasthour), "Gencos")
    })
    output$totalThermalEnergylasthour <- renderText({
      paste(int(total_thermal_energylasthour), "MWh")
    })
  })
  # last hour first section ends
  observe({
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    
    filtered_data <- mw_data %>%
      filter(as.Date(Time) >= start_date, as.Date(Time) <= end_date)
    
    total_Public_hydro_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("HYDEL")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_Private_hydro_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("IPPS HYDEL HYDEL")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_hydro_energy <- total_Public_hydro_energy + total_Private_hydro_energy
    
    total_solar_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("SOLAR")) %>%
      summarise(total_energy = sum(Energy_MWh))
    total_wind_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("WIND")) %>%
      summarise(total_energy = sum(Energy_MWh))
    total_bagasse_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("IPPS BAGASSE BAGASSE")) %>%
      summarise(total_energy = sum(Energy_MWh))
    total_renewable_energy <- total_solar_energy + total_wind_energy + total_bagasse_energy
    
    total_nuclear_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("NUCLEAR")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_ipps_coal_thermal_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("IPPS FOSSIL FUEL Coal")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_ipps_rlng_thermal_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("IPPS FOSSIL FUEL RLNG")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_ipps_fo_thermal_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("IPPS FOSSIL FUEL FO")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_ipps_gas_thermal_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("IPPS FOSSIL FUEL Gas")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_ipps_thermal_energy <- total_ipps_coal_thermal_energy + total_ipps_rlng_thermal_energy + total_ipps_fo_thermal_energy + total_ipps_gas_thermal_energy
    
    total_gencos_gas_thermal_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("GENCOS Gas")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_gencos_rlng_thermal_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("GENCOS RLNG")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_gencos_coal_thermal_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("GENCOS Coal")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_gencos_thermal_energy <- total_gencos_gas_thermal_energy + total_gencos_rlng_thermal_energy + total_gencos_coal_thermal_energy
      
    total_thermal_energy <- total_gencos_thermal_energy + total_ipps_thermal_energy
    
    output$totalPublicHydroEnergy <- renderText({
      paste(total_Public_hydro_energy$total_energy, "Public")
    })
    output$totalPrivateHydroEnergy <- renderText({
      paste(total_Private_hydro_energy$total_energy, "Prvate")
    })
    output$totalHydroEnergy <- renderText({
      paste(total_hydro_energy$total_energy, "MWh")
    })
    
    output$totalSolarEnergy <- renderText({
      paste(total_solar_energy$total_energy, "Solar")
    })
    output$totalWindEnergy <- renderText({
      paste(total_wind_energy$total_energy, "Wind")
    })
    output$totalBagasseEnergy <- renderText({
      paste(total_bagasse_energy$total_energy, "Bagasse")
    })
    output$totalRenewableEnergy <- renderText({
      paste(total_renewable_energy$total_energy, "MWh")
    })
    
    output$totalNuclearEnergy <- renderText({
      paste(total_nuclear_energy$total_energy, "MWh")
    })
    
    output$totalIppsGasThermalEnergy <- renderText({
      paste(int(total_ipps_gas_thermal_energy$total_energy), "Gas")
    })
    output$totalIppsCoalThermalEnergy <- renderText({
      paste(int(total_ipps_coal_thermal_energy$total_energy), "Coal")
    })
    output$totalIppsFoThermalEnergy <- renderText({
      paste(int(total_ipps_fo_thermal_energy$total_energy), "Fo")
    })
    output$totalIppsRlngThermalEnergy <- renderText({
      paste(int(total_ipps_rlng_thermal_energy$total_energy), "Rlng")
    })
    output$totalIppsThermalEnergy <- renderText({
      paste(int(total_ipps_thermal_energy$total_energy), "Ipps")
    })
    output$totalGencosGasThermalEnergy <- renderText({
      paste(int(total_gencos_gas_thermal_energy$total_energy), "Gas")
    })
    output$totalGencosCoalThermalEnergy <- renderText({
      paste(int(total_gencos_coal_thermal_energy$total_energy), "Coal")
    })
    output$totalGencosRlngThermalEnergy <- renderText({
      paste(int(total_gencos_rlng_thermal_energy$total_energy), "Rlng")
    })
    output$totalGencosThermalEnergy <- renderText({
      paste(int(total_gencos_thermal_energy$total_energy), "Gencos")
    })
    output$totalThermalEnergy <- renderText({
      paste(int(total_thermal_energy$total_energy), "MWh")
    })
  })
  
  # Update the name filter choices based on the selected date
  observeEvent(input$date, {
    names_for_selected_date <- data$name[data$Time == input$date]
    updateSelectInput(session, "name", choices = names_for_selected_date)
  })
  
  # Append rows from peakhours_data to global_df
  observe({
    global_df <- global_df[0, ]
    selected_date <- as.Date(input$date)
    filtered_data <- peakhours_data[as.Date(peakhours_data$Time) == selected_date & peakhours_data$name == input$name, ]
    global_df <<- rbind(global_df, data.frame(Time = as.POSIXct(filtered_data$Time, format = "%Y-%m-%d %H:%M:%S"), Name = filtered_data$name, Energy = filtered_data$peak_values))
  })
  
  # Append rows from hourly_data to global_df
  observe({
    selected_date <- as.Date(input$date)
    hourly_data <- mw_data[mw_data$Time >= selected_date & mw_data$Time < selected_date + lubridate::dhours(24) & mw_data$Name == input$name, c("Time", "Name", "Energy_MWh")]
    global_df <<- rbind(global_df, data.frame(Time = as.POSIXct(hourly_data$Time, format = "%Y-%m-%d %H:%M:%S"), Name = hourly_data$Name, Energy = hourly_data$Energy_MWh))
  })
  
  # output$table <- renderTable({
  #   # Extract the date part from the selected date
  #   selected_date <- as.Date(input$date)
  #   
  #   # Subset the data by the selected date and name from "peakhours_data"
  #   filtered_data <- peakhours_data[as.Date(peakhours_data$Time) == selected_date & peakhours_data$name == input$name, ]
  #   
  #   # Format the "Time" column as datetime (adjust format as needed)
  #   filtered_data$Time <- format(filtered_data$Time, "%Y-%m-%d %H:%M:%S")
  #   
  #   # Return the filtered data as a table
  #   return(filtered_data)
  # })
  
  # Render the cards
  output$cards <- renderUI({
    # Extract the date part from the selected date
    selected_date <- as.Date(input$date)
    
    # Subset the data by the selected date and name from "peakhours_data"
    filtered_data <- peakhours_data[as.Date(peakhours_data$Time) == selected_date & peakhours_data$name == input$name, ]
    
    # Format the "Time" column as datetime (adjust format as needed)
    filtered_data$Time <- format(filtered_data$Time, "%Y-%m-%d %H:%M:%S")
    
    # Create a list of bslib cards
    cards <- lapply(1:nrow(filtered_data), function(i) {
      card(
        card_header(filtered_data$Time[i]),
        card_body(
          p("Name:", filtered_data$name[i]),
          p("Value:", filtered_data$peak_values[i])
        )
      )
    })
    
    # Split the cards into two rows
    rows <- list(
      fluidRow(
        column(2, cards[1]),
        column(2, cards[2]),
        column(2, cards[3]),
        column(2, cards[4]),
        column(2, cards[5]),
        column(2, cards[6]),
      ),
      fluidRow(
        
        column(4, cards[7]),
        column(4, cards[8]),
        column(4, cards[9])
      )
    )
    
    # Return the rows of cards
    tagList(rows)
  })
  
  
  # output$hourlyData <- renderTable({
  #   # Extract the date part from the selected date
  #   selected_date <- as.Date(input$date)
  #   
  #   # # Print some debugging information
  #   # print(input$date) # Check the selected date
  #   # print(input$name) # Check the selected name
  #   
  #   # Subset the mw_data by the selected date and name
  #   hourly_data <- mw_data[mw_data$Time >= selected_date & mw_data$Time < selected_date + lubridate::dhours(24) & mw_data$Name == input$name, c("Time", "Name", "Energy_MWh")]
  #   
  #   # # Print some debugging information
  #   # print(names(hourly_data)) # Check the column names
  #   # print(unique(hourly_data$Name)) # Check unique "Name" values
  #   
  #   # Format the "Time" column as datetime (adjust format as needed)
  #   hourly_data$Time <- format(hourly_data$Time, "%Y-%m-%d %H:%M:%S")
  #   
  #   # Return the 24-hour data as a table
  #   return(hourly_data)
  # })
  
  output$areaspline_chart <- renderHighchart({
    # Filter the combined data based on user input
    selected_date <- as.Date(input$date)
    selected_name <- input$name
    # Extract the date from the first row of global_df
    date_variable <- as.Date(global_df$Time[1])
    print(date_variable)
    # Remove the date part from the Time column
    global_df$Time <- format(global_df$Time, format = "%H:%M")
    # Sort the records in global_df by the Time column
    global_df <- global_df %>%
      arrange(Time)
    # print(filtered_data)
    print(global_df)
    
    # Create the Highcharter chart
    hc <- highchart() %>%
      hc_title(text = "Area Spline Chart") %>%
      hc_xAxis(type = "category", title = list(text = date_variable)) %>%
      hc_yAxis(title = list(text = "Energy (MWh)")) %>%
      hc_add_series(
        data = global_df,
        type = "areaspline", # Specify the chart type
        hcaes(x = Time, y = Energy),
        # hcaes(x = format(Time, "%Y-%m-%d %H:%M:%S"), y = Energy),
        name = selected_name
      )
    
    return(hc)
  })
  observeEvent(input$date_min_max_avg, {
    names_for_selected_date <- data_min_max_avg$name[data_min_max_avg$Time == input$date_min_max_avg]
    updateSelectInput(session, "name_min_max_avg", choices = names_for_selected_date)
  })
  
  output$valueBoxes_min_max_avg <- renderUI({
    # Subset the data_min_max_avg by the selected date and name
    filtered_data <- data_min_max_avg[data_min_max_avg$Time == input$date_min_max_avg & data_min_max_avg$name == input$name_min_max_avg, ]
    # Convert the "Time" column to a character with the desired format
    filtered_data$Time <- format(filtered_data$Time, format = "%Y-%m-%d")
    
    # Define value boxes
    value_boxes <- list(
      fluidRow(
        h3(paste0("ALL DAY : ", as.character(filtered_data$time_range_one))),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$max_1,
            subtitle = "MAX",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MAX")
          )
        ),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$min_1,
            subtitle = "MIN",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MIN")
          )
        ),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = int(filtered_data$average),
            subtitle = "AVERAGE",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("AVERAGE")
          )
        )
      ),
      fluidRow(
        h3(paste0("OFF-PEAK DURATION : ", as.character(filtered_data$time_range_two))),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$max_2,
            subtitle = "MAX",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MAX")
          )
        ),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$min_2,
            subtitle = "MIN",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MIN")
          )
        )
      ),
      fluidRow(
        h3(paste0("PEAK DURATION : ", as.character(filtered_data$time_range_three))),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$max_3,
            subtitle = "MAX",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MAX")
          )
        ),
        column(
          4,
          value_box(
            title = filtered_data$name,
            value = filtered_data$min_3,
            subtitle = "MIN",
            icon = icon("bolt", lib = "font-awesome"),
            color = "danger",
            p("MIN")
          )
        )
      )
    )
    
    tagList(value_boxes)
  })
  # drilldown chart
  output$drilldown_Chart <- renderHighchart({
    
    # js_code <- sprintf(js_code, desired_hydro_json_output, desired_renewable_json_output)
    hc <- highchart() %>%
      hc_chart(
        type = "column",
        events = list(
          click = JS('function (e) {
        var pointName = e.point.name;
        Shiny.onInputChange("pointName", pointName);
      }')
          ,
          drilldown = JS(js_code)
        )
      ) %>%
      hc_title(text = "Drill Downs") %>%
      hc_xAxis(type = "category") %>%
      hc_plotOptions(series = list(stacking = "normal")) %>%
      # hc_yAxis(max = 1000) %>%
      hc_add_series(
        name = "Hydro",
        color = "blue",
        data = list(
          list(name = "2022-03-02", y = 30, drilldown = T), 
          list(name = "2022-03-03", y = 25, drilldown = T),
          list(name = "2022-03-04", y = 30, drilldown = T),
          list(name = "2022-03-05", y = 25, drilldown = T),
          list(name = "2022-03-06", y = 30, drilldown = T), 
          list(name = "2022-03-07", y = 25, drilldown = T), 
          list(name = "2022-03-08", y = 25, drilldown = T),
          list(name = "2022-03-09", y = 30, drilldown = T), 
          list(name = "2022-03-10", y = 25, drilldown = T), 
          list(name = "2022-03-11", y = 25, drilldown = T),
          list(name = "2022-03-12", y = 30, drilldown = T), 
          list(name = "2022-03-13", y = 25, drilldown = T), 
          list(name = "2022-03-14", y = 25, drilldown = T),
          list(name = "2022-03-15", y = 30, drilldown = T), 
          list(name = "2022-03-16", y = 25, drilldown = T), 
          list(name = "2022-03-17", y = 25, drilldown = T),
          list(name = "2022-03-18", y = 30, drilldown = T), 
          list(name = "2022-03-19", y = 25, drilldown = T), 
          list(name = "2022-03-20", y = 25, drilldown = T),
          list(name = "2022-03-21", y = 30, drilldown = T)
        )
      ) %>%
      hc_add_series(
        name = "Renewable",
        color = "green",
        data = list(
          list(name = "2022-03-02", y = 60, drilldown = T), 
          list(name = "2022-03-03", y = 65, drilldown = T),
          list(name = "2022-03-04", y = 30, drilldown = T),
          list(name = "2022-03-05", y = 25, drilldown = T),
          list(name = "2022-03-06", y = 30, drilldown = T), 
          list(name = "2022-03-07", y = 25, drilldown = T), 
          list(name = "2022-03-08", y = 25, drilldown = T),
          list(name = "2022-03-09", y = 30, drilldown = T), 
          list(name = "2022-03-10", y = 25, drilldown = T), 
          list(name = "2022-03-11", y = 25, drilldown = T),
          list(name = "2022-03-12", y = 30, drilldown = T), 
          list(name = "2022-03-13", y = 25, drilldown = T), 
          list(name = "2022-03-14", y = 25, drilldown = T),
          list(name = "2022-03-15", y = 30, drilldown = T), 
          list(name = "2022-03-16", y = 25, drilldown = T), 
          list(name = "2022-03-17", y = 25, drilldown = T),
          list(name = "2022-03-18", y = 30, drilldown = T), 
          list(name = "2022-03-19", y = 25, drilldown = T), 
          list(name = "2022-03-20", y = 25, drilldown = T),
          list(name = "2022-03-21", y = 30, drilldown = T)
        )
      ) %>%
      hc_add_series(
        name = "Nuclear",
        color = "yellow",
        data = list(
          list(name = "2022-03-02", y = 60, drilldown = T), 
          list(name = "2022-03-03", y = 65, drilldown = T),
          list(name = "2022-03-04", y = 30, drilldown = T), 
          list(name = "2022-03-05", y = 25, drilldown = T),
          list(name = "2022-03-06", y = 30, drilldown = T), 
          list(name = "2022-03-07", y = 25, drilldown = T), 
          list(name = "2022-03-08", y = 25, drilldown = T),
          list(name = "2022-03-09", y = 30, drilldown = T), 
          list(name = "2022-03-10", y = 25, drilldown = T), 
          list(name = "2022-03-11", y = 25, drilldown = T),
          list(name = "2022-03-12", y = 30, drilldown = T), 
          list(name = "2022-03-13", y = 25, drilldown = T), 
          list(name = "2022-03-14", y = 25, drilldown = T),
          list(name = "2022-03-15", y = 30, drilldown = T), 
          list(name = "2022-03-16", y = 25, drilldown = T), 
          list(name = "2022-03-17", y = 25, drilldown = T),
          list(name = "2022-03-18", y = 30, drilldown = T), 
          list(name = "2022-03-19", y = 25, drilldown = T), 
          list(name = "2022-03-20", y = 25, drilldown = T),
          list(name = "2022-03-21", y = 30, drilldown = T)
        )
      ) %>%
      hc_add_series(
        name = "Thermal",
        color = "red",
        data = list(
          list(name = "2022-03-02", y = 60, drilldown = T), 
          list(name = "2022-03-03", y = 65, drilldown = T),
          list(name = "2022-03-04", y = 30, drilldown = T),
          list(name = "2022-03-05", y = 25, drilldown = T),
          list(name = "2022-03-06", y = 30, drilldown = T), 
          list(name = "2022-03-07", y = 25, drilldown = T), 
          list(name = "2022-03-08", y = 25, drilldown = T),
          list(name = "2022-03-09", y = 30, drilldown = T), 
          list(name = "2022-03-10", y = 25, drilldown = T), 
          list(name = "2022-03-11", y = 25, drilldown = T),
          list(name = "2022-03-12", y = 30, drilldown = T), 
          list(name = "2022-03-13", y = 25, drilldown = T), 
          list(name = "2022-03-14", y = 25, drilldown = T),
          list(name = "2022-03-15", y = 30, drilldown = T), 
          list(name = "2022-03-16", y = 25, drilldown = T), 
          list(name = "2022-03-17", y = 25, drilldown = T),
          list(name = "2022-03-18", y = 30, drilldown = T), 
          list(name = "2022-03-19", y = 25, drilldown = T), 
          list(name = "2022-03-20", y = 25, drilldown = T),
          list(name = "2022-03-21", y = 30, drilldown = T)
        )
      )
  })
}


shinyApp(ui, server)