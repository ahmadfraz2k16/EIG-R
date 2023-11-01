library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
int <- function(x) {
  as.integer(floor(x))
}

# Load your CSV mw_data
# Specify the file paths for both datasets
mw_file_path <- "C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/mw_new.csv"
# Read the CSV files
mw_data <- read.csv(mw_file_path)

mw_data$Time <- as.POSIXct(mw_data$Time, format = "%Y-%m-%d %H:%M", tz = "UTC")

# Determine the minimum and maximum dates in the CSV mw_data
min_date <- min(as.Date(mw_data$Time))
max_date <- max(as.Date(mw_data$Time))
# Get the last date
last_date <- max_date


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
}


shinyApp(ui, server)