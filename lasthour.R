library(shiny)
library(bslib)
library(bsicons)

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
              selectInput("timeRange", "Select Time Range", choices = NULL, selected = "23:00"),  # We set initial choices to NULL
              fluidRow(
                layout_column_wrap(
                  width = "150px",
                  !!!value_boxes_major_categories
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
  updateSelectInput(session, "timeRange", choices = last_date_times, selected = "23:00")
  
  # Define a reactive expression to filter data based on the time range selected
  filtered_data <- reactive({
    # Filter for the selected time range and the last date
    filtered_data <- mw_data %>%
      filter(format(Time, "%H:%M") == input$timeRange, as.Date(Time) == last_date)
    return(filtered_data)
  })
  
  observe({
    filtered_data_val <- filtered_data()
    print(filtered_data_val)
    
    total_Public_hydro_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "HYDEL") %>%
      summarise(total_energy = sum(Energy_MWh))
    
    print(total_Public_hydro_energy)
    
    total_Private_hydro_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS HYDEL HYDEL") %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_hydro_energy <- sum(total_Public_hydro_energy$total_energy, total_Private_hydro_energy$total_energy)
    
    total_solar_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "SOLAR") %>%
      summarise(total_energy = sum(Energy_MWh))
    total_wind_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "WIND") %>%
      summarise(total_energy = sum(Energy_MWh))
    total_bagasse_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS BAGASSE BAGASSE") %>%
      summarise(total_energy = sum(Energy_MWh))
    total_renewable_energy <- sum(total_solar_energy$total_energy, total_wind_energy$total_energy, total_bagasse_energy$total_energy)
    
    total_nuclear_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "NUCLEAR") %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_ipps_coal_thermal_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS FOSSIL FUEL Coal") %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_ipps_rlng_thermal_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS FOSSIL FUEL RLNG") %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_ipps_fo_thermal_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS FOSSIL FUEL FO") %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_ipps_gas_thermal_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "IPPS FOSSIL FUEL Gas") %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_ipps_thermal_energy <- sum(total_ipps_coal_thermal_energy$total_energy, total_ipps_rlng_thermal_energy$total_energy, total_ipps_fo_thermal_energy$total_energy, total_ipps_gas_thermal_energy$total_energy)
    
    total_gencos_gas_thermal_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "GENCOS Gas") %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_gencos_rlng_thermal_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "GENCOS RLNG") %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_gencos_coal_thermal_energy <- filtered_data_val %>%
      filter(sub_categories_by_fuel == "GENCOS Coal") %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_gencos_thermal_energy <- sum(total_gencos_gas_thermal_energy$total_energy, total_gencos_rlng_thermal_energy$total_energy, total_gencos_coal_thermal_energy$total_energy)
    
    total_thermal_energy <- total_ipps_thermal_energy + total_gencos_thermal_energy
    
    output$totalPublicHydroEnergy <- renderText({
      paste(total_Public_hydro_energy$total_energy, "Public")
    })
    output$totalPrivateHydroEnergy <- renderText({
      paste(total_Private_hydro_energy$total_energy, "Private")
    })
    output$totalHydroEnergy <- renderText({
      paste(total_hydro_energy, "MWh")
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
      paste(total_renewable_energy, "MWh")
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
      paste(int(total_ipps_thermal_energy), "Ipps")
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
      paste(int(total_gencos_thermal_energy), "Gencos")
    })
    output$totalThermalEnergy <- renderText({
      paste(int(total_thermal_energy), "MWh")
    })
  })
}


shinyApp(ui, server)