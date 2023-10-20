library(shiny)
library(dplyr)
library(bslib)
library(bsicons)

int <- function(x) {
  as.integer(floor(x))
}

# Load your CSV data
data <- read.csv("C:/xampp/htdocs/latest_Dash/html/iconbar/include/csv/mw_new.csv")
# Determine the minimum and maximum dates in the CSV data
min_date <- min(as.Date(data$Time))
max_date <- max(as.Date(data$Time))
# # Filter the data initially with the default date range
# default_filtered_data <- data %>%
#   filter(as.Date(Time) >= min_date, as.Date(Time) <= max_date)
# Define a function to filter data and calculate total energy
calculateCategoryEnergy <- function(data, category) {
  filtered_data <- data %>%
    filter(sub_categories_by_fuel %in% category)
  total_energy <- filtered_data %>%
    summarise(total_energy = sum(Energy_MWh))
  return(total_energy$total_energy)
}
# # Filter the data initially with the default date range
# default_filtered_data <- data %>%
#   filter(as.Date(Time) >= min_date, as.Date(Time) <= max_date)
# 
# # Calculate the total energy for Hydro power plants
# default_total_hydro_energy <- default_filtered_data %>%
#   filter(sub_categories_by_fuel %in% c("HYDEL", "IPPS HYDEL HYDEL")) %>%
#   summarise(total_energy = sum(Energy_MWh))

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
              )),
    nav_panel("Main", 
              layout_column_wrap(
                width = "250px",
                !!!vbs
              ))
  # )
))

server <- function(input, output, session) {
  
  
  # output$totalPublicHydroEnergy <- renderText({
  #   Public_hydro_energy <- calculateCategoryEnergy(default_filtered_data, "HYDEL")
  #   paste(Public_hydro_energy, "MWh")
  # })
  # output$totalPrivateHydroEnergy <- renderText({
  #   Private_hydro_energy <- calculateCategoryEnergy(default_filtered_data, "IPPS HYDEL HYDEL")
  #   paste(Private_hydro_energy, "MWh")
  # })
  # output$totalHydroEnergy <- renderText({
  #   hydro_energy <- calculateCategoryEnergy(default_filtered_data, c("HYDEL", "IPPS HYDEL HYDEL"))
  #   paste(hydro_energy, "MWh")
  # })
  # 
  # 
  # 
  # output$totalRenewableEnergy <- renderText({
  #   renewable_energy <- calculateCategoryEnergy(default_filtered_data, c("IPPS BAGASSE BAGASSE", "SOLAR", "WIND"))
  #   paste(renewable_energy, "MWh")
  # })
  # 
  # output$totalNuclearEnergy <- renderText({
  #   nuclear_energy <- calculateCategoryEnergy(default_filtered_data, c("NUCLEAR"))
  #   paste(nuclear_energy, "MWh")
  # })
  # 
  # output$totalThermalEnergy <- renderText({
  #   thermal_energy <- calculateCategoryEnergy(default_filtered_data, c("IPPS FOSSIL FUEL Coal", "IPPS FOSSIL FUEL RLNG", "IPPS FOSSIL FUEL FO", "IPPS FOSSIL FUEL Gas", "GENCOS Gas", "GENCOS RLNG", "GENCOS Coal"))
  #   paste(int(thermal_energy), "MWh")
  # })
  
  observe({
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    
    filtered_data <- data %>%
      filter(as.Date(Time) >= start_date, as.Date(Time) <= end_date)
    
    total_Public_hydro_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("HYDEL")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_Private_hydro_energy <- filtered_data %>%
      filter(sub_categories_by_fuel %in% c("IPPS HYDEL HYDEL")) %>%
      summarise(total_energy = sum(Energy_MWh))
    
    total_hydro_energy <- total_Public_hydro_energy + total_Private_hydro_energy
    # total_hydro_energy <- filtered_data %>%
    #   filter(sub_categories_by_fuel %in% c("HYDEL", "IPPS HYDEL HYDEL")) %>%
    #   summarise(total_energy = sum(Energy_MWh))
    
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
}
# 
# server <- function(input, output, session) {
#   observeEvent(input$calculateButton, {
#     # Extract the start and end dates from the date range input
#     start_date <- input$dateRange[1]
#     end_date <- input$dateRange[2]
#     
#     # Filter the data based on the selected date range
#     filtered_data <- data %>%
#       filter(as.Date(Time) >= start_date, as.Date(Time) <= end_date)
#     
#     # Calculate the total energy for Hydro power plants
#     total_hydro_energy <- filtered_data %>%
#       filter(sub_categories_by_fuel %in% c("HYDEL", "IPPS HYDEL HYDEL")) %>%
#       summarise(total_energy = sum(Energy_MWh))
#     
#     # Display the total energy in the output
#     output$totalHydroEnergy <- renderText({
#       paste("MWh", total_hydro_energy$total_energy)
#     })
#   })
# }

shinyApp(ui, server)