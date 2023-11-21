# Load libraries
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(highcharter)
library(lubridate)

# Read the CSV data
df <- read.csv("log.csv", stringsAsFactors = FALSE)
df$Time <- as.POSIXct(df$Time, format = "%Y-%m-%d %H:%M")
unique(df$Tab)
value_boxes_major_categories <- list(
  value_box(
    title = "Live Update Tab",
    value = textOutput("liveUpdateCount"),
    showcase = bs_icon("play-circle"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Monthly Report Count",
    value = textOutput("monthlyReportCount"),
    showcase = bs_icon("calendar-fill"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Comparative Count",
    value = textOutput("comparativeCount"),
    showcase = bs_icon("arrow-left-right"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Historical Count",
    value = textOutput("historicalCounts"),
    showcase = bs_icon("arrow-down-left-square"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Summary Count",
    value = textOutput("summaryCount"),
    showcase = bs_icon("text-left"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Settings Count",
    value = textOutput("settingsCount"),
    showcase = bs_icon("gear"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Electric Count",
    value = textOutput("electricCount"),
    showcase = bs_icon("cloud-lightning"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Solar Count",
    value = textOutput("solarCount"),
    showcase = bs_icon("sunrise-fill"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "last Opened Tab",
    value = textOutput("lastOpenedTabCount"),
    showcase = bs_icon("file-richtext-fill"),
    theme_color = "warning",
    style = "color: white;"
  )
  
)
# DateOnly
value_boxes_major_categories_date_only <- list(
  value_box(
    title = "Live Update Tab",
    value = textOutput("liveUpdateCountDateOnly"),
    showcase = bs_icon("play-circle"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Monthly Report Count",
    value = textOutput("monthlyReportCountDateOnly"),
    showcase = bs_icon("calendar-fill"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Comparative Count",
    value = textOutput("comparativeCountDateOnly"),
    showcase = bs_icon("arrow-left-right"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Historical Count",
    value = textOutput("historicalCountsDateOnly"),
    showcase = bs_icon("arrow-down-left-square"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Summary Count",
    value = textOutput("summaryCountDateOnly"),
    showcase = bs_icon("text-left"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Settings Count",
    value = textOutput("settingsCountDateOnly"),
    showcase = bs_icon("gear"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Electric Count",
    value = textOutput("electricCountDateOnly"),
    showcase = bs_icon("cloud-lightning"),
    theme_color = "info",
    style = "color: white;"
  ),
  value_box(
    title = "Solar Count",
    value = textOutput("solarCountDateOnly"),
    showcase = bs_icon("sunrise-fill"),
    theme_color = "info",
    style = "color: white;"
  )
  
)
# UI
ui <- fluidPage(page_navbar(
  
  nav_panel("All Users",
            card(
              card_header(
                class = "bg-dark",
                "Access Over Time For All Users"
              ),
              fluidRow(
                column(3, dateRangeInput("dateRangeDateRangeOnly", "Select Date Range", start = min(df$Time), end = max(df$Time), min = min(df$Time), max = max(df$Time))),
                column(9, ),
                column(3, value_boxes_major_categories_date_only[1]),
                column(3, value_boxes_major_categories_date_only[2]),
                column(3, value_boxes_major_categories_date_only[3]),
                column(3, value_boxes_major_categories_date_only[4]),
                column(3, value_boxes_major_categories_date_only[5]),
                column(3, value_boxes_major_categories_date_only[6]),
                column(3, value_boxes_major_categories_date_only[7]),
                column(3, value_boxes_major_categories_date_only[8]),
                column(12, highchartOutput("accessOverTimeDateRangeOnly"))
              )
            ),
            card(
              card_header(
                class = "bg-dark",
                "Tab Distribution For All Users"
              ),
              # actionButton("calculateButton", "Filter"),
              fluidRow(
                column(12, highchartOutput("tabDistributionDateOnly"))
              )
              
            ),
            card(
              card_header(
                class = "bg-dark",
                "User Access Count For All Users"
              ),
              # actionButton("calculateButton", "Filter"),
              fluidRow(
                column(12, highchartOutput("userAccessCountDateOnly"))
              )
              
              
            )
  ),
  # navset_card_tab(
  # sidebar = sidebar("sidebar"),
  nav_panel("Specific User",
            card(
              card_header(
                class = "bg-dark",
                "Tabs Access Counts"
              ),
               # We set initial choices to NULL
              fluidRow(
                column(3, dateRangeInput("dateRange", "Select Date Range", start = min(df$Time), end = max(df$Time), min = min(df$Time), max = max(df$Time))),
                column(3, selectInput("username", "Select Username", choices = unique(df$Username))),
                column(6,value_boxes_major_categories[9]),
                column(3, value_boxes_major_categories[1]),
                column(3, value_boxes_major_categories[2]),
                column(3, value_boxes_major_categories[3]),
                column(3, value_boxes_major_categories[4]),
                column(3, value_boxes_major_categories[5]),
                column(3, value_boxes_major_categories[6]),
                column(3, value_boxes_major_categories[7]),
                column(3, value_boxes_major_categories[8]),
                
              )
            ),
            card(
              card_header(
                class = "bg-dark",
                "Access Over Time"
              ),
              # actionButton("calculateButton", "Filter"),
              fluidRow(
                
                column(12, highchartOutput("accessOverTime")),
                
                
              )
            ),
            card(
              card_header(
                class = "bg-dark",
                "Tab Distribution"
              ),
              # actionButton("calculateButton", "Filter"),
              fluidRow(
                column(12, highchartOutput("tabDistribution"))
              )
              
            ),
            card(
              card_header(
                class = "bg-dark",
                "User Access Count"
              ),
              # actionButton("calculateButton", "Filter"),
              fluidRow(
                column(12, highchartOutput("userAccessCount"))
              )
              
              
            )
            )))
# ui <- fluidPage(
#   theme = bslib::bs_theme(),
#   titlePanel("User Access Dashboard"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       dateRangeInput("dateRange", "Select Date Range", start = min(df$Time), end = max(df$Time)),
#       selectInput("username", "Select Username", choices = unique(df$Username))
#     ),
#     
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Access Counts", value = "counts",
#                  fluidRow(
#                    
#                    column(3, valueBoxOutput("liveUpdateCount")),
#                    column(3, valueBoxOutput("monthlyReportCount")),
#                    column(3, valueBoxOutput("comparativeCount")),
#                    column(3, valueBoxOutput("historicalCount")),
#                    column(3, valueBoxOutput("summaryCount")),
#                    column(3, valueBoxOutput("settingsCount")),
#                    column(3, valueBoxOutput("electricCount")),
#                    column(3, valueBoxOutput("solarCount")),
#                    
#                    # Add the new valueBox here
#                    column(3, valueBoxOutput("yourNewValueBox")),
#                    column(12, highchartOutput("accessOverTime")),
#                  )
#         )
#       )
#     )
#   )
# )

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    start_date <- as.POSIXct(input$dateRange[1])
    end_date <- as.POSIXct(input$dateRange[2])
    username <- input$username
    
    df_filtered <- df %>%
      mutate(Time = as.POSIXct(Time)) %>%
      filter(Time >= start_date, Time <= end_date, Username == username)
    print(str(df_filtered))
    return(df_filtered)
  })
  
 
  
  output$liveUpdateCount <- renderText({
    liveUpdateCount <- nrow(filtered_data() %>% filter(Tab == "live_update_tab"))
    liveUpdateCount
  })
  
  output$monthlyReportCount <- renderText({
    monthlyReportCount <- nrow(filtered_data() %>% filter(Tab == "monthlyReport_tab"))
    monthlyReportCount
  })
  
  output$comparativeCount <- renderText({
    comparativeCount <- nrow(filtered_data() %>% filter(Tab == "comparative_tab"))
    comparativeCount
  })
  
  output$historicalCounts <- renderText({
    historicalCount <- nrow(filtered_data() %>% filter(Tab == "historical_tab"))
    historicalCount
  })
  
  output$summaryCount <- renderText({
    summaryCount <- nrow(filtered_data() %>% filter(Tab == "summary_tab"))
    summaryCount
  })
  
  output$settingsCount <- renderText({
    summaryCount <- nrow(filtered_data() %>% filter(Tab == "settings_tab"))
    summaryCount
  })
  
  output$electricCount <- renderText({
    summaryCount <- nrow(filtered_data() %>% filter(Tab == "Electricity_bill_tab"))
    summaryCount
  })
  
  output$solarCount <- renderText({
    summaryCount <- nrow(filtered_data() %>% filter(Tab == "solar_estimator_tab"))
    summaryCount
  })
  
  output$lastOpenedTabCount <- renderText({
    lastOpenedTab <- filtered_data() %>%
      arrange(desc(Time)) %>%
      slice(1) %>%
      pull(Tab)
    
    lastOpenedTabCount <- nrow(filtered_data() %>% filter(Tab == lastOpenedTab))
    lastOpenedTab
    # paste(lastOpenedTabCount, "# (Last Opened Tab: ", lastOpenedTab, ")")
  })
  
  output$accessOverTime <- renderHighchart({
    data <- filtered_data() %>%
      count(Time, Tab) %>%
      mutate(Time = as.POSIXct(Time))
    tabs <- unique(data$Tab)
    highchart() %>%
      hc_title(text = "Access Over Time") %>%
      hc_xAxis(type = "datetime", title = list(text = "Time"), dateTimeLabelFormats = list(day = '%Y-%m-%d %H:%M')) %>%
      hc_yAxis(title = list(text = "Access Count")) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_tooltip(
        headerFormat = '<span style="font-size:10px">{point.x:%Y-%m-%d %H:%M}</span><br/>',
        pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y}</b><br/>'
      ) %>%
      hc_add_series(
        data = data,
        type = "column",
        hcaes(x = highcharter::datetime_to_timestamp(Time), y = n, group = Tab),
        name = tabs
      )
  })
  
  output$tabDistribution <- renderHighchart({
    data <- filtered_data() %>%
      count(Tab) %>%
      mutate(Tab = factor(Tab))
    tabs <- unique(data$Tab)
    highchart() %>%
      hc_title(text = "Tab Distribution") %>%
      hc_chart(type = "pie") %>%
      hc_plotOptions(pie = list(allowPointSelect = TRUE, cursor = "pointer")) %>%
      hc_add_series(
        data = data,
        type = "pie",
        hcaes(name = Tab, y = n),
        dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.percentage:.1f}%')
      )
    
  })
  
  output$userAccessCount <- renderHighchart({
    data <- filtered_data() %>%
      group_by(Username) %>%
      summarise(Count = n())
    
    highchart() %>%
      hc_title(text = "User Access Count") %>%
      hc_xAxis(categories = data$Username) %>%
      hc_yAxis(title = list(text = "Access Count")) %>%
      hc_add_series(
        data = data,
        type = "bar",
        hcaes(x = Username, y = Count),
        name = "Total Access Count"
      )
  })
  
  
  
  
  
  filtered_data_date_range_only <- reactive({
    start_date <- as.POSIXct(input$dateRangeDateRangeOnly[1])
    end_date <- as.POSIXct(input$dateRangeDateRangeOnly[2])
    
    df_filtered <- df %>%
      mutate(Time = as.POSIXct(Time)) %>%
      filter(Time >= start_date, Time <= end_date)
    
    return(df_filtered)
  })
  
  
  output$liveUpdateCountDateOnly <- renderText({
    liveUpdateCount <- nrow(filtered_data_date_range_only() %>% filter(Tab == "live_update_tab"))
    liveUpdateCount
  })
  
  output$monthlyReportCountDateOnly <- renderText({
    monthlyReportCount <- nrow(filtered_data_date_range_only() %>% filter(Tab == "monthlyReport_tab"))
    monthlyReportCount
  })
  
  output$comparativeCountDateOnly <- renderText({
    comparativeCount <- nrow(filtered_data_date_range_only() %>% filter(Tab == "comparative_tab"))
    comparativeCount
  })
  
  output$historicalCountsDateOnly <- renderText({
    historicalCount <- nrow(filtered_data_date_range_only() %>% filter(Tab == "historical_tab"))
    historicalCount
  })
  
  output$summaryCountDateOnly <- renderText({
    summaryCount <- nrow(filtered_data_date_range_only() %>% filter(Tab == "summary_tab"))
    summaryCount
  })
  
  output$settingsCountDateOnly <- renderText({
    summaryCount <- nrow(filtered_data_date_range_only() %>% filter(Tab == "settings_tab"))
    summaryCount
  })
  
  output$electricCountDateOnly <- renderText({
    summaryCount <- nrow(filtered_data_date_range_only() %>% filter(Tab == "Electricity_bill_tab"))
    summaryCount
  })
  
  output$solarCountDateOnly <- renderText({
    summaryCount <- nrow(filtered_data_date_range_only() %>% filter(Tab == "solar_estimator_tab"))
    summaryCount
  })
  
  
  
  output$accessOverTimeDateRangeOnly <- renderHighchart({
    data <- filtered_data_date_range_only() %>%
      count(Time, Tab) %>%
      mutate(Time = as.POSIXct(Time))
    tabs <- unique(data$Tab)
    highchart() %>%
      hc_title(text = "Access Over Time (Date Range Only)") %>%
      hc_xAxis(type = "datetime", title = list(text = "Time"), dateTimeLabelFormats = list(day = '%Y-%m-%d %H:%M')) %>%
      hc_yAxis(title = list(text = "Access Count")) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_tooltip(
        headerFormat = '<span style="font-size:10px">{point.x:%Y-%m-%d %H:%M}</span><br/>',
        pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y}</b><br/>'
      ) %>%
      hc_add_series(
        data = data,
        type = "column",
        hcaes(x = highcharter::datetime_to_timestamp(Time), y = n, group = Tab),
        name = tabs
      )
  })
  
  
  # output$tabDistributionDateOnly <- renderHighchart({
  #   data <- filtered_data_date_range_only() %>%
  #     count(Tab) %>%
  #     mutate(Tab = factor(Tab))
  #   tabs <- unique(data$Tab)
  #   highchart() %>%
  #     hc_title(text = "Tab Distribution") %>%
  #     hc_chart(type = "pie") %>%
  #     hc_plotOptions(pie = list(allowPointSelect = TRUE, cursor = "pointer")) %>%
  #     hc_add_series(
  #       data = data,
  #       type = "pie",
  #       hcaes(name = Tab, y = n),
  #       dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.percentage:.1f}%')
  #     )
  # 
  # })
  output$tabDistributionDateOnly <- renderHighchart({
    data <- filtered_data_date_range_only() %>%
      count(Tab) %>%
      mutate(Tab = factor(Tab))
    tabs <- unique(data$Tab)
    highchart() %>%
      hc_title(text = "Tab Distribution") %>%
      hc_chart(type = "pie") %>%
      hc_plotOptions(pie = list(allowPointSelect = TRUE, cursor = "pointer")) %>%
      hc_add_series(
        data = data,
        type = "pie",
        hcaes(name = Tab, y = n),
        dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.percentage:.1f}%')
      ) %>%
      hc_tooltip(formatter = JS("function () {
        return 'Count: ' + this.point.y + '/' + this.point.total + ' (' + this.point.percentage.toFixed(1) + '%)';
      }"))
  })
  

  
  
  
  # output$userAccessCountDateOnly <- renderHighchart({
  #   data <- filtered_data_date_range_only() %>%
  #     group_by(Username) %>%
  #     summarise(Count = n())
  # 
  #   highchart() %>%
  #     hc_title(text = "User Access Count") %>%
  #     hc_xAxis(categories = data$Username) %>%
  #     hc_yAxis(title = list(text = "Access Count")) %>%
  #     hc_add_series(
  #       data = data,
  #       type = "bar",
  #       hcaes(x = Username, y = Count),
  #       name = "Total Access Count"
  #     )
  # })
  
  
#   output$userAccessCountDateOnly <- renderHighchart({
#   data <- filtered_data_date_range_only() %>%
#     group_by(Username, Tab) %>%
#     summarise(Count = n())
# 
#   highchart() %>%
#     hc_title(text = "User Access Count") %>%
#     hc_xAxis(categories = unique(data$Username)) %>%
#     hc_yAxis(title = list(text = "Access Count")) %>%
#     hc_add_series(
#       data = data,
#       type = "bar",
#       hcaes(x = Username, y = Count, group = Tab),
#       name = "{point.name}"
#     ) %>%
#     hc_plotOptions(
#       bar = list(
#         stacking = 'normal',
#         dataLabels = list(enabled = TRUE)
#       )
#     ) %>%
#     hc_legend(align = "right", verticalAlign = "top", layout = "vertical", reversed = TRUE)
# })

  output$userAccessCountDateOnly <- renderHighchart({
    data <- filtered_data_date_range_only() %>%
      group_by(Username, Tab) %>%
      summarise(Count = n())
    tabs <- unique(data$Tab)
    # Assuming your data frame is named df
    print(data, n = nrow(data))
    # Plot a stacked column bar chart
    hchart(data, type = "column", hcaes(x = Username, y = Count, group = Tab), stacking = "normal")
    # # Reshape the data frame into a wide format
    # data_wide <- reshape2::dcast(data, Username ~ Tab, value.var = "Count")
    
    # # Plot a stacked column bar chart
    # highchart() %>%
    #   hc_xAxis(categories = data_wide$Username) %>%
    #   hc_add_series(name = "Electricity_bill_tab", type = "column", data = data_wide$Electricity_bill_tab, stacking = "normal") %>%
    #   hc_add_series(name = "historical_tab", type = "column", data = data_wide$historical_tab, stacking = "normal") %>%
    #   hc_add_series(name = "live_update_tab", type = "column", data = data_wide$live_update_tab, stacking = "normal") %>%
    #   hc_add_series(name = "summary_tab", type = "column", data = data_wide$summary_tab, stacking = "normal") %>%
    #   hc_add_series(name = "comparative_tab", type = "column", data = data_wide$comparative_tab, stacking = "normal") %>%
    #   hc_add_series(name = "solar_estimator_tab", type = "column", data = data_wide$solar_estimator_tab, stacking = "normal")%>%
    #   hc_add_series(name = "settings_tab", type = "column", data = data_wide$settings_tab, stacking = "normal")%>%
    #   hc_add_series(name = "monthlyReport_tab", type = "column", data = data_wide$monthlyReport_tab, stacking = "normal")
  })
  
  
  
  
  
  
}

shinyApp(ui, server)
