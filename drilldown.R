# Load the highcharter package
library(highcharter)
library(jsonlite)
library(shiny)
library(plumber)

# #* Get JSON data from a file
# #* @get /api/data
# function() {
#   # Load data from a JSON file (modify the file path accordingly)
#   data <- jsonlite::fromJSON("drilldownJSONdata.json")
#   
#   # Return the data as JSON
#   return(data)
# }

# Read the JSON file line by line
jsondata <- readLines("drilldownJSONdata.json")
# Paste the lines together as a string
jsondata <- paste(jsondata, collapse = "")
# Print the string
print(jsondata)

# jsondata <- jsonlite::fromJSON("drilldownJSONdata.json")
# print(jsondata)
# jsondata <- read_json("drilldownJSONdata.json")
# print(toJSON(jsondata, pretty = TRUE))

# Sample R data frames
privateData <- data.frame(
  name = "Private",
  color = "red",
  data = list(
    list("00:00", 125),
    list("01:00", 135),
    list("02:00", 133)
  )
)

publicData <- data.frame(
  name = "Public",
  color = "blue",
  data = list(
    list("00:00", 1059),
    list("01:00", 1111),
    list("02:00", 1071)
  )
)
date_check <- "2022-03-02"
# Convert R data frames to JSON
privateDataJSON <- toJSON(privateData, pretty = TRUE)
publicDataJSON <- toJSON(publicData, pretty = TRUE)
desired_hydro_json_output <- '{
  "Private": {
    "name": "Private",
    "color": "red",
    "data": [
      ["00:00", 125],
      ["01:00", 135],
      ["02:00", 133]
    ]
  },
  "Public": {
    "name": "Public",
    "color": "blue",
    "data": [
      ["00:00", 1059],
      ["01:00", 1111],
      ["02:00", 1071]
    ]
  }
}'
desired_renewable_json_output <-  "{
      'Solar': {
        'name': 'Solar',
        'color': 'yellow',
        'data': [
          [
            '00:00',
            50
          ],
          [
            '01:00',
            60
          ],
          [
            '02:00',
            70
          ]
        ]
      },
      'Wind': {
        'name': 'Wind',
        'color': 'blue',
        'data': [
          [
            '00:00',
            564
          ],
          [
            '01:00',
            289
          ],
          [
            '02:00',
            317
          ]
        ]
      },
      'Bagasse': {
        'name': 'Bagasse',
        'color': 'green',
        'data': [
          [
            '00:00',
            180
          ],
          [
            '01:00',
            187
          ],
          [
            '02:00',
            189
          ]
        ]
      }
    }"
# # Create sample data
# data <- data.frame(
#   category = c("A", "B", "C", "D"),
#   value1 = c(10, 15, 8, 12),
#   value2 = c(5, 10, 6, 8),
#   value3 = c(3, 7, 4, 6)
# )
# 
# # Create the stacked bar column chart
# highchart() %>%
#   hc_chart(type = "column") %>%
#   hc_title(text = "Stacked Bar Column Chart") %>%
#   hc_xAxis(categories = data$category) %>%
#   hc_yAxis(title = list(text = "Value")) %>%
#   hc_plotOptions(column = list(
#     stacking = "normal",
#     dataLabels = list(enabled = TRUE)
#   )) %>%
#   hc_add_series(name = "Value 1", data = data$value1) %>%
#   hc_add_series(name = "Value 2", data = data$value2) %>%
#   hc_add_series(name = "Value 3", data = data$value3)

# Define the JavaScript code
js_code <- "function(e) {
    var javascriptArray = %s;
    var dataParentLayerIPPS = javascriptArray.DataForParentOne.DataParentLayerIPPS;
    var dataParentLayerGENCOS = javascriptArray.DataForParentOne.DataParentLayerGENCOS;
    var desiredOutputIPPS = [];
    var desiredOutputGENCOS = [];
// Loop through the data and push each entry to the desired output array
    for (var date in dataParentLayerGENCOS) {
        if (dataParentLayerGENCOS.hasOwnProperty(date)) {
            desiredOutputGENCOS.push(dataParentLayerGENCOS[date]);
        }
    }
    // Loop through the data and push each entry to the desired output array
    for (var date in dataParentLayerIPPS) {
        if (dataParentLayerIPPS.hasOwnProperty(date)) {
            desiredOutputIPPS.push(dataParentLayerIPPS[date]);
        }
    }
    var specificDate = javascriptArray[e.point.name];
    var chart = this,
    Hydro = specificDate.Hydro,

    Renewables = specificDate.Renewable,
    
   

    Thermal = {

        'IPPS': {
            name: 'IPPS',
            color: 'Brown',
            data: desiredOutputIPPS,
            stack: 'move'

        },


        'GENCOS': {

            name: 'GENCOS',
            color: 'DarkGrey',
            data: desiredOutputGENCOS,
            stack: 'move'

        }

    },


    IPPS = specificDate.IPPS,


    Gencos = specificDate.GENCOS,



    Nuclear = specificDate.Nuclear
    
    
    
    
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
hc

# # Your data
# data <- list(
#   "2022-03-02" = list(
#     Hydro = list(
#       Private = list(
#         name = "Private",
#         color = "red",
#         data = list(
#           c("00:00", 125), c("01:00", 135), c("02:00", 133), c("03:00", 135), c("04:00", 133),
#           c("05:00", 133), c("06:00", 134), c("07:00", 136), c("08:00", 134), c("09:00", 135),
#           c("10:00", 135), c("11:00", 140), c("12:00", 189), c("13:00", 180), c("14:00", 180),
#           c("15:00", 180), c("16:00", 182), c("17:00", 196), c("18:00", 186), c("19:00", 180),
#           c("20:00", 181), c("21:00", 181), c("22:00", 133), c("23:00", 124)
#         )
#       ),
#       Public = list(
#         name = "Public",
#         color = "blue",
#         data = list(
#           c("00:00", 1059), c("01:00", 1111), c("02:00", 1071), c("03:00", 1070), c("04:00", 955),
#           c("05:00", 986), c("06:00", 1806), c("07:00", 1364), c("08:00", 1369), c("09:00", 1789),
#           c("10:00", 1708), c("11:00", 1879), c("12:00", 1558), c("13:00", 1046), c("14:00", 1005),
#           c("15:00", 931), c("16:00", 959), c("17:00", 1201), c("18:00", 1813), c("19:00", 2319),
#           c("20:00", 2006), c("21:00", 1614), c("22:00", 1439), c("23:00", 1219)
#         )
#       )
#     ),
#     Renewable = list(
#       Solar = list(
#         name = "Solar",
#         color = "yellow",
#         data = list(
#           c("00:00", 0), c("01:00", 0), c("02:00", 0), c("03:00", 0), c("04:00", 0), c("05:00", 0),
#           c("06:00", 0), c("07:00", 8), c("08:00", 86), c("09:00", 151), c("10:00", 136), c("11:00", 59),
#           c("12:00", 86), c("13:00", 131), c("14:00", 139), c("15:00", 106), c("16:00", 146),
#           c("17:00", 43), c("18:00", 0), c("19:00", 0), c("20:00", 0), c("21:00", 0), c("22:00", 0),
#           c("23:00", 0)
#         )
#       ),
#       Wind = list(
#         name = "Wind",
#         color = "blue",
#         data = list(
#           c("00:00", 564), c("01:00", 289), c("02:00", 317), c("03:00", 285), c("04:00", 226),
#           c("05:00", 101), c("06:00", 376), c("07:00", 150), c("08:00", 78), c("09:00", 93),
#           c("10:00", 232), c("11:00", 71), c("12:00", 50), c("13:00", 50), c("14:00", 78),
#           c("15:00", 92), c("16:00", 123), c("17:00", 182), c("18:00", 364), c("19:00", 522),
#           c("20:00", 331), c("21:00", 337), c("22:00", 436), c("23:00", 477)
#         )
#       ),
#       Bagasse = list(
#         name = "Bagasse",
#         color = "green",
#         data = list(
#           c("00:00", 180), c("01:00", 187), c("02:00", 189), c("03:00", 185), c("04:00", 188),
#           c("05:00", 182), c("06:00", 187), c("07:00", 182), c("08:00", 191), c("09:00", 178),
#           c("10:00", 186), c("11:00", 185), c("12:00", 182), c("13:00", 186), c("14:00", 187),
#           c("15:00", 185), c("16:00", 185), c("17:00", 186), c("18:00", 185), c("19:00", 187),
#           c("20:00", 188), c("21:00", 184), c("22:00", 173), c("23:00", 182)
#         )
#       )
#     ),
#     IPPS = list(
#       Gas = list(
#         name = "Gas",
#         color = "yellow",
#         data = list(
#           c("00:00", 1091), c("01:00", 1080), c("02:00", 811), c("03:00", 812), c("04:00", 813),
#           c("05:00", 910), c("06:00", 1064), c("07:00", 1051), c("08:00", 1007), c("09:00", 875),
#           c("10:00", 870), c("11:00", 873), c("12:00", 1024), c("13:00", 1083), c("14:00", 1085),
#           c("15:00", 1079), c("16:00", 1082), c("17:00", 1080), c("18:00", 1081), c("19:00", 1085),
#           c("20:00", 1078), c("21:00", 1089), c("22:00", 1084), c("23:00", 1085)
#         )
#       ),
#       Coal = list(
#         name = "Coal",
#         color = "green",
#         data = list(
#           c("00:00", 2798), c("01:00", 2605), c("02:00", 2371), c("03:00", 2359), c("04:00", 2357),
#           c("05:00", 2399), c("06:00", 2486), c("07:00", 3528), c("08:00", 3912), c("09:00", 3834),
#           c("10:00", 3748), c("11:00", 3743), c("12:00", 3756), c("13:00", 3667), c("14:00", 3662),
#           c("15:00", 3677), c("16:00", 3714), c("17:00", 3737), c("18:00", 3823), c("19:00", 3741),
#           c("20:00", 3742), c("21:00", 3743), c("22:00", 3599), c("23:00", 3025)
#         )
#       ),
#       FO = list(
#         name = "FO",
#         color = "red",
#         data = list(
#           c("00:00", 0), c("01:00", 0), c("02:00", 0), c("03:00", 0), c("04:00", 0), c("05:00", 34),
#           c("06:00", 339), c("07:00", 494), c("08:00", 594), c("09:00", 770), c("10:00", 1023),
#           c("11:00", 1082), c("12:00", 1093), c("13:00", 1107), c("14:00", 1068), c("15:00", 1080),
#           c("16:00", 1095), c("17:00", 1099), c("18:00", 1088), c("19:00", 890), c("20:00", 682),
#           c("21:00", 600), c("22:00", 0), c("23:00", 0)
#         )
#       ),
#       RLNG = list(
#         name = "RLNG",
#         color = "blue",
#         data = list(
#           c("00:00", 2171), c("01:00", 2013), c("02:00", 1894), c("03:00", 2063), c("04:00", 1986),
#           c("05:00", 1977), c("06:00", 1925), c("07:00", 2584), c("08:00", 2242), c("09:00", 2949),
#           c("10:00", 2482), c("11:00", 2169), c("12:00", 2375), c("13:00", 2360), c("14:00", 2264),
#           c("15:00", 2420), c("16:00", 2417), c("17:00", 2264), c("18:00", 2022), c("19:00", 2164),
#           c("20:00", 2267), c("21:00", 2522), c("22:00", 2318), c("23:00", 2169)
#         )
#       )
#     ),
#     GENCOS = list(
#       Gas = list(
#         name = "Gas",
#         color = "yellow",
#         data = list(
#           c("00:00", 234), c("01:00", 233), c("02:00", 193), c("03:00", 191), c("04:00", 191),
#           c("05:00", 191), c("06:00", 231), c("07:00", 233), c("08:00", 233), c("09:00", 233),
#           c("10:00", 233), c("11:00", 234), c("12:00", 234), c("13:00", 234), c("14:00", 234),
#           c("15:00", 234), c("16:00", 234), c("17:00", 234), c("18:00", 233), c("19:00", 233),
#           c("20:00", 233), c("21:00", 234), c("22:00", 234), c("23:00", 234)
#         )
#       ),
#       Coal = list(
#         name = "Coal",
#         color = "green",
#         data = list(
#           c("00:00", 234), c("01:00", 233), c("02:00", 193), c("03:00", 191), c("04:00", 191),
#           c("05:00", 191), c("06:00", 231), c("07:00", 233), c("08:00", 233), c("09:00", 233),
#           c("10:00", 233), c("11:00", 234), c("12:00", 234), c("13:00", 234), c("14:00", 234),
#           c("15:00", 234), c("16:00", 234), c("17:00", 234), c("18:00", 233), c("19:00", 233),
#           c("20:00", 233), c("21:00", 234), c("22:00", 234), c("23:00", 234)
#         )
#       ),
#       RLNG = list(
#         name = "RLNG",
#         color = "blue",
#         data = list(
#           c("00:00", 234), c("01:00", 233), c("02:00", 193), c("03:00", 191), c("04:00", 191),
#           c("05:00", 191), c("06:00", 231), c("07:00", 233), c("08:00", 233), c("09:00", 233),
#           c("10:00", 233), c("11:00", 234), c("12:00", 234), c("13:00", 234), c("14:00", 234),
#           c("15:00", 234), c("16:00", 234), c("17:00", 234), c("18:00", 233), c("19:00", 233),
#           c("20:00", 233), c("21:00", 234), c("22:00", 234), c("23:00", 234)
#         )
#       )
#     ),
#     Nuclear = list(
#       Nuclear = list(
#         name = "Nuclear",
#         color = "blue",
#         data = list(
#           c("00:00", 2015), c("01:00", 2221), c("02:00", 2221), c("03:00", 2222), c("04:00", 2220),
#           c("05:00", 2223), c("06:00", 2223), c("07:00", 2237), c("08:00", 2234), c("09:00", 2229),
#           c("10:00", 2227), c("11:00", 2227), c("12:00", 2228), c("13:00", 2225), c("14:00", 2226),
#           c("15:00", 2225), c("16:00", 2226), c("17:00", 2224), c("18:00", 2225), c("19:00", 2228),
#           c("20:00", 2227), c("21:00", 2226), c("22:00", 2224), c("23:00", 2089)
#         )
#       )
#     ),
#     privateSum = 3700,
#     publicSum = 33277,
#     hydroSum = 36977,
#     nuclearSum = 53072,
#     bagasseSum = 4430,
#     windSum = 5824,
#     solarSum = 1091,
#     renewableSum = 11345,
#     ippsrlngSum = 54017,
#     ippsgasSum = 24192,
#     ippscoalSum = 80026,
#     ippsfoSum = 14138,
#     ippsSum = 172373,
#     gencosrlngSum = 5435,
#     gencoscoalSum = 5435,
#     gencosgasSum = 5435,
#     gencosSum = 16305,
#     thermalSum = 188678
#   ),
#   DataForParentOne = list(
#     DataParentLayerHydro = list(
#       `2022-03-02` = list(
#         name = "2022-03-02",
#         y = 36977,
#         drilldown = TRUE
#       )
#     ),
#     DataParentLayerRenewable = list(
#       `2022-03-02` = list(
#         name = "2022-03-02",
#         y = 11345,
#         drilldown = TRUE
#       )
#     ),
#     DataParentLayerNuclear = list(
#       `2022-03-02` = list(
#         name = "2022-03-02",
#         y = 53072,
#         drilldown = TRUE
#       )
#     ),
#     DataParentLayerThermal = list(
#       `2022-03-02` = list(
#         name = "2022-03-02",
#         y = 188678,
#         drilldown = TRUE
#       )
#     ),
#     DataParentLayerIPPS = list(
#       `2022-03-02` = list(
#         name = "2022-03-02",
#         y = 172373,
#         drilldown = TRUE
#       )
#     ),
#     DataParentLayerGENCOS = list(
#       `2022-03-02` = list(
#         name = "2022-03-02",
#         y = 16305,
#         drilldown = TRUE
#       )
#     )
#   )
# )
# 
# # Create the R highchart
# highchart() %>%
#   hc_chart(type = "column") %>%
#   hc_title(text = "Energy Generation Data") %>%
#   hc_xAxis(
#     type = "category",
#     labels = list(rotation = -45, align = "right")
#   ) %>%
#   hc_tooltip(shared = TRUE) %>%
#   hc_plotOptions(
#     series = list(
#       stacking = "normal",
#       dataLabels = list(enabled = TRUE)
#     )
#   ) %>%
#   hc_series(list(
#     name = "Hydro",
#     data = data$`2022-03-02`$Hydro$Private$data,
#     stack = "Hydro",
#     color = data$`2022-03-02`$Hydro$Private$color
#   ), list(
#     name = "Public",
#     data = data$`2022-03-02`$Hydro$Public$data,
#     stack = "Hydro",
#     color = data$`2022-03-02`$Hydro$Public$color
#   ), list(
#     name = "Solar",
#     data = data$`2022-03-02`$Renewable$Solar$data,
#     stack = "Renewable",
#     color = data$`2022-03-02`$Renewable$Solar$color
#   ), list(
#     name = "Wind",
#     data = data$`2022-03-02`$Renewable$Wind$data,
#     stack = "Renewable",
#     color = data$`2022-03-02`$Renewable$Wind$color
#   ), list(
#     name = "Bagasse",
#     data = data$`2022-03-02`$Renewable$Bagasse$data,
#     stack = "Renewable",
#     color = data$`2022-03-02`$Renewable$Bagasse$color
#   ), list(
#     name = "Nuclear",
#     data = data$`2022-03-02`$Nuclear$Nuclear$data,
#     stack = "Nuclear",
#     color = data$`2022-03-02`$Nuclear$Nuclear$color
#   ), list(
#     name = "Gas",
#     data = data$`2022-03-02`$IPPS$Gas$data,
#     stack = "IPPS",
#     color = data$`2022-03-02`$IPPS$Gas$color
#   ), list(
#     name = "Coal",
#     data = data$`2022-03-02`$IPPS$Coal$data,
#     stack = "IPPS",
#     color = data$`2022-03-02`$IPPS$Coal$color
#   ), list(
#     name = "FO",
#     data = data$`2022-03-02`$IPPS$FO$data,
#     stack = "IPPS",
#     color = data$`2022-03-02`$IPPS$FO$color
#   ), list(
#     name = "Gas",
#     data = data$`2022-03-02`$GENCOS$Gas$data,
#     stack = "GENCOS",
#     color = data$`2022-03-02`$GENCOS$Gas$color
#   ), list(
#     name = "Coal",
#     data = data$`2022-03-02`$GENCOS$Coal$data,
#     stack = "GENCOS",
#     color = data$`2022-03-02`$GENCOS$Coal$color
#   ), list(
#     name = "RLNG",
#     data = data$`2022-03-02`$GENCOS$RLNG$data,
#     stack = "GENCOS",
#     color = data$`2022-03-02`$GENCOS$RLNG$color
#   )) %>%
#   hc_drilldown(
#     allowPointDrilldown = TRUE,
#     series = list(
#       list(
#         id = "Hydro",
#         data = data$DataForParentOne$DataParentLayerHydro
#       ),
#       list(
#         id = "Renewable",
#         data = data$DataForParentOne$DataParentLayerRenewable
#       ),
#       list(
#         id = "Nuclear",
#         data = data$DataForParentOne$DataParentLayerNuclear
#       ),
#       list(
#         id = "Thermal",
#         data = data$DataForParentOne$DataParentLayerThermal
#       ),
#       list(
#         id = "IPPS",
#         data = data$DataForParentOne$DataParentLayerIPPS
#       ),
#       list(
#         id = "GENCOS",
#         data = data$DataForParentOne$DataParentLayerGENCOS
#       )
#     )
#   )
