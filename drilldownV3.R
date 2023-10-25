# Install and load the htmlwidgets and htmltools packages
install.packages(c("htmlwidgets", "htmltools", "jsonlite"))
library(htmlwidgets)
library(htmltools)
# Load the jsonlite package if not already loaded
library(jsonlite)

# Your data
final_drill_down_JSON <- list(
  "2022-03-02" = list(
    Hydro = list(
      Private = list(
        name = "Private",
        color = "red",
        data = list(
          c("00:00", 125), c("01:00", 135), c("02:00", 133), c("03:00", 135), c("04:00", 133),
          c("05:00", 133), c("06:00", 134), c("07:00", 136), c("08:00", 134), c("09:00", 135),
          c("10:00", 135), c("11:00", 140), c("12:00", 189), c("13:00", 180), c("14:00", 180),
          c("15:00", 180), c("16:00", 182), c("17:00", 196), c("18:00", 186), c("19:00", 180),
          c("20:00", 181), c("21:00", 181), c("22:00", 133), c("23:00", 124)
        )
      ),
      Public = list(
        name = "Public",
        color = "blue",
        data = list(
          c("00:00", 1059), c("01:00", 1111), c("02:00", 1071), c("03:00", 1070), c("04:00", 955),
          c("05:00", 986), c("06:00", 1806), c("07:00", 1364), c("08:00", 1369), c("09:00", 1789),
          c("10:00", 1708), c("11:00", 1879), c("12:00", 1558), c("13:00", 1046), c("14:00", 1005),
          c("15:00", 931), c("16:00", 959), c("17:00", 1201), c("18:00", 1813), c("19:00", 2319),
          c("20:00", 2006), c("21:00", 1614), c("22:00", 1439), c("23:00", 1219)
        )
      )
    ),
    Renewable = list(
      Solar = list(
        name = "Solar",
        color = "yellow",
        data = list(
          c("00:00", 0), c("01:00", 0), c("02:00", 0), c("03:00", 0), c("04:00", 0), c("05:00", 0),
          c("06:00", 0), c("07:00", 8), c("08:00", 86), c("09:00", 151), c("10:00", 136), c("11:00", 59),
          c("12:00", 86), c("13:00", 131), c("14:00", 139), c("15:00", 106), c("16:00", 146),
          c("17:00", 43), c("18:00", 0), c("19:00", 0), c("20:00", 0), c("21:00", 0), c("22:00", 0),
          c("23:00", 0)
        )
      ),
      Wind = list(
        name = "Wind",
        color = "blue",
        data = list(
          c("00:00", 564), c("01:00", 289), c("02:00", 317), c("03:00", 285), c("04:00", 226),
          c("05:00", 101), c("06:00", 376), c("07:00", 150), c("08:00", 78), c("09:00", 93),
          c("10:00", 232), c("11:00", 71), c("12:00", 50), c("13:00", 50), c("14:00", 78),
          c("15:00", 92), c("16:00", 123), c("17:00", 182), c("18:00", 364), c("19:00", 522),
          c("20:00", 331), c("21:00", 337), c("22:00", 436), c("23:00", 477)
        )
      ),
      Bagasse = list(
        name = "Bagasse",
        color = "green",
        data = list(
          c("00:00", 180), c("01:00", 187), c("02:00", 189), c("03:00", 185), c("04:00", 188),
          c("05:00", 182), c("06:00", 187), c("07:00", 182), c("08:00", 191), c("09:00", 178),
          c("10:00", 186), c("11:00", 185), c("12:00", 182), c("13:00", 186), c("14:00", 187),
          c("15:00", 185), c("16:00", 185), c("17:00", 186), c("18:00", 185), c("19:00", 187),
          c("20:00", 188), c("21:00", 184), c("22:00", 173), c("23:00", 182)
        )
      )
    ),
    IPPS = list(
      Gas = list(
        name = "Gas",
        color = "yellow",
        data = list(
          c("00:00", 1091), c("01:00", 1080), c("02:00", 811), c("03:00", 812), c("04:00", 813),
          c("05:00", 910), c("06:00", 1064), c("07:00", 1051), c("08:00", 1007), c("09:00", 875),
          c("10:00", 870), c("11:00", 873), c("12:00", 1024), c("13:00", 1083), c("14:00", 1085),
          c("15:00", 1079), c("16:00", 1082), c("17:00", 1080), c("18:00", 1081), c("19:00", 1085),
          c("20:00", 1078), c("21:00", 1089), c("22:00", 1084), c("23:00", 1085)
        )
      ),
      Coal = list(
        name = "Coal",
        color = "green",
        data = list(
          c("00:00", 2798), c("01:00", 2605), c("02:00", 2371), c("03:00", 2359), c("04:00", 2357),
          c("05:00", 2399), c("06:00", 2486), c("07:00", 3528), c("08:00", 3912), c("09:00", 3834),
          c("10:00", 3748), c("11:00", 3743), c("12:00", 3756), c("13:00", 3667), c("14:00", 3662),
          c("15:00", 3677), c("16:00", 3714), c("17:00", 3737), c("18:00", 3823), c("19:00", 3741),
          c("20:00", 3742), c("21:00", 3743), c("22:00", 3599), c("23:00", 3025)
        )
      ),
      FO = list(
        name = "FO",
        color = "red",
        data = list(
          c("00:00", 0), c("01:00", 0), c("02:00", 0), c("03:00", 0), c("04:00", 0), c("05:00", 34),
          c("06:00", 339), c("07:00", 494), c("08:00", 594), c("09:00", 770), c("10:00", 1023),
          c("11:00", 1082), c("12:00", 1093), c("13:00", 1107), c("14:00", 1068), c("15:00", 1080),
          c("16:00", 1095), c("17:00", 1099), c("18:00", 1088), c("19:00", 890), c("20:00", 682),
          c("21:00", 600), c("22:00", 0), c("23:00", 0)
        )
      ),
      RLNG = list(
        name = "RLNG",
        color = "blue",
        data = list(
          c("00:00", 2171), c("01:00", 2013), c("02:00", 1894), c("03:00", 2063), c("04:00", 1986),
          c("05:00", 1977), c("06:00", 1925), c("07:00", 2584), c("08:00", 2242), c("09:00", 2949),
          c("10:00", 2482), c("11:00", 2169), c("12:00", 2375), c("13:00", 2360), c("14:00", 2264),
          c("15:00", 2420), c("16:00", 2417), c("17:00", 2264), c("18:00", 2022), c("19:00", 2164),
          c("20:00", 2267), c("21:00", 2522), c("22:00", 2318), c("23:00", 2169)
        )
      )
    ),
    GENCOS = list(
      Gas = list(
        name = "Gas",
        color = "yellow",
        data = list(
          c("00:00", 234), c("01:00", 233), c("02:00", 193), c("03:00", 191), c("04:00", 191),
          c("05:00", 191), c("06:00", 231), c("07:00", 233), c("08:00", 233), c("09:00", 233),
          c("10:00", 233), c("11:00", 234), c("12:00", 234), c("13:00", 234), c("14:00", 234),
          c("15:00", 234), c("16:00", 234), c("17:00", 234), c("18:00", 233), c("19:00", 233),
          c("20:00", 233), c("21:00", 234), c("22:00", 234), c("23:00", 234)
        )
      ),
      Coal = list(
        name = "Coal",
        color = "green",
        data = list(
          c("00:00", 234), c("01:00", 233), c("02:00", 193), c("03:00", 191), c("04:00", 191),
          c("05:00", 191), c("06:00", 231), c("07:00", 233), c("08:00", 233), c("09:00", 233),
          c("10:00", 233), c("11:00", 234), c("12:00", 234), c("13:00", 234), c("14:00", 234),
          c("15:00", 234), c("16:00", 234), c("17:00", 234), c("18:00", 233), c("19:00", 233),
          c("20:00", 233), c("21:00", 234), c("22:00", 234), c("23:00", 234)
        )
      ),
      RLNG = list(
        name = "RLNG",
        color = "blue",
        data = list(
          c("00:00", 234), c("01:00", 233), c("02:00", 193), c("03:00", 191), c("04:00", 191),
          c("05:00", 191), c("06:00", 231), c("07:00", 233), c("08:00", 233), c("09:00", 233),
          c("10:00", 233), c("11:00", 234), c("12:00", 234), c("13:00", 234), c("14:00", 234),
          c("15:00", 234), c("16:00", 234), c("17:00", 234), c("18:00", 233), c("19:00", 233),
          c("20:00", 233), c("21:00", 234), c("22:00", 234), c("23:00", 234)
        )
      )
    ),
    Nuclear = list(
      Nuclear = list(
        name = "Nuclear",
        color = "blue",
        data = list(
          c("00:00", 2015), c("01:00", 2221), c("02:00", 2221), c("03:00", 2222), c("04:00", 2220),
          c("05:00", 2223), c("06:00", 2223), c("07:00", 2237), c("08:00", 2234), c("09:00", 2229),
          c("10:00", 2227), c("11:00", 2227), c("12:00", 2228), c("13:00", 2225), c("14:00", 2226),
          c("15:00", 2225), c("16:00", 2226), c("17:00", 2224), c("18:00", 2225), c("19:00", 2228),
          c("20:00", 2227), c("21:00", 2226), c("22:00", 2224), c("23:00", 2089)
        )
      )
    ),
    privateSum = 3700,
    publicSum = 33277,
    hydroSum = 36977,
    nuclearSum = 53072,
    bagasseSum = 4430,
    windSum = 5824,
    solarSum = 1091,
    renewableSum = 11345,
    ippsrlngSum = 54017,
    ippsgasSum = 24192,
    ippscoalSum = 80026,
    ippsfoSum = 14138,
    ippsSum = 172373,
    gencosrlngSum = 5435,
    gencoscoalSum = 5435,
    gencosgasSum = 5435,
    gencosSum = 16305,
    thermalSum = 188678
  ),
  DataForParentOne = list(
    DataParentLayerHydro = list(
      `2022-03-02` = list(
        name = "2022-03-02",
        y = 36977,
        drilldown = TRUE
      )
    ),
    DataParentLayerRenewable = list(
      `2022-03-02` = list(
        name = "2022-03-02",
        y = 11345,
        drilldown = TRUE
      )
    ),
    DataParentLayerNuclear = list(
      `2022-03-02` = list(
        name = "2022-03-02",
        y = 53072,
        drilldown = TRUE
      )
    ),
    DataParentLayerThermal = list(
      `2022-03-02` = list(
        name = "2022-03-02",
        y = 188678,
        drilldown = TRUE
      )
    ),
    DataParentLayerIPPS = list(
      `2022-03-02` = list(
        name = "2022-03-02",
        y = 172373,
        drilldown = TRUE
      )
    ),
    DataParentLayerGENCOS = list(
      `2022-03-02` = list(
        name = "2022-03-02",
        y = 16305,
        drilldown = TRUE
      )
    )
  )
)


# Convert your JSON data to a JSON string
final_drill_down_JSON <- toJSON(final_drill_down_JSON, auto_unbox = TRUE)
# Add the Highcharts CDNs
highcharts_cdns <- c(
  "https://code.highcharts.com/highcharts.js",
  "https://code.highcharts.com/modules/data.js",
  "https://code.highcharts.com/modules/drilldown.js",
  "https://code.highcharts.com/modules/exporting.js",
  "https://code.highcharts.com/modules/export-data.js",
  "https://code.highcharts.com/modules/accessibility.js"
)
# Create a simple HTML widget with a Highcharts chart
widget <- htmlwidgets::createWidget(
  name = "highchart",
  x = list(data = rnorm(100)),
  width = 400,
  height = 300,
  package = "highchart",
  elementId = "container",  # Set the element ID to match your JavaScript code
  sizingPolicy = htmlwidgets::sizingPolicy(),
  dependencies = highcharts_cdns
)

# Add some custom JavaScript code to the widget
widget <- htmlwidgets::onRender(
  widget,
  jsCode = sprintf("var javascriptArray =  %s;  // Pass the R JSON array here
        // Access DataParentLayerHydro object
        var dataParentLayerHydro = javascriptArray.DataForParentOne.DataParentLayerHydro;
        var dataParentLayerNuclear = javascriptArray.DataForParentOne.DataParentLayerNuclear;
        var dataParentLayerThermal = javascriptArray.DataForParentOne.DataParentLayerThermal;
        var dataParentLayerRenewable = javascriptArray.DataForParentOne.DataParentLayerRenewable;
        var dataParentLayerIPPS = javascriptArray.DataForParentOne.DataParentLayerIPPS;
        var dataParentLayerGENCOS = javascriptArray.DataForParentOne.DataParentLayerGENCOS;
        // Initialize an array to store the desired output
        var desiredOutputHydro = [];
        var desiredOutputRenewable = [];
        var desiredOutputNuclear = [];
        var desiredOutputThermal = [];
        var desiredOutputIPPS = [];
        var desiredOutputGENCOS = [];
        // Loop through the data and push each entry to the desired output array
        for (var date in dataParentLayerHydro) {
            if (dataParentLayerHydro.hasOwnProperty(date)) {
                desiredOutputHydro.push(dataParentLayerHydro[date]);
            }
        }
        // Loop through the data and push each entry to the desired output array
        for (var date in dataParentLayerRenewable) {
            if (dataParentLayerRenewable.hasOwnProperty(date)) {
                desiredOutputRenewable.push(dataParentLayerRenewable[date]);
            }
        }
        // Loop through the data and push each entry to the desired output array
        for (var date in dataParentLayerNuclear) {
            if (dataParentLayerNuclear.hasOwnProperty(date)) {
                desiredOutputNuclear.push(dataParentLayerNuclear[date]);
            }
        }
        // Loop through the data and push each entry to the desired output array
        for (var date in dataParentLayerThermal) {
            if (dataParentLayerThermal.hasOwnProperty(date)) {
                desiredOutputThermal.push(dataParentLayerThermal[date]);
            }
        }
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
        // // Now, desiredOutput contains the desired data
        // console.log(desiredOutputHydro);
        // console.log(desiredOutputHydro);
        // console.log(desiredOutputHydro);
        // console.log(desiredOutputHydro);
        // console.log(desiredOutputHydro);

        Highcharts.Tick.prototype.drillable = function() {};
        Highcharts.chart('container', {
            chart: {
                type: 'column',

                events: {
                    drilldown: function(e) {
                        if (!e.seriesOptions) {
                            // if (e.point.name == '2022-03-02') {
                            //     alert(e.point.name);
                            // }
                            var specificDate = javascriptArray[e.point.name]
                            console.log(specificDate);
                            console.log(specificDate.GENCOS);
                            console.log(specificDate.IPPS);
                            console.log(specificDate.Nuclear);
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

                            // if (e.point.name == '2022-03-02') {
                            //     console.log(e.point.name)
                            //     console.log(e); // Log the entire e object to the console
                            // } else {
                            //     console.log(e.point.name + '_' + 'ipps')
                            // }


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

                        }
                    }
                } //event end
            },


            xAxis: {

                type: 'category'
            },
            tooltip: {
                enabled: true
            },

            plotOptions: {
                series: {

                    stacking: 'normal'
                }
            },

            series: [{
                    color: 'blue',
                    name: 'Hydro',
                    data: desiredOutputHydro,
                    stack: 'move',
                    // drilldown: true
                },

                {
                    color: 'green',
                    name: 'Renewables',
                    data: desiredOutputRenewable,
                    stack: 'move',
                    // drilldown: true
                },

                {
                    color: 'red',
                    name: 'Thermal',
                    data: desiredOutputThermal,
                    stack: 'move',
                    drilldown: true
                },

                {

                    color: 'yellow',
                    name: 'Nuclear',
                    data: desiredOutputNuclear,
                    stack: 'move'
                    // drilldown: true
                }
            ]

        });", htmlwidgets::JS(final_drill_down_JSON))
)

# Render the widget in RStudio Viewer or a web browser
widget
