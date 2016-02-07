library(shiny)
library(leaflet)
library(dygraphs)
library(lubridate)

# Define UI
shinyUI(fluidPage(
    br(),
    sidebarLayout(
        sidebarPanel(width = 3,
            titlePanel("EOBS data extractor"),
            tags$head(tags$style(type="text/css",
                                 "#loadmessage {
                                 position: fixed;
                                 top: 0px;
                                 left: 0px;
                                 width: 100%;
                                 padding: 5px 0px 5px 0px;
                                 text-align: center;
                                 font-weight: bold;
                                 font-size: 100%;
                                 color: #000000;
                                 background-color: #CEE3F6;
                                 z-index: 105; }")),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div("Loading...", id = "loadmessage")),
            br(),
            br(),
            dateRangeInput("DATES",
                           "Select dates",
                           start = max(dates.all) - days(365),
                           end = max(dates.all),
                           min = min(dates.all),
                           max = max(dates.all)),
            br(),
            selectInput("PARAM",
                        "Weather Parameter",
                        parameters,
                        selected = "tx"),
            br(),
            selectInput("MASK",
                        "Crop Mask",
                        c("none", "maize", "wheat"),
                        selected = "none"),
            br(),
            #numericInput("LAT",
            #             "Latitude",
            #             68.12),
            #numericInput("LON",
            #             "Longitude",
            #             33.6),
            uiOutput("ui.button")
        ),
        mainPanel(width = 9,
            leafletOutput("mymap", "100%", 500)
        )
    ),
    br(),
    hr(),
    br(),
    dygraphOutput("ts")
))

# LAT <- 68.12
# LON <- 33.6
# PARAM <- "tx"
#
# DATES <- (ymd("2010-01-01") + days(0:365)) %>% as.Date