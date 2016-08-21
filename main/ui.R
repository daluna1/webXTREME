#----------------------------------------------------------------------------------------
# Author: Tommy Klein
# Affiliation: Agroscope, Institute for Sustainability Sciences ISS (Zurich, Switzerland)
# Date: July 2015
# Project: webXTREME
#----------------------------------------------------------------------------------------

library(shiny)
library(shinyBS)
library(DT)
library(dygraphs)
library(shinythemes)
library(knitr)
library(lubridate)

# Generate potential start dates (2015 is arbitrary)
dates.start <- ymd("2015-01-01") + days(0:364)
dates.start.md <- format(dates.start, "%B %d")  # format should be month-day

### Introduction text to be displayed on 'About this App' tab
app.name.short <- "webXTREME"
app.name.long <- "web-based tool for the assessment of extreme years"

introduction.panel1 <- sprintf("Welcome to %s!", app.name.short)
introduction.panel2 <- sprintf("(%s)", app.name.long)
introduction.panel3 <- "This tab provides a short user guide how to use this web application,
an overview of the core functionalities, and key references related to the main computations"

rmarkdown::render("./www/tutorial.Rmd")
###

### UI starts here
shinyUI(fluidPage(
    theme = shinytheme("united"),
    img(src = "logo.png", height = 70, width = 330),  # logo
    titlePanel(app.name.short),  # app name
    sidebarLayout(
        sidebarPanel(width = 3,
                     conditionalPanel(condition="input.conditionedPanels==1",
                                      strong(introduction.panel1),
                                      p(introduction.panel2),
                                      br(),
                                      p(introduction.panel3)),
                     conditionalPanel(condition="input.conditionedPanels==2",
                                      br(),
                                      fileInput("DATAFILE", "Upload Weather Data"),
                                      br(),
                                      helpText("Weather data need to be saved in a CSV-file (one record per row) with
                                               column names 'DATE' (YYYY-MM-DD, DD/MM/YYYY or MM/DD/YYY), 'RAIN', 'AIRTMAX', 'AIRTMIN' and 'ET0' (optional)")
                                      #helpText("Data to be used with Bioma should work flawlessly with this app without any modifications")
                                      ),
                     conditionalPanel(condition="input.conditionedPanels==3",
                                      numericInput("LATITUDE", "Latitude", 0.,
                                                   min = -90, max = 90),
                                      bsTooltip("LATITUDE", "Range: [-90, 90] / Used to estimate ET0 if not provided",
                                                "right", options = list(container = "body")),
                                      numericInput("EXTRACTION_DEPTH",
                                                   label = "Extraction Depth",
                                                   value = 1.2,
                                                   min = 0.5,
                                                   max = 2.,
                                                   step = 0.1),
                                      bsTooltip("EXTRACTION_DEPTH", "Units: m / Range: [0.5, 2m] / Used to compute soil moisture",
                                                "right", options = list(container = "body")),
                                      selectInput("TEXTURE_CLASS",
                                                  label = "Soil Texture Class",
                                                  choices = names(soil.params),
                                                  selected = names(soil.params)[1]),
                                      bsTooltip("TEXTURE_CLASS", "Used to match soil parameters (e.g., water holding capacity, drainage factor, curve number) to compute soil moisture",
                                                "right", options = list(container = "body")),
                                      br(),
                                      actionButton("button.sm", strong("Run Soil Moisture Model")),
                                      br(),
                                      br(),
                                      helpText("This tab allows you to compute the amount of soil moisture (mm) in the root zone based on a simple bucket model;
                                               note that indicators related to aridity only become available after the soil moisture budget has been computed"),
                                      helpText("The latitude of the location is used to calculate ET0 (required component of soil moisture budget),
while the extraction depth and the soil texture class are used to calculate daily soil moisture")),
                     conditionalPanel(condition="input.conditionedPanels==4",
                                      selectInput("DATE_START",
                                                  label = "Start Date",
                                                  choices = dates.start.md,
                                                  selected = dates.start.md[91]),
                                      uiOutput("ui.end.date"),
                                      helpText("Please provide the desired period (starting and end dates) during which the criterion is evaluated"),
                                      br(),
                                      uiOutput("ui.underlying"),
                                      uiOutput("ui.threshold"),
                                      helpText(textOutput("threshold.help")),
                                      helpText(textOutput("ui.criterion")),
                                      br(),
                                      actionButton("button.indicator", strong("Compute Indicator")))),

        mainPanel(
            tabsetPanel(
                tabPanel(title = "About this App",
                         value = 1,
                         br(),
                         includeHTML('./www/tutorial.html')),
                tabPanel(title = "Data Upload",
                         value = 2,
                         DT::dataTableOutput("TableData")),
                tabPanel(title = "Soil Moisture Budget Computation",
                         value = 3,
                         DT::dataTableOutput("TableSM")),
                tabPanel(title = "Occurrence of Extreme Conditions",
                         value = 4,
                         br(),
                         h3(textOutput("plot.title")),
                         uiOutput("ui.button"),
                         br(),
                         dygraphOutput("plot", width = "100%", height = "500px"),
                         helpText(textOutput("plot.help"))),
                id = "conditionedPanels"))
    )
))