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

introduction.underlyings <-
    "Given time series of daily precipitation (RAIN, mm), minimum (AIRTMIN, ºC) and maximum
temperature (AIRTMAX, ºC), and (as an option) reference
evapotranspiration (ET0, mm), this tool evaluates the number of occurrences per
year of extreme temperatures (heat and cold shocks) and extreme aridity.
The latter is based on the use of the Agricultural Reference Index for Drought (ARID, Woli et al., 2012), which is defined as the ratio between actual transpiration
(T) and the reference evapotranspiration (ET0):"

introduction.equation.ARID <- withMathJax("$$\\mathit{ARID} = 1 - \\frac{T}{\\mathit{ET0}}$$")

introduction.ARID <-
    "and ranges from 0 (no water deficit) to 1 (most extreme aridity, T = 0)."

introduction.indicators1 <-
    "By providing appropriate thresholds in the tab 'Occurrence of Extreme Conditions',
the following criteria can be specified by the user:"
introduction.indicators2 <-
    sprintf("- Heat shocks: $$\\mathit{AIRTMAX} > \\mathit{AIRTMAX}_\\mathit{crit} \\ \\textrm{where } \\ \\mathit{AIRTMAX}_\\mathit{crit} \\in [%.1f, %.1f] \\ ^{\\circ}\\textrm{C}$$",
            AIRTMAXcr.l, AIRTMAXcr.h)
introduction.indicators3 <-
    sprintf("- Cold shocks: $$\\mathit{AIRTMIN} < \\mathit{AIRTMIN}_\\mathit{crit} \\ \\textrm{where } \\ \\mathit{AIRTMIN}_\\mathit{crit} \\in [%.1f, %.1f] \\ ^{\\circ}\\textrm{C}$$",
            AIRTMINcr.l, AIRTMINcr.h)
introduction.indicators4 <-
    sprintf("- Extreme aridity: $$\\mathit{ARID} > \\mathit{ARID}_\\mathit{crit} \\ \\textrm{where } \\ \\mathit{ARID}_\\mathit{crit} \\in [%.1f, %.1f]$$",
            ARIDcr.l, ARIDcr.h)
introduction.indicators5 <-
    "The criteria are evaluated with respect to the time of the year
('Start Date' – 'End Date') defined by the user in the same tab."

introduction.inputs <-
    "Input data should be provided in a CSV-file, using the same format as
required by the crop modelling platform BIOMA"

introduction.ET0 <-
    "If ET0 is not provided as input, it will automatically be estimated based on
Hargreaves' equation (Hargreaves and Samani, 1982).
In this case, the latitude of the location of interest should be specified for
the estimation of incoming solar radiation at the top of the atmosphere.
Alternatively, solar radiation and ET0 can be evaluated using the tools
developed by Donatelli et al. (2006a and 2006b), which are implemented in BIOMA."

introduction.SM <-
    "To calculate the soil moisture budget (mm) and ARID (-) go to tab 'Soil Moisture Budget Computation'."

introduction.ClimIndices <-
    "This tool can be used as a complement to ClimIndices, a software component for
the evaluation of agroclimatic indicators developed by Confalonieri et al. (2010)
also available through the BIOMA platform."

introduction.acknowledgements <-
    "This web application was developed by Tommy Klein and Pierluigi Calanca
(Agroscope, Institute for Sustainability Sciences ISS) in the context of the
MODEXTREME project (Modelling vegetation response to EXTREMe Events, http://modextreme.org),
which has received funding from the European Community’s Seven Framework Programme-FP7
(KBBE.2013.1.4-09) under Grant Agreement No. 613817.2013-2016. "

introduction.main.reference1 <-
    "Confalonieri, R., Bellocchi, G., Donatelli, M., 2010. A software component to compute agro-meteorological indicators. Environmental Modelling & Software 25, 1485-1486"
introduction.main.reference2 <-
    "Donatelli, M., Carlini, L., Bellocchi, G., 2006a. A software component for estimating solar radiation. Environmental Modelling & Software 21, 411-416"
introduction.main.reference3 <-
    "Donatelli, M., Bellocchi, G., Carlini, L., 2006b. Sharing knowledge via software components: Models on reference evapotranspiration. European Journal of Agronomy 24 (2006) 186-192"
introduction.main.reference4 <-
    "Hargreaves, G.H., Samani, Z.A., 1982. Estimating potential evapotranspiration. Journal of the Irrigation and Drainage Division 108, 225-230"
introduction.main.reference5 <-
    "Woli, P., Jones, J.W., Ingram, K.T., Fraisse, C.W., 2012. Agricultural Reference Index for Drought (ARID). Agronomy Journal 104, 287-300"
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
                         p(introduction.underlyings),
                         p(introduction.equation.ARID),
                         p(introduction.ARID),
                         br(),
                         p(introduction.indicators1),
                         p(introduction.indicators2),
                         p(introduction.indicators3),
                         p(introduction.indicators4),
                         p(introduction.indicators5),
                         br(),
                         p(introduction.inputs,
                           "(a sample file can be downloaded", a("here", href = "TestLocation.csv"),
                           "). To upload the data go to tab 'Data Upload'."),
                         br(),
                         p(introduction.ET0),
                         br(),
                         p(introduction.SM),
                         br(),
                         p(introduction.ClimIndices),
                         br(),
                         h4("Acknowledgements"),
                         p(introduction.acknowledgements),
                         br(),
                         h4("References"),
                         p(introduction.main.reference1),
                         p(introduction.main.reference2),
                         p(introduction.main.reference3),
                         p(introduction.main.reference4),
                         p(introduction.main.reference5)),
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