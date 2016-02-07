#----------------------------------------------------------------------------------------
# Author: Tommy Klein
# Affiliation: Agroscope, Institute for Sustainability Sciences ISS (Zurich, Switzerland)
# Date: July 2015
# Project: webEXTREME
#----------------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(shiny)  # developer version
library(shinyBS)
library(tidyr)
library(lubridate)
library(foreach)
library(DT)  # developer version (install htmlwigdet developer version)
library(dygraphs)
library(xts)

source("./attachments/ET0.R")  # functions to compute the reference ET
source("./attachments/SM.R")  # functions to compute soil moisture

# Predefined underlyings (weather parameter and operator)
underlyings.mapping <- list("Heat" = list("variable" = "AIRTMAX",
                                          "operator" = ">"),
                            "Cold" = list("variable" = "AIRTMIN",
                                          "operator" = "<"),
                            "Aridity" = list("variable" = "ARID",
                                             "operator" = ">"))

shinyServer(function(input, output, session) {
    makeReactiveBinding('unit.and.range')
    makeReactiveBinding('criterion')
    makeReactiveBinding('plot.title')
    makeReactiveBinding('location')
    makeReactiveBinding('dat.all')
    makeReactiveBinding('calculate.ET0')

    getData <- reactive({
        # Get data from uploaded csv file
        infile <- input$DATAFILE
        if (is.null(infile)) {
            # User has not uploaded a file yet
            return(NULL)
        } else {
            dat.all <<- NULL
            dat.raw <- read.csv(infile$datapath, stringsAsFactors = F)
            location <<- infile$name %>% strsplit(split = ".csv") %>% unlist
            names(dat.raw) <- toupper(names(dat.raw))
            # Parse date (test 3 different formats)
            if(!is.na(ymd(dat.raw$DATE[365]))) {
                date.formated <- ymd(dat.raw$DATE)
            } else if (!is.na(dmy(dat.raw$DATE[365]))){
                date.formated <- dmy(dat.raw$DATE)
            } else if (!is.na(mdy(dat.raw$DATE[365]))){
                date.formated <- mdy(dat.raw$DATE)
            }
            # Extract year, month and day of the year & round parameters
            dat <- data.frame(DATE = date.formated) %>%
                mutate(YEAR = year(DATE), MONTH = month(DATE), DAY = day(DATE),
                       RAIN = dat.raw$RAIN %>% as.numeric %>% round(1),
                       AIRTMAX = dat.raw$AIRTMAX %>% as.numeric %>%round(1),
                       AIRTMIN = dat.raw$AIRTMIN %>% as.numeric %>% round(1))

            # Only estimate ET0 if not provided in csv
            if (any(names(dat.raw) %in% "ET0")) {
                calculate.ET0 <<- F
                dat.all <<- cbind(dat, ET0 = dat.raw$ET0)
            } else {
                calculate.ET0 <<- T
                dat.all <<- dat
            }

            rm(dat.raw)  # delete large temp df
            dat
        }
    })

    output$ui.underlying <- renderUI({
        # Get available underlyings based on uploaded csv
        if (!is.null(dat.all)) {
            variables <- names(dat.all)[!names(dat.all) %in% c("DATE", "YEAR", "MONTH", "DAY")]

            underlyings <-
                names(underlyings.mapping)[sapply(1:length(underlyings.mapping),
                                                  function (x) underlyings.mapping[[x]]$variable %in% variables)]

            selectInput("UNDERLYING",
                        label = "Category",
                        choices = underlyings)
        } else {
            div("Please upload weather data before computing indicators",
                style = "color:red")
        }
    })

    output$ui.threshold <- renderUI({
        # Adapt range, default and critical values based on selected
        # underlying
        underlying <- input$UNDERLYING
        if (!is.null(underlying) && underlying != "") {
            variable <- underlyings.mapping[[underlying]]$variable

            unit <- switch(variable,
                           "AIRTMAX" = "ºC",
                           "AIRTMIN" = "ºC",
                           "AIRTAVE" = "ºC",
                           "RAIN" = "mm",
                           "ET0" = "mm",
                           "SOIL MOISTURE" = "mm",
                           "ARID" = "-")
            range.upper <- switch(variable,
                                  "AIRTMAX" = AIRTMAXcr.h,
                                  "AIRTMIN" = AIRTMINcr.h,
                                  "AIRTAVE" = 50,
                                  "RAIN" = 100,
                                  "ET0" = 15,
                                  "SOIL MOISTURE" = 300,
                                  "ARID" = ARIDcr.h)
            range.lower <- switch(variable,
                                  "AIRTMAX" = AIRTMAXcr.l,
                                  "AIRTMIN" = AIRTMINcr.l,
                                  "AIRTAVE" = -30,
                                  "RAIN" = 0,
                                  "ET0" = 0,
                                  "SOIL MOISTURE" = 0,
                                  "ARID" = ARIDcr.l)
            default <- switch(variable,
                              "AIRTMAX" = AIRTMAXcr.m,
                              "AIRTMIN" = AIRTMINcr.m,
                              "AIRTAVE" = ARIDcr.m,
                              "RAIN" = 10,
                              "ET0" = 5,
                              "SOIL MOISTURE" = 30,
                              "ARID" = 0.85)
            unit.and.range <<- sprintf("Units: %s / Range: [%.1f, %.1f]",
                                       unit, range.lower, range.upper)

            numericInput("THRESHOLD",
                         label = "Critical Value",
                         value = default,
                         min = range.lower,
                         max = range.upper)
        }
    })

    output$ui.criterion <- renderText({
        # Create critterion string (underlying + threshold)
        underlying <- input$UNDERLYING
        threshold <- input$THRESHOLD
        if(!is.null(underlying) && !is.null(threshold)) {
            out <- paste(underlyings.mapping[[input$UNDERLYING]]$variable,
                         underlyings.mapping[[input$UNDERLYING]]$operator,
                         round(input$THRESHOLD, 2))
            criterion <<- out
            paste("Criterion:", out)
        } else {
            NULL
        }
    })

    output$threshold.help <- renderText({
        # Helpers
        if (!is.null(unit.and.range)) {
            unit.and.range
        } else {
            ""
        }
    })

    getSoilParams <- reactive({
        # Soil parameters based on UI inputs (soil texture class and
        # extraction depth)
        this.params <- soil.params[[input$TEXTURE_CLASS]]
        this.params$zeta <- input$EXTRACTION_DEPTH * 1000.
        this.params
    })

    output$ui.end.date <- renderUI({
        # Display possible end dates for risk period
        # The risk period is maximum 1 year
        # 2014 is arbitraty (risk defined as month-day)
        date.start <- paste("2014 ", input$DATE_START) %>%
            as.Date("%Y %B %d")
        dates.end <- date.start + days(1:364)
        dates.end.md <- format(dates.end, "%B %d")

        selectInput("DATE_END",
                    label = "End Date",
                    choices = dates.end.md,
                    selected = dates.end.md[182])
    })

    ComputeSM <- eventReactive(input$button.sm, {
        # The computation of soil moisture is triggered when button.sm is pressed
        if (!is.null(dat.all)) {
            dat <- dat.all
            if (calculate.ET0) {
                # Compute ET0 if necessary
                dat$ET0 <- compute.et0.Hargreaves(yday(dat$DATE),
                                                  input$LATITUDE,
                                                  tmin.C = dat$AIRTMIN,
                                                  tmax.C = dat$AIRTMAX) %>%
                    round(1)
            } else {
                dat$ET0 <- dat$ET0 %>% as.numeric %>% round(1)
            }
            dat$ET0 <- ifelse(dat$ET0 == 0, 0.00001, dat$ET0)

            soil.params <- getSoilParams()
            # Compute soil moisture and ARID index
            sm.out <- run.soil.moisture.model(precip = dat$RAIN,
                                              ET0 = dat$ET0,
                                              date = dat$DATE,
                                              soil.params = soil.params)
            dat$"SOIL MOISTURE" <- sm.out$soil.moisture %>% round(1)
            dat$ARID <- compute.ARID(transpiration = sm.out$transpiration,
                                     ET0 = dat$ET0) %>% round(2)
            dat.all <<- dat
        } else {
            stop("Please upload weather data first and press 'Run Soil Moisture Model' again")
        }
    })

    ComputeIndicators <- eventReactive(input$button.indicator, {
        # This function calculates the number of critical days (daily value >
        # threshold) for the given risk period
        dat <- dat.all
        underlying <- input$UNDERLYING
        threshold <- input$THRESHOLD
        start <- input$DATE_START
        end <- input$DATE_END

        start.dates <- paste(unique(dat$YEAR), start) %>% as.Date("%Y %B %d")
        end.dates <- paste(unique(dat$YEAR), end) %>% as.Date("%Y %B %d")

        if (!is.null(dat)) {
            operator <- get(underlyings.mapping[[underlying]]$operator)
            variable <- underlyings.mapping[[underlying]]$variable
            dat$date2 <- as.Date(dat$DATE)
            start.dates <- start.dates[start.dates %in% dat$date2]
            end.dates <- end.dates[end.dates %in% dat$date2]
            # Check if first or last year need(s) to be excluded
            if (end.dates[1] < start.dates[1]) end.dates <- end.dates[-1]
            if (start.dates[length(start.dates)] >
                end.dates[length(end.dates)]) start.dates <- start.dates[-length(start.dates)]
            index <- foreach (idx = 1:length(start.dates), .combine = "rbind") %do% {
                this.start <- start.dates[idx]
                this.end <- end.dates[idx]
                # Subset dates to match risk period
                this.dat <- dplyr::filter(dat, date2 >= this.start & date2 <= this.end)
                # Count days that fullfill criterion
                this.index <- operator(this.dat[, variable], threshold) %>% sum()
                data.frame(START = this.start,
                           END = this.end,
                           INDEX = this.index)
            }

        } else {
            index <- NULL
        }
        index
    })

    output$ui.button <- renderUI({
        # Display button to download indices after they have been computed
        indices <- ComputeIndicators()
        if (!is.null(indices)) {
            downloadButton('downloadData', 'Download Indicator')
        }
    })

    output$TableIndices <- DT::renderDataTable({
        # Interactive table with indicator values
        indices <- ComputeIndicators()
        if (!is.null(indices)) {
            out <- DT::datatable(indices, rownames = F, options = list(pageLength = 10, searching = T))
        } else {
            out <- NULL
        }
        out
    })

    output$TableData <- DT::renderDataTable({
        # Interactive table with raw weather data
        dat <- getData()
        if (!is.null(dat)) {
            dat <- dat %>% select(-DATE)
            out <- DT::datatable(dat, rownames = F, options = list(pageLength = 25, searching = T))
        } else {
            out <- NULL
        }
        out
    })

    output$TableSM <- DT::renderDataTable({
        # Interactive table with soil moisture estimates
        ComputeSM()
        dat <- dat.all
        if (!is.null(dat) && any(names(dat) %in% "SOIL MOISTURE")) {
            dat <- dat %>% select(-DATE, -AIRTMAX, -AIRTMIN)
            out <- DT::datatable(dat, rownames = F, options = list(pageLength = 25, searching = T))
        } else {
            out <- NULL
        }
        out
    })

    output$plot.title <- renderText({
        # Plot title
        indices <- ComputeIndicators()
        if (!is.null(indices)) {
            plot.title <<- paste0(isolate(input$UNDERLYING) %>% tolower,
                                  "_", location)
            paste("Number of days with", isolate(criterion), "from",
                  isolate(input$DATE_START), "to" , isolate(input$DATE_END),
                  "(", location, ")")
        }
    })

    output$plot.help <- renderText({
        # Helper for interactive plot
        indices <- ComputeIndicators()
        if (!is.null(indices)) {
            "Use left/right bars to display a subset of index values for a specic period"
        }
    })

    output$plot <- renderDygraph({
        # Interactive plot with indicator values
        indices <- ComputeIndicators()

        if (!is.null(indices)) {
            # Indicator values are linked with a date corresponding to Jan 1
            # of the year of the end of the risk period
            dygraph.dat <- dplyr::mutate(indices,
                                         DATE = sprintf("%i-01-01", year(END)) %>% ymd()) %>%
                select(DATE, INDEX) %>%
                bind_rows(data.frame(DATE = sprintf("%i-01-01", year(indices$END) %>% max + 1) %>% ymd(),
                                     INDEX = 0)) %>%
                as.data.frame()

            # dygraph expects xts format
            qxts <- xts(dygraph.dat, order.by = dygraph.dat[, 1])
            qxts$DATE <- NULL
            dygraph(qxts) %>%
                dyAxis("y", label = "Index") %>%
                dySeries("INDEX", label = "Index", stepPlot = T, color = "blue", fillGraph = T) %>%
                dyRangeSelector()
        }
    })

    output$downloadData <- downloadHandler(
        # Download indicator values in a csv file
        filename <- function() {
            paste0(plot.title, '.csv')
        },
        content <- function(con) {
            indices <- ComputeIndicators()
            if (!is.null(indices)) {
                colnames.csv <- c("Start date", "End date", "Index")
                dat.csv <- indices
                colnames(dat.csv) <- colnames.csv
            } else {
                dat.csv <- NULL
            }
            write.csv(dat.csv, con, quote = F, row.names = F)
        }
    )
})
