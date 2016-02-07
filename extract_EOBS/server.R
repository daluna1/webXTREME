library(shiny)
library(leaflet)
library(dygraphs)
library(xts)
library(raster)

maize.mask <- raster("./attachments/maize_HarvestedAreaFraction.tif")
wheat.mask <- raster("./attachments/wheat_HarvestedAreaFraction.tif")

shinyServer(function(input, output, session) {
    makeReactiveBinding('this.grid')
    makeReactiveBinding('latlon')

    output$mymap <- renderLeaflet({
        print("leaflet")
        leaflet() %>%
            addTiles(attribution = "", options = tileOptions(minZoom = 4)) %>%
            setMaxBounds(lng1 = min(lon.all), lat1 = min(lat.all), lng2 = max(lon.all), lat2 = max(lat.all)) %>%
            setView(lat = mean(lat.all), lng = mean(lon.all), zoom = 4)
    })

    observe({
        mask.name <- input$MASK
        zoom.map.in <- isolate(input$mymap_zoom)
        bounds <- isolate(input$mymap_bounds)

        if (!is.null(bounds) && zoom.map.in >= 4 && mask.name != "none") {
            latRng <- range(bounds$north, bounds$south)
            lngRng <- range(bounds$east, bounds$west)

            mask <- switch(mask.name,
                           "maize" = maize.mask,
                           "wheat" = wheat.mask)
            e <- extent(lngRng[1] , lngRng[2],
                        latRng[1], latRng[2])
            ras.cropped <- mask %>% crop(e)
            ras.range <- range(values(ras.cropped), na.rm = T)
            pal <- colorNumeric("YlGn", ras.range,
                                na.color = "transparent")

            leafletProxy("mymap") %>%
                clearImages() %>%
                clearControls() %>%
                addRasterImage(ras.cropped, colors = pal, opacity = 0.6) %>%
                addLegend(pal = pal, values = ras.range,
                          title = "Harvested Area Fraction")
        } else {
            leafletProxy("mymap") %>%
                clearImages() %>%
                clearControls()
        }
    })

    observe({
        bounds <- input$mymap_bounds
        zoom.map.in <- input$mymap_zoom
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)

        if (!is.null(bounds) && zoom.map.in >= 7) {
            this.grid <<- filter(grid,
                                 lat1 >= latRng[1] - 1 * res & lat2 <= latRng[2] + 1 * res &
                                 lng1 >= lngRng[1] - 1 * res& lng2 <= lngRng[2] + 1 * res)

            leafletProxy("mymap") %>%
                clearShapes() %>%
                addRectangles(lng1 = this.grid$lng1,
                              lat1 = this.grid$lat1,
                              lng2 = this.grid$lng2,
                              lat2 = this.grid$lat2,
                              layerId = 1:nrow(this.grid),
                              color = "black", fillColor = "black", fillOpacity = 0.05, weight =2)
        } else {
            leafletProxy("mymap") %>%
                clearShapes()
        }
    })

        observe({
            event <- input$mymap_shape_click

            if (!is.null(event)) {

                latlon <<- isolate(this.grid)[event$id, ]

                popup.message <- paste0("<b>Latitude</b>: ", latlon$lat, "<br/>",
                                        "<b>Longitude</b>: ", latlon$lon, "<br/>")
                leafletProxy("mymap") %>%
                    clearPopups() %>%
                    addPopups(lng = latlon$lon, lat = latlon$lat, popup.message)

            } else {
                leafletProxy("mymap") %>%
                    clearPopups()
            }
        })

        output$ui.button <- renderUI({
            if (!is.null(latlon)) {
                actionButton("go", strong("Get data"))
            } else {
                NULL
            }
        })

        GetData <- eventReactive(input$go, {
            this.dates <- which(dates.all %in% input$DATES) - 1
            this.start.date <- min(this.dates)
            this.end.date <- max(this.dates)
            this.lat <- which.min(abs(lat.all - latlon$lat)) - 1
            this.lon <- which.min(abs(lon.all - latlon$lon)) - 1

            dat <- get.data(lat = this.lat, lon = this.lon, time.start = this.start.date,
                            time.end = this.end.date, string = catalog.datasets, param = input$PARAM) %>%
                mutate(date = dates.all[(this.start.date + 1):(this.end.date + 1)])
        })

        output$ts <- renderDygraph({
            dat <- GetData()
            if (!is.null(dat)) {
                dat.plot <- dat[, c("date", isolate(input$PARAM))]
                plot.title <- parameters[parameters == isolate(input$PARAM)] %>% names
                #dat.plot$date <- as.POSIXct(dat.plot$date)
                qxts <- xts(dat.plot[, -1], order.by = dat.plot[, 1])
                dg <- dygraph(qxts, main = plot.title) %>%
                    dyAxis("y", label = "") %>%
                    dySeries("V1", label = isolate(input$PARAM), stepPlot = F, fillGraph = F, strokeWidth = 2) %>%
                    dyRangeSelector()
            } else {
                NULL
            }
        })
})