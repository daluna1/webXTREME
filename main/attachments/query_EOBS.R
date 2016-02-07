library(RCurl)
library(XML)
library(dplyr)
library(stringr)
library(httr)
library(lubridate)

#### METADATA
catalog.datasets <- getURL("http://opendap.knmi.nl/knmi/thredds/catalog/e-obs_0.25regular/catalog.html") %>%
    strsplit("\n") %>% unlist()
catalog.metadata <- getURL("http://opendap.knmi.nl/knmi/thredds/catalog/e-obs_0.25regular/catalog.html?dataset=e-obs_0.25regular") %>%
    strsplit("\n") %>% unlist()

start.date <- grep("Start", catalog.metadata, value = T) %>% str_sub(start = 25) %>% as.Date()
end.date <- grep("End", catalog.metadata, value = T) %>% str_sub(start = 23) %>% as.Date()
dates.all <- as.Date(start.date : end.date, origin = "1970-01-01")

res <- grep("Dataset", catalog.metadata, value = T) %>%
    str_sub(start = 14, end = 17) %>% as.numeric()

# lat.range <- grep("Latitude", catalog.metadata, value = T)
# lat.start <- lat.range %>% str_sub(start = 27, end = 30) %>% as.numeric()
# lat.end <- lat.range %>% str_sub(start = 35, end = 38) %>% as.numeric()
# lat.all <- seq(lat.start, lat.end, res)
#
# lon.range <- grep("Longitude", catalog.metadata, value = T)
# lon.start <- lon.range %>% str_sub(start = 28, end = 32) %>% as.numeric()
# lon.end <- lon.range %>% str_sub(start = 37, end = 40) %>% as.numeric()
# lon.all <- seq(lon.start, lon.end, res)

#### FUNCTIONS

parse.dataset.name <- function(string, param) {
    param.long <- paste0(param, "_", res, "deg_reg")
    lines <- grep(param.long, string, value = T)
    this.line <- lines[!grepl(pattern = "stderr", x = lines)]
    dataset <- this.line %>% strsplit("dataset=") %>% unlist() %>% .[2] %>% strsplit(".nc") %>% unlist() %>% .[1]
    paste0("http://opendap.knmi.nl/knmi/thredds/dodsC/", dataset, ".nc.ascii?")
}

get.data <- function(lat, lon, time.start, time.end, string, param) {
    scale.factor <- switch(param,
                           "rr" = 0.1,
                           "tn" = 0.01,
                           "tx" = 0.01)
    time <- sprintf("[%i:%i]", time.start, time.end)
    lat.str <- sprintf("[%i]", lat)
    lon.str <- sprintf("[%i]", lon)

    api <- parse.dataset.name(string = string, param = param) %>% paste0(param)
    query <- paste0(api, time, lat.str, lon.str)

    dat <- GET(query) %>% content() %>% str_split("\n") %>% .[[1]] %>% .[-1:-12] %>%
        head(., (time.end - time.start) + 1) %>%
        str_split(", ", ) %>% unlist %>% matrix(ncol = 2, byrow = T) %>% as.data.frame(stringsAsFactors = F)

    colnames(dat) <- c("time", param)
    dat[, param] <- as.numeric(dat[, param]) * scale.factor

    dat
}

####
api.elev <- parse.dataset.name(string = catalog.datasets, param = "elev")
lat.query <- api.elev %>% paste0("latitude")
lon.query <- api.elev %>% paste0("longitude")

lat.all <- GET(lat.query) %>% content() %>% str_split("\n") %>%
    .[[1]] %>% .[6] %>% str_split(", ") %>% unlist %>% as.numeric()
lon.all <- GET(lon.query) %>% content() %>% str_split("\n") %>%
    .[[1]] %>% .[6] %>% str_split(", ") %>% unlist %>% as.numeric()

lat.deg <- 68.12
lon.deg <- 33.6
Nyears <- 10
param <- "tx"

this.start.date <- which(dates.all == (end.date - years(Nyears))) - 1
this.end.date <- length(dates.all) - 1
this.lat <- which.min(abs(lat.all - lat.deg)) - 1
this.lon <- which.min(abs(lon.all - lon.deg)) - 1

dat <- get.data(lat = this.lat, lon = this.lon, time.start = this.start.date,
                time.end = this.end.date, string = catalog.datasets, param = param) %>%
    mutate(date = dates.all[(this.start.date + 1):(this.end.date + 1)])
