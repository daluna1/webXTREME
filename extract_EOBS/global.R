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

parameters <- c("Maximum air temperature (ºC)" = "tx",
                "Minimum air temperature (ºC)" = "tn",
                "Precipitation (mm)" = "rr")

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

grid <- expand.grid(lat = lat.all, lon = lon.all) %>%
    mutate(lng1 = lon - res / 2,
           lat1 = lat - res / 2,
           lng2 = lon + res / 2,
           lat2 = lat + res / 2)