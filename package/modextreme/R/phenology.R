compute.pheno <- function(crop.param,
                          tave.C,
                          date) {

    require(foreach)
    require(iterators)
    require(stringr)
    require(plyr)

    wx <- data.frame(date = date, tave.C = tave.C)
    wx$year <- as.numeric(format(wx$date, "%Y"))

    crop <- names(crop.param)

    tbase <- crop.param[[crop]]$tbase
    if (crop == "grassland") {
        compute.season.length <- function(Ta,
                                          TaGS = tbase) {
            N <- length(Ta)
            doy <- 1:N
            doy.dormancy <- which((Ta - TaGS) < 0.)
            if (length(doy.dormancy) == 0) {
                stage <- rep("growing.season", N)
            } else {
                if (max(doy.dormancy) < 180) {
                    season.start <- max(doy.dormancy) + 1
                    season.end <- N
                } else if (min(doy.dormancy) > 180) {
                    season.start <- 1
                    season.end <- min(doy.dormancy) - 1
                } else {
                    season.start <- doy.dormancy[which.max(diff(doy.dormancy))] + 1
                    season.end <- season.start + max(diff(doy.dormancy)) - 2
                }
                stage <- ifelse(doy >= season.start & doy <= season.end, "growing.season", "dormancy")
            }
            return (stage)
        }
        wx$stage <- unlist(sapply(min(wx$year) : max(wx$year),
                                  function (x) compute.season.length(wx[wx$year == x, "tave.C"])))
        return(wx[, c("date", "stage")])
    } else {
        tcutoff <- crop.param[[crop]]$tcutoff
        GDD.requirements <- unlist(crop.param[[crop]][grep("GDD", names(crop.param[[crop]]))])
        GDD.requirements.cum <- cumsum(as.numeric(GDD.requirements))

        Kc.start <- crop.param[[crop]]$Kc.start
        Kc.mid <- crop.param[[crop]]$Kc.mid
        Kc.end <- crop.param[[crop]]$Kc.end
        planting.date <- as.character(crop.param[[crop]]$planting.date)

        wx$planting <- as.Date(paste(wx$year, planting.date, sep="-"))
        wx$end.date <- as.Date(paste(wx$year, "10-15", sep="-"))
        wx$stage <- ""
        wx$GDD <- 0
        wx$GDD.cum <- 0

        # GDD computation
        compute.gdd <- function(temp, tbase, tcutoff) {
            temp <- ifelse(temp <= tbase, tbase, temp)
            temp <- ifelse(temp >= tcutoff, tcutoff, temp)
            return(temp - tbase)
        }
        wx$GDD <- compute.gdd(wx$tave.C, tbase, tcutoff)
        wx$GDD[wx$date < wx$planting] <- 0

        compute.GDD.cum <- function(year) {
            as.numeric(cumsum(wx[wx$year == year, "GDD"]))
        }
        wx$GDD.cum <- as.numeric(unlist(sapply(min(wx$year) : max(wx$year), FUN = compute.GDD.cum)))

        wx$stage <- "maturity"
        wx$stage[wx$GDD.cum < GDD.requirements.cum[3]] <- "reproductive"
        wx$stage[wx$GDD.cum < GDD.requirements.cum[2]] <- "vegetative"
        wx$stage[wx$GDD.cum < GDD.requirements.cum[1]] <- "after planting"
        wx$stage[wx$date < wx$planting] <- "before planting"
        wx$stage[wx$date > wx$end.date] <- "maturity"

        wx$stage_rel <- 2
        wx$stage_rel[wx$GDD.cum < GDD.requirements.cum[3]] <- 1 + ((wx$GDD.cum[wx$GDD.cum < GDD.requirements.cum[3]] -
                                                                        GDD.requirements[2]) / GDD.requirements[3])
        wx$stage_rel[wx$GDD.cum < GDD.requirements.cum[2]] <- (wx$GDD.cum[wx$GDD.cum < GDD.requirements.cum[2]] /
                                                                   GDD.requirements.cum[2])
        wx$Kc <- as.numeric(mapvalues(wx$stage,
                                      c("before planting", "after planting", "vegetative", "reproductive", "maturity"),
                                      c(Kc.start, Kc.start, (Kc.start + Kc.mid) / 2, Kc.mid, Kc.end), warn_missing = FALSE))
        return(wx[, c("date", "stage", "GDD", "GDD.cum", "stage_rel", "Kc")])
    }
}
