#' Compute the extraterrestrial radiation
#'
#' Compute the extraterrestrial radiation (MJ m-2 day-1) for a given location
#' and given day.
#'
#' Reference:
#' - Allen, R.G., Raes, D., Smith, M., 1998. Crop Evapotranspiration.
#'      Guidelines for Computing Crop Water Requirements (No. 56: FAO Irrigation and Drainage Paper).
#'      Food and Agriculture Organization (FAO) of the United Nations.
#'
#' @param doy day of the year (1-366)
#' @param lat.deg latitude of location in degrees
#'
#' @return Ra extraterrestrial radiation (MJ m-2 day-1)
#'
#' @examples
#' compute.extraterrestrial.radiation(doy = 246, lat.deg = -20)
#'
#' @export
compute.extraterrestrial.radiation <- function(doy, lat.deg) {
    SolarCst <- 0.082
    omegaD <- (2 * pi) / 365.
    lat.rad <- lat.deg * pi / 180.
    IRelDst <- 1. + 0.033 * cos(omegaD * doy)
    SolDecl <- 0.409 * sin(omegaD * doy - 1.39)
    SunSet  <- acos(-tan(lat.rad) * tan(SolDecl))
    DayLen  <- (24. * 60.)
    Ra <- DayLen / pi * SolarCst * IRelDst *
        (sin(lat.rad) * sin(SolDecl) * SunSet + cos(lat.rad) * cos(SolDecl) * sin(SunSet))

    return(Ra)
}

#' Compute day length
#'
#' Compute the day length for a given location
#' and given day.
#'
#' @param doy day of the year (1-366)
#' @param lat.deg latitude of location in degrees
#'
#' @return day length (h)
#'
#' @examples
#' compute.day.length(246, -20.)
#'
#' @export
compute.day.length <- function(doy, lat.deg) {
    omegaD <- (2 * pi) / 365.
    lat.rad <- lat.deg * pi/180.
    SolDecl <- 0.409 * sin(omegaD * doy - 1.39)
    temp <- pmax(pmin(1, -tan(lat.rad) * tan(SolDecl)), -1)

    SunSet  <- acos(temp)
    DayLen  <- (24. / pi) * SunSet

    return(DayLen)
}

#' Compute solar radiation based on on the Hargreaves' formula
#'
#' Derive solar radiation based on Tmax and Tmin based on the Hargreaves'
#' formula.
#'
#' References:
#' - Allen, R.G., Raes, D., Smith, M., 1998. Crop Evapotranspiration.
#' Guidelines for Computing Crop Water Requirements (No. 56: FAO Irrigation and Drainage Paper).
#' Food and Agriculture Organization (FAO) of the United Nations.
#' - Hargreaves, G.H., Samani, Z.A., 1982. Estimating potential evapotranspiration.
#' Journal of the Irrigation and Drainage Division 108, 225-230
#'
#' @param doy day of the year (1-366)
#' @param lat.deg latitude of location in degrees
#' @param tmin.C daily minimum temperature (in degrees C,)
#' @param tmax.C daily maximum temperature (in degrees C)
#' @param krs coastal coefficient (-)
#'
#' @return incoming solar radiation (MJ m-2 d-1)
#'
#' @examples
#' compute.incoming.solar.radiation.Hargreaves(doy = 196, lat.deg = 45.7166667, tmax.C = 26.6,
#' tmin.C = 14.8, krs = 0.16)
#'
#' @export
compute.incoming.solar.radiation.Hargreaves <- function(doy, lat.deg, tmin.C,
                                                        tmax.C, krs = 0.19) {

    Ra <- compute.extraterrestrial.radiation(doy, lat.deg)

    Rs <- Ra * krs * sqrt(tmax.C - tmin.C)

    return(Rs)
}

#' Compute ET0 with Hargreaves' formula
#'
#' Compute ET0 (mm) based on Tmax and Tmin based on the Hargreaves'
#' formula.
#'
#' References:
#' - Allen, R.G., Raes, D., Smith, M., 1998. Crop Evapotranspiration.
#' Guidelines for Computing Crop Water Requirements (No. 56: FAO Irrigation and Drainage Paper).
#' Food and Agriculture Organization (FAO) of the United Nations.
#' - Hargreaves, G.H., Samani, Z.A., 1982. Estimating potential evapotranspiration.
#' Journal of the Irrigation and Drainage Division 108, 225-230
#'
#' @param doy day of the year (1-366)
#' @param lat.deg latitude of location in degrees
#' @param tmin.C daily minimum temperature (in degrees C,)
#' @param tmax.C daily maximum temperature (in degrees C)
#' @param tave.C daily average temperature (in degrees C)
#'
#' @return reference evapotranspiration (ET0 in mm) based on Hargreaves' formula
#'
#' @examples
#'
#' @export
compute.et0.Hargreaves <- function(doy, lat.deg, tmin.C,
                                   tmax.C, tave.C = NULL) {
    if (is.null(tave.C)) {
        tave.C <- (tmax.C + tmin.C) / 2.
    }
    latent.heat.vap <- 2.45 # MJ kg-1
    Ra.MJ.m2.day <- compute.extraterrestrial.radiation(doy, lat.deg)
    Ra.mm.day <- Ra.MJ.m2.day / latent.heat.vap

    c1 <- 0.0023
    c2 <- 17.8
    et0 <-  c1 * (tave.C + c2) * sqrt(tmax.C - tmin.C) * Ra.mm.day

    return(et0)
}
