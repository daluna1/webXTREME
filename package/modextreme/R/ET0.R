#' compute.extraterrestrial.radiation
#'
#' @param doy
#' @param lat.deg
#'
#' @return Ra: extraterrestrial.radiation
#' @export
#'
#' @examples
compute.extraterrestrial.radiation <- function(doy, lat.deg) {
    # Computes the extraterrestrial radiation.
    #
    # Args:
    #   doy: day of the year
    #   lat.deg: latitude of the location (in degrees)
    #
    # Returns:
    #   extraterrestrial radiation (MJ m-2 day-1)
    #
    # References:
    #   - Allen, R.G., Raes, D., Smith, M., 1998. Crop Evapotranspiration.
    #       Guidelines for Computing Crop Water Requirements (No. 56: FAO Irrigation and Drainage Paper).
    #       Food and Agriculture Organization (FAO) of the United Nations.

    SolarCst <- 0.082 # MJ m-2 min-1
    omegaD <- (2 * pi) / 365.
    lat.rad <- lat.deg * pi/180.
    IRelDst <- 1. + 0.033 * cos(omegaD * doy)
    SolDecl <- 0.409 * sin(omegaD * doy - 1.39)
    SunSet  <- acos(-tan(lat.rad) * tan(SolDecl))
    DayLen  <- (24. * 60.)
    Ra <- DayLen / pi * SolarCst * IRelDst *
        (sin(lat.rad) * sin(SolDecl) * SunSet + cos(lat.rad) * cos(SolDecl) * sin(SunSet))
    return(Ra)
}

#' compute.day.length
#'
#' @param doy
#' @param lat.deg
#'
#' @return day length (h)
#' @export
#'
#' @examples
compute.day.length <- function(doy, lat.deg) {
    # FAO 56 eq 34
    SolarCst <- 0.082 #MJ m-2 min-1
    omegaD <- (2 * pi) / 365.
    lat.rad <- lat.deg * pi/180.
    IRelDst <- 1. + 0.033 * cos(omegaD * doy)
    SolDecl <- 0.409 * sin(omegaD * doy - 1.39)
    temp <- pmax(pmin(1, -tan(lat.rad) * tan(SolDecl)), -1)

    SunSet  <- acos(temp)
    DayLen  <- (24. / pi) * SunSet

    DayLen
}

#' compute.incoming.solar.radiation.Hargreaves
#'
#' @param doy
#' @param lat.deg
#' @param tmin.C
#' @param tmax.C
#' @param krs
#'
#' @return incoming solar radiation (MJ m-2 d-1)
#' @export
#'
#' @examples
compute.incoming.solar.radiation.Hargreaves <- function(doy, lat.deg, tmin.C,
                                                        tmax.C, krs = 0.19) {
    # Derive solar radiation based on Tmax and Tmin.
    #
    # Args:
    #   doy: day of the year
    #   lat.deg: latitude of the location (in degrees)
    #   tmin.C: daily minimum temperature (in degrees C, array)
    #   tmax.C: daily maximum temperature (in degrees C, array)
    #   krs: coastal coefficient (-)
    #
    # Returns:
    #   incoming solar radiation (MJ m-2 day-1)
    #
    # References:
    #   - Allen, R.G., Raes, D., Smith, M., 1998. Crop Evapotranspiration.
    #       Guidelines for Computing Crop Water Requirements (No. 56: FAO Irrigation and Drainage Paper).
    #       Food and Agriculture Organization (FAO) of the United Nations.
    #   - Hargreaves, G.H., Samani, Z.A., 1982. Estimating potential evapotranspiration.
    #       Journal of the Irrigation and Drainage Division 108, 225-230

    Ra <- compute.extraterrestrial.radiation(doy, lat.deg)

    Rs <- Ra * krs * sqrt(tmax.C - tmin.C)

    return(Rs)
}

#' compute.et0.Hargreaves
#'
#' @param doy
#' @param lat.deg
#' @param tmin.C
#' @param tmax.C
#' @param tave.C
#'
#' @return ET0 (mm)
#' @export
#'
#' @examples
compute.et0.Hargreaves <- function(doy, lat.deg, tmin.C,
                                   tmax.C, tave.C = NULL) {
    # Compute ET0 with Hargreaves function.
    #
    # Args:
    #   doy: day of the year
    #   lat.deg: latitude of the location (in degrees)
    #   tmin.C: daily minimum temperature (in degrees C, array)
    #   tmax.C: daily maximum temperature (in degrees C, array)
    #   tave.C: daily average temperature (in degrees C, array, optional)
    #
    # Returns:
    #   reference evapotranspiration (ET0) based on Hargreaves' function
    #
    # References:
    #   - Hargreaves, G.H., Samani, Z.A., 1982. Estimating potential evapotranspiration.
    #       Journal of the Irrigation and Drainage Division 108, 225-230

    # Estimate the average air temperature if not provided

    if (is.null (tave.C)) {
        tave.C = (tmax.C + tmin.C) / 2.
    }
    latent.heat.vap <- 2.45 # MJ kg-1
    Ra.MJ.m2.day <- compute.extraterrestrial.radiation(doy, lat.deg)
    Ra.mm.day <- Ra.MJ.m2.day / latent.heat.vap

    c1 <- 0.0023
    c2 <- 17.8
    et0 <-  c1 * (tave.C + c2) * sqrt(tmax.C - tmin.C) * Ra.mm.day

    return(et0)
}
