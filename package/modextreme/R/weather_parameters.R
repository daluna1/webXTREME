#' Esat
#'
#' @param temp
#'
#' @return vapor pressure at saturation
#' @export
#'
#' @examples
Esat <- function(temp){
    # computes vapor pressure at saturation in Pa following "Fondamentaux de
    # météorologie" by S. Malardel (2008)
    rgas <- 8.314
    asat <- 6763.6
    bsat <- 4.9283
    csat <- 54.233
    temp0 <- 273.15
    tk <- temp + 273.15
    psat0 <- 100. * exp(-asat / temp0 - bsat * log(temp0) + csat)
    eaice <- 51012.
    e0  <- ifelse(tk >= temp0,
                  (100. * exp(-asat / tk - bsat * log(tk) + csat)),
                  (psat0 * exp(eaice * (tk - temp0) / (rgas * tk * temp0))))
    e0
}

#' compute.sat.vapor.pressure
#'
#' @param temp
#'
#' @return
#' @export
#'
#' @examples
compute.sat.vapor.pressure <- function(temp) {
    c1 <- 0.6108
    c2 <- 17.27
    c3 <- 237.3
    sat.vapor.pressure <- c1 * exp((temp * c2) / (temp + c3))
    return(sat.vapor.pressure)
}
