#' degF2degC
#'
#' @param temp_degF
#'
#' @return temperature in degrees F
#' @export
#'
#' @examples
degF2degC <- function (temp_degF){
    # converts air temperature in degF to degC
    temp_degC <- (temp_degF - 32) * 5. / 9.
    temp_degC
}

#' in2mm
#'
#' @param precip_in
#'
#' @return precip in mm
#' @export
#'
#' @examples
in2mm <- function (precip_in){
    # converts precipitation amount in in to mm
    precip_mm <- precip_in * 25.4
    precip_mm
}
