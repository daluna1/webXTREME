compute.ARID <- function(transpiration, ETc) {
    # Woli, P., Jones, J.W., Ingram, K.T., Fraisse, C.W., 2012.
    # Agricultural Reference Index for Drought (ARID).
    # Agronomy Journal 104, 287
    ARID <- 1 - (transpiration / ETc)
    return(ARID)
}


# Compute relative soil moisture that is assumed to be close to FTSW
compute.FTSW <- function(soil.moisture, soil.moisture.max) {
    FTSW <- soil.moisture / soil.moisture.max
}


compute.fu.index <- function(sum.precip, sum.ET0, param = 3) {
    fu <- (- sum.precip / sum.ET0) + (1 + (sum.precip / sum.ET0)^param)^(1 / param)
    return(fu)
}
