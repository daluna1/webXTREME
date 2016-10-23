#' compute.soil.moisture.init
#'
#' @param WHC: water holding capacity (m3/m3)
#' @param zeta: rooting depth (mm)
#'
#' @return initial soil moisture (mm)
#' @export
#'
#' @examples
compute.soil.moisture.init <- function(WHC = 0.13,
                                       zeta) {
    # Initial soil moisture.
    #
    # Args:
    #   WHC: water holding capacity (m3/m3)
    #   zeta: rooting depth (mm)
    #
    # Returns:
    #   initial soil moisture (mm)
    WHC * zeta
}

compute.soil.moisture.balance <- function(soil.moisture.previous,
                                          precip, Ta, D, R,
                                          I = 0) {
    # Daily soil moisture balance.
    #
    # Args:
    #   soil.moisture.previous: soil moisture at t-1 (mm)
    #   precip: precipitation (mm)
    #   Ta: crop actual transpiration (mm)
    #   D: deep drainage (mm)
    #   R: runoff (mm)
    #   I: irrigation amount (mm)
    #
    # Returns:
    #   soil moisture at current time step (mm)
    soil.moisture <- soil.moisture.previous + precip + I - Ta - D - R

    return(soil.moisture)
}

compute.theta <- function (soil.moisture,
                           zeta) {
    # soil moisture in mm to m3/m3
    return(soil.moisture / zeta)
}


compute.water.uptake <- function (alpha = 0.096,
                                  zeta,
                                  theta.ad.previous) {
    # Water uptake.
    #
    # Args:
    #   alpha: max daily uptake (fraction)
    #   zeta: rooting depth (mm)
    #   theta.ad.previous: volumetric soil moisture after drainage at t-1 (m3/m3)
    #
    # Returns:
    #   daily water uptake (mm)
    #
    # References:
    #   - Woli, P., Jones, J.W., Ingram, K.T., Fraisse, C.W., 2012.
    #       Agricultural Reference Index for Drought (ARID).
    #       Agronomy Journal 104, 287
    WU <- alpha * zeta * theta.ad.previous
    return (WU)
}


compute.transpiration <- function (WU, ET0) {
    # Actual transpiration is assumed to be the min between ET0 and water uptake
    Ta <- min(WU, ET0)

    return (Ta)
}


compute.deep.drainage <- function (beta = 0.55,
                                   zeta,
                                   theta.bd,
                                   WHC = 0.13) {
    # Deep drainage.
    #
    # Args:
    #   beta: drainage factor (-)
    #   zeta: rooting depth (mm)
    #   theta.bd: volumetric soil moisture after drainage at t (m3/m3)
    #
    # Returns:
    #   water lost by deep drainage (mm)
    #
    # References:
    #   - Woli, P., Jones, J.W., Ingram, K.T., Fraisse, C.W., 2012.
    #       Agricultural Reference Index for Drought (ARID).
    #       Agronomy Journal 104, 287
    D <- beta * zeta * (theta.bd - WHC)

    D <- ifelse(D > 0, D, 0)

    return(D)
}


compute.runoff <- function (precip,
                            curve.number = 65) {
    # Runoff.
    #
    # Args:
    #   precip: daily precipitation (mm)
    #   curve.number: curve number (-)
    #
    # Returns:
    #   water lost by runoff (mm)
    #
    # References:
    #   - Woli, P., Jones, J.W., Ingram, K.T., Fraisse, C.W., 2012.
    #       Agricultural Reference Index for Drought (ARID).
    #       Agronomy Journal 104, 287
    s1 <- 25400
    s2 <- 254
    S <- (s1 / curve.number) - s2

    i1 <- 0.2
    I <- i1 * S

    R <- ifelse(precip <= I, 0,
                ((precip - I)^2) / (precip - I + S))

    return(R)
}


compute.daily.soil.moisture <- function(soil.moisture.previous,
                                        zeta,
                                        alpha = 0.096,
                                        beta= 0.55,
                                        WHC = 0.13,
                                        ET0,
                                        precip) {
    # Compute soil moisture for a single day.
    #
    # Args:
    #   soil.moisture.previous: soil moisture at t-1 (mm)
    #   zeta: rooting depth (mm)
    #   alpha: max daily uptake (fraction)
    #   beta: drainage coefficient
    #   WHC: water holding capacity (m3/m3)
    #   ET0: reference evapotranspiratuin (mm, array)
    #   precip: daily precipitation (mm, array)
    #
    # Returns:
    #   soil moisture at current time step (mm)
    #
    # References:
    #   - Woli, P., Jones, J.W., Ingram, K.T., Fraisse, C.W., 2012.
    #       Agricultural Reference Index for Drought (ARID).
    #       Agronomy Journal 104, 287

    # Compute runoff
    R <- compute.runoff(precip = precip)

    # Convert mm to m3/m3
    theta.ad.previous <- compute.theta(soil.moisture.previous, zeta)

    # Compute crop water uptake
    WU <- compute.water.uptake(zeta = zeta,
                               theta.ad.previous = theta.ad.previous,
                               alpha = alpha)

    # Compute crop transpiration
    Ta <- compute.transpiration(WU = WU, ET0 = ET0)

    # Compute soil moisture before drainage at the current time step
    soil.moisture.bd <- compute.soil.moisture.balance(soil.moisture.previous = soil.moisture.previous,
                                                      precip = precip,
                                                      Ta = Ta,
                                                      D = 0,
                                                      R = R)
    theta.bd <- compute.theta(soil.moisture.bd, zeta)

    # Compute drainage
    D <- compute.deep.drainage(zeta = zeta, theta.bd = theta.bd, WHC = WHC, beta = beta)

    # Compute soil moisture after drainage at the current time step
    soil.moisture <- compute.soil.moisture.balance(soil.moisture.previous = soil.moisture.previous,
                                                   precip = precip,
                                                   Ta = Ta,
                                                   D = D,
                                                   R = R)
    theta <- compute.theta(soil.moisture, zeta)

    out <- list(soil.moisture = soil.moisture,
                theta = theta,
                transpiration = Ta,
                drainage = D,
                runoff = R)

    return(out)
}


compute.ARID <- function(transpiration, ET0) {
    # Compute ARID drought index.
    #
    # Args:
    #   transpiration: actual transpiration (mm)
    #   ET0: reference evapotranspiration (mm)
    #
    # Returns:
    #   ARID (-)
    #
    # References:
    #   - Woli, P., Jones, J.W., Ingram, K.T., Fraisse, C.W., 2012.
    #       Agricultural Reference Index for Drought (ARID).
    #       Agronomy Journal 104, 287
    ARID <- 1 - (transpiration / ET0)
    return(ARID)
}


run.soil.moisture.model <- function(precip,
                                    ET0,
                                    date,
                                    soil.params) {
    # Compute soil moisture for all days.
    #
    # Args:
    #   precip: daily precipitation (mm, array)
    #   ET0: reference evapotranspiratuin (mm, array)
    #   date: dates (array)
    #   soil.params: soil parameters (list)
    #
    # Returns:
    #   array of soil moisture in the rooting depth (mm)
    zeta <- soil.params$'zeta'
    WHC <- soil.params$'WHC'
    alpha <- soil.params$'alpha'
    beta <- soil.params$'beta'
    curve.number <- soil.params$'curve.number'

    n <- length(date)
    doy <- as.numeric(format(date, "%j"))

    # Initialize data structures
    soil.moisture <- numeric(length = n)
    theta <- numeric(length = n)
    transpiration <- numeric(length = n)
    drainage <- numeric(length = n)
    runoff <- numeric(length = n)

    # Set initial conditions
    soil.moisture.init <- compute.soil.moisture.init(zeta = zeta, WHC = WHC)
    soil.moisture[1] <- soil.moisture.init
    theta.init <- compute.theta(soil.moisture[1], zeta)
    theta[1] <- theta.init
    transpiration[1] <- ET0[1]

    year.print <- 0
    out <- for (i in 2:n) {

        # Reinitialize to field capacity on Jan 1 of each year
        if (doy[i] == 1) {
            soil.moisture[i] <- soil.moisture.init
            theta[i] <- theta.init
            drainage[i] <- 0.
            runoff[i] <- 0.
            transpiration[i] <- ET0[i]
        } else {
            soil.moisture.balance.i <- compute.daily.soil.moisture(soil.moisture.previous = soil.moisture[i - 1],
                                                                   zeta = zeta,
                                                                   alpha = alpha,
                                                                   beta = beta,
                                                                   WHC = WHC,
                                                                   ET0 = as.numeric(ET0[i]),
                                                                   precip = as.numeric(precip[i]))

            soil.moisture[i] <- soil.moisture.balance.i[['soil.moisture']]
            theta[i] <- soil.moisture.balance.i[['theta']]
            transpiration[i] <- soil.moisture.balance.i[['transpiration']]
            drainage[i] <- soil.moisture.balance.i[['drainage']]
            runoff[i] <- soil.moisture.balance.i[['runoff']]
        }
    }
    data.frame(soil.moisture = soil.moisture,
               transpiration = transpiration,
               drainage = drainage,
               runoff = runoff)
}

compute.time.series.1.location <- function(precip,
                                           ET0,
                                           tave.C,
                                           date,
                                           crop.params,
                                           soil.params,
                                           use.ETc = F) {
    #cat("computing time series (phenology and soil moisture)...")

    dat <- data.frame(date = date,
                      tave.C = tave.C,
                      precip = precip,
                      ET0 = ET0)

    # Source scripts
    if (!exists("compute.pheno")) stop("source phenology functions!")
    if (!exists("compute.soil.moisture.init")) stop("source sm functions!")

    zeta <- soil.params$'zeta'
    WHC <- soil.params$'WHC'
    alpha <- soil.params$'alpha'
    beta <- soil.params$'beta'
    curve.number <- soil.params$'curve.number'

    crops <- names(crop.params)

    ### PHENOLOGY STARTS ###
    for (i in crops) {
        out.pheno <- compute.pheno(crop.param = crop.params[i],
                                   tave.C = tave.C,
                                   date = date)
        if (i == "grassland") {
            dat[, paste("stage", i, sep = ".")] <- out.pheno$stage
        } else {
            dat[, paste("stage", i, sep = ".")] <- out.pheno$stage
            dat[, paste("stage_rel", i, sep = ".")] <- out.pheno$stage_rel
            dat[, paste("ETc", i, sep = ".")] <- out.pheno$Kc * dat$ET0
        }
    }
    ### PHENOLOGY END ###

    ### SOIL MOISTURE STARTS ###
    # ?TODO? loop to compute soil moisture for multiple crops
    n <- nrow(dat)
    doy <- as.numeric(format(date, "%j"))
    if (use.ETc) {
        ETc <- as.numeric(dat$ETc)
    } else {
        ETc <- as.numeric(dat$ET0)
    }

    # Initialize data structures
    soil.moisture <- numeric(length = n)
    theta <- numeric(length = n)
    transpiration <- numeric(length = n)
    drainage <- numeric(length = n)
    runoff <- numeric(length = n)

    # Set initial conditions
    soil.moisture.init <- compute.soil.moisture.init(zeta = zeta, WHC = WHC)
    soil.moisture[1] <- soil.moisture.init
    theta.init <- compute.theta(soil.moisture[1], zeta)
    theta[1] <- theta.init
    transpiration[1] <- ETc[1]

    year.print <- 0
    out <- for (i in 2:n) {

        # Reinitialize to field capacity on Jan 1 of each year
        if (doy[i] == 1) {
            soil.moisture[i] <- soil.moisture.init
            theta[i] <- theta.init
            drainage[i] <- 0.
            runoff[i] <- 0.
            transpiration[i] <- ETc[i]
        } else {
            soil.moisture.balance.i = run.soil.moisture.model(soil.moisture.previous = soil.moisture[i - 1],
                                                              zeta = zeta,
                                                              alpha = alpha,
                                                              beta = beta,
                                                              WHC = WHC,
                                                              # use ET0 instead of ETc for now to avoid compute soil moisture balance for each crop
                                                              ETc = ETc[i],
                                                              precip = as.numeric(dat$precip[i]))

            soil.moisture[i] <- soil.moisture.balance.i[['soil.moisture']]
            theta[i] <- soil.moisture.balance.i[['theta']]
            transpiration[i] <- soil.moisture.balance.i[['transpiration']]
            drainage[i] <- soil.moisture.balance.i[['drainage']]
            runoff[i] <- soil.moisture.balance.i[['runoff']]
        }
    }
    ### SOIL MOISTURE ENDS ###

    return(cbind(dat,
                 soil.moisture = soil.moisture,
                 theta = theta,
                 transpiration = transpiration,
                 drainage = drainage,
                 runoff = runoff))

}
