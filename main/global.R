# Critical (higher and lower boundaries) and default values
AIRTMAXcr.h <- 50
AIRTMAXcr.l <- 30
AIRTMAXcr.m <- 34

AIRTMINcr.h <- 5
AIRTMINcr.l <- -20
AIRTMINcr.m <- 0

ARIDcr.h <- 1.
ARIDcr.l <- 0.5
ARIDcr.m <- 0.85

# Headers eather data

headers.csv <- c("DATE", "RAIN","AIRTMAX", "AIRTMIN", "AIRTAVE", "ET0")

# SoilParams
# Taken from Soltani, A., Sinclair, T.R., 2012. Modeling physiology of crop development,
# growth and yield. CABI, Wallingford, Oxfordshire, UK; Cambridge, MA.
silty.clay.params <- list(alpha = 0.096,
                          beta= 0.2,
                          WHC = 0.132,
                          curve.number = 87.)
silty.loam.params <- list(alpha = 0.096,
                          beta= 0.3,
                          WHC = 0.132,
                          curve.number = 79.)
sandy.loam.params <- list(alpha = 0.096,
                          beta= 0.5,
                          WHC = 0.131,
                          curve.number = 70.)
sand.params <- list(alpha = 0.096,
                    beta= 0.5,
                    WHC = 0.1,
                    curve.number = 70.)
soil.params <- list("Silty clay" = silty.clay.params,
                    "Silty loam" = silty.loam.params,
                    "Sandy loam" = sandy.loam.params,
                    "Sand" = sand.params)