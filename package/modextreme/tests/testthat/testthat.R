library(testthat)
library(tkweather)

# compute.extraterrestrial.radiation
# test 1 from Allen et al. (1998) p. 47
ra.calculated <- round(compute.extraterrestrial.radiation(doy = 246, lat.deg = -20), 2)
expect_that(ra.calculated, equals(32.19))
# test 2 from Allen et al. (1998) p. 50
ra.calculated <- round(compute.extraterrestrial.radiation(doy = 135, lat.deg = -22.9), 1)
expect_that(ra.calculated, equals(25.1))
# test 3 from Allen et al. (1998) p. 61
ra.calculated <- round(compute.extraterrestrial.radiation(doy = 196, lat.deg = 45.7166667), 1)
expect_that(ra.calculated, equals(40.6))

# compute.incoming.solar.radiation.Hargreaves
# test 1 from Allen et al. (1998) p. 61
rs.calculated <- round(compute.incoming.solar.radiation.Hargreaves(doy = 196,
                                                                   lat.deg = 45.7166667,
                                                                   tmax.C = 26.6,
                                                                   tmin.C = 14.8,
                                                                   krs = 0.16), 1)
expect_that(rs.calculated, equals(22.3))

# compute.et0.Hargreaves
# test 1 from Allen et al. (1998) p. 77
et0.calculated <- round(compute.et0.Hargreaves(doy = 196,
                                              lat.deg = 45.7166667,
                                              tmax.C = 26.6,
                                              tmin.C = 14.8,), 1)
expect_that(et0.calculated, equals(5.0))

# compute.day.length <- function(doy, lat.deg) {
# test from Allen et al. (1998) p. 49
day.length.calculated <- round(compute.day.length(246, -20.), 2)
expect_that(day.length.calculated, equals(11.67))
