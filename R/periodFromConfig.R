#' get Period from config
#'
#' extract different periods from a given BRICK config
#'
#' @author Robin Hasse
#'
#' @param config named list with run configuration
#' @param periodType type of period(s)
#' @returns numeric vector with periods

periodFromConfig <- function(config, periodType) {

  startyear <- config[["startyear"]]
  ttot <- sort(unique(config[["periods"]]))

  if (startyear <= min(ttot)) {
    stop("startyear cannot be equal or before the first period. ",
         "There has to be at least one historic period.")
  }

  t <- ttot[which(ttot >= startyear)]

  if (periodType == "tcalib") {
    calibperiods <- config[["calibperiods"]]
    #TODO: Add check that calibration periods are consistent with t
  } else {
    calibperiods <- min(t)
  }

  switch(periodType,
    ttot = ttot,
    tall = min(ttot):max(ttot),
    startyear = startyear,
    t = t,
    thist = setdiff(ttot, t),
    tinit = min(ttot),
    t0 = min(t),
    tcalib = calibperiods,
    stop("unknown type of period: ", periodType)
  )
}
