#' Create sets
#'
#' Add all sets to gams container based on config
#'
#' @param m gams Container, central object to store all data for input.gdx
#' @param config named list with run configuration
#' @returns gams Container with sets added
#'
#' @author Robin Hasse

createSets <- function(m, config) {

  # Fundamentals ---------------------------------------------------------------

  # sets that are independent of the scenario config

  invisible(m$addSet(
    name = "cost",
    records = c("tangible", "intangible"),
    description = "type of cost"
  ))
  var <- m$addSet(
    name = "var",
    records = c("stock", "construction", "renovation", "renovationBS", "renovationHS", "demolition"),
    description = "major variables of the model"
  )
  invisible(m$addSet(
    name = "qty",
    records = c("area", "num"),
    description = "quantity unit to measure stocks and flows in"
  ))


  # Temporal -------------------------------------------------------------------

  ttotNum <- periodFromConfig(config, "ttot")

  invisible(m$addSet(
    name = "tall",
    records = periodFromConfig(config, "tall"),
    description = "all time steps"
  ))

  ttot <- m$addSet(
    name = "ttot",
    records = ttotNum,
    description = "all modelling time steps"
  )

  tinit <- periodFromConfig(config, "tinit")
  invisible(m$addSet(
    name = "tinit",
    records = tinit,
    description = "initial modelling time step"
  ))

  tNum <- periodFromConfig(config, "t")
  invisible(m$addSet(
    name = "t",
    records = tNum,
    description = "modelled time steps"
  ))

  invisible(m$addSet(
    name = "thist",
    records = periodFromConfig(config, "thist"),
    description = "historic time steps"
  ))

  if (!is.null(config[["calibperiods"]])) {
    tcalib <- periodFromConfig(config, "tcalib")
  } else {
    if (config[["switches"]][["RUNTYPE"]] == "calibration") {
      stop("Calibration time periods are missing from the calibration config.")
    }
    if (!is.null(config[["calibrationRun"]])) {
      if (file.exists(file.path(config[["calibrationRun"]], "config", "config_COMPILED.yaml"))) {
        calibConfig <- readConfig(
          file.path(config[["calibrationRun"]], "config", "config_COMPILED.yaml"),
          readDirect = TRUE
        )
        tcalib <- periodFromConfig(calibConfig, "tcalib")
      } else {
        stop("The given calibration run does not contain a config file,",
             "which is required to determine the calibration time periods.")
      }
    } else {
      warning("Time steps of calibration are not given and could not be determined from calibration config.\n",
              "Assuming that the first model time step is the (only) calibration time step.",
              "If this is not a test run, please specify calibration time periods or a calibration run and restart.")
      tcalib <- min(tNum)
    }
  }
  tcalibLast <- max(tcalib)

  invisible(m$addSet(
    "tcalib",
    records = tcalib,
    description = "time steps considered by the calibration when minimising deviation from target trajectories"
  ))
  invisible(m$addSet(
    "tcalibLast",
    records = tcalibLast,
    description = "last time step of calibration or initial time step if tcalib is empty"
  ))



  # Vintages -------------------------------------------------------------------

  vintages <- getDimMap("vin", config[["granularity"]])

  if (identical(config[["switches"]][["AGGREGATEDIM"]], "vin")
      && identical(config[["switches"]][["CALIBRATIONMETHOD"]], "optimization")) {

    vin <- m$addSet(
      name = "vin",
      records = c(unique(getElement(vintages, "vin")), "all"),
      description = "construction vintage cohort"
    )

    vinExists <- expandSets(ttot, vin) %>%
      filter(.data$vin != "all") %>%
      left_join(vintages, by = "vin") %>%
      filter(.data[["ttot"]] > .data[["from"]] - 1) %>%
      select("ttot", "vin")

    if (config[["switches"]][["RUNTYPE"]] == "calibration") {
      vinCalib <- vinExists %>%
        filter(.data$ttot %in% tcalib) %>%
        mutate(vin = "all") %>%
        unique()
    }

  } else {

    vin <- m$addSet(
      name = "vin",
      records = unique(getElement(vintages, "vin")),
      description = "construction vintage cohort"
    )

    vinExists <- expandSets(ttot, vin) %>%
      left_join(vintages, by = "vin") %>%
      filter(.data[["ttot"]] > .data[["from"]] - 1) %>%
      select("ttot", "vin")

    if (config[["switches"]][["RUNTYPE"]] == "calibration") {
      vinCalib <- vinExists %>%
        filter(ttot %in% tcalib)
    }

  }

  vinExists <- m$addSet(
    name = "vinExists",
    domain = c(ttot, vin),
    records = vinExists,
    description = "Can this vintage cohort exist i.e. ttot cannot be before cohort starts"
  )

  if (config[["switches"]][["RUNTYPE"]] == "calibration") {
    vinCalib <- m$addSet(
      name = "vinCalib",
      domain = c(ttot, vin),
      records = vinCalib,
      description = "vintage cohort to calibrate on"
    )
  }



  # Building state alternatives ------------------------------------------------

  ## building shell ====

  bs <- getBrickMapping("dim_bs.csv") %>%
    getElement("bs") %>%
    unique()
  if (config[["ignoreShell"]]) bs <- head(bs, 1)
  bs <-  m$addSet(
    name = "bs",
    records = bs,
    description = "building shell"
  )

  bsr <- m$addSet(
    name = "bsr",
    records = c(bs$getUELs(), 0),
    description = "renovated building shell"
  )


  ## heating system ====

  hsMap <- getBrickMapping("dim_hs.csv", "sectoral")

  hs <- hsMap %>%
    getElement("hs") %>%
    unique()
  hs <-  m$addSet(
    name = "hs",
    records = hs,
    description = "heating system"
  )

  hsr <- m$addSet(
    name = "hsr",
    records = c(0, hs$getUELs()),
    description = "renovated heating system"
  )

  carrier <- hsMap %>%
    getElement("carrier") %>%
    unique()
  carrier <- m$addSet(
    name = "carrier",
    records = carrier,
    description = "energy carrier"
  )

  hsCarrier <- unique(hsMap[, c("hs", "carrier")])
  hsCarrier <- m$addSet(
    name = "hsCarrier",
    domain = c(hs, carrier),
    records = hsCarrier,
    description = "mapping between heating system and energy carrier"
  )



  # Independent stock subset ---------------------------------------------------

  region <- m$addSet(
    name = "region",
    records = config[["regions"]],
    description = "region"
  )

  loc <- getDimMap("loc", config[["granularity"]]) %>%
    getElement("loc") %>%
    unique()
  loc <- m$addSet(
    name = "loc",
    records = loc,
    description = "location of building (rural, urban)"
  )

  typMap <- getDimMap("typ", config[["granularity"]])
  typ <- typMap %>%
    getElement("typ") %>%
    unique()
  typ <- m$addSet(
    name = "typ",
    records = typ,
    description = "type of building (SFH, MFH, Com)"
  )

  inc <- getBrickMapping("dim_inc.csv") %>%
    getElement("inc") %>%
    unique()
  inc <- m$addSet(
    name = "inc",
    records = inc,
    description = "income quantile"
  )



  # Other ----------------------------------------------------------------------


  ## Boiler ban ====

  # read ban definition from config
  hsBanConfig <- config[["boilerBan"]]
  if (!is.null(hsBanConfig)) {
    varsBan <- c("stock", "construction", "renovation")
    if ("var" %in% names(hsBanConfig) && !all(hsBanConfig$var %in% varsBan)) {
      stop("hsBan can only be defined for the following variables: ",
           paste(varsBan, collapse = ", "))
    }

    # read ban definition from config
    hsBanConfig <- hsBanConfig %>%
      listToDf(split = "\\.") %>%
      toModelResolution(m)
    hsBan <- expandSets(var, region, ttot, hs) %>%
      left_join(hsBanConfig,
                by = setdiff(names(hsBanConfig), "value")) %>%
      filter(!is.na(.data$value) & .data$ttot > .data$value) %>%
      select("var", "region", "ttot", "hs")
  } else {
    hsBan <- NULL
  }
  hsBan <- m$addSet(
    name = "hsBan",
    records = hsBan,
    domain = c(var, region, ttot, hs),
    description = "forbidden heating systems in the respective variable in given period"
  )


  ## Allowed renovations ====

  # no decline on energy ladder
  renAllowed <- lapply(setNames(nm = c("bs", "hs")), function(dim) {
    dimr <- paste0(dim, "r")
    ladder <- getBrickMapping(paste0("dim_", dim, ".csv")) %>%
      select(all_of(c(dim, "energyLadder")))
    expandSets("bs", "hs", dimr, .m = m) %>%
      left_join(ladder, by = dim) %>%
      left_join(ladder, by = setNames(dim, dimr), suffix = c("Before", "After")) %>%
      mutate(step = replace_na(.data$energyLadderBefore - .data$energyLadderAfter, 0)) %>%
      filter(.data$step >= 0) %>%
      select(all_of(c("bs", "hs", dimr)))
  })


  if (config[["ignoreShell"]]) {
    renAllowed$bs <- renAllowed$bs %>%
      filter(.data$bsr == "0")
  }

  renAllowed$all <- full_join(renAllowed$bs, renAllowed$hs, by = c("bs", "hs"))

  invisible(m$addSet(
    name = "renAllowedBS",
    domain = c(bs, hs, bsr),
    records = renAllowed$bs,
    description = "allowed building shell retrofits"
  ))

  invisible(m$addSet(
    name = "renAllowedHS",
    domain = c(bs, hs, hsr),
    records = renAllowed$hs,
    description = "allowed heating system replacements"
  ))

  invisible(m$addSet(
    name = "renAllowed",
    domain = c(bs, hs, bsr, hsr),
    records = renAllowed$all,
    description = "all allowed replacements"
  ))


  ## Buildings subsectors ====

  sec <- typMap %>%
    getElement("subsector") %>%
    unique()
  sec <- m$addSet(
    name = "sec",
    records = sec,
    description = "buildings subsector"
  )

  typInSec <- typMap %>%
    select("typ", sec = "subsector") %>%
    unique()
  typInSec <- m$addSet(
    name = "typInSec",
    domain = c(typ, sec),
    records = typInSec,
    description = "mapping between building type and buildings subsector"
  )



  # RETURN ---------------------------------------------------------------------

  return(m)
}
