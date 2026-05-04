#' Create parameters
#'
#' Add all parameters to gams container based on config
#'
#' @param m gams Container, central object to store all data for input.gdx
#' @param config named list with run configuration
#' @param inputDir directory of input folder
#' @returns gams Container with parameters added
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr %>% .data left_join rename mutate filter select cross_join
#'   group_by ungroup across all_of arrange inner_join relocate
#' @importFrom tidyr pivot_wider
#' @importFrom stats pweibull

createParameters <- function(m, config, inputDir) {

  # restore set objects
  ttotNum <- readSymbol(m, "ttot")
  vinExists <- readSymbol(m, "vinExists", stringAsFactor = FALSE)
  state <- c("bs", "hs")
  levels <- readSymbol(m, "level")



  # Periods --------------------------------------------------------------------

  dt <- diff(ttotNum)
  dt <- data.frame(ttot = ttotNum, value = c(dt[1], dt))
  p_dt <- m$addParameter(
    name = "p_dt",
    domain = "ttot",
    records = dt,
    description = "length of time step in yr"
  )

  vintages <- getDimMap("vin", config[["granularity"]])
  p_dtVin <- expandSets("ttot", "vin", .m = m) %>%
    left_join(vintages, by = "vin") %>%
    left_join(dt, by = "ttot") %>%
    rename(dt = "value") %>%
    mutate(value = pmax(
      0,
      pmin(.data[["to"]], .data[["ttot"]])
      - pmax(.data[["from"]] - 1, .data[["ttot"]] - .data[["dt"]])
    )) %>%
    select("ttot", "vin", "value")
  p_dtVin <- m$addParameter(
    name = "p_dtVin",
    domain = c("ttot", "vin"),
    records = p_dtVin,
    description = "intersection of time step and vintage cohort in yr"
  )

  invisible(m$addParameter(
    name = "t0",
    records = periodFromConfig(config, "t0"),
    description = "reference year for discounting"
  ))



  # Specific cost --------------------------------------------------------------

  # intangible cost assumption files
  intangCostFiles <- list(con = brick.file("assump", "costIntangCon.csv"),
                          ren = brick.file("assump", "costIntangRen.csv"))
  if (is.null(config[["calibrationRun"]])) {
    intangCostFilesCfg <- config[["intangCostFiles"]]
    if (is.list(intangCostFilesCfg)) {
      var <- intersect(names(intangCostFiles), names(intangCostFilesCfg))
      intangCostFiles[var] <- intangCostFilesCfg[var]
    }
  } else {
    pattern <- ".*costIntang(.+)\\.csv$"
    intangCostFilesCalib <- list.files(config[["calibrationRun"]],
                                       pattern, full.names = TRUE)
    intangCostFilesCalib <- setNames(intangCostFilesCalib,
                                     sub(pattern, "\\1", intangCostFilesCalib))
    intangCostFiles[["con"]] <- intangCostFilesCalib[["Con"]]
    intangCostFiles[["ren"]] <- if (isTRUE(config[["switches"]][["SEQUENTIALREN"]])) {
      list(BS = intangCostFilesCalib[["RenBS"]],
           HS = intangCostFilesCalib[["RenHS"]])
    } else {
      intangCostFilesCalib[["Ren"]]
    }
  }


  ## construction ====

  p_specCostConTang <- readInput("f_costConstruction.cs4r",
                                 c("ttot", "region", "bs", "hs", "typ"),
                                 inputDir) %>%
    toModelResolution(m)
  p_specCostCon <- expandSets("cost", "bs", "hs", "region", "loc", "typ", "inc", "ttot",
                              .m = m)
  p_specCostConTang <- p_specCostCon %>%
    filter(.data[["cost"]] == "tangible") %>%
    left_join(p_specCostConTang,
              by = c("bs", "hs", "region", "typ", "ttot"))
  p_specCostConIntang <- p_specCostCon %>%
    filter(.data[["cost"]] == "intangible") %>%
    addAssump(intangCostFiles[["con"]])
  p_specCostCon <- rbind(p_specCostConTang, p_specCostConIntang)
  p_specCostCon <- m$addParameter(
    name = "p_specCostCon",
    domain = c("cost", state, "region", "loc", "typ", "inc", "ttot"),
    records = p_specCostCon,
    description = "floor-space specific construction cost in USD/m2"
  )


  ## renovation ====

  p_specCostRenBS_tang <- readInput("f_costRenovationBS.cs4r",
                                    c("ttot", "region", "typ", "bs", "bsr", "vin"),
                                    inputDir) %>%
    toModelResolution(m) %>%
    .explicitZero()
  p_specCostRenBS_tang <- expandSets("bs", "hs", "bsr", "vin", "region",
                                     "loc", "typ", "inc", "ttot", .m = m) %>%
    .filter(readSymbol(m, "renAllowedBS"), vinExists) %>%
    mutate(cost = "tangible", .before = 1) %>%
    left_join(p_specCostRenBS_tang,
              by = c("bs", "bsr", "vin", "region", "typ", "ttot"))

  p_specCostRenHS_tang <- readInput("f_costRenovationHS.cs4r",
                                    c("ttot", "region", "hs", "hsr", "bs", "typ", "vin"),
                                    inputDir) %>%
    toModelResolution(m) %>%
    .explicitZero()
  p_specCostRenHS_tang <- expandSets("bs", "hs", "hsr", "vin", "region",
                                     "loc", "typ", "inc", "ttot", .m = m) %>%
    .filter(readSymbol(m, "renAllowedHS"), vinExists) %>%
    mutate(cost = "tangible", .before = 1) %>%
    left_join(p_specCostRenHS_tang,
              by = c("bs", "hs", "hsr", "vin", "region", "typ", "ttot"))

  if (isTRUE(config[["identVinCharact"]])) {
    p_specCostRenBS_tang <- .makeIdentVin(p_specCostRenBS_tang)
    p_specCostRenHS_tang <- .makeIdentVin(p_specCostRenHS_tang)
  }

  if (isTRUE(config[["switches"]][["SEQUENTIALREN"]])) {

    p_specCostRenBS_intang <- expandSets("bs", "hs", "bsr", "vin", "region",
                                         "loc", "typ", "inc", "ttot", .m = m) %>%
      .filter(readSymbol(m, "renAllowedBS"), vinExists) %>%
      mutate(cost = "intangible", .before = 1) %>%
      addAssump(intangCostFiles[["ren"]], key = "BS")
    p_specCostRenHS_intang <- expandSets("bs", "hs", "hsr", "vin", "region",
                                         "loc", "typ", "inc", "ttot", .m = m) %>%
      .filter(readSymbol(m, "renAllowedHS"), vinExists) %>%
      mutate(cost = "intangible", .before = 1) %>%
      addAssump(intangCostFiles[["ren"]], key = "HS") %>%
      .adjustCostBalance(config[["costReductionZeroF"]])

    p_specCostRenBS <- rbind(p_specCostRenBS_tang, p_specCostRenBS_intang)
    p_specCostRenHS <- rbind(p_specCostRenHS_tang, p_specCostRenHS_intang)

    p_specCostRenBS <- m$addParameter(
      name = "p_specCostRenBS",
      domain = c("cost", state, "bsr", "vin", "region", "loc", "typ", "inc", "ttot"),
      records = p_specCostRenBS,
      description = "floor-space specific building shell retrofit cost [USD/m2]"
    )
    p_specCostRenHS <- m$addParameter(
      name = "p_specCostRenHS",
      domain = c("cost", state, "hsr", "vin", "region", "loc", "typ", "inc", "ttot"),
      records = p_specCostRenHS,
      description = "floor-space specific heating system replacement cost [USD/m2]"
    )

  } else {

    p_specCostRen_intang <- expandSets("bs", "hs", "bsr", "hsr", "vin", "region",
                                       "loc", "typ", "inc", "ttot", .m = m) %>%
      mutate(cost = "intangible", .before = 1) %>%
      addAssump(intangCostFiles[["ren"]]) %>%
      .adjustCostBalance(config[["costReductionZeroF"]])

    p_specCostRen_tang <- full_join(p_specCostRenBS_tang, p_specCostRenHS_tang,
                                    by = c("cost", state, "vin", "region", "loc", "typ", "inc", "ttot"),
                                    suffix = c(".bs", ".hs")) %>%
      relocate("hsr", .after = "bsr") %>%
      mutate(value = .data$value.bs + .data$value.hs, .keep = "unused")

    p_specCostRen <- rbind(p_specCostRen_tang, p_specCostRen_intang)

    p_specCostRen <- m$addParameter(
      name = "p_specCostRen",
      domain = c("cost", state, "bsr", "hsr", "vin", "region", "loc", "typ", "inc", "ttot"),
      records = p_specCostRen,
      description = "floor-space specific renovation cost [USD/m2]"
    )

  }


  ## operation ====

  # scenario assumptions
  carbonPrice <- config[["carbonPrice"]]
  carrierPriceLevel <- config[["carrierPrices"]]
  carrierEmiLevel   <- config[["carrierEmi"]]

  ### carbon price ####
  p_carbonPrice <- expandSets("carrier", "ttot", .m = m) %>%
    mutate(value = 0)
  if (!is.null(carbonPrice)) {
    carbonPrice <- carbonPrice %>%
      listToDf(split = "\\.") %>%
      toModelResolution(m)
    p_carbonPrice <- carbonPrice %>%
      right_join(p_carbonPrice,
                 by = setdiff(names(carbonPrice), "value"),
                 suffix = c("Config", "")) %>%
      relocate("carrier", "ttot", "value") %>%
      mutate(value = .data$value + replace_na(.data$valueConfig, 0),
             .keep = "unused")
  }
  p_carbonPrice <- m$addParameter(
    name = "p_carbonPrice",
    domain = c("carrier", "ttot"),
    records = p_carbonPrice,
    description = "Carbon price in USD/t_CO2eq"
  )

  ### energy carrier prices and emission intensities ####
  carrierData <- readInput("f_carrierPrices.cs4r",
                           c("ttot", "region", "variable", "unit", "carrier", "level"),
                           inputDir)

  p_carrierPrice <- carrierData %>%
    filter(.data[["variable"]] == "price") %>%
    .filterLevel(carrierPriceLevel, "carrierPrice") %>%
    select("carrier", "region", "ttot", "value") %>%
    toModelResolution(m)
  p_carrierPrice <- m$addParameter(
    name = "p_carrierPrice",
    domain = c("carrier", "region", "ttot"),
    records = p_carrierPrice,
    description = "final energy carrier price in USD/kWh"
  )

  p_carrierEmi <- carrierData %>%
    filter(.data[["variable"]] == "emi") %>%
    .filterLevel(carrierEmiLevel, "carrierEmi") %>%
    select("carrier", "region", "ttot", "value") %>%
    toModelResolution(m)
  p_carrierEmi <- m$addParameter(
    name = "p_carrierEmi",
    domain = c("carrier", "region", "ttot"),
    records = p_carrierEmi,
    description = "energy carrier emission intensity in t_CO2/kWh"
  )

  ### useful energy demand for space heating ####
  p_ueDemand <- readInput("f_ueDemand.cs4r",
                          c("region", "typ", "vin", "bs", "value"),
                          inputDir) %>%
    select("bs", "vin", "region", "typ", "value") %>%
    toModelResolution(m)
  if (isTRUE(config[["identVinCharact"]])) {
    p_ueDemand <- .makeIdentVin(p_ueDemand)
  }
  p_ueDemand <- m$addParameter(
    name = "p_ueDemand",
    domain = c("bs", "vin", "region", "typ"),
    records = p_ueDemand,
    description = "floor-space specific useful energy demand for space heating in kWh/yr/m2"
  )

  ### FE-to-UE-efficiency of heating systems ####
  p_eff <- readInput("f_heatingEfficiency.cs4r",
                     c("ttot", "region", "hs", "typ", "value"),
                     inputDir) %>%
    select("hs", "region", "typ", "ttot", "value") %>%
    toModelResolution(m)
  p_eff <- m$addParameter(
    name = "p_eff",
    domain = c("hs", "region", "typ", "ttot"),
    records = p_eff,
    description = "technical efficiency of space heating technologies"
  )


  ## demolition ====

  invisible(m$addParameter(
    name = "p_specCostDem",
    records = 15,
    description = "floor-space specific demolition cost in USD/m2"
  ))


  # Life time ------------------------------------------------------------------

  # cut off Weibull above this value and assume 1 for technology life time
  cutOffShare <- 0.95

  # read Weibull life time parameters
  lt   <- readInput("f_lifetimeBuilding.cs4r",      c("region", "typ", "variable"),       inputDir)
  ltBs <- readInput("f_lifetimeBuildingShell.cs4r", c("region", "variable"),              inputDir)
  ltHs <- readInput("f_lifetimeHeatingSystem.cs4r", c("region", "typ", "hs", "variable"), inputDir)

  # shift scale parameter of Weibull distribution
  ltHsShift <- config[["ltHsShift"]]
  if (!is.null(ltHsShift)) {
    ltHsShift <- data.frame(hs = names(ltHsShift),
                            variable = "scale",
                            shift = as.numeric(ltHsShift))
    ltHs <- ltHs %>%
      left_join(ltHsShift, by = c("hs", "variable")) %>%
      mutate(value = .data$value + replace_na(.data$shift, 0)) %>%
      select(-"shift")
  }


  ## building ====

  # parameter for monitoring purposes
  p_probDem <- expandSets("region", "typ", ttotIn = "ttot", ttotOut = "ttot", .m = m) %>%
    filter(.data$ttotOut >= .data$ttotIn) %>%
    left_join(lt, by = c("region", "typ"), relationship = "many-to-many") %>%
    pivot_wider(names_from = "variable") %>%
    mutate(value = (1 - config[["ltEternalShare"]]) *
             pweibull(.data$ttotOut - .data$ttotIn,
                      .data$shape,
                      .data$scale * config[["ltFactor"]])) %>%
    select(-"shape", -"scale")
  p_probDem <- m$addParameter(
    name = "p_probDem",
    domain = c("region", "typ", "ttotIn", "ttotOut"),
    records = p_probDem,
    description = "probability of a building having reached its end of life"
  )

  # share of stock from previous time step that has to be demolished as it
  # reaches its end of life
  p_shareDem <- expandSets("vin", "region", "typ", "ttot", .m = m) %>%
    left_join(vintages, by = "vin") %>%
    left_join(lt, by = c("region", "typ"), relationship = "many-to-many") %>%
    pivot_wider(names_from = "variable") %>%
    mutate(tcon = (.data$from + pmin(.data$ttot, .data$to)) / 2,
           p = (1 - config[["ltEternalShare"]]) *
             pweibull(.data$ttot - .data$tcon,
                      .data$shape,
                      .data$scale * config[["ltFactor"]])) %>%
    left_join(readSymbol(p_dt) %>%
                rename(dt = "value"),
              by = "ttot") %>%
    group_by(across(all_of(c("vin", "region", "typ")))) %>%
    arrange(.data$ttot) %>%
    mutate(value = (.data$p - lag(.data$p)) / (1 - lag(.data$p)) / .data$dt) %>%
    inner_join(vinExists, by = c("vin", "ttot")) %>%
    select("vin", "region", "typ", "ttot", "value")
  p_shareDem <- m$addParameter(
    name = "p_shareDem",
    domain = c("vin", "region", "typ", "ttot"),
    records = p_shareDem,
    description = "minimum share of demolition at end of life"
  )


  ## building shell ====

  p_lifeTimeBS <- ltBs %>%
    calc_addVariable(lt = "scale * gamma(1 + 1 / shape)", only.new = TRUE) %>%
    select("region", "value") %>%
    toModelResolution(m)
  p_lifeTimeBS <- m$addParameter(
    name = "p_lifeTimeBS",
    domain = "region",
    records = p_lifeTimeBS,
    description = "life time of building shell in yr"
  )

  p_shareRenBS <- computeShareRen(ltBs, m, cutOffShare) %>%
    select("region", "ttotIn", "ttotOut", "value") %>%
    toModelResolution(m, unfilteredDims = c("ttotIn", "ttotOut"))
  p_shareRenBS <- m$addParameter(
    name = "p_shareRenBS",
    domain = c("region", "ttotIn", "ttotOut"),
    records = p_shareRenBS,
    description = "minimum share of renovation from the building shell reaching end of life"
  )

  p_shareRenBSinit <- computeShareRen(ltBs, m, standingLifeTime = TRUE, vintages = vintages) %>%
    select("region", "vin", "ttotIn", "ttotOut", "value") %>%
    toModelResolution(m, unfilteredDims = c("ttotIn", "ttotOut"))
  p_shareRenBSinit <- m$addParameter(
    name = "p_shareRenBSinit",
    domain = c("region", "vin", "ttotIn", "ttotOut"),
    records = p_shareRenBSinit,
    description = "minimum share of renovation from the building shell of initial stock reaching end of life"
  )


  ## heating system ====

  p_lifeTimeHS <- ltHs %>%
    calc_addVariable(lt = "scale * gamma(1 + 1 / shape)", only.new = TRUE) %>%
    select("hs", "region", "typ", "value") %>%
    toModelResolution(m)
  p_lifeTimeHS <- m$addParameter(
    name = "p_lifeTimeHS",
    domain = c("hs", "region", "typ"),
    records = p_lifeTimeHS,
    description = "life time of heating system in yr"
  )

  # Temporal scope expanded by highly resolved time steps after installation
  timePeriodsConfig <- expandSets(ttotIn = "ttot", ttotOut = "ttot", .m = m)
  timePeriodsHighRes <- expandSets(ttotIn = "ttot", .m = m) %>%
    tidyr::crossing(lt = seq(0, 60, 0.5)) %>%
    mutate(ttotOut = .data$ttotIn + .data$lt) %>%
    select(-"lt")
  timePeriodsFull <- full_join(timePeriodsConfig, timePeriodsHighRes,
                               by = c("ttotIn", "ttotOut"))
  timePeriodsFullInit <- timePeriodsFull %>%
    filter(.data$ttotIn == readSymbol(m, symbol = "tinit"))

  p_shareRenHSfull <- computeShareRen(ltHs, m, cutOffShare, timePeriods = timePeriodsFull) %>%
    select("hs", "region", "typ", "ttotIn", "ttotOut", "value") %>%
    toModelResolution(m, unfilteredDims = c("ttotIn", "ttotOut"))
  invisible(m$addParameter(
    name = "p_shareRenHSfull",
    domain = c("hs", "region", "typ", "ttotIn", "ttotOut"),
    records = p_shareRenHSfull,
    description = "distribution of hs renovation at high time resolution"
  ))

  # Share of heating systems in the standing stock at time ttotIn = tinit - standingLifeTimeHs
  # to be replaced by time ttotOut.
  p_shareRenHSfullInit <- computeShareRen(
    ltHs,
    m,
    standingLifeTime = TRUE,
    timePeriods = timePeriodsFullInit,
    vintages = vintages,
    levels = setdiff(levels, "matched")
  ) %>%
    select("level", "hs", "region", "typ", "vin", "ttotIn", "ttotOut", "value") %>%
    toModelResolution(m, unfilteredDims = c("ttotIn", "ttotOut"))
  invisible(m$addParameter(
    name = "p_shareRenHSfullInit",
    domain = c("level", "hs", "region", "typ", "vin", "ttotIn", "ttotOut"),
    records = p_shareRenHSfullInit,
    description = "distribution of hs renovation for initial stock at high time resolution"
  ))

  p_shareRenHS <- p_shareRenHSfull %>%
    semi_join(timePeriodsConfig, by = c("ttotIn", "ttotOut")) %>%
    toModelResolution(m, unfilteredDims = c("ttotIn", "ttotOut"))
  p_shareRenHS <- m$addParameter(
    name = "p_shareRenHS",
    domain = c("hs", "region", "typ", "ttotIn", "ttotOut"),
    records = p_shareRenHS,
    description = "minimum share of renovation from the heating system reaching end of life"
  )

  if (config[["switches"]][["RUNTYPE"]] != "renCorrect") {
    if (config[["switches"]][["RUNTYPE"]] == "matching") {
      # matching runs get a corridor within which standing heating systems are replaced
      p_shareRenHSinit <- p_shareRenHSfullInit %>%
        semi_join(timePeriodsConfig, by = c("ttotIn", "ttotOut")) %>%
        toModelResolution(m, unfilteredDims = c("ttotIn", "ttotOut"))
    } else {
      # other runs use the result of a matching run
      folder <- switch(config[["switches"]][["RUNTYPE"]],
                       calibration = .normaliseMatchingPath(config[["matchingRun"]]),
                       scenario = config[["calibrationRun"]],
                       stop("Unexpected run type."))
      p_shareRenHSinit <- read.csv(file.path(folder, "shareRenHSinit.csv"),
                                   comment.char = "#") %>%
        toModelResolution(m, unfilteredDims = "ttotIn")
      p_shareRenHSinit <- expandSets("level", "hs", "region", "typ", "vin", ttotIn = "tinit", ttotOut = "ttot", .m = m) %>%
        filter(.data$level == "matched") %>%
        inner_join(vinExists, by = c("vin", ttotIn = "ttot")) %>%
        left_join(p_shareRenHSinit, by = c("level", "hs", "region", "typ", "vin", "ttotIn", ttotOut = "ttot")) %>%
        .explicitZero()
    }
    p_shareRenHSinit <- m$addParameter(
      name = "p_shareRenHSinit",
      domain = c("level", "hs", "region", "typ", "vin", "ttotIn", "ttotOut"),
      records = p_shareRenHSinit,
      description = "minimum share of renovation from the heating system of initial stock reaching end of life"
    )
  }


  # Other ----------------------------------------------------------------------


  ## discount rate ====

  p_discountRate <- readInput("f_discountRate.cs4r", "typ")
  p_discountRate <- expandSets("typ", "ttot", .m = m) %>%
    left_join(p_discountRate, by = "typ")
  m$addParameter(
    name = "p_discountRate",
    domain = c("typ", "ttot"),
    records = p_discountRate,
    description = "discount rate (incl. implicit discount) in 1/yr"
  )


  ## price sensitivity ====

  priceSensBS <- unlist(config[["priceSens"]][["bs"]])
  priceSensBS <- expandSets("var", "region", "loc", "typ", "inc", .m = m) %>%
    filter(.data[["var"]] %in% names(priceSensBS)) %>%
    mutate(value = priceSensBS[.data[["var"]]])
  priceSensBS <- m$addParameter(
    name = "priceSensBS",
    domain = c("var", "region", "loc", "typ", "inc"),
    records = priceSensBS,
    description = "price sensitivity of building shell choice"
  )

  priceSensHS <- unlist(config[["priceSens"]][["hs"]])
  priceSensHS <- expandSets("var", "region", "loc", "typ", "inc", .m = m) %>%
    filter(.data[["var"]] %in% names(priceSensHS)) %>%
    mutate(value = priceSensHS[.data[["var"]]])
  priceSensHS <- m$addParameter(
    name = "priceSensHS",
    domain = c("var", "region", "loc", "typ", "inc"),
    records = priceSensHS,
    description = "price sensitivity of heating system choice"
  )


  ## Delta t for difference quotient in optimization calibration

  if (identical(config[["switches"]][["CALIBRATIONMETHOD"]], "optimization")) {
    invisible(m$addParameter(
      name = "p_diff",
      records = config[["calibrationParameters"]][["deltaDiffQuotient"]],
      description = "Delta used to compute the difference quotient in optimization calibration"
    ))
  }


  ## discrete choice calibration =====

  if (is.character(config[["statusQuoPreference"]])) {
    if (file.exists(config[["statusQuoPreference"]])) {
      p_statusQuoPref <- read.csv(config[["statusQuoPreference"]])
      p_statusQuoPref <- expandSets("hs", "region", .m = m) %>%
        left_join(p_statusQuoPref, by = intersect(colnames(p_statusQuoPref), c("hs", "region")))
    } else {
      stop("The file passed as status quo preference does not exist.")
    }
  } else if (is.numeric(config[["statusQuoPreference"]])) {
    p_statusQuoPref <- expandSets("hs", "region", .m = m) %>%
      mutate(value = config[["statusQuoPreference"]])
  } else {
    stop("Invalid specification of status quo preference. Please check your config file.")
  }

  invisible(m$addParameter(
    name = "p_statusQuoPref",
    domain = c("hs", "region"),
    records = p_statusQuoPref,
    description = "preference for replacing a heating system with the same technology in USD/m2"
  ))


  ## population ====

  # SSP scenario
  popScenario <- config[["popScenario"]]

  # read pop data
  pop <- readInput("f_population.cs4r",
                   c("ttot", "region", "scenario", "loc", "typ"),
                   inputDir)

  if (!isTRUE(popScenario %in% unique(pop[["scenario"]]))) {
    stop("The switch 'popScenario' has to be exatly one out of [",
         paste(unique(pop[["scenario"]]), collapse = ", "), "], not ",
         if (is.null(popScenario)) "NULL" else popScenario, ".")
  }

  pop <- pop %>%
    .filterLevel(popScenario, "popScenario", "scenario") %>%
    toModelResolution(m)

  p_population <- expandSets("region", "loc", "typ", "inc", "ttot", .m = m) %>%
    left_join(pop, by = c("region", "typ", "loc", "ttot"),
              relationship = "many-to-many") %>%
    select("region", "loc", "typ", "inc", "ttot", "value")

  p_population <- m$addParameter(
    name = "p_population",
    domain = c("region", "loc", "typ", "inc", "ttot"),
    records = p_population,
    description = "number of people in million"
  )


  ## floor space per capita ====

  # SSP scenario
  fsScenario <- config[["fsScenario"]]

  # read floor space data
  p_floorPerCap <- readInput("f_floorspacePerCap.cs4r",
                             c("ttot", "region", "scenario", "typ", "loc"),
                             inputDir) %>%
    .filterLevel(fsScenario, "fsScenario", "scenario") %>%
    toModelResolution(m)

  p_floorPerCap <- expandSets("region", "loc", "typ", "inc", "ttot", .m = m) %>%
    left_join(p_floorPerCap, by = c("region", "loc", "typ", "ttot"))

  p_floorPerCap <- m$addParameter(
    name = "p_floorPerCap",
    domain = c("region", "loc", "typ", "inc", "ttot"),
    records = p_floorPerCap,
    description = "floor space per capita in m2"
  )


  ## renovation depth ====
  p_renDepth <- readInput("f_renovationDepth.cs4r", c("bs", "bsr"), inputDir)

  p_renDepth <- expandSets("bs", "bsr", .m = m) %>%
    left_join(p_renDepth, by = c("bs", "bsr")) %>%
    .explicitZero()

  p_renDepth <- m$addParameter(
    name = "p_renDepth",
    domain = c("bs", "bsr"),
    records = p_renDepth,
    description = "renovation depth"
  )


  # status quo share ====

  p_statusQuoShare <- readInput("f_statusQuoShare.cs4r", "hs", inputDir)

  p_statusQuoShare <- expandSets("hs", .m = m) %>%
    left_join(p_statusQuoShare, by = "hs")

  p_statusQuoShare <- m$addParameter(
    name = "p_statusQuoShare",
    domain = c("hs"),
    records = p_statusQuoShare,
    description = "share of heating systems that are identically replaced"
  )


  # share-dependent intang cost parameters ====

  p_factorIntangParams <- config[["shareHpIntangCost"]] %>%
    as.data.frame() %>%
    pivot_longer(everything(), names_to = "params")

  p_factorIntangParams <- m$addParameter(
    name = "p_factorIntangParams",
    domain = c("params"),
    records = p_factorIntangParams,
    description = "parameters for adjustment of heat pump intangible costs"
  )



  # Historic stock -------------------------------------------------------------

  # stock of residential floor space
  p_stockHist <- if (is.null(config[["calibrationRun"]])) {
    readInput("f_buildingStock.cs4r",
              c("ttot", "region", "variable", "typ", "loc", "vin", "hs", "bs"),
              inputDir) %>%
      filter(.data[["variable"]] == "floor") %>%
      select(-"variable") %>%
      mutate(qty = "area")
  } else {
    read.csv(file.path(config[["calibrationRun"]], "stockCalibration.csv")) %>%
      mutate(qty = "area")
  }
  p_stockHist <- expandSets("qty", "bs", "hs", "vin", "region", "loc", "typ",
                            "inc", "ttot", .m = m) %>%
    inner_join(vinExists, by = c("vin", "ttot")) %>%
    left_join(p_stockHist,
              by = setdiff(names(p_stockHist), "value")) %>%
    mutate(value = replace_na(.data[["value"]], 0))

  if (isTRUE(config[["ignoreShell"]])) {
    p_stockHist <- p_stockHist %>%
      group_by(across(-all_of(c("bs", "value")))) %>%
      mutate(value = ifelse(.data[["bs"]] == "low", sum(.data[["value"]]), 0)) %>%
      ungroup()
  }

  p_stockHist <- m$addParameter(
    name = "p_stockHist",
    domain = c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", "ttot"),
    records = p_stockHist,
    description = "historic stock of buildings in million m2"
  )



  # Target trajectory ----------------------------------------------------------


  ## aggregated matching variables ====
  if (identical(config[["switches"]][["RUNTYPE"]], "renCorrect")) {
    # read uncorrected aggregated matching results
    matchingFolder <- file.path(config[["matchingRun"]],
                                "aggregationForCalibration",
                                "uncorrectedAggregation")
    matchingFiles <- list.files(matchingFolder, full.names = TRUE)

    for (file in matchingFiles) {
      var <- sub("(.*)\\.csv$", "\\1", basename(file))
      data <- read.csv(file)
      invisible(m$addParameter(
        name = paste0("p_", var),
        domain = head(names(data), -1),
        records = data
      ))
    }
  }



  # Freeze parameters ----------------------------------------------------------

  freezeParams <- config[["freezeParams"]]
  if (!is.null(freezeParams)) {
    m <- .freezeParameters(m = m,
                           params = freezeParams[["params"]],
                           period = freezeParams[["period"]])
  }



  return(m)
}





#' filter rows with specified entry in column
#'
#' used to select a specifc level or scenario from a data frame with alternative
#' values.
#'
#' @param df data.frame
#' @param lvl value used to select rows
#' @param switchName character, corresponding BRICK switch name (only used for
#'   more informative error message)
#' @param lvlCol character, colname containing \code{lvl}
#' @returns data.frame with selected rows without \code{lvlCol}

.filterLevel <- function(df, lvl, switchName = "", lvlCol = "level") {

  if (!isTRUE(lvl %in% unique(df[[lvlCol]]))) {
    stop("The switch '", switchName, "' has to be exactly one out of [",
         paste(unique(df[[lvlCol]]), collapse = ", "), "], not '",
         if (is.null(lvl)) "NULL" else lvl, "'.")
  }

  df[df[[lvlCol]] == lvl, setdiff(colnames(df), lvlCol)]
}



#' Make vintage characteristic identical
#'
#' fill value column with average across vintages
#'
#' @param x data.frame with brick parameter, requires vin and value columns
#' @returns data.frame with identical structure but averaged values
#'
#' @importFrom dplyr %>% group_by all_of mutate .data ungroup

.makeIdentVin <- function(x) {
  x %>%
    group_by(across(-all_of(c("vin", "value")))) %>%
    mutate(value = mean(.data$value, na.rm = TRUE)) %>%
    ungroup()
}


#' Adjust zero flow costs
#'
#' Reduce zero flow costs by given value and rebalance all costs to obtain non-negative intangible costs.
#'
#' @param df data frame with cost data to be adjusted
#' @param costRed numeric, value by which to decrease the zero flow costs
#'
#' @returns data frame
#'
.adjustCostBalance <- function(df, costRed) {
  if (!is.null(costRed)) {
    df <- df %>%
      mutate(value = .data$value - ifelse(.data$hsr == "0", costRed, 0)) %>%
      group_by(across(-any_of(c("bsr", "hsr", "value")))) %>%
      mutate(value = .data$value - min(.data$value, na.rm = TRUE) + 1E-5) %>%
      ungroup()
  }
  df
}



#' Freeze Parameters
#'
#' Select parameters and freeze them to the value in a selected period
#'
#' @param m gamstransfer Container with parameters
#' @param params character vector of parameter names
#' @param period numeric, period that all values are freezed to; has to be an
#'   element of ttot
#' @returns gams Container
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr %>% .data group_by across all_of mutate

.freezeParameters <- function(m, params, period) {
  if (is.null(period) || !period %in% readSymbol(m, "ttot")) {
    stop("The freeze period has to be an element of ttot.")
  }
  for (paramName in params) {
    param <- m$getSymbols(paramName)[[1]]
    if (!"ttot" %in% param$domain) {
      stop("Can't freeze ", paramName, ". No ttot dimension.")
    }
    records <- param$records %>%
      group_by(across(-all_of(c("ttot", "value")))) %>%
      mutate(value = .data$value[.asNumeric(.data$ttot) == period]) %>%
      ungroup()
    param$setRecords(records)
  }
  m
}
