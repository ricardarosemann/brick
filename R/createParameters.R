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

createParameters <- function(m, config, inputDir) {

  # restore set objects
  ttot <- readSymbol(m, "ttot")
  ttotNum <- as.numeric(as.character(ttot))
  t <- periodFromConfig(config, "t")
  vinExists <- readSymbol(m, "vinExists", stringAsFactor = FALSE)
  stateR <- c("bsr", "hsr")
  state <- c("bs", "hs")



  # Periods --------------------------------------------------------------------

  dt <- diff(ttotNum)
  dt <- data.frame(ttot = ttotNum, value = c(dt[1], dt))
  p_dt <- m$addParameter(
    name = "p_dt",
    domain = "ttot",
    records = dt,
    description = "length of time step in yr"
  )

  vintages <- getBrickMapping("vintage.csv")
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


  ## construction ====

  p_specCostConTang <- readInput("f_costConstruction.cs4r",
                                 c("ttot", "reg", "bs", "hs", "typ"),
                                 inputDir) %>%
    toModelResolution(m)
  p_specCostCon <- expandSets("cost", "bs", "hs", "reg", "loc", "typ", "inc", "ttot",
                              .m = m)
    p_specCostConTang <- p_specCostCon %>%
    filter(.data[["cost"]] == "tangible") %>%
    left_join(p_specCostConTang,
              by = c("bs", "hs", "reg", "typ", "ttot"))
  if (config[["switches"]][["CALIBRATIONINPUT"]] == "randomCost") {
    needNewFile <- TRUE
    randDev <- config[["parameters"]][["randDev"]]
    fileNameRandCost <- paste0("inputRandCost", ifelse(config[["minimal"]], "Min", ""), randDev, ".gdx")
    if (file.exists(fileNameRandCost)) {
      input <- gamstransfer::Container$new(fileNameRandCost)
      # input$read("inputRandCost.gdx", "p_specCostCon")
      p_specCostConTangRead <- readSymbol(input, symbol = "p_specCostCon") %>%
        filter(.data[["cost"]] == "tangible") %>%
        filter(.data[["ttot"]] %in% ttotNum)
      if (all(ttotNum %in% p_specCostConTangRead[["ttot"]])) {
        needNewFile <- FALSE
        p_specCostConTang <- p_specCostConTangRead
        message(paste("Randomized costs read from", fileNameRandCost, "."))
      }
    }
    if (needNewFile) {
      p_specCostConTang <- p_specCostConTang %>%
        mutate(value = .data[["value"]] * max(0, (runif(1, 1 - randDev / 100, 1 + randDev / 100))))
      message("Costs are randomized.")
    }
  } else if (!isFALSE(config[["switches"]][["CALIBRATIONHICAP"]])) {
    p_specCostConTang <- p_specCostConTang %>%
      mutate(value = ifelse(.data[["hs"]] == config[["switches"]][["CALIBRATIONHICAP"]],
                            10 * .data[["value"]], .data[["value"]]))
  }

  if (is.null(config[["calibGdx"]])) {
    p_specCostConIntang <- p_specCostCon %>%
      filter(.data[["cost"]] == "intangible") %>%
      addAssump(brick.file("assump/costIntangCon.csv"))
  } else {
    # TODO: Put this in a function
    calibInput <- gamstransfer::Container$new(config[["calibGdx"]])
    p_specCostConCalib <- readSymbol(calibInput, "p_specCostCon") %>%
      filter(.data[["cost"]] == "intangible")
    p_calibLast <- p_specCostConCalib %>%
      filter(ttot == max(ttot)) %>%
      rename(valueLast = .data[["value"]], ttotLast = .data[["ttot"]]) %>%
      tidyr::crossing(ttot = ttotNum)
    p_specCostConIntang <- p_specCostCon %>%
      filter(.data[["cost"]] == "intangible") %>%
      left_join(p_specCostConCalib) %>%
      left_join(p_calibLast) %>%
      mutate(value = ifelse(is.na(.data[["value"]]), .data[["valueLast"]], .data[["value"]])) %>%
      select(-"valueLast", -"ttotLast")
  }
  p_specCostCon <- rbind(p_specCostConTang, p_specCostConIntang)
  p_specCostCon <- m$addParameter(
    "p_specCostCon",
    c("cost", state, "reg", "loc", "typ", "inc", "ttot"),
    p_specCostCon,
    description = "floor-space specific construction cost in USD/m2"
  )


  ## renovation ====

  p_specCostRenTang <- readInput("f_costRenovation.cs4r",
                                 c("ttot", "reg", "bs", "hs", "bsr", "hsr",
                                   "typ", "vin"),
                                 inputDir) %>%
    toModelResolution(m)
  p_specCostRen <- expandSets("cost", "bs", "hs", "bsr", "hsr", "vin", "reg",
                              "loc", "typ", "inc", "ttot", .m = m)
  p_specCostRenTang <- p_specCostRen %>%
    filter(.data[["cost"]] == "tangible") %>%
    left_join(p_specCostRenTang,
              by = c("ttot", "reg", "bs", "hs", "bsr", "hsr", "typ", "vin"))

  if (config[["switches"]][["CALIBRATIONINPUT"]] == "randomCost") {
    if (!needNewFile) {
      p_specCostRenTang <- readSymbol(input, symbol = "p_specCostRen") %>%
        filter(.data[["cost"]] == "tangible") %>%
        filter(.data[["ttot"]] %in% ttotNum)
    } else {
      p_specCostRenTang <- p_specCostRenTang %>%
        group_by(across(all_of(c("bsr", "hsr", "vin")))) %>%
        mutate(value = .data[["value"]] + max(0, dplyr::first(.data[["value"]])
                                              * (runif(1, -randDev / 100, randDev / 100)))) %>%
        ungroup()
    }
  } else if (!isFALSE(config[["switches"]][["CALIBRATIONHICAP"]])) {
    p_specCostRenTang <- p_specCostRenTang %>%
      mutate(value = ifelse(.data[["hsr"]] == config[["switches"]][["CALIBRATIONHICAP"]],
                            10 * .data[["value"]], .data[["value"]]))
  }

  if (is.null(config[["calibGdx"]])) {
    p_specCostRenIntang <- p_specCostRen %>%
      filter(.data[["cost"]] == "intangible") %>%
      addAssump(brick.file("assump/costIntangRen.csv"))
  } else {
    # TODO: Put this in a function
    vinCalib <- readSymbol(calibInput, "vinCalib")[["vin"]]
    p_specCostRenCalib <- readSymbol(calibInput, "p_specCostRen") %>%
      filter(.data[["cost"]] == "intangible")
    p_calibLast <- p_specCostRenCalib %>%
      filter(ttot == max(ttot)) %>%
      rename(valueLast = .data[["value"]], ttotLast = .data[["ttot"]]) %>%
      tidyr::crossing(ttot = ttotNum)
    p_specCostRenIntang <- p_specCostRen %>%
      filter(.data[["cost"]] == "intangible") %>%
      left_join(p_specCostRenCalib) %>%
      left_join(p_calibLast) %>%
      mutate(value = ifelse(is.na(.data[["value"]]), .data[["valueLast"]], .data[["value"]]),
             value = ifelse(!.data[["vin"]] %in% vinCalib, 0, .data[["value"]])) %>%
      select(-"valueLast", -"ttotLast")
  }
  p_specCostRen <- rbind(p_specCostRenTang, p_specCostRenIntang)
  p_specCostRen <- m$addParameter(
    "p_specCostRen",
    c("cost", state, stateR, "vin", "reg", "loc", "typ", "inc", "ttot"),
    p_specCostRen,
    description = "floor-space specific renovation cost in USD/m2"
  )

  if (config[["switches"]][["CALIBRATIONINPUT"]] == "randomCost" && needNewFile) {
    m$write(fileNameRandCost, symbols = c("p_specCostCon", "p_specCostRen"))
    message(paste("New file", fileNameRandCost, "containing the time periods",
                  paste(ttotNum, collapse = ", "), "written. \n"))
  }


  ## operation ====

  # scenario assumptions
  carbonPrice <- config[["carbonPrice"]]
  carrierPriceLevel <- config[["carrierPrices"]]
  carrierEmiLevel   <- config[["carrierEmi"]]

  ### carbon price ####
  p_carbonPrice <- if (is.null(carbonPrice)) {
    data.frame(ttot = ttotNum, value = 0)
  } else {
    carbonPrice %>%
      listToDf() %>%
      guessColnames(m) %>%
      toModelResolution(m)
  }
  p_carbonPrice <- m$addParameter(
    name = "p_carbonPrice",
    domain = "ttot",
    records = p_carbonPrice,
    description = "Carbon price in USD/t_CO2eq"
  )

  ### energy carrier prices and emission intensities ####
  carrierData <- readInput("f_carrierPrices.cs4r",
                           c("ttot", "reg", "variable", "unit", "carrier", "level"),
                           inputDir)

  p_carrierPrice <- carrierData %>%
    filter(.data[["variable"]] == "price") %>%
    .filterLevel(carrierPriceLevel, "carrierPrice") %>%
    select("carrier", "reg", "ttot", "value") %>%
    toModelResolution(m)

  p_carrierEmi <- carrierData %>%
    filter(.data[["variable"]] == "emi") %>%
    .filterLevel(carrierEmiLevel, "carrierEmi") %>%
    select("carrier", "reg", "ttot", "value") %>%
    toModelResolution(m)
  p_carrierEmi <- m$addParameter(
    name = "p_carrierEmi",
    domain = c("carrier", "reg", "ttot"),
    records = p_carrierEmi,
    description = "energy carrier emission intensity in t_CO2/kWh"
  )

  ### useful energy demand for space heating ####
  p_ueDemand <- readInput("f_ueDemand.cs4r",
                          c("reg", "typ", "vin", "bs", "value"),
                          inputDir) %>%
    select("bs", "vin", "reg", "typ", "value") %>%
    toModelResolution(m)

  ### FE-to-UE-efficiency of heating systems ####
  p_eff <- readInput("f_heatingEfficiency.cs4r",
                     c("ttot", "reg", "hs", "typ", "value"),
                     inputDir) %>%
    select("hs", "reg", "typ", "ttot", "value") %>%
    toModelResolution(m)

  ### Add carrier prices to the gams object
  # If artificially modifying operation costs, require ue and efficiency
  # to restore previous behavior.
  if (!isFALSE(config[["switches"]][["CALIBRATIONLOWOP"]])) {
    hsCarrierMap <- getBrickMapping("heatingSystem.csv") %>%
      select("hs", "carrier")

    p_carrierPrice <- p_carrierPrice %>%
      left_join(hsCarrierMap, by = c("carrier")) %>%
      left_join(p_ueDemand %>%
                  rename(ue = "value"),
                by = c("reg")) %>%
      left_join(p_eff %>%
                  rename(eff = "value"),
                by = c("hs", "reg", "typ", "ttot")) %>%
      mutate(value = ifelse(.data[["hs"]] == config[["switches"]][["CALIBRATIONLOWOP"]],
                            0.001 * .data[["eff"]] / .data[["ue"]],
                            .data[["value"]])) %>%
      group_by(across(all_of(c("carrier", "reg", "ttot")))) %>%
      summarise(value = mean(.data[["value"]], na.rm = TRUE), .groups = "drop")
  }

  # Store operational cost related parameters in the gamstransfer object
  p_carrierPrice <- m$addParameter(
    name = "p_carrierPrice",
    domain = c("carrier", "reg", "ttot"),
    records = p_carrierPrice,
    description = "final energy carrier price in USD/kWh"
  )

  p_ueDemand <- m$addParameter(
    name = "p_ueDemand",
    domain = c("bs", "vin", "reg", "typ"),
    records = p_ueDemand,
    description = "floor-space specific useful energy demand for space heating in kWh/yr/m2"
  )

  p_eff <- m$addParameter(
    name = "p_eff",
    domain = c("hs", "reg", "typ", "ttot"),
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
  lt   <- readInput("f_lifetimeBuilding.cs4r",      c("reg", "typ", "variable"),       inputDir)
  ltBs <- readInput("f_lifetimeBuildingShell.cs4r", c("reg", "variable"),              inputDir)
  ltHs <- readInput("f_lifetimeHeatingSystem.cs4r", c("reg", "typ", "hs", "variable"), inputDir)

  # shift scale parameter of Weibull distribution
  ltHsShift <- config[["ltHsShift"]]
  if (!is.null(ltHsShift)) {
    ltHsShift <- data.frame(hs = names(ltHsShift),
                            variable = "scale",
                            shift = as.numeric(ltHsShift))
    ltHs <- ltHs %>%
      left_join(ltHsShift, by = c("hs", "variable")) %>%
      mutate(value = .data[["value"]] + replace_na(.data[["shift"]], 0)) %>%
      select(-"shift")
  }

  # Calculate share of buildings that need to be renovated or demolished between
  # given time steps assuming a Weibull distribution of the technology life time.
  # Optionally pass the prior standing life time; adjust the share to subtract demolitions
  # during the prior standing life time.
  shareRen <- function(ttot2, params, standingLifeTime = 0) {

    expandSets(ttot2 = "ttot", "ttot", .m = m) %>%
      left_join(readSymbol(p_dt) %>%
                  rename(dt = "value"),
                by = c(ttot2 = "ttot")) %>%
      cross_join(params) %>%
      pivot_wider(names_from = "variable") %>%
      # pweibull(0) = 0, so for standingLifeTime = 0 we have value = pweibull(lt)
      mutate(lt = .data[["ttot"]] - .data[["ttot2"]]
             + .data[["dt"]] / 2 + standingLifeTime,
             value = (pweibull(.data[["lt"]], .data[["shape"]], .data[["scale"]])
                      - pweibull(standingLifeTime, .data[["shape"]], .data[["scale"]]))
             / (1 - pweibull(standingLifeTime, .data[["shape"]], .data[["scale"]])),
             value = ifelse(.data[["value"]] > cutOffShare,
                            1, .data[["value"]])) %>%
      select(-"shape", -"scale", -"dt", -"lt")
  }

  ## building ====

  # parameter for monitoring purposes
  p_probDem <- expandSets("reg", "typ", ttot2 = "ttot", "ttot", .m = m) %>%
    filter(.data[["ttot2"]] >= .data[["ttot"]]) %>%
    left_join(lt, by = c("reg", "typ"), relationship = "many-to-many") %>%
    pivot_wider(names_from = "variable") %>%
    mutate(value = pweibull(.data[["ttot2"]] - .data[["ttot"]],
                            .data[["shape"]], .data[["scale"]])) %>%
    select(-"shape", -"scale")
  p_probDem <- m$addParameter(
    name = "p_probDem",
    domain = c("reg", "typ", "ttot2", "ttot"),
    records = p_probDem,
    description = "probability of a building having reached its end of life"
  )

  # share of stock from previous time step that has to be demolished as it
  # reaches its end of life
  p_shareDem <- expandSets("vin", "reg", "typ", "ttot", .m = m) %>%
    left_join(vintages, by = "vin") %>%
    inner_join(vinExists,
               by = c("vin", "ttot")) %>%
    left_join(lt, by = c("reg", "typ"), relationship = "many-to-many") %>%
    pivot_wider(names_from = "variable") %>%
    mutate(tcon = (.data[["from"]] + pmin(.data[["ttot"]], .data[["to"]])) / 2,
           p = pweibull(.data[["ttot"]] - .data[["tcon"]],
                        .data[["shape"]], .data[["scale"]])) %>%
    left_join(readSymbol(p_dt) %>%
                rename(dt = "value"),
              by = "ttot") %>%
    group_by(across(all_of(c("vin", "reg", "typ")))) %>%
    arrange(.data[["ttot"]]) %>%
    mutate(value = c(0, diff(.data[["p"]])) /
             (1 - lag(.data[["p"]], default = 0)) / .data[["dt"]]) %>%
    select("vin", "reg", "typ", "ttot", "value")
  p_shareDem <- m$addParameter(
    name = "p_shareDem",
    domain = c("vin", "reg", "typ", "ttot"),
    records = p_shareDem,
    description = "minimum share of demolition at end of life"
  )

  ## building shell ====

  p_lifeTimeBS <- ltBs %>%
    calc_addVariable(lt = "scale * gamma(1 + 1 / shape)", only.new = TRUE) %>%
    select("reg", "value") %>%
    toModelResolution(m)
  p_lifeTimeBS <- m$addParameter(
    name = "p_lifeTimeBS",
    domain = "reg",
    records = p_lifeTimeBS,
    description = "life time of heating system in yr"
  )

  p_shareRenBS <- shareRen(ttot, ltBs) %>%
    select("reg", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenBS <- m$addParameter(
    name = "p_shareRenBS",
    domain = c("reg", "ttot2", "ttot"),
    records = p_shareRenBS,
    description = "minimum share of renovation from the building shell reaching end of life"
  )

  # assumption: average life time of initial stock of building shells: 12 years
  p_shareRenBSinit <- shareRen(ttot, ltBs, standingLifeTime = 12) %>%
    select("reg", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenBSinit <- m$addParameter(
    name = "p_shareRenBSinit",
    domain = c("reg", "ttot2", "ttot"),
    records = p_shareRenBSinit,
    description = "minimum share of renovation from the building shell of initial stock reaching end of life"
  )

  ## heating system ====

  p_lifeTimeHS <- ltHs %>%
    calc_addVariable(lt = "scale * gamma(1 + 1 / shape)", only.new = TRUE) %>%
    select("hs", "reg", "typ", "value") %>%
    toModelResolution(m)
  p_lifeTimeHS <- m$addParameter(
    name = "p_lifeTimeHS",
    domain = c("hs", "reg", "typ"),
    records = p_lifeTimeHS,
    description = "life time of heating system in yr"
  )

  p_shareRenHS <- shareRen(ttot, ltHs) %>%
    select("hs", "reg", "typ", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenHS <- m$addParameter(
    name = "p_shareRenHS",
    domain = c("hs", "reg", "typ", "ttot2", "ttot"),
    records = p_shareRenHS,
    description = "minimum share of renovation from the heating system reaching end of life"
  )

  # assumption: average life time of initial stock of heating systems: 6 years
  p_shareRenHSinit <- shareRen(ttot, ltHs, standingLifeTime = 6) %>%
    select("hs", "reg", "typ", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenHSinit <- m$addParameter(
    name = "p_shareRenHSinit",
    domain = c("hs", "reg", "typ", "ttot2", "ttot"),
    records = p_shareRenHSinit,
    description = "minimum share of renovation from the heating system of initial stock reaching end of life"
  )


  # Other ----------------------------------------------------------------------


  ## discount factor ====

  p_interestRate <- expandSets("typ", "ttot", .m = m) %>%
    mutate(value = c(SFH = 0.21, MFH = 0.25)[.data[["typ"]]]) # Giraudet et al. 2012
  p_interestRate <- m$addParameter(
    name = "p_interestRate",
    domain = c("typ", "ttot"),
    records = p_interestRate,
    description = "interest rate (incl. implicit) w.r.t. t0 in 1/yr"
  )


  ## population ====

  # SSP scenario
  popScenario <- config[["popScenario"]]

  # read pop data
  pop <- readInput("f_population.cs4r",
                   c("ttot", "reg", "scenario", "loc", "typ"),
                   inputDir)

  if (!isTRUE(popScenario %in% unique(pop[["scenario"]]))) {
    stop("The switch 'popScenario' has to be exatly one out of [",
         paste(unique(pop[["scenario"]]), collapse = ", "), "], not ",
         if (is.null(popScenario)) "NULL" else popScenario, ".")
  }

  pop <- pop %>%
    .filterLevel(popScenario, "popScenario", "scenario") %>%
    toModelResolution(m)

  p_population <- expandSets("reg", "loc", "typ", "inc", "ttot", .m = m) %>%
    left_join(pop, by = c("reg", "typ", "loc", "ttot"),
              relationship = "many-to-many") %>%
    select("reg", "loc", "typ", "inc", "ttot", "value")

  p_population <- m$addParameter(
    name = "p_population",
    domain = c("reg", "loc", "typ", "inc", "ttot"),
    records = p_population,
    description = "number of people in million"
  )


  ## floor space per capita ====

  # SSP scenario
  fsScenario <- config[["fsScenario"]]

  # read floor space data
  p_floorPerCap <- readInput("f_floorspacePerCap.cs4r",
                             c("ttot", "reg", "scenario", "typ", "loc"),
                             inputDir) %>%
    .filterLevel(fsScenario, "fsScenario", "scenario") %>%
    toModelResolution(m)

  p_floorPerCap <- expandSets("reg", "loc", "typ", "inc", "ttot", .m = m) %>%
    left_join(p_floorPerCap, by = c("reg", "loc", "typ", "ttot"))

  p_floorPerCap <- m$addParameter(
    name = "p_floorPerCap",
    domain = c("reg", "loc", "typ", "inc", "ttot"),
    records = p_floorPerCap,
    description = "floor space per capita in m2"
  )


  # Stock ----------------------------------------------------------------------

  # stock of residential floor space
  if (config[["switches"]][["RUNTYPE"]] == "scenario"
      || config[["switches"]][["CALIBRATIONINPUT"]] == "data") {
    p_stockHist <- readInput("f_buildingStock.cs4r",
                             c("ttot", "reg", "variable", "typ", "loc", "vin", "hs"),
                             inputDir) %>%
      filter(.data[["variable"]] == "floor") %>%
      select(-"variable") %>%
      mutate(bs  = "low",
             qty = "area")
    p_stockHist <- expandSets("qty", "bs", "hs", "vin", "reg", "loc", "typ",
                              "inc", "ttot", .m = m) %>%
      filter(.data[["qty"]] == "area") %>%
      inner_join(vinExists, by = c("vin", "ttot")) %>%
      left_join(p_stockHist,
                by = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "ttot")) %>%
      mutate(value = replace_na(.data[["value"]], 0))
    message("Stock data read from cs4r file.")

    # Scale up stock in minimal scenario/data runs
    if (config[["minimal"]]) {
      stockSum <- p_stockHist %>%
        group_by(across(all_of(c("reg", "loc", "typ", "inc", "ttot")))) %>%
        dplyr::summarize(stock = sum(.data[["value"]])) %>%
        mutate(ttot = factor(ttot, levels = levels(p_population$records[["ttot"]]), ordered = TRUE))
      housingDem <- full_join(p_population$records %>%
                                rename(pop = "value"),
                              p_floorPerCap$records %>%
                                rename(floor = "value"),
                              by = c("reg", "loc", "typ", "inc", "ttot")) %>%
        mutate(dem = .data[["pop"]] * .data[["floor"]]) %>%
        full_join(stockSum, by = c("reg", "loc", "typ", "inc", "ttot")) %>%
        mutate(scale = ifelse(.data[["stock"]] != 0, .data[["dem"]] / .data[["stock"]], 0))

      p_stockHist <- p_stockHist %>%
        mutate(ttot = factor(ttot, levels = levels(p_population$records[["ttot"]]), ordered = TRUE)) %>%
        left_join(housingDem, by = c("reg", "loc", "typ", "inc", "ttot")) %>%
        mutate(value = .data[["value"]] * .data[["scale"]]) %>%
        select(-"pop", -"floor", -"dem", -"stock", -"scale")
    }
  } else {
    fileNameHist <- paste0("inputHist", ifelse(config[["minimal"]], "Min", ""), as.character(max(ttotNum)), ".gdx")
    fileNameRandHist <- paste0("inputRandHist", ifelse(config[["minimal"]], "Min", ""),
                               as.character(max(ttotNum)), ".gdx")
    input <- gamstransfer::Container$new()

    if (file.exists(fileNameHist)) {
      input$read(fileNameHist, "v_stock")
    } else {
      stop(paste("Required file for historical stock", fileNameHist, "does not exist. Stopping."))
    }

    p_stockHist <- input["v_stock"]$records %>%
      select(-"marginal", -"lower", -"upper", -"scale") %>%
      rename(value = "level") %>%
      filter(.data[["ttot"]] %in% ttotNum)
    if (config[["switches"]][["CALIBRATIONINPUT"]] == "randomTarget"
        && config[["switches"]][["CALIBRATIONTYPE"]] == "stocks") {
      p_stockHist <- p_stockHist %>%
        mutate(value = .data[["value"]] * (runif(1, 0.95, 1.05)))
      message("Historic stock data generated by random perturbation.")
    } else {
      message(paste("Historic stock data read from", fileNameHist))
    }

    p_stockHist <- expandSets("qty", "bs", "hs", "vin", "reg", "loc", "typ",
                              "inc", "ttot", .m = m) %>%
      inner_join(vinExists, by = c("vin", "ttot")) %>%
      mutate(ttot = factor(.data[["ttot"]], levels = levels(p_stockHist[["ttot"]]))) %>%
      filter(.data[["qty"]] == "area") %>%
      left_join(p_stockHist,
                by = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", "ttot"))
  }

  # flows (for calibration)
  if (config[["switches"]][["RUNTYPE"]] == "calibration"
      && config[["switches"]][["CALIBRATIONINPUT"]] != "data") {

    fileNameHist <- paste0("inputHist", ifelse(config[["minimal"]], "Min", ""), as.character(max(ttotNum)), ".gdx")
    fileNameRandHist <- paste0("inputRandHist", ifelse(config[["minimal"]], "Min", ""),
                               as.character(max(ttotNum)), ".gdx")

    if (file.exists(fileNameRandHist)
        && config[["switches"]][["CALIBRATIONINPUT"]] == "randomTarget"
        && config[["switches"]][["CALIBRATIONTYPE"]] == "flows") {
      input <- gamstransfer::Container$new()

      input$read(fileNameRandHist, "p_constructionHist")

      p_constructionHist <- input["p_constructionHist"]$records %>%
        filter(.data[["ttot"]] %in% t)

      input$read(fileNameRandHist, "p_renovationHist")

      p_renovationHist <- input["p_renovationHist"]$records %>%
        filter(.data[["ttot"]] %in% t)

      message(paste("Reading random flow data from", fileNameRandHist))
    } else {

      if (!file.exists(fileNameHist)) {
        stop(paste("Required file for historical stock", fileNameHist, "does not exist. Stopping."))
      }
      input <- gamstransfer::Container$new()
      input$read(fileNameHist, "v_construction")

      p_constructionHist <- input["v_construction"]$records %>%
        select(-"marginal", -"lower", -"upper", -"scale") %>%
        rename(value = "level") %>%
        filter(.data[["ttot"]] %in% t)
      if (config[["switches"]][["CALIBRATIONINPUT"]] == "randomTarget"
          && config[["switches"]][["CALIBRATIONTYPE"]] == "flows") {
        p_constructionHist <- p_constructionHist %>%
          mutate(value = .data[["value"]] * (runif(1, 0.80, 1.20)))
      }

      input$read(fileNameHist, "v_renovation")

      p_renovationHist <- input["v_renovation"]$records %>%
        select(-"marginal", -"lower", -"upper", -"scale") %>%
        rename(value = "level") %>%
        filter(.data[["ttot"]] %in% t)
      if (config[["switches"]][["CALIBRATIONINPUT"]] == "randomTarget"
          && config[["switches"]][["CALIBRATIONTYPE"]] == "flows") {
        p_renovationHist <- p_renovationHist %>%
          mutate(value = .data[["value"]] * runif(1, 0.80, 1.20))
        message("Historic flow data generated by random perturbation.")
      } else {
        message(paste("Historic flow data read from", fileNameHist))
      }

    }

    p_constructionHist <- expandSets("qty", "bs", "hs", "reg", "loc", "typ",
                                     "inc", "ttot", .m = m) %>%
      filter(.data[["qty"]] == "area",
             .data[["ttot"]] %in% t) %>%
      mutate(ttot = factor(.data[["ttot"]], levels = levels(p_constructionHist[["ttot"]]))) %>%
      left_join(p_constructionHist,
                by = c("qty", "bs", "hs", "reg", "loc", "typ", "inc", "ttot"))

    p_constructionHist <- m$addParameter(
      "p_constructionHist",
      c("qty", "bs", "hs", "reg", "loc", "typ", "inc", "ttot"),
      p_constructionHist,
      description = "Historic construction flow in million m2"
    )

    p_renovationHist <- expandSets("qty", "bs", "hs", "bsr", "hsr", "vin", "reg",
                                   "loc", "typ", "inc", "ttot", .m = m) %>%
      inner_join(vinExists, by = c("vin", "ttot")) %>%
      filter(.data[["qty"]] == "area",
             .data[["ttot"]] %in% periodFromConfig(config, "t")) %>%
      mutate(ttot = factor(.data[["ttot"]], levels = levels(p_renovationHist[["ttot"]]))) %>%
      left_join(p_renovationHist,
                by = c("qty", "bs", "hs", "bsr", "hsr", "vin", "reg", "loc", "typ", "inc", "ttot"))

    p_renovationHist <- m$addParameter(
      "p_renovationHist",
      c("qty", "bs", "hs", "bsr", "hsr", "vin", "reg", "loc", "typ", "inc", "ttot"),
      p_renovationHist,
      description = "Historic renovation flow in million m2"
    )

    if (!file.exists(fileNameRandHist)
        && config[["switches"]][["CALIBRATIONINPUT"]] == "randomTarget") {
      m$write(fileNameRandHist, symbols = c("p_constructionHist", "p_renovationHist"))
    }
  }

  p_stockHist <- m$addParameter(
    "p_stockHist",
    c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", "ttot"),
    p_stockHist,
    description = "historic stock of buildings in million m2"
  )


  # Price sensitivity ---------------------------------------------------
  allPriceSensHS <- c(seq(200, 1000, 100), seq(2000, 10000, 1000))

  allPriceSensHS <- expandSets("iteration", .m = m) %>%
    mutate(value = allPriceSensHS[1:config[["parameters"]][["iteration"]]])

  allPriceSensHS <- m$addParameter(
    "allPriceSensHS",
    "iteration",
    records = allPriceSensHS,
    description = "Different price sensitivities"
  )

  priceSensHS <- expandSets("flow", "reg", "loc", "typ", "inc", .m = m) %>%
    mutate(value = ifelse(.data[["flow"]] == "construction", 0.01, 0.008))

  priceSensHS <- m$addParameter(
    "priceSensHS",
    c("flow", "reg", "loc", "typ", "inc"),
    priceSensHS,
    description = "price sensitivity of heating system choice"
  )

  # Calibration parameters ------------------------------------------------
  if (config[["switches"]][["RUNTYPE"]] == "calibration") {
    alpha <- config[["parameters"]][["alpha"]]

    p_alphaL <- m$addParameter(
      "p_alphaL",
      records = alpha,
      description = "Lower bound of alpha before Armijo backtracking in calibration"
    )

    if (is.null(config[["parameters"]][["diff"]])) {
      p_diff <- alpha / 100
    } else {
      p_diff <- config[["parameters"]][["diff"]]
    }

    p_diff <- m$addParameter(
      "p_diff",
      records = p_diff,
      description = "Difference to compute the difference quotient"
    )

    p_beta <- m$addParameter(
      "p_beta",
      records = 0.5,
      description = "Factor in Armijo backtracking"
    )

    p_sigma <- m$addParameter(
      "p_sigma",
      records = 0.01,
      description = "Factor in Armijo condition"
    )
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
