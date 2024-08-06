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


  # Price sensitivity ----------------------------------------------------------

  invisible(m$addParameter(
    name = "priceSensHs",
    records = config[["priceSensHs"]],
    description = "price sensitivity of heating system choice"
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
  p_specCostConIntang <- p_specCostCon %>%
    filter(.data[["cost"]] == "intangible") %>%
    addAssump(brick.file("assump", "costIntangCon.csv"))
  p_specCostCon <- rbind(p_specCostConTang, p_specCostConIntang)
  if ("construction" %in% config[["costMod"]][["costType"]]) {
    p_specCostCon <- modifyCosts(p_specCostCon, config[["costMod"]], periodFromConfig(config, "t0"))
  }
  p_specCostCon <- m$addParameter(
    name = "p_specCostCon",
    domain = c("cost", state, "reg", "loc", "typ", "inc", "ttot"),
    records = p_specCostCon,
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
  p_specCostRenIntang <- p_specCostRen %>%
    filter(.data[["cost"]] == "intangible") %>%
    addAssump(brick.file("assump", "costIntangRen.csv"))
  p_specCostRen <- rbind(p_specCostRenTang, p_specCostRenIntang)
  if ("renovation" %in% config[["costMod"]][["costType"]]) {
    p_specCostRen <- modifyCosts(p_specCostRen, config[["costMod"]], periodFromConfig(config, "t0"))
  }
  p_specCostRen <- m$addParameter(
    name = "p_specCostRen",
    domain = c("cost", state, stateR, "vin", "reg", "loc", "typ", "inc", "ttot"),
    records = p_specCostRen,
    description = "floor-space specific renovation cost in USD/m2"
  )


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
  if ("carrier" %in% config[["costMod"]][["costType"]]) {
    p_carrierPrice <- modifyCosts(p_carrierPrice, config[["costMod"]], periodFromConfig(config, "t0"))
  }
  p_carrierPrice <- m$addParameter(
    name = "p_carrierPrice",
    domain = c("carrier", "reg", "ttot"),
    records = p_carrierPrice,
    description = "final energy carrier price in USD/kWh"
  )

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
  p_ueDemand <- m$addParameter(
    name = "p_ueDemand",
    domain = c("bs", "vin", "reg", "typ"),
    records = p_ueDemand,
    description = "floor-space specific useful energy demand for space heating in kWh/yr/m2"
  )

  ### FE-to-UE-efficiency of heating systems ####
  p_eff <- readInput("f_heatingEfficiency.cs4r",
                     c("ttot", "reg", "hs", "typ", "value"),
                     inputDir) %>%
    select("hs", "reg", "typ", "ttot", "value") %>%
    toModelResolution(m)
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

  # calculate share of buildings that need to be renovated or demolished between
  # given time steps assuming a Weibull distribution of the technology life time
  shareRen <- function(ttot2, params, standingLifeTime = 0) {

    expandSets(ttot2 = "ttot", "ttot", .m = m) %>%
      left_join(readSymbol(p_dt) %>%
                  rename(dt = "value"),
                by = c(ttot2 = "ttot")) %>%
      cross_join(params) %>%
      pivot_wider(names_from = "variable") %>%
      mutate(lt = .data[["ttot"]] - .data[["ttot2"]]
             + .data[["dt"]] / 2 + standingLifeTime,
             value = pweibull(.data[["lt"]], .data[["shape"]], .data[["scale"]]),
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
  p_stockHist <- readInput("f_buildingStock.cs4r",
                           c("ttot", "reg", "variable", "typ", "loc", "vin", "hs"),
                           inputDir) %>%
    filter(.data[["variable"]] == "floor") %>%
    select(-"variable") %>%
    mutate(bs  = "low",
           qty = "area")
  p_stockHist <- expandSets("qty", "bs", "hs", "vin", "reg", "loc", "typ",
                            "inc", "ttot", .m = m) %>%
    inner_join(vinExists, by = c("vin", "ttot")) %>%
    left_join(p_stockHist,
              by = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "ttot")) %>%
    mutate(value = replace_na(.data[["value"]], 0))

  p_stockHist <- m$addParameter(
    name = "p_stockHist",
    domain = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "inc", "ttot"),
    records = p_stockHist,
    description = "historic stock of buildings in million m2"
  )


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
