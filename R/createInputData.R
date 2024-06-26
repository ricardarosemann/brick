#' Create input data
#'
#' Create a complete set of input data for the gams optimisation.
#'
#' This function reads static input data according to the input data revision in
#' the config and creates all required sets and parameters for the gams
#' optimisation depending on the switches in the config.
#'
#' @author Robin Hasse
#'
#' @param path character vector with folders to write input data into
#' @param config named list with run configuration
#' @param overwrite logical, should existing input.gdx be overwritten?
#'
#' @importFrom quitte calc_addVariable
#' @importFrom tidyr complete pivot_wider replace_na
#' @importFrom stats pweibull
#' @importFrom utils head
#' @importFrom dplyr %>% mutate select group_by filter ungroup arrange left_join
#'   .data rename lag all_of across inner_join everything cross_join
#' @importFrom gamstransfer Container

createInputData <- function(path,
                            config,
                            overwrite = FALSE) {


  # PREPARE --------------------------------------------------------------------

  ## check input ====

  # break down multiple paths into single function calls
  # TODO: This could be parallelised
  if (length(path) > 1) {
    return(lapply(path, createInputData,
                  overwrite = overwrite))
  }

  # check file path
  inputFilePath <- file.path(path, "input.gdx")
  if (file.exists(inputFilePath)) {
    if (overwrite) {
      warning("Input file '", inputFilePath, "' overwritten.")
    } else {
      stop("Input file '", inputFilePath,
           "' cannot be created as it already exists.")
    }
  }


  ## load madrat input data ====

  loadReturn <- loadMadratData(config)
  inputDir <- loadReturn[["inputDir"]]
  regionmapping <- loadReturn[["regionmapping"]]

  missingRegions <- setdiff(config[["regions"]], regionmapping[["RegionCode"]])
  if (length(missingRegions) > 0) {
    stop("The regions in your config don't match the region mapping. ",
         "The following regions are not part of the mapping:\n  ",
         paste(missingRegions, collapse = c(", ")))
  }


  ## create container ====

  m <- Container$new()
  message("Start input data creation...")



  # SETS -----------------------------------------------------------------------


  ## fundamentals ====

  # sets that are independent of the scenario config

  cost <- m$addSet(
    "cost",
    records = c("tangible", "intangible"),
    description = "type of cost"
  )
  var <- m$addSet(
    "var",
    records = c("stock", "construction", "renovation", "demolition"),
    description = "mayor variables of the model"
  )
  qty <- m$addSet(
    "qty",
    records = c("area", "dwel"),
    description = "quantity unit to measure stocks and flows in"
  )


  ## temporal ====

  startyear <- config[["startyear"]]
  ttotNum <- sort(unique(config[["periods"]]))

  if (startyear <= min(ttotNum)) {
    stop("startyear cannot be equal or before the first period. ",
         "There has to be at least one historic period.")
  }

  invisible(m$addSet(
    "tall",
    records = min(ttotNum):max(ttotNum),
    description = "all time steps"
  ))

  ttot <- m$addSet(
    "ttot",
    records = ttotNum,
    description = "all modelling time steps"
  )
  ttot2 <- m$addAlias("ttot2", ttot)

  invisible(m$addSet(
    "tinit",
    records = min(ttotNum),
    description = "initial modelling time step"
  ))
  t <- m$addSet(
    "t",
    records = ttot$getUELs()[which(ttot$getUELs() >= startyear)],
    description = "modelled time steps"
  )

  invisible(m$addSet(
    "thist",
    records = setdiff(ttot$getUELs(), t$getUELs()),
    description = "historic time steps"
  ))


  ## vintages ====

  vintages <- getBrickMapping("vintage.csv")

  vin <- m$addSet(
    "vin",
    records = unique(getElement(vintages, "vin")),
    description = "construction vintage cohort"
  )

  vinExists <- expandSets(ttot, vin) %>%
    left_join(vintages, by = "vin") %>%
    filter(.data[["ttot"]] > .data[["from"]] - 1) %>%
    select("ttot", "vin")
  vinExists <- m$addSet(
    "vinExists",
    c(ttot, vin),
    records = vinExists,
    description = "Can this vintage cohort exist i.e. ttot cannot be before cohort starts"
  )


  ## building state alternatives ====

  # building shell
  bs <- getBrickMapping("buildingShell.csv") %>%
    getElement("bs") %>%
    unique()
  if (config[["ignoreShell"]]) bs <- head(bs, 1)
  bs <-  m$addSet(
    "bs",
    records = bs,
    description = "building shell"
  )

  bsr <- m$addSet(
    "bsr",
    records = c(bs$getUELs(), 0),
    description = "renovated building shell"
  )

  # heating system
  hs <- getBrickMapping("heatingSystem.csv", "sectoral") %>%
    getElement("hs") %>%
    unique()
  hs <-  m$addSet(
    "hs",
    records = hs,
    description = "heating system"
  )

  hsr <- m$addSet(
    "hsr",
    records = c(0, hs$getUELs()),
    description = "renovated heating system"
  )

  # building states
  stateR <- c("bsr", "hsr")
  state <- c("bs", "hs")



  ## independent stock subset ====

  reg <- m$addSet(
    "reg",
    records = config[["regions"]],
    description = "region"
  )

  loc <- getBrickMapping("location.csv") %>%
    getElement("loc") %>%
    unique()
  loc <- m$addSet(
    "loc",
    records = loc,
    description = "location of building (rural, urban)"
  )

  typ <- getBrickMapping("buildingType.csv") %>%
    getElement("typ") %>%
    unique()
  typ <- m$addSet(
    "typ",
    records = c("SFH", "MFH"),
    description = "type of residential building (SFH, MFH)"
  )

  inc <- getBrickMapping("incomeQuantile.csv") %>%
    getElement("inc") %>%
    unique()
  inc <- m$addSet(
    "inc",
    records = inc,
    description = "income quantile"
  )


  ## boiler ban ====

  # read ban definition from config
  hsBanConfig <- config[["boilerBan"]] %>%
    listToDf() %>%
    guessColnames(m)

  if (!(is.null(hsBanConfig) || identical(hsBanConfig, "NULL"))) {

    # list banned periods
    hsBanConfig <- hsBanConfig %>%
      group_by(across(everything())) %>%
      mutate(ttot = head(ttotNum, 1)) %>%
      complete(ttot = ttotNum) %>%
      ungroup() %>%
      filter(.data[["ttot"]] > .data[["value"]]) %>%
      select(-"value")
    hsBan <- expandSets(var, reg, ttot, hs) %>%
      mutate(across(everything(), as.character)) %>%
      mutate(ttot = as.numeric(.data[["ttot"]]))
    hsBan <- hsBan %>%
      inner_join(hsBanConfig, by = intersect(colnames(hsBan),
                                             colnames(hsBanConfig))) %>%
      select("var", "reg", "ttot", "hs")
  } else {
    hsBan <- NULL
  }
  hsBan <- m$addSet(
    "hsBan",
    records = hsBan,
    domain = c(var, reg, ttot, hs),
    description = "forbidden heating systems in the respective variable in given period"
  )


  ## allowed renovations ====

  # no decline on energy ladder
  ladderHs <- getBrickMapping("heatingSystem.csv") %>%
    select("hs", ladderHs = "energyLadder")
  ladderBs <- getBrickMapping("buildingShell.csv") %>%
    select("bs", ladderBs = "energyLadder")
  renAllowed <- expandSets(bs, hs, bsr, hsr) %>%
    left_join(ladderHs, by = "hs") %>%
    left_join(ladderBs, by = "bs") %>%
    left_join(ladderHs, by = c(hsr = "hs")) %>%
    left_join(ladderBs, by = c(bsr = "bs")) %>%
    mutate(stepHs = replace_na(.data[["ladderHs.x"]] - .data[["ladderHs.y"]], 0),
           stepBs = replace_na(.data[["ladderBs.x"]] - .data[["ladderBs.y"]], 0)) %>%
    filter(.data[["stepBs"]] >= 0, .data[["stepHs"]] >= 0) %>%
    select("bs", "hs", "bsr", "hsr")

  if (config[["ignoreShell"]]) {
    renAllowed <- renAllowed %>%
      filter(.data[["bsr"]] == "0")
  }

  renAllowed <- m$addSet(
    "renAllowed",
    domain = c(bs, hs, bsr, hsr),
    records = renAllowed,
    description = "Is this renovation transition allowed"
  )



  # PARAMETERS -----------------------------------------------------------------


  ## periods ====

  dt <- diff(ttotNum)
  dt <- data.frame(ttot = ttotNum, value = c(dt[1], dt))
  p_dt <- m$addParameter(
    "p_dt",
    ttot,
    dt,
    description = "length of time step in yr"
  )

  p_dtVin <- expandSets(ttot, vin) %>%
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
    "p_dtVin",
    c(ttot, vin),
    p_dtVin,
    description = "intersection of time step and vintage cohort in yr"
  )

  t0 <- m$addParameter(
    "t0",
    records = as.numeric(t$getUELs()[1]),
    description = "reference year for discounting"
  )


  ## specific cost ====

  ### construction ####
  p_specCostConTang <- readInput("f_costConstruction.cs4r",
                                 c("ttot", "reg", "bs", "hs", "typ"),
                                 inputDir) %>%
    toModelResolution(m)
  p_specCostCon <- expandSets(cost, bs, hs, reg, loc, typ, inc, ttot)
  p_specCostConTang <- p_specCostCon %>%
    filter(.data[["cost"]] == "tangible") %>%
    left_join(p_specCostConTang,
              by = c("bs", "hs", "reg", "typ", "ttot"))
  p_specCostConIntang <- p_specCostCon %>%
    filter(.data[["cost"]] == "intangible") %>%
    addAssump(brick.file("assump/costIntangCon.csv"))
  p_specCostCon <- rbind(p_specCostConTang, p_specCostConIntang)
  p_specCostCon <- m$addParameter(
    "p_specCostCon",
    c(cost, bs, hs, reg, loc, typ, inc, ttot),
    p_specCostCon,
    description = "floor-space specific construction cost in USD/m2"
  )

  ### renovation ####
  p_specCostRenTang <- readInput("f_costRenovation.cs4r",
                                 c("ttot", "reg", "bs", "hs", "bsr", "hsr",
                                   "typ", "vin"),
                                 inputDir) %>%
    toModelResolution(m)
  p_specCostRen <- expandSets(cost, bs, hs, bsr, hsr, vin, reg, loc, typ, inc, ttot)
  p_specCostRenTang <- p_specCostRen %>%
    filter(.data[["cost"]] == "tangible") %>%
    left_join(p_specCostRenTang,
              by = c("ttot", "reg", "bs", "hs", "bsr", "hsr", "typ", "vin"))
  p_specCostRenIntang <- p_specCostRen %>%
    filter(.data[["cost"]] == "intangible") %>%
    addAssump(brick.file("assump/costIntangRen.csv"))
  p_specCostRen <- rbind(p_specCostRenTang, p_specCostRenIntang)
  p_specCostRen <- m$addParameter(
    "p_specCostRen",
    c(cost, state, stateR, vin, reg, loc, typ, inc, ttot),
    p_specCostRen,
    description = "floor-space specific renovation cost in USD/m2"
  )


  ### operation ####

  # scenario assumptions
  carbonPrice <- config[["carbonPrice"]]
  carrierPriceLevel <- config[["carrierPrices"]]
  carrierEmiLevel   <- config[["carrierEmi"]]

  # carbon price
  carbonPrice <- if (is.null(carbonPrice)) {
    data.frame(ttot = ttotNum, value = 0)
  } else {
    carbonPrice %>%
      listToDf() %>%
      guessColnames(m) %>%
      toModelResolution(m)
  }
  carbonPrice <- rename(carbonPrice, carbonPrice = "value")

  # energy carrier prices and emission intensities
  hsCarrierMap <- getBrickMapping("heatingSystem.csv") %>%
    select("hs", "carrier")

  carrierData <- readInput("f_carrierPrices.cs4r",
                           c("ttot", "reg", "variable", "unit", "carrier",
                             "level"),
                           inputDir) %>%
    left_join(hsCarrierMap, by = "carrier", relationship = "many-to-many") %>%
    select(-"carrier", -"unit")
  carrierPrices <- carrierData %>%
    filter(.data[["variable"]] == "price") %>%
    pivot_wider(names_from = "variable")
  carrierEmi <- carrierData %>%
    filter(.data[["variable"]] == "emi") %>%
    pivot_wider(names_from = "variable")

  if (!isTRUE(carrierPriceLevel %in% unique(carrierPrices[["level"]]))) {
    stop("The switch 'carrierPrices' has to be exatly one out of [",
         paste(unique(carrierPrices[["level"]]), collapse = ", "), "], not ",
         if (is.null(carrierPriceLevel)) "NULL" else carrierPriceLevel, ".")
  }
  if (!isTRUE(carrierEmiLevel %in% unique(carrierEmi[["level"]]))) {
    stop("The switch 'carrierEmi' has to be exatly one out of [",
         paste(unique(carrierEmi[["level"]]), collapse = ", "), "], not ",
         if (is.null(carrierEmiLevel)) "NULL" else carrierEmiLevel, ".")
  }

  carrierPrices <- carrierPrices %>%
    filter(.data[["level"]] == carrierPriceLevel) %>%
    select(-"level") %>%
    toModelResolution(m, "price")
  carrierEmi <- carrierEmi %>%
    filter(.data[["level"]] == carrierEmiLevel) %>%
    select(-"level") %>%
    toModelResolution(m, "emi")

  # useful energy demand for space heating
  ueDemand <- readInput("f_ueDemand.cs4r",
                        c("reg", "typ", "vin", "bs", "ueDem"),
                        inputDir) %>%
    toModelResolution(m, "ueDem")

  # FE-to-UE-efficiency of heating systems
  eff <- readInput("f_heatingEfficiency.cs4r",
                   c("ttot", "reg", "hs", "typ", "eff"),
                   inputDir) %>%
    toModelResolution(m, "eff")

  # calculate operational cost
  p_specCostOpe <- expandSets(bs, hs, vin, reg, loc, typ, ttot = t)
  p_specCostOpe <- p_specCostOpe %>%
    left_join(carrierPrices, by = c("hs", "reg", "ttot")) %>%
    left_join(carrierEmi, by = c("hs", "reg", "ttot")) %>%
    left_join(carbonPrice, by = intersect(colnames(p_specCostOpe),
                                          colnames(carbonPrice))) %>%
    left_join(ueDemand, by = c("bs", "vin", "reg", "typ")) %>%
    left_join(eff, by = c("hs", "reg", "typ", "ttot")) %>%
    mutate(value =
             (.data[["price"]] + .data[["carbonPrice"]] * .data[["emi"]]) *
             .data[["ueDem"]] / .data[["eff"]]) %>%
    select(-"price", -"carbonPrice", -"emi", -"ueDem", -"eff")

  p_specCostOpe <- m$addParameter(
    "p_specCostOpe",
    c(state, vin, reg, loc, typ, ttot),
    p_specCostOpe,
    description = "floor-space specific operation cost in EUR/(m2.yr)"
  )


  ### demolition ####
  invisible(m$addParameter(
    "p_specCostDem",
    records = 15,
    description = "floor-space specific demolition cost in USD/m2"
  ))


  ## lifetime ====

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

    expandSets(ttot2 = ttot2, ttot) %>%
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

  ### building ####

  # parameter for monitoring purposes
  p_probDem <- expandSets(reg, typ, ttot2, ttot) %>%
    filter(.data[["ttot2"]] >= .data[["ttot"]]) %>%
    left_join(lt, by = c("reg", "typ"), relationship = "many-to-many") %>%
    pivot_wider(names_from = "variable") %>%
    mutate(value = pweibull(.data[["ttot2"]] - .data[["ttot"]],
                            .data[["shape"]], .data[["scale"]])) %>%
    select(-"shape", -"scale")
  p_probDem <- m$addParameter(
    "p_probDem",
    c(reg, typ, ttot2, ttot),
    p_probDem,
    description = "probability of a building having reached its end of life"
  )

  # share of stock from previous time step that has to be demolished as it
  # reaches its end of life
  p_shareDem <- expandSets(vin, reg, typ, ttot) %>%
    left_join(vintages, by = "vin") %>%
    inner_join(readSymbol(vinExists, stringAsFactor = FALSE),
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
    "p_shareDem",
    c(vin, reg, typ, ttot),
    p_shareDem,
    description = "minimum share of demolition at end of life"
  )

  ### building shell ####

  p_lifeTimeBS <- ltBs %>%
    calc_addVariable(lt = "scale * gamma(1 + 1 / shape)", only.new = TRUE) %>%
    select("reg", "value") %>%
    toModelResolution(m)
  p_lifeTimeBS <- m$addParameter(
    "p_lifeTimeBS",
    reg,
    p_lifeTimeBS,
    description = "life time of heating system in yr"
  )

  p_shareRenBS <- shareRen(ttot, ltBs) %>%
    select("reg", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenBS <- m$addParameter(
    "p_shareRenBS",
    c(reg, ttot2, ttot),
    p_shareRenBS,
    description = "minimum share of renovation from the building shell reaching end of life"
  )

  p_shareRenBSinit <- shareRen(ttot, ltBs, 12) %>%
    select("reg", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenBSinit <- m$addParameter(
    "p_shareRenBSinit",
    c(reg, ttot2, ttot),
    p_shareRenBSinit,
    description = "minimum share of renovation from the building shell of initial stock reaching end of life"
  )

  ### heating system ####

  p_lifeTimeHS <- ltHs %>%
    calc_addVariable(lt = "scale * gamma(1 + 1 / shape)", only.new = TRUE) %>%
    select("hs", "reg", "typ", "value") %>%
    toModelResolution(m)
  p_lifeTimeHS <- m$addParameter(
    "p_lifeTimeHS",
    c(hs, reg, typ),
    p_lifeTimeHS,
    description = "life time of heating system in yr"
  )

  p_shareRenHS <- shareRen(ttot, ltHs) %>%
    select("hs", "reg", "typ", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenHS <- m$addParameter(
    "p_shareRenHS",
    c(hs, reg, typ, ttot2, ttot),
    p_shareRenHS,
    description = "minimum share of renovation from the heating system reaching end of life"
  )

  p_shareRenHSinit <- shareRen(ttot, ltHs, 6) %>%
    select("hs", "reg", "typ", "ttot2", "ttot", "value") %>%
    toModelResolution(m)
  p_shareRenHSinit <- m$addParameter(
    "p_shareRenHSinit",
    c(hs, reg, typ, ttot2, ttot),
    p_shareRenHSinit,
    description = "minimum share of renovation from the heating system of initial stock reaching end of life"
  )


  ## other ====

  ### discount factor ####
  p_discountFac <- expandSets(typ, ttot) %>%
    left_join(readSymbol(p_dt) %>%
                rename(dt = "value"),
              by = "ttot") %>%
    mutate(r = c(SFH = 0.21, MFH = 0.25)[.data[["typ"]]], # Giraudet et al. 2012
           value = 1 / (1 + .data[["r"]])^(.data[["ttot"]] - .data[["dt"]] / 2
                                           - unlist(readSymbol(t0)))) %>%
    select("typ", "ttot", "value")
  p_discountFac <- m$addParameter(
    "p_discountFac",
    c(typ, ttot),
    p_discountFac,
    description = "discount factor w.r.t. t0"
  )


  ### population ####

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
    filter(.data[["scenario"]] == popScenario) %>%
    select(-"scenario") %>%
    toModelResolution(m)

  p_population <- expandSets(reg, loc, typ, inc, ttot) %>%
    left_join(pop, by = c("reg", "typ", "loc", "ttot"),
              relationship = "many-to-many") %>%
    select("reg", "loc", "typ", "inc", "ttot", "value")

  p_population <- m$addParameter(
    "p_population",
    c(reg, loc, typ, inc, ttot),
    p_population,
    description = "number of people in million"
  )


  ### floor space per capita ####

  # SSP scenario
  fsScenario <- config[["fsScenario"]]

  # read floor space data
  p_floorPerCap <- readInput("f_floorspacePerCap.cs4r",
                             c("ttot", "reg", "scenario", "typ", "loc"),
                             inputDir)

  # pick scenario
  if (!isTRUE(fsScenario %in% unique(p_floorPerCap[["scenario"]]))) {
    stop("The switch 'fsScenario' has to be exatly one out of [",
         paste(unique(p_floorPerCap[["scenario"]]), collapse = ", "), "], not ",
         if (is.null(fsScenario)) "NULL" else fsScenario, ".")
  }

  p_floorPerCap <- p_floorPerCap %>%
    filter(.data[["scenario"]] == fsScenario) %>%
    select(-"scenario") %>%
    toModelResolution(m)

  p_floorPerCap <- expandSets(reg, loc, typ, inc, ttot) %>%
    left_join(p_floorPerCap, by = c("reg", "loc", "typ", "ttot"))

  p_floorPerCap <- m$addParameter(
    "p_floorPerCap",
    c(reg, loc, typ, inc, ttot),
    p_floorPerCap,
    description = "floor space per capita in m2"
  )


  ## stock ====

  # stock of residential floor space
  p_stockHist <- readInput("f_buildingStock.cs4r",
                           c("ttot", "reg", "variable", "typ", "loc", "vin", "hs"),
                           inputDir) %>%
    filter(.data[["variable"]] == "floor") %>%
    select(-"variable") %>%
    mutate(bs  = "low",
           qty = "area")
  p_stockHist <- expandSets(qty, bs, hs, vin, reg, loc, typ, inc, ttot) %>%
    inner_join(readSymbol(vinExists, stringAsFactor = FALSE),
               by = c("vin", "ttot")) %>%
    left_join(p_stockHist,
              by = c("qty", "bs", "hs", "vin", "reg", "loc", "typ", "ttot")) %>%
    mutate(value = replace_na(.data[["value"]], 0))

  p_stockHist <- m$addParameter(
    "p_stockHist",
    c(qty, bs, hs, vin, reg, loc, typ, inc, ttot),
    p_stockHist,
    description = "historic stock of buildings in million m2"
  )



  # WRITE GDX ------------------------------------------------------------------

  m$write(inputFilePath, compress = TRUE)
  message("  ... done.")

}
