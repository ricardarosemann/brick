#' Create input data
#'
#' Create a complete set of input data for the gams optimisation.
#'
#' This is still mostly test data created with ugly code that will be replaced
#' by proper input data preparation bit by bit.
#'
#' @author Robin Hasse
#'
#' @param path character vector with folders to write input data into
#' @param config named list with run configuration
#' @param aggregateProblem boolean, should the problem be agregated?
#' @param overwrite boolean, should existing input.gdx be overwritten?
#'
#' @importFrom quitte inline.data.frame as.quitte interpolate_missing_periods
#'   revalue.levels calc_addVariable
#' @importFrom dplyr %>% mutate select group_by summarise filter ungroup arrange
#'   left_join right_join .data rename lag all_of across sym rename_with
#'   inner_join everything
#' @importFrom tidyr complete
#' @importFrom madrat calcOutput readSource
#' @importFrom magclass collapseDim mselect getYears mbind getItems<- getItems
#'   add_dimension
#' @importFrom gdxrrw igdx wgdx
#' @importFrom gamstransfer Container
#' @importFrom stats pweibull
#' @importFrom utils head tail
#' @importFrom zoo rollmean
#' @export
#'
createInputData <- function(path,
                            config,
                            aggregateProblem = FALSE,
                            overwrite = FALSE) {

  # FUNCTIONS ------------------------------------------------------------------

  expandSets <- function(...) {
    lst <- list(...)
    setElements <- lapply(lst, function(l) l$getUELs())[]
    setNames <- as.character(lapply(lst, function(l) l$name))
    if (!is.null(names(lst))) {
      setNames <- ifelse(names(lst) == "",
                         setNames,
                         names(lst))
    }

    expand.grid(setNames(setElements, setNames))
  }



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


  ## create container ====

  m <- Container$new()



  # SETS -----------------------------------------------------------------------


  ## fundamentals ====

  cost <- m$addSet(
    "cost",
    records = c("tangible", "intangible"),
    description = "type of cost")

  var <- m$addSet(
    "var",
    records = c("stock", "construction", "renovation", "demolition"),
    description = "mayor variables of the model")

  qty <- m$addSet(
    "qty",
    records = c("area", "dwel"),
    description = "quantity unit to measure stocks and flows in")


  ## temporal ====

  startyear <- config[["startyear"]]
  ttotNum <- config[["periods"]]

  if (startyear <= min(ttotNum)) {
    stop("startyear cannot be equal or before the first period. ",
         "There has to be at least one historic period.")
  }

  invisible(m$addSet(
    "tall",
    records = min(ttotNum):max(ttotNum),
    description = "all time steps"))
  ttot <- m$addSet(
    "ttot",
    records = ttotNum,
    description = "all modelling time steps")
  tinit <- m$addSet(
    "tinit",
    records = min(ttotNum),
    description = "initial modelling time step")
  t <- m$addSet(
    "t",
    records = ttot$getUELs()[which(ttot$getUELs() >= startyear)],
    description = "modelled time steps")
  thist <- m$addSet( # nolint: object_usage_linter.
    "thist",
    records = setdiff(ttot$getUELs(), t$getUELs()),
    description = "modelled time steps")
  ttot2 <- m$addAlias("ttot2", ttot)


  ## vintages ====

  vintages <- inline.data.frame(
    "vin;          from;  to",
    "before 1945;  1850;  1944",
    "1945-1969;    1945;  1969",
    "1970-1979;    1970;  1979",
    "1980-1989;    1980;  1989",
    "1990-1999;    1990;  1999",
    "2000-2010;    2000;  2010",
    "2011-2020;    2011;  2020",
    "2021-2030;    2021;  2030",
    "2031-2040;    2031;  2040",
    "2041-2050;    2041;  2050",
    "2051-2060;    2051;  2060",
    "2061-2070;    2061;  2070",
    "2071-2080;    2071;  2080",
    "2081-2090;    2081;  2090",
    "2091-2100;    2091;  2100",
    "after 2100;   2101;  2150"
  )
  vin <- m$addSet(
    "vin",
    records = getElement(vintages, "vin"),
    description = "construction vintage cohort")
  vinExists <- expandSets(ttot, vin) %>%
    left_join(vintages, by = "vin") %>%
    filter(as.numeric(as.character(.data[["ttot"]])) > .data[["from"]] - 1) %>%
    select("ttot", "vin")
  vinExists <- m$addSet(
    "vinExists",
    c(ttot, vin),
    records = vinExists,
    description = "Can this vintage cohort exist i.e. ttot cannot be before cohort starts"
  )


  ## building state alternatives ====

  bsr <- m$addSet(
    "bsr",
    # records = c("original", "rMedium", 0),  # TODO: temporary deactivation of renovation
    records = c("original", 0),
    description = "renovated building shell")
  hsr <- m$addSet(
    "hsr",
    records = c(0, "biom", "dihe", "ehp1", "gabo", "libo", "reel", "sobo"),
    description = "renovated heating system")
  bs <-  m$addSet(
    "bs",
    records = setdiff(bsr$getUELs(), "0"),
    description = "building shell")
  hs <-  m$addSet(
    "hs",
    records = setdiff(hsr$getUELs(), "0"),
    description = "heating system")

  stateR <- c("bsr", "hsr")
  state <- c("bs", "hs")



  ## independent stock subset ====

  reg <- m$addSet(
    "reg",
    records = config[["regions"]],
    description = "region")
  loc <- m$addSet(
    "loc",
    records = c("rural", "urban"),
    description = "location of building (rural, urban)")
  typ <- m$addSet(
    "typ",
    records = c("SFH", "MFH"),
    description = "type of residential building (SFH, MFH)")
  inc <- m$addSet(
    "inc",
    records = "all",
    description = "income quantile")


  ## boiler ban ====
  hsBan <- expandSets(var, reg, ttot, hs) %>%
    mutate(across(everything(), as.character)) %>%
        mutate(ttot = as.numeric(.data[["ttot"]]))
  if ("boilerBan" %in% config[["iamcSwitch"]]) {
    hsBanConfig <- inline.data.frame(
      "var;           tout;  hs",
      "renovation;    2030;  sobo",
      "renovation;    2030;  libo",
      "renovation;    2025;  gabo",
      "construction;  2030;  sobo",
      "construction;  2030;  libo",
      "construction;  2025;  gabo"
    ) %>%
      group_by(across(everything())) %>%
      mutate(ttot = head(ttotNum, 1)) %>%
      complete(ttot = ttotNum) %>%
      ungroup() %>%
      filter(.data[["ttot"]] > .data[["tout"]]) %>%
      select(-"tout")

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
    description = "heating systems are forbidden in the respective variable after given period"
  )



  # PARAMETERS -----------------------------------------------------------------


  ## periods ====

  dt <- diff(ttotNum)
  dt <- data.frame(ttot = ttot$getUELs(), value = c(dt[1], dt))
  p_dt <- m$addParameter(
    "p_dt",
    ttot,
    dt,
    description = "length of time step in yr")

  p_dtVin <- expandSets(ttot, vin) %>%
    left_join(vintages, by = "vin") %>%
    left_join(dt, by = "ttot") %>%
    rename(dt = "value") %>%
    mutate(ttot = as.numeric(as.character(.data[["ttot"]])),
           value = pmax(0, pmin(.data[["to"]], .data[["ttot"]]) -
                     pmax(.data[["from"]] - 1, .data[["ttot"]] - .data[["dt"]]))) %>%
    select("ttot", "vin", "value")
  p_dtVin <- m$addParameter(
    "p_dtVin",
    c(ttot, vin),
    p_dtVin,
    description = "intersection of time step and vintage cohort in yr")

  t0 <- m$addParameter(
    "t0",
    records = as.numeric(t$getUELs()[1]),
    description = "reference year for discounting")


  ## specific cost ====

  ### construction ####

  # investment cost of Germany from https://www.iea.org/articles/are-renewable-heating-options-cost-competitive-with-fossil-fuels-in-the-residential-sector
  # average across multiple matching technologies
  hsUnitCost <- inline.data.frame(
    "hs;    hsInstCost",
    "reel;   3000",  # wild guess
    "ehp1;  14200",
    "libo;   6350",
    "gabo;   5100",
    "sobo;   6350",  # assumption: equal to libo
    "dihe;  10000",  # wild guess
    "biom;   9100",
    "0;         0"
  )
  # assumption to make unit prices relative to floor space
  m2PerUnit <- inline.data.frame(
    "typ;  m2PerUnit",
    "SFH;  100",
    "MFH;  200"
  )
  constCost <- inline.data.frame(
    "bs;        constCost",
    "original;  1615"  # Mastrucci et al. 2021
  )
  p_specCostCon <- expandSets(cost, bs, hs, reg, loc, typ, inc, ttot)
  p_specCostConTang <- p_specCostCon %>%
    filter(.data[["cost"]] == "tangible") %>%
    left_join(constCost, by = "bs") %>%
    left_join(hsUnitCost, by = "hs") %>%
    left_join(m2PerUnit, by = "typ") %>%
    mutate(value =
             .data[["constCost"]] +
             .data[["hsInstCost"]] / .data[["m2PerUnit"]]) %>%
    select(-"constCost", -"hsInstCost", -"m2PerUnit")
  p_specCostConIntang <- p_specCostCon %>%
    filter(.data[["cost"]] == "intangible") %>%
    addAssump("inst/assump/costIntangCon.csv")
  p_specCostCon <- rbind(p_specCostConTang, p_specCostConIntang)
  m$addParameter(
    "p_specCostCon",
    c(cost, bs, hs, reg, loc, typ, inc, ttot),
    p_specCostCon,
    description = "floor-space specific construction cost in USD/m2")

  ### renovation ####
  renCost <- inline.data.frame(
    "bsr;      renCost",
    "original; 487",
    "0;        0"
  )
  p_specCostRen <- expandSets(cost, bs, hs, bsr, hsr, vin, reg, loc, typ, inc, ttot)
  p_specCostRenTang <- p_specCostRen %>%
    filter(.data[["cost"]] == "tangible") %>%
    left_join(renCost, by = "bsr") %>%
    left_join(hsUnitCost, by = c(hsr = "hs")) %>%
    left_join(m2PerUnit, by = "typ") %>%
    mutate(value =
             .data[["renCost"]] +
             .data[["hsInstCost"]] / .data[["m2PerUnit"]]) %>%
    select(-"renCost", -"hsInstCost", -"m2PerUnit")
  p_specCostRenIntang <- p_specCostRen %>%
    filter(.data[["cost"]] == "intangible") %>%
    addAssump("inst/assump/costIntangRen.csv")
  p_specCostRen <- rbind(p_specCostRenTang, p_specCostRenIntang)
  p_specCostRen <- m$addParameter(
    "p_specCostRen",
    c(cost, state, stateR, vin, reg, loc, typ, inc, ttot),
    p_specCostRen,
    description = "floor-space specific renovation cost in USD/m2")


  ### operation ####

  # from EU Commission report on Renovation
  renovationFactor <- inline.data.frame(
    "bs;        factor",
    "original;  1",
    "rLight;    0.873",
    "rMedium;   0.589",
    "rDeep;     0.34"
  )

  # floor-space specific UE demand for space heating
  vinMap <- inline.data.frame(
    "vin;          vinHotmaps",
    "before 1945;  Before 1945",
    "1945-1969;    1945 - 1969",
    "1970-1979;    1970 - 1979",
    "1980-1989;    1980 - 1989",
    "1990-1999;    1990 - 1999",
    "2000-2010;    2000 - 2010",
    "2011-2020;    Post 2010",
    "2021-2030;    Post 2010",
    "2031-2040;    Post 2010",
    "2041-2050;    Post 2010",
    "2051-2060;    Post 2010",
    "2061-2070;    Post 2010",
    "2071-2080;    Post 2010",
    "2081-2090;    Post 2010",
    "2091-2100;    Post 2010",
    "after 2100;   Post 2010"
  )
  ueDem <- prepareUEHeatingDemand(reg$getUELs()) %>%
    rename(vinHotmaps = "vin", reg = "region", ueDem = "value") %>%
    right_join(vinMap, by = "vinHotmaps", relationship = "many-to-many") %>%
    select(-"vinHotmaps")

  # FE-to-UE efficiency
  eff <- inline.data.frame(
    "hs;    eff",
    "reel;  0.98",
    "ehp1;  3.00",
    "libo;  0.74",
    "gabo;  0.90",
    "sobo;  0.75",
    "dihe;  0.98",
    "biom;  0.70")

  # fuel price
  carbonPrice <- if ("carbonPrice" %in% config[["iamcSwitch"]]) {
    data.frame(
      period = c(2020, 2030, 2050),
      value = c(0, 300, 500))
  } else {
    NULL
  }

  heatingCarrierMap <- inline.data.frame(
    "hs;    carrier",
    "reel;  Electricity",
    "ehp1;  Electricity",
    "libo;  Liquids - Oil",
    "gabo;  Gases - Fossil Gas",
    "sobo;  Solids - Coal",
    "dihe;  Heat",
    "biom;  Solids - Biomass"
  )
  fuelPrices <- prepareFuelPrices(carbonPrice, reg$getUELs(), ttotNum) %>%
    right_join(heatingCarrierMap, by = "carrier", relationship = "many-to-many") %>%
    select(-"carrier") %>%
    mutate(value = .data[["value"]] / 1E3) %>%# EUR/MWh -> EUR/kWh
    rename(fuelPrice = "value", reg = "region", ttot = "period")

  p_specCostOpe <- expandSets(bs, hs, vin, reg, loc, typ, ttot = t) %>%
    mutate(ttot = as.numeric(as.character(.data[["ttot"]]))) %>%
    left_join(fuelPrices, by = c("hs", "reg", "ttot")) %>%
    left_join(ueDem, by = c("vin", "reg", "typ")) %>%
    left_join(eff, by = "hs") %>%
    left_join(renovationFactor, by = "bs") %>%
    mutate(value =
             .data[["fuelPrice"]] *
             .data[["ueDem"]] *
             .data[["factor"]] /
             .data[["eff"]]) %>%
    select(-"fuelPrice", -"ueDem", -"factor", -"eff")

  p_specCostOpe <- m$addParameter(
    "p_specCostOpe",
    c(state, vin, reg, loc, typ, ttot),
    p_specCostOpe,
    description = "floor-space specific operation cost in EUR/(m2.yr)")


  ### demolition ####
  invisible(m$addParameter(
    "p_specCostDem",
    records = 15,
    description = "floor-space specific demolition cost in USD/m2"))


  ## lifetime ====

  # cut off Weibull above this value and assume 1 for technology life time
  cutOffShare <- 0.95

  # calculate share of buildings that need to be renovated or demolished between
  # given time steps assuming a Weibull distribution of thetechnology life time
  shareRen <- function(tech, ttot2, shape, scale, standingLifetTime = 0) {
    expandSets(switch(tech, bs = bs, hs = hs), ttot2 = ttot2, ttot) %>%
      mutate(ttot  = as.numeric(as.character(.data[["ttot"]])),
             ttot2 = as.numeric(as.character(.data[["ttot2"]]))) %>%
      left_join(p_dt$records %>%
                  mutate(ttot = as.numeric(as.character(.data[["ttot"]]))) %>%
                  rename(dt = "value"),
                by = c(ttot = "ttot")) %>%
      mutate(lifetime = .data[["ttot"]] - .data[["ttot2"]]
             + .data[["dt"]] / 2 + standingLifetTime,
             value = pweibull(.data[["lifetime"]], shape, scale),
             value = ifelse(.data[["value"]] > cutOffShare,
                            1, .data[["value"]])) %>%
      select(tech, "ttot2", "ttot", "value")
  }

  ### building ####
  p_shareDem <- expandSets(vin, ttot) %>%
    left_join(vintages, by = "vin") %>%
    mutate(ttot = as.numeric(as.character(.data[["ttot"]])),
           tcon = (.data[["from"]] + pmin(.data[["ttot"]], .data[["to"]])) / 2,
           p = pweibull(.data[["ttot"]] - .data[["tcon"]], 2.95, 70.82)) %>%
    left_join(p_dt$records %>%
                mutate(ttot = as.numeric(as.character(.data[["ttot"]]))) %>%
                rename(dt = "value"),
              by = "ttot") %>%
    group_by(.data[["vin"]]) %>%
    arrange(.data[["ttot"]]) %>%
    mutate(value = c(0, diff(.data[["p"]])) /
             (1 - lag(.data[["p"]], default = 0)) / .data[["dt"]]) %>%
    select("vin", "ttot", "value")
  p_shareDem <- m$addParameter(
    "p_shareDem",
    c(vin, ttot),
    p_shareDem,
    description = "minimum share of demolition at end of life")

  ### building shell ####
  p_shareRenBS <- m$addParameter(  # nolint: object_usage_linter.
    "p_shareRenBS",
    c(bs, ttot2, ttot),
    shareRen("bs", ttot, 3, 40),
    description = "minimum share of renovation from the building shell reaching end of life")
  p_shareRenBSinit <- m$addParameter( # nolint: object_usage_linter.
    "p_shareRenBSinit",
    c(bs, ttot2, ttot),
    shareRen("bs", tinit, 3, 40, 12),
    description = "minimum share of renovation from the building shell of initial stock reaching end of life")

  ### heating system ####
  p_shareRenHS <- m$addParameter( # nolint: object_usage_linter.
    "p_shareRenHS",
    c(hs, ttot2, ttot),
    shareRen("hs", ttot, 5, 20),
    description = "minimum share of renovation from the heating system reaching end of life")
  p_shareRenHSinit <- m$addParameter( # nolint: object_usage_linter.
    "p_shareRenHSinit",
    c(hs, ttot2, ttot),
    shareRen("hs", tinit, 5, 20, 6),
    description = "minimum share of renovation from the heating system of initial stock reaching end of life")


  ## other ====

  ### discount factor ####
  p_discountFac <- expandSets(typ, ttot) %>%
    mutate(ttot = as.numeric(as.character(.data[["ttot"]])),
           r = c(SFH = 0.21, MFH = 0.25)[.data[["typ"]]], # Giraudet et al. 2012
           value = 1 / (1 + .data[["r"]])^(.data[["ttot"]] - as.numeric(t0$records))) %>%
    select(-"r")
  p_discountFac <- m$addParameter(
    "p_discountFac",
    c(typ, ttot),
    p_discountFac,
    description = "discount factor w.r.t. t0")

  ### population ####
  pop <- calcOutput("Population", aggregate = FALSE) %>%
    mselect(iso3c = reg$getUELs(),
            year = paste0("y", ttot$getUELs()),
            variable = "pop_SSP2") %>%
    collapseDim(dim = "variable")
  urbanShare <- calcOutput("Urban", aggregate = FALSE) %>%
    mselect(iso3c = reg$getUELs(),
            year = paste0("y", ttot$getUELs()),
            variable = "urb_SSP2") %>%
    collapseDim(dim = "variable")
  p_population <- mbind(
    magclass::setNames(pop * urbanShare, "urban"),
    magclass::setNames(pop * (1 - urbanShare), "rural")
  )

  if (aggregateProblem) {
    p_population <- p_population[, , "rural"] + p_population[, , "urban"]
    getItems(p_population, 3) <- "all"
  }

  # allowed renovations
  renAllowed <- expandSets(bs, hs, bsr, hsr) %>%
    filter(!((.data[["hs"]] %in% c("ehp1", "dihe") &
                .data[["hsr"]] %in% c("reel", "sobo", "libo", "gabo")) |
               (.data[["bs"]] == "rMedium" &
                  .data[["bsr"]] == "original")),
           .data[["bsr"]] == "0") # TODO: temporary deactivation of renovation
  renAllowed <- m$addSet(
    "renAllowed",
    domain = c(bs, hs, bsr, hsr),
    records = renAllowed,
    description = "Is this renovation transition allowed"
  )


  ## stock ====

  # stock of residential floor space
  p_stockHist <- calcOutput("BuildingStock", subtype = "residential",
                            aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE) %>%
    filter(.data[["variable"]] == "floor",
           .data[["region"]] %in% reg$getUELs(),
           as.character(.data[["period"]]) %in% ttot$getUELs()) %>%
    revalue.levels(vintage = c(`after 2010` = "2011-2020"),
                   heating = c(`resistElec` = "reel",
                               `heatpump`   = "ehp1",
                               `oil`        = "libo",
                               `gas`        = "gabo",
                               `coal`       = "sobo",
                               `biomod`     = "biom",
                               `heat`       = "dihe")) %>%
    mutate(buildingShell  = "original",
           inc = "all",
           qty = "area",
           value = .data[["value"]] / 1E6) %>%
    group_by(across(all_of(c("heating", "vintage", "region", "location",
                             "buildingType", "inc", "period")))) %>%
    complete(buildingShell = bs$getUELs(), fill = list(value = 0)) %>%
    select("qty",
           bs = "buildingShell",
           hs = "heating",
           vin = "vintage",
           reg = "region",
           loc = "location",
           typ = "buildingType",
           "inc",
           ttot = "period",
           "value")

  if (aggregateProblem) {
    p_stockHist <- p_stockHist %>%
      group_by(across(c(-"loc", -"typ", -"value"))) %>%
      summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
      mutate(loc = "all", typ = "all", .after = "reg") %>%
      ungroup()
  }

  p_stockHist <- m$addParameter(
    "p_stockHist",
    c(qty, bs, hs, vin, reg, loc, typ, inc, ttot),
    p_stockHist,
    description = "historic stock of buildings in million m2"
  )

  # population
  p_population <- p_population %>%
    as.quitte() %>%
    filter(.data[["period"]] %in% ttotNum) %>%
    select(reg = "region",
           loc = "variable",
           ttot = "period",
           "value") %>%
    left_join(p_stockHist$records %>%
                rename_with(function(col) sub("_\\d*", "", col)) %>%
                mutate(ttot = as.numeric(as.character(.data[["ttot"]]))) %>%
                group_by(across(all_of(c("reg", "loc", "ttot", "typ")))) %>%
                summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
                group_by(across(all_of(c("reg", "loc", "ttot")))) %>%
                mutate(value = proportions(.data[["value"]])) %>%
                ungroup() %>%
                mutate(reg = droplevels(.data[["reg"]])) %>%
                interpolate_missing_periods(ttot = ttotNum,
                                            expand.values = TRUE,
                                            combinations = "crossing"),
              by = c("reg", "loc", "ttot")) %>%
    mutate(value = .data[["value.x"]] * .data[["value.y"]],
           inc = "all") %>%
    select("reg", "loc", "typ", "inc", "ttot", "value")
  p_population <- m$addParameter(
    "p_population",
    c(reg, loc, typ, inc, ttot),
    p_population,
    description = "number of people in million"
  )

  # floor space per capita
  p_floorPerCap <- readSource("EDGE", subtype = "Floorspace") %>%
    mselect(scenario = "gdp_SSP2",
            variable = "residential",
            region = reg$getUELs()) %>%
    collapseDim(3.1) %>%
    as.quitte(na.rm = TRUE) %>%
    group_by(.data[["region"]]) %>%
    arrange(.data[["period"]]) %>%
    mutate(value = rollmean(.data[["value"]], 3, na.pad = TRUE)) %>%
    left_join(p_stockHist$records %>%
                mutate(ttot = as.numeric(as.character(.data[["ttot"]]))) %>%
                group_by(across(all_of(c("ttot", "reg")))) %>%
                summarise(value = sum(.data[["value"]]), .groups = "drop"),
              by = c(period = "ttot", region = "reg")) %>%
    group_by(.data[["region"]]) %>%
    mutate(value = .data[["value.x"]] *
             (.data[["value.y"]] / .data[["value.x"]])[.data[["period"]] == min(ttotNum)]) %>%
    ungroup() %>%
    select(-"value.x", -"value.y") %>%
    filter(.data[["period"]] %in% ttotNum) %>%
    as.quitte() %>%
    as.magpie()


  p_floorPerCap <- pop %>%
    mselect(year = getYears(p_floorPerCap)) %>%
    add_dimension(add = "variable", nm = "pop") %>%
    mbind(p_floorPerCap) %>%
    as.quitte(na.rm = TRUE) %>%
    calc_addVariable(floorPerCap = "residential / pop",
                     only.new = TRUE) %>%
    select(reg = "region", ttot = "period", "value")
  p_floorPerCap <- p_stockHist$records %>%
    rename_with(function(col) sub("_\\d*$", "", col)) %>%
    mutate(ttot = as.numeric(as.character(.data[["ttot"]]))) %>%
    group_by(across(all_of(c("reg", "loc", "typ", "ttot")))) %>%
    summarise(value = sum(.data[["value"]]),
              inc = "all",
              .groups = "drop") %>%
    right_join(p_population$records %>%
                 rename_with(function(col) sub("_\\d*$", "", col)) %>%
                 mutate(ttot = as.numeric(as.character(.data[["ttot"]]))),
               by = c("reg", "loc", "typ", "inc", "ttot")) %>%
    mutate(value = .data[["value.x"]] / .data[["value.y"]]) %>%
    select(-"value.x", -"value.y") %>%
    left_join(p_floorPerCap,
              by = c("reg", "ttot")) %>%
    arrange(.data[["ttot"]]) %>%
    mutate(value = ifelse(is.na(.data[["value.x"]]),
                          .data[["value.y"]] *
                            tail((.data[["value.x"]] / .data[["value.y"]])[!is.na(.data[["value.x"]])], 1),
                          .data[["value.x"]])) %>%
    select("reg", "loc", "typ", "inc", "ttot", "value") %>%
    interpolate_missing_periods(ttot = ttotNum, expand.values = TRUE)
  p_floorPerCap <- m$addParameter(
    "p_floorPerCap",
    c(reg, loc, typ, inc, ttot),
    p_floorPerCap,
    description = "floor space per capita in m2"
  )



  # WRITE GDX ------------------------------------------------------------------

  m$write(inputFilePath, compress = TRUE)

}
