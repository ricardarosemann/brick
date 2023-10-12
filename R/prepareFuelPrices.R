#' Prepare fuel prices
#'
#' @author Robin Hasse
#'
#' @param carbonPrice data.frame with time series of carbon price
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom tidyr pivot_longer replace_na
#' @export

prepareFuelPrices <- function(carbonPrice = NULL, regions, periods, brickDir, result = "fuelPrices") {



  # FE prices ------------------------------------------------------------------

  # prices incl taxes, excl VAT, excl CO2 price


  # Read Excel file from ECEMF
  ecemf <- read.xlsx(
    file.path(brickDir, "inst", "assump", "FE Prices_2022-04-14_v2.xlsx"),
    "Price markups",
    fillMergedCells = TRUE)

  # All prices except district heat from ECEMF
  prices <- ecemf %>%
    select(carrier = "X1", sector = "X3", component = "X4", unit = "X7",
           matches("\\d{4}")) %>%
    mutate(carrier = ifelse(grepl("average of model ensemble|from CO2 price path",
                                  .data[["carrier"]]),
                            NA,
                            .data[["carrier"]]),
           pos = cumsum(!is.na(.data[["carrier"]]))) %>%
    filter(.data[["pos"]] > 0) %>%
    mutate(carrier = unique(.data[["carrier"]][!is.na(.data[["carrier"]])])[.data[["pos"]]]) %>%
    filter(.data[["unit"]] == "EUR/MWh",
           .data[["sector"]] == "Res&Com" | is.na(.data[["sector"]]),
           !.data[["component"]] == "CO2 markup ",
           !grepl("^total", .data[["component"]])) %>%
    select(-"pos", -"sector", -"unit") %>%
    mutate(component = ifelse(grepl("^Price (PE|Pe|SE)", .data[["component"]]),
                              "energy",
                              .data[["component"]])) %>%
    pivot_longer(matches("\\d{4}"), names_to = "period", values_to = "value") %>%
    mutate(period = as.numeric(.data[["period"]])) %>%
    group_by(across(-all_of(c("component", "value")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    mutate(region = head(regions, 1)) %>%
    group_by(across(all_of(c("carrier", "period")))) %>%
    complete(region = regions) %>%
    mutate(value = unique(.data[["value"]][!is.na(.data[["value"]])])) %>%
    ungroup() %>%
    interpolate_missing_periods(periods, expand.values = TRUE)

  # district heating price
  extrapolate <- function(p, v, until = 2020) {
    linModel <- lm(value~period, data.frame(period = p, value = v))
    vUntil <- predict(linModel, newdata = data.frame(period = until))[[1]]
    return(c(v, vUntil))
  }
  pricesDH <- readSource("Energiforsk2016") %>%
    as.quitte(na.rm = TRUE) %>%
    select("region", "period", "value")
  pricesDH <- pricesDH %>%
    group_by(.data[["period"]]) %>%
    summarise(value = mean(.data[["value"]]),
              region = setdiff(regions, pricesDH$region),
              .groups = "drop") %>%
    rbind(pricesDH) %>%
    filter(.data[["region"]] %in% regions) %>%
    mutate(value = .data[["value"]] * 3.6) %>% # EUR/GJ -> EUR/MWh
    group_by(.data[["region"]]) %>%
    reframe(value = extrapolate(.data[["period"]], .data[["value"]]),
            period = c(.data[["period"]], 2020)) %>%
    group_by(.data[["period"]]) %>%
    complete(region = regions) %>%
    mutate(value = ifelse(is.na(.data[["value"]]),
                          mean(.data[["value"]], na.rm = TRUE),
                          .data[["value"]])) %>%
    ungroup() %>%
    filter(.data[["period"]] %in% periods) %>%
    interpolate_missing_periods(periods, expand.values = TRUE) %>%
    mutate(carrier = "Heat")



  # emission intensities -------------------------------------------------------

  # extremely rough assumption based on remind results
  emissionIntensityHeat <- data.frame(
      period  = c(2000, 2022, 2045),
      value   = c(0.4, 0.3, 0),
      region = head(regions, 1)) %>%
    group_by(.data[["period"]]) %>%
    complete(region = regions) %>%
    mutate(value = unique(.data[["value"]][!is.na(.data[["value"]])])) %>%
    ungroup() %>%
    interpolate_missing_periods(periods, expand.values = TRUE) %>%
    mutate(carrier = "Heat")

  # https://www.eea.europa.eu/ims/greenhouse-gas-emission-intensity-of-1
  emissionIntensityElec <- read.csv(
    file.path(brickDir, "inst", "assump", "co2-emission-intensity-13.csv")) %>%
    rename(period = 1, region = 2, value = 3, low = 4, high = 5) %>%
    mutate(value = ifelse(is.na(.data[["value"]]),
                          rowMeans(cbind(.data[["low"]], .data[["high"]]), na.rm = TRUE),
                          .data[["value"]])) %>%
    select(-"low", -"high") %>%
    filter(.data[["region"]] != "EU-27",
           .data[["period"]] %in% periods) %>%
    mutate(region = toolCountry2isocode(.data[["region"]]),
           value = .data[["value"]] / 1000) %>% # g_CO2/kWh -> t_CO2/MWh
    group_by(.data[["region"]]) %>%
    complete(period = periods) %>%
    ungroup() %>%
    group_by(.data[["period"]]) %>%
    complete(region = regions) %>%
    mutate(value = ifelse(is.na(.data[["value"]]),
                          mean(.data[["value"]], na.rm = TRUE),
                          .data[["value"]])) %>%
    ungroup() %>%
    mutate(value = ifelse(.data[["period"]] == 2035,
                          0,
                          .data[["value"]])) %>%
    interpolate_missing_periods(periods, expand.values = TRUE) %>%
    mutate(carrier = "Electricity")

  # fossil carriers from ECEMF
  emissionIntensityFossil <- ecemf %>%
    select(carrier = "X1", value = "X5", unit = "X6") %>%
    filter(!is.na(.data[["unit"]])) %>%
    select(-"unit") %>%
    mutate(period = 2020,
           region = head(regions, 1),
           value = as.numeric(.data[["value"]])) %>%
    group_by(across(all_of(c("carrier", "period")))) %>%
    complete(region = regions) %>%
    mutate(value = unique(.data[["value"]][!is.na(.data[["value"]])])) %>%
    ungroup() %>%
    interpolate_missing_periods(periods, expand.values = TRUE)

  # combined emission intensity
  emissionIntensity <- rbind(
    emissionIntensityFossil,
    emissionIntensityElec,
    emissionIntensityHeat
  )


  # carbon price ---------------------------------------------------------------

  if (is.null(carbonPrice)) {
    carbonPrice <- data.frame(period = periods, value = 0)
  } else {
    carbonPrice <- carbonPrice %>%
      interpolate_missing_periods(periods, expand.values = TRUE)
  }

  carbonPriceHist <- data.frame(
    carrier = rep(c("Electricity", "Heat"), 3),
    period = rep(c(2015, 2020, 2025), each = 2),
    value.z = rep(c(5, 50, 100), each = 2)
  ) %>%
    interpolate_missing_periods(periods, expand.values = TRUE, value = "value.z")

  carbonMarkup <- emissionIntensity %>%
    left_join(carbonPrice, by = setdiff(colnames(carbonPrice), "value")) %>%
    left_join(carbonPriceHist, by = c("carrier", "period")) %>%
    mutate(value = .data[["value.x"]] *
             pmax(.data[["value.y"]], .data[["value.z"]], na.rm = TRUE)) %>%
    select(-"value.x", -"value.y", -"value.z")



  # enduser prices -------------------------------------------------------------

  # assumed average VAT
  # https://taxation-customs.ec.europa.eu/system/files/2021-06/vat_rates_en.pdf
  vat <- 0.2

  finalPrices <- rbind(prices, pricesDH) %>%
    left_join(carbonMarkup, by = c("region", "period", "carrier")) %>%
    mutate(value.y = replace_na(.data[["value.y"]], 0),
           value = .data[["value.x"]] + .data[["value.y"]],
           value = (1 + vat) * .data[["value"]]) %>%
    select(-"value.x", -"value.y")



  return(switch(result,
                fuelPrices        = finalPrices,
                emissionIntensity = emissionIntensity))
}
