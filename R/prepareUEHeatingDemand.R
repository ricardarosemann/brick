#' Usefule energy demand for space heating
#'
#' From hotmaps in kWh/(m2.yr)
#' @export

prepareUEHeatingDemand <- function(regions, brickDir) {

  hotmaps <- read.csv(file.path(brickDir, "inst", "assump", "building_stock.csv"), sep = "|")

  typMap <- c(`Single family- Terraced houses` = "SFH",
              `Multifamily houses`             = "MFH",
              `Appartment blocks`              = "MFH")

  ueDem <- hotmaps %>%
    filter(.data[["sector"]] == "Residential sector",
           .data[["feature"]] == "Useful energy demand",
           .data[["subsector"]] != "Total",
           grepl("Space heating", .data[["type"]]),
           !grepl("Berfore", .data[["bage"]])) %>%
    select(region = "country", typ = "subsector", vin = "bage", "value") %>%
    mutate(typ = typMap[.data[["typ"]]]) %>%
    group_by(across(all_of(c("region", "typ", "vin")))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop")

  ueDemAvg <- ueDem %>%
    filter(.data[["region"]] == "EU27+UK")

  ueDem <- ueDem %>%
    filter(.data[["region"]] != "EU27+UK") %>%
    mutate(region = toolCountry2isocode(.data[["region"]])) %>%
    filter(.data[["region"]] %in% regions)

  ueDemAvg <- ueDemAvg %>%
    group_by(across(-all_of("region"))) %>%
    reframe(region = setdiff(regions, unique(ueDem$region))) %>%
    ungroup()

  return(ueDem)
}
