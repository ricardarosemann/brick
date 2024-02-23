#' Create data for reference matching
#'
#' @author Robin Hasse
#'
#' @param path character vector with folders to run the model in
#' @param config run configurations
#' @param overwrite logical, should existing data be overwritten?
#'
#' @importFrom madrat calcOutput toolGetMapping
#' @importFrom quitte as.quitte
#' @importFrom tidyr matches
#' @importFrom dplyr select
#' @importFrom utils read.csv read.csv2
#' @importFrom stats median
#' @export
#'
createMatchingData <- function(path, config, overwrite = FALSE) {

  # check file path
  refFilePath <- file.path(path, "references.gdx")
  if (file.exists(refFilePath)) {
    if (overwrite) {
      warning("References file '", refFilePath, "' overwritten.")
    } else {
      stop("References file '", refFilePath,
           "' cannot be created as it already exists.")
    }
  }

  regions <- config[["regions"]]
  periods <- config[["periods"]]

  m <- gamstransfer::Container$new()

  # Read passed references
  refsDf <- read.csv2(file.path(path, "references.csv"), row.names = 1)
  refs <- refsDf[["references"]]
  names(refs) <- rownames(refsDf)

  # READ DATA ------------------------------------------------------------------

  references <- c(
    "Odyssee_stock",
    "Odyssee_construction",
    "Odyssee_constructionFloor",
    "Odyssee_dwelSize",
    "Odyssee_heatingShare",
    "IDEES_heatingShare",
    "EUBDB_stock",
    "EUBDB_vintage",
    "mredgebuildings_location",
    "mredgebuildings_vintage",
    "mredgebuildings_buildingType",
    "mredgebuildings_heating",
    "EuropeanCommissionRenovation"
  )
  refs <- c(
    "mredgebuildings_location",
    "mredgebuildings_buildingType",
    "mredgebuildings_heating",
    "EUBDB_vintage",
    "Odyssee_constructionFloor",
    "Odyssee_heatingShare",
    "IDEES_heatingShare",
    "EuropeanCommissionRenovation"
  )

  # reference values
  # TODO: this should come from refs
  refVals <- do.call(rbind, lapply(references, function(ref) {
    calcOutput("MatchingReference", subtype = ref, aggregate = FALSE) %>%
      as.quitte(na.rm = TRUE) %>%
      filter(.data[["region"]] %in% regions,
             .data[["period"]] %in% periods) %>%
      group_by(across(-all_of(c("region", "period", "value")))) %>%
      complete(region = regions, period = periods) %>%
      ungroup() %>%
      mutate(ref = ref) %>%
      select("ref", refVar = "variable", reg = "region", ttot = "period", "value")
  }))
  refValsMed <- refVals %>%
    group_by(across(all_of(c("ref", "reg")))) %>%
    summarise(value = median(abs(.data[["value"]][.data[["value"]] != 0]),
                             na.rm = TRUE),
              .groups = "drop")

  # variable mappings
  refMaps <- lapply(references, function(ref) {
    toolGetMapping(name = paste0("refMap_", ref, ".csv"),
                   type = "sectoral",
                   where = "mappingfolder",
                   returnPathOnly = TRUE) %>%
      read.csv(comment.char = "#") %>%
      select(-matches("^\\.")) %>%
      unique()
  }) %>%
    `names<-`(references)

  for (ref in references) {
    invisible(m$addSet(
      name = paste0("refMap_", ref),
      records = refMaps[[ref]],
      domain = colnames(refMaps[[ref]]),
      domainForwarding = TRUE,
      description = paste("Mapping of brick variables to reference variables:",
                          ref)
    ))

    refVar <- m$addSet(
      name = paste0("refVar_", ref),
      records = unique(refMaps[[ref]][["variable"]]),
      description = paste("variables of reference source:", ref)
    )
  }

  invisible(m$addParameter(
    name = "p_refVals",
    domain = c("ref", "refVar", "reg", "ttot"),
    records = refVals,
    domainForwarding = TRUE,
    description = "Reference values to match"
  ))

  invisible(m$addParameter(
    name = "p_refValsMed",
    domain = c("ref", "reg"),
    records = refValsMed,
    domainForwarding = TRUE,
    description = "Reference values to match"
  ))

  refVar <- m$addSet(
    name = "refVar",
    records = unique(refVals[["refVar"]]),
    description = "variables of reference sources"
  )

  ref <- m$addSet(
    name = "ref",
    records = unique(refVals[["ref"]]),
    description = "reference sources that historic quantities can be calibrated to"
  )

  invisible(m$addSet(
    name = "r",
    records = refs,
    description = "reference sources that historic quantities are calibrated to"
  ))

  invisible(m$addSet(
    name = "refVarRef",
    domain = c(ref, refVar),
    records = unique(refVals[, c("ref", "refVar")]),
    description = "mapping references to reference variables"
  ))

  invisible(m$addSet(
    name = "refVarExists",
    domain = c("ref", "refVar", "reg", "ttot"),
    records = unique(refVals[!is.na(refVals$value),
                             c("ref", "refVar", "reg", "ttot")]),
    description = "There is a value for this combination of reference, variable, region and period"
  ))

  m$write(refFilePath, compress = TRUE)

}
