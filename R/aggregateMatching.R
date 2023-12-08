#' Aggregate matching run results for calibration
#'
#' Temporary function that prepares historic stock and flows for calibration
#'
#' This functionality will migrate to mredgebuidlings but this infrastructure
#' requires more time to be developed.
#'
#' @author Robin Hasse
#'
#' @param path character, path to calibration run
#' @param config named list, configuration of calibration run
#' @param overwrite logical, should existing data be overwritten?
#'
#' @importFrom dplyr %>% rename left_join arrange reframe ungroup group_by
#'   mutate lag right_join summarise

aggregateMatching <- function(path, config, overwrite = FALSE) {

  # check file path
  calibFilePath <- file.path(path, "calibrationTarget.gdx")
  if (file.exists(calibFilePath)) {
    if (overwrite) {
      warning("Calibration target file '", calibFilePath, "' overwritten.")
    } else {
      stop("Calibration target file '", calibFilePath,
           "' cannot be created as it already exists.")
    }
  }


  symbols <- c("stock", "construction", "renovation", "demolition")
  flows <- setdiff(symbols, "stock")



  # read calibration settings --------------------------------------------------

  periods <- config[["periods"]]
  startyear <- config[["startyear"]]



  # load matching results ------------------------------------------------------

  pathMatching <- config[["matchingRun"]]

  if (!dir.exists(pathMatching)) {
    stop("Cannot find path to matching run: ", pathMatching)
  }

  gdxMatching <- file.path(pathMatching, "output.gdx")

  if (!file.exists(gdxMatching)) {
    stop("Cannot find suitable gdx in matching run directory: ", pathMatching)
  }

  m <- gamstransfer::Container$new(gdxMatching)

  dt <- readSymbol(m, "p_dt") %>%
    rename(dt = "value")

  ttotNum <- readSymbol(m, "ttot") %>%
    getElement("tall")

  # mapping for temporal aggregation
  periodMap <- data.frame(period = periods) %>%
    arrange(.data[["period"]]) %>%
    mutate(from = lag(.data[["period"]]) + 1) %>%
    filter(.data[["period"]] >= startyear) %>%
    group_by(.data[["period"]]) %>%
    reframe(ttot = seq(.data[["from"]], .data[["period"]]),
            from = .data[["from"]]) %>%
    ungroup() %>%
    filter(.data[["ttot"]] %in% ttotNum) %>%
    left_join(dt, by = "ttot") %>%
    mutate(dt = .data[["ttot"]] - pmax(.data[["ttot"]] - .data[["dt"]],
                                       .data[["from"]] - 1)) %>%
    filter(.data[["dt"]] > 0)


  ## temporal compatibility ====

  missingPeriods <- setdiff(periods, ttotNum)
  if (length(missingPeriods) > 0) {
    stop("Some periods of the calibration run are missing in the matching run: ",
         paste(missingPeriods, collapse = ", "))
  }

  # check if all periods are fully covered
  periodsNotCovered <- periodMap %>%
    group_by(.data[["period"]]) %>%
    reframe(covered = sum(.data[["dt"]]),
            years = .data[["period"]] - .data[["from"]] + 1) %>%
    ungroup() %>%
    filter(.data[["covered"]] != .data[["years"]])
  if (nrow(periodsNotCovered) > 0) {
    stop("Some periods of the calibration are not fully covered by matching ",
         "periods:\n", periodsNotCovered)
  }

  periodMap[["from"]] <- NULL


  # read symbols
  matchingResult <- paste0("v_", symbols) %>%
    lapply(readSymbol, m = m) %>%
    `names<-`(symbols)



  # aggregate to calibration resolution ----------------------------------------


  ## flows ====

  calibInput <- lapply(flows, function(s) {
    matchingResult[[s]] %>%
      right_join(periodMap, by = "ttot") %>%
      group_by(across(-all_of(c("ttot", "dt", "value")))) %>%
      summarise(value = sum(proportions(.data[["dt"]]) * .data[["value"]]),
                .groups = "drop") %>%
      rename(ttot = "period")
  }) %>%
    `names<-`(flows)


  ## stock ====

  calibInput[["stock"]] <- matchingResult[["stock"]] %>%
    filter(.data[["ttot"]] %in% periods)



  # write gdx ------------------------------------------------------------------

  mCalib <- gamstransfer::Container$new()

  for (s in symbols) {
    invisible(mCalib$addParameter(
      name = paste0("p_", s, "Hist"),
      domain = head(colnames(calibInput[[s]]), -1),
      records = calibInput[[s]],
      domainForwarding = TRUE
    ))
  }

  mCalib$write(calibFilePath, compress = TRUE)

}
