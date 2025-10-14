#' Create calibration targets
#'
#' read results from matching run and aggregate them spatially and temporally
#' to the resolution of calibration runs
#'
#' The temporal aggregation of renovation flows is non-trivial: It is generally
#' possible that buildings can change states multiple time within one aggregated
#' time step but they can't be tracked through renovations individually.
#' Currently, we first assume the mean of renovations across all years of an
#' aggregated time step. In a second step, we perform a renovation correction
#' run with BRICK that minimises deviation from these mean flows and the stock
#' subject to the stock balance and building shell and heating system life
#' times. The correction assures consistency, mostly changing the zero flow that
#' depends most on the temporal resolution. This renovation correction run is
#' performed in a sub folder of the matching run.
#'
#' @author Robin Hasse
#'
#' @param path character, directory of matching folder
#' @param outDir character, directory to save calibration target files in
#' @param calibConfig character, calibration config
#' @param digits integer indicating the number of decimal places
#' @param rmCorrectionRun logical, if \code{TRUE}, the run folder of the
#'   renovation correction is eventually removed
#' @returns named list with aggregated and corrected stock and flows
#'
#' @importFrom utils write.table write.csv
#' @importFrom dplyr .data %>% mutate filter select group_by across all_of
#'   ungroup reframe inner_join arrange summarise right_join
#' @export

createCalibrationTarget <- function(path,
                                    calibConfig,
                                    outDir = NULL,
                                    digits = 4,
                                    rmCorrectionRun = FALSE) {

  # results have to be saved somewhere
  if (is.null(outDir) && isTRUE(rmCorrectionRun)) {
    stop("'rmCorrectionRun' can only be TRUE if you pass a 'outDir'.")
  }



  # FUNCTIONS ------------------------------------------------------------------

  # build mapping between matching and calibration periods
  .buildPeriodMap <- function(cfgCalib, cfgMatching) {
    periods <- cfgCalib$calibperiods

    dt <- data.frame(ttotAgg = periods, dt = c(diff(periods)[1], diff(periods)))

    periodMap <- dt %>%
      group_by(.data$ttotAgg) %>%
      reframe(ttot = seq(to = .data$ttotAgg, length.out = .data$dt))

    missingPeriods <- setdiff(periodMap$ttot, cfgMatching$periods)
    if (length(missingPeriods)) {
      stop("The matching run ", path, "is missing the following periods: ",
           paste(missingPeriods, collapse = ", "))
    }

    periodMap
  }


  # build mapping between matching and calibration regions
  .buildRegionMap <- function(cfgCalib, cfgMatching) {
    cfgRegions <- cfgCalib[["regions"]]

    regionMap <- .findRegionMapping(cfgCalib[["regionmapping"]]) %>%
      select(region = "CountryCode", regionAgg = "RegionCode") %>%
      filter(.data$regionAgg %in% cfgRegions)

    missingRegions <- setdiff(regionMap$region, cfgMatching$regions)
    if (length(missingRegions)) {
      stop("The matching run ", path, "is missing the following regions: ",
           paste(missingRegions, collapse = ", "))
    }

    regionMap
  }


  .relSSE <- function(x, y) {
    if (all(x == y)) {
      return(0)
    }
    sum((x - y)^2) / sum(x^2)
  }


  .dropZero <- function(x, col) {
    if (col %in% names(x)) {
      x <- x[x[[col]] != "0", ]
    }
    x
  }


  .calcMaxDeviation <- function(v, vCorrected, flows, startyear) {
    lapply(setNames(nm = names(v)), function(var) {
      inner_join(v[[var]], vCorrected[[var]],
                 by = setdiff(names(v[[var]]), "value"),
                 suffix = c("Uncorrected", "Corrected")) %>%
        .dropZero("bsr") %>%
        .dropZero("hsr") %>%
        filter(if (var %in% flows) .data$ttot >= startyear else TRUE) %>%
        group_by(across(-any_of(c("bs", "hs", "bsr", "hsr", "valueCorrected", "valueUncorrected")))) %>%
        summarise(relSSE = .relSSE(.data$valueCorrected, .data$valueUncorrected),
                  .groups = "drop") %>%
        ungroup() %>%
        arrange(-.data$relSSE) %>%
        filter(row_number() <= 3)
    })
  }



  # CONFIG ---------------------------------------------------------------------

  cfgCalib <- readConfig(calibConfig)
  cfgMatching <- readConfig(file.path(path, "config", "config_COMPILED.yaml"), readDirect = TRUE)

  periodMap <- .buildPeriodMap(cfgCalib, cfgMatching)
  regionMap <- .buildRegionMap(cfgCalib, cfgMatching)



  # READ MATCHING --------------------------------------------------------------

  matchingGdx <- file.path(path, "output.gdx")
  sequentialRen <- isTRUE(cfgMatching$switches$SEQUENTIALREN)

  vars <- c(
    stock = "v_stock",
    construction = "v_construction",
    demolition = "v_demolition"
  )

  if (sequentialRen) {
    vars[["renovationHS"]] <- "v_renovationHS"
    vars[["renovationBS"]] <- "v_renovationBS"
  } else {
    vars[["renovation"]] <- "v_renovation"
  }

  flows <- setdiff(names(vars), "stock")

  v <- lapply(vars, readSymbol, x = matchingGdx, selectArea = FALSE)



  # AGGREGATION ----------------------------------------------------------------

  ## temporal ====

  v$stock <- v$stock %>%
    filter(.data$ttot %in% cfgCalib$periods)

  v[flows] <- lapply(v[flows], function(flow) {
    flow %>%
      filter(.data$qty == "area") %>%
      right_join(periodMap, by = "ttot") %>%
      group_by(across(-all_of(c("ttot", "value")))) %>%
      summarise(value = mean(.data$value), .groups = "drop") %>%
      rename(ttot = "ttotAgg")
  })


  ## spatial ====

  v <- lapply(v, function(x) {
    x %>%
      right_join(regionMap, by = "region") %>%
      group_by(across(-all_of(c("region", "value")))) %>%
      summarise(value = round(sum(.data$value), digits)) %>%
      rename(region = "regionAgg") %>%
      select(all_of(names(x))) # reorder columns
  })


  # WRITE UNCORRECTED FILES ----------------------------------------------------

  # aggregation folder in matching run path
  outputFolder <- file.path(path, "aggregationForCalibration")
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  uncorrectedFileFolder <- file.path(outputFolder, "uncorrectedAggregation")
  dir.create(uncorrectedFileFolder)

  for (var in names(v)) {
    write.csv(x = v[[var]],
              file = file.path(uncorrectedFileFolder, paste0(var, ".csv")),
              row.names = FALSE)
  }



  # CORRECT RENOVATION ---------------------------------------------------------

  # create run config based on calibration config
  cfgCalib[["switches"]][["RUNTYPE"]] <- "renCorrect"
  cfgCalib[["switches"]][["CALIBRATIONMETHOD"]] <- NULL
  cfgCalib[["title"]] <- paste(basename(path), "for", basename(calibConfig), sep = "_")
  cfgCalib[["matchingRun"]] <- normalizePath(path)
  cfgCalib[["periods"]] <- cfgCalib$calibperiods

  runPath <- initModel(config = cfgCalib,
                       outputFolder = outputFolder,
                       runReporting = FALSE,
                       sendToSlurm = FALSE)

  correctionGdx <- file.path(runPath, "output.gdx")
  vCorrected <- lapply(vars, readSymbol, x = correctionGdx, selectArea = FALSE)

  deviation <- .calcMaxDeviation(v, vCorrected, flows, cfgCalib$startyear)
  message("Largest deviations from renovation correction:")
  print(deviation)



  # OUTPUT ---------------------------------------------------------------------

  # write files into aggregation subfolder
  header <- c(paste0("* matching run: ", path),
              paste0("* calibration config: ", calibConfig),
              paste0("* correction run", if (rmCorrectionRun) " (deleted)", ": ", runPath))
  outFiles <- lapply(names(vCorrected), function(var) {
    outFile <- file.path(runPath, paste0("f_", var, "CalibTarget.cs4r"))
    writeLines(header, outFile)
    write.table(vCorrected[[var]], outFile, append = TRUE, quote = FALSE, sep = ",",
                row.names = FALSE, col.names = FALSE)
    outFile
  })

  # copy files to given directory
  if (!is.null(outDir)) {
    if (!dir.exists(outDir)) {
      dir.create(outDir)
    }
    file.copy(outFiles, outDir, overwrite = TRUE)
    message("Calibration targets written to ", outDir, ".\n")
  }

  # remove run folder
  if (rmCorrectionRun) {
    unlink(runPath, recursive = TRUE, force = TRUE)
    message("Renovation correction run folder removed.")
  }



  return(invisible(vCorrected))
}
