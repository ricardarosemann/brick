#' Modify costs as specified in config
#'
#' @param config character, file name  of the config to be used
#' @param method character, specification on how to modify costs
#' @param allScales numeric, scale/values for cost modification
#' @param hs character, heating system for which the costs are modified
#' @param fullRun logical, whether to call brick;
#'   can be switched off to evaluate results after conducting runs with SLURM
#' @param ... named list, arguments passed to initModel
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% .data group_by mutate ungroup
#' @importFrom piamPlotComparison compareScenarios
#' @importFrom yaml read_yaml

sensitivityRun <- function(config, method = "scale", allScales = NULL, hs = "gabo", fullRun = TRUE, ...) {

  configFolder <- brick.file("config")
  output <- "./output"

  allLambda <- c(0.02, 0.1, 0.5)

  if (is.null(allScales)) allScales <- c(0.75, 1, 1.25)
  allCosts <- list("construction", "renovation", "carrier",
                   c("construction", "renovation"), c("construction", "renovation", "carrier"))

  for (ct in allCosts) {
    for (l in allLambda) {
      allScens <- NULL
      for (s in allScales) {
      
        currCfg <- read_yaml(file.path(configFolder, config))
        origTitle <- currCfg[["title"]]
        currName <- gsub("\\.", "-", paste("l", l, "sc", s, paste(ct, collapse = "-"), sep = "_"))

        currCfg[["title"]] <- paste0(currCfg[["title"]], "_", currName)

        currCfg[["costMod"]][["costType"]] <- ct
        currCfg[["costMod"]][["scale"]] <- s
        currCfg[["priceSensHs"]] <- l

        currCfg[["costMod"]][["method"]] <- method
        currCfg[["costMod"]][["hs"]] <- hs

        if (isTRUE(fullRun)) {
          currCfgName <- paste0(paste(sub(".yaml$", "", config), currName), ".yaml")
          write_yaml(currCfg, file.path(configFolder, currCfgName))

          initModel(config = currCfgName, ...)

          file.remove(file.path(configFolder, currCfgName))
        }

        allScens <- c(allScens, currCfg[["title"]])
      }

      if (file.exists(file.path(output, .findScen(allScens[1]), "BRICK_general.mif"))) {
        allMifScens <- file.path(output, vapply(allScens, .findScen, character(1)), "BRICK_general.mif")
        names(allMifScens) <- sub(paste0(origTitle, "_"), "", allScens)
        stamp <- format(Sys.time(), "%Y-%m-%d_%H.%M.%S")

        compScenName <- paste("sensitivity", "l", sub("\\.", "-", l),
                              paste(ct, collapse = "-"), sep = "_")
        outputFolder <- file.path(output, compScenName)
        if (!dir.exists(outputFolder)) {
          dir.create(outputFolder)
        }
        outputFile <- paste0(paste(compScenName, stamp, sep = "_"), ".pdf")
        if (isSlurmAvailable()) {
          mifScenString <- paste(allMifScens, collapse = ",")
          scenNameString <- paste(names(allMifScens), collapse = ",")

          slurmScriptPath <- brick.file("clusterstart", "startCompScenSlurm.R")
          logFilePath <- file.path(outputFolder, "log.txt")
          exitCode <- system(paste0("sbatch --job-name=",
                                    compScenName,
                                    " --output=", logFilePath,
                                    " --mail-type=END",
                                    " --comment=COMPSCEN-BRICK",
                                    " --wrap=\"",
                                    paste("Rscript", slurmScriptPath,
                                          mifScenString, scenNameString,
                                          outputFolder, outputFile),
                                    "\" ",
                                    paste(setSlurmConfig(tasksPerNode = 1), "--time=01:00:00")))

          Sys.sleep(1)

          if (exitCode > 0) {
            message("Executing compareScenarios failed with exit code ", exitCode, ".")
          }
        } else {
          compareScenarios(
            projectLibrary = "reportbrick",
            mifScen = allMifScens, # vector of paths to MIF
            mifHist = allMifScens[1], # pass any valid MIF
            outputFile = outputFile,
            outputDir = "./output",
            mainReg = "DEU",
            yearsHist = NULL
          )
        }
      }
    }

    # if (file.exists(file.path(output, .findScen(allScens[1]), "BRICK_general.mif"))) {
    #   allMifScens <- file.path(output, vapply(allScens, .findScen, character(1)), "BRICK_general.mif")
    #   names(allMifScens) <- allScens
    #   stamp <- format(Sys.time(), "%Y-%m-%d_%H.%M.%S")
    #   compareScenarios(
    #     projectLibrary = "reportbrick",
    #     mifScen = allMifScens, # vector of paths to MIF
    #     mifHist = allMifScens[1], # pass any valid MIF
    #     outputFile = paste0(paste("sensitivity", paste(ct, collapse = "-"), stamp, sep = "_"), ".pdf"),
    #     outputDir = "./output",
    #     mainReg = "DEU",
    #     yearsHist = NULL
    #   )
    # }
  }

}

.findScen <- function(scenName) {

  output <- "./output"

  thisScen <- list.files(path = output, pattern = scenName)
  scenDates <- as.Date(grep("\\d{4}-\\d{2}-\\d{2}_\\d{2}\\.\\d{2}\\.\\d{2}$", thisScen))

  thisScen[which(scenDates == max(scenDates))]
}
