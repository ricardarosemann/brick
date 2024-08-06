#' Modify costs as specified in config
#'
#' @param config character, file name  of the config to be used
#' @param method character, specification on how to modify costs
#' @param allScales numeric, scale/values for cost modification
#' @param hs character, heating system for which the costs are modified
#' @param ... named list, arguments passed to initModel
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% .data group_by mutate ungroup
#' @importFrom piamPlotComparison compareScenarios
#' @importFrom yaml read_yaml

sensitivityRun <- function(config, method = "scale", allScales = NULL, hs = "gabo", ...) {

  configFolder <- brick.file("config")
  output <- "./output"

  allLambda <- c(0.02, 0.1, 0.5)

  if (is.null(allScales)) allScales <- c(0.75, 1, 1.25)
  allCosts <- list("construction", "renovation", "carrier",
                   c("construction", "renovation"), c("construction", "renovation", "carrier"))

  for (ct in allCosts) {
    allScens <- NULL
    for (s in allScales) {
      for (l in allLambda) {
        currCfg <- read_yaml(file.path(configFolder, config))
        currName <- gsub("\\.", "-", paste("l", l, "sc", s, paste(ct, collapse = "-"), sep = "_"))

        currCfg[["title"]] <- paste0(currCfg[["title"]], "_", currName)

        currCfg[["costMod"]][["costType"]] <- ct
        currCfg[["costMod"]][["scale"]] <- s
        currCfg[["priceSensHs"]] <- l

        currCfg[["costMod"]][["method"]] <- method
        currCfg[["costMod"]][["hs"]] <- hs

        currCfgName <- paste0(paste(sub(".yaml$", "", config), currName), ".yaml")
        write_yaml(currCfg, file.path(configFolder, currCfgName))

        initModel(config = currCfgName, ...)

        allScens <- c(allScens, currCfg[["title"]])
        file.remove(file.path(configFolder, currCfgName))
      }
    }

    allMifScens <- file.path(output, vapply(allScens, .findMif, character(1)), "BRICK_general.mif")
    names(allMifScens) <- allScens
    compareScenarios(
      projectLibrary = "reportbrick",
      mifScen = allMifScens, # vector of paths to MIF
      mifHist = "./output/noShellDEU_2024-08-01_14.06.02/BRICK_general.mif", # pass any valid MIF
      outputFile = paste("sensitivity", paste(ct, collapse = "-"), sep = "_"),
      outputDir = "./output",
      mainReg = "DEU",
      yearsHist = NULL
    )
  }

}

.findMif <- function(scenName) {

  output <- "./output"

  thisScen <- list.files(path = output, pattern = scenName)
  scenDates <- as.Date(grep("\\d{4}-\\d{2}-\\d{2}_\\d{2}\\.\\d{2}\\.\\d{2}$", thisScen))

  thisScen[which(scenDates == max(scenDates))]
}
