writeNextConfigs <- function(configs, path) {
  if (is.null(configs)) {
    return(invisible(NULL))
  }
  if (!dir.exists(path)) {
    dir.create(path)
  }
  cfgNames <- names(configs)
  cfgPaths <- setNames(file.path(path, cfgNames), cfgNames)
  Map(saveConfig, configs, cfgPaths)
  for (nm in cfgNames) {
    writeNextConfigs(attr(configs[[nm]], "nextConfigs", exact = TRUE), cfgPaths[[nm]])
  }
}


readNextConfigs <- function(path) {
  nextScens <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  if (length(nextScens) == 0) {
    return(NULL)
  }
  configs <- lapply(stats::setNames(nm = nextScens), function(scen) {
    scenPath <- file.path(path, scen)
    yamls <- list.files(scenPath, "\\.yaml$")
    if (length(yamls) == 2 && CONFIG_COMPILED %in% yamls) {
      config <- readConfig(file.path(scenPath, CONFIG_COMPILED), readDirect = TRUE)
    } else {
      stop("Can't read next config from here: ", scenPath,
           "\nUnexpected yaml files found: ", paste(yamls, collapse = ", "))
    }
    nextConfigs <- readNextConfigs(scenPath)
    attr(config, "nextConfigs") <- nextConfigs
    config
  })
  attr(configs, "isCascade") <- TRUE
  configs
}
