makeCascade <- function(configs, configFolder) {

  # FUNCTIONS ------------------------------------------------------------------
  # read switch from all configs
  .readSwitch <- function(cfgs, field) {
    lapply(cfgs, function(cfg) cfg[[field]])
  }

  .getScensWithNULL <- function(x) {
    names(x)[vapply(x, is.null, logical(1))]
  }

  .getScensWhereFileExists <- function(x) {
    names(x)[vapply(x, function(item) isTRUE(.fileExists(item)), logical(1))]
  }

  .getScensNotInNames <- function(x) {
    names(x)[!x %in% names(x)]
  }

  .addDependentConfigs <- function(configs, allConfigs, rel) {
    isCascade <- length(configs) > 1
    for (scen in names(configs)) {
      config <- configs[[scen]]
      if (scen %in% rel) {
        isCascade <- TRUE
        dependentScens <- names(rel)[rel == scen]
        attr(config, "nextConfigs") <-
          .addDependentConfigs(allConfigs[dependentScens], allConfigs, rel)
        configs[[scen]] <- config
      }
    }
    if (isCascade) {
      attr(configs, "isCascade") <- TRUE
    }
    return(configs)
  }



  # ORDER CONFIGS --------------------------------------------------------------

  ## configs with historic reference ====

  cfgs <- lapply(configs, readConfig, configFolder = configFolder)
  names(cfgs) <- .readSwitch(cfgs, "title")
  historic <- .readSwitch(cfgs, "historic")


  ## independent configs to start with ====

  scensHistIsNULL <- .getScensWithNULL(historic)
  scensHistExists <- .getScensWhereFileExists(historic)
  scensHistUnknown <- setdiff(.getScensNotInNames(historic),
                              union(scensHistIsNULL, scensHistExists))
  scensIndependent <- unique(c(scensHistIsNULL, scensHistExists, scensHistUnknown))

  if (length(scensIndependent) == 0) {
    stop("There are no independent scenarios. Check for circle dependency.")
  }

  if (length(scensHistUnknown) > 0) {
    warning("There are scenarios that start from a historic scenario that might not be found: ",
            paste(scensHistUnknown, "starting from", historic[scensHistUnknown], collapse = ", "))
  }

  cascade <- .addDependentConfigs(cfgs[scensIndependent], cfgs, historic)

  return(cascade)
}
