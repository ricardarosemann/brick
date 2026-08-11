#' Update bundle of scenario runs
#'
#' Run a bundle of scenario runs or update selected runs. This function requires
#' local configs and a bundle setting file. The settings file serves two
#' purposes: First, it holds the settings that control which scenarios should be
#' updated when running this function. Second, it holds the run paths to the
#' most recent runs that should be used by post processing, reporting or
#' plotting routines.
#'
#' This function provides convenient features for scenario studies but it
#' assumes a specific setup that is advised to follow and might not perform
#' otherwise:
#' \itemize{
#'   \item Choose one central directory from where you call this function.
#'   \item Place a bundle settings file in this directory. You can use this
#'     function to create a template.
#'   \item Place a config folder and an output folder inside the central
#'     directory and reference both as relative paths in the settings file.
#' }
#' Mind that this function changes the config files. This should only affect the
#' starting point and historic. But it is advisable to commit the config files
#' to git before to avoid loosing information.
#'
#' @author Robin Hasse
#'
#' @param bundleSettings character, path to settings file. If the file doesn't
#'   exist, a new file is written as a template.
#' @returns path to the started runs
#'
#' @export

updateScenarioRuns <- function(bundleSettings) {

  if (!identical(normalizePath(getwd()),
                 normalizePath(dirname(bundleSettings)))) {
    warning("Working directory and settings file location are not identical. ",
            "This can lead to unexpected behaviour.")
  }

  if (!file.exists(bundleSettings)) {
    .createNewSettings(bundleSettings)
    message("Bundle settings file did not exist so a new template was written: ",
            bundleSettings,
            "\nAdapt the template and rerun.")
    return(invisible(NULL))
  }

  settings <- .readSettings(bundleSettings)
  configs <- .getConfigs(settings)

  settings <- .initialiseScenarios(settings, bundleSettings)
  settings <- .updateRefScens(configs, settings, "startingPoint", allowSelfRef = FALSE)
  settings <- .updateRefScens(configs, settings, "historic")

  yaml::write_yaml(settings, bundleSettings)
  startScens <- names(settings$newRunNext[which(unlist(settings$newRunNext))])

  runPaths <- if (length(startScens) > 0) {
    message("starting the following scenarios as bundle: ",
            paste(startScens, collapse = ", "))
    initModel(config = configs[startScens],
              configFolder = .getSettingsPath(settings, "configFolder"),
              outputFolder = .getSettingsPath(settings, "outputFolder"),
              sendToSlurm = settings$sendToSlurm,
              .bundleSettings = normalizePath(bundleSettings))
  } else {
    NULL
  }

  return(invisible(runPaths))

}





#' Read bundle settings
#'
#' This function also checks that the settings contain all required keys and
#' only arguments to \code{initModel} as further arguments.
#'
#' @param path character, path to bundle settings file
#' @returns named list with settings

.readSettings <- function(path) {
  settings <- yaml::read_yaml(path)
  keys <- names(settings)

  requiredKeys <- c("newRunAlways", "run", "newRunNext", "configFolder")
  missingKeys <- setdiff(requiredKeys, keys)
  if (length(missingKeys) > 0) {
    stop("Missing keys in settings file ", path, " detected: ",
         paste(missingKeys, collapse = ", "))
  }

  furtherAllowedKeys <- setdiff(names(formals(initModel)), c("config", "path", "restart"))
  wrongKeys <- setdiff(keys, union(requiredKeys, furtherAllowedKeys))
  if (length(wrongKeys) > 0) {
    stop("Unallowed keys in settings file ", path, " detected: ",
         paste(wrongKeys, collapse = ", "))
  }

  attr(settings, "dirname") <- dirname(path)

  return(settings)
}





#' Change config switch
#'
#' Reset value of specified switch in given config. If the switch has not been
#' found in the config before, it is appended at the end to avoid disturbing the
#' nesting of the yaml file.
#'
#' @param configPath character, file path to config
#' @param param character, name of switch
#' @param value value of switch
#' @param comment character, comment behind switch value

.changeConfig <- function(configPath, param, value, comment = NULL) {
  cfg <- readLines(configPath)
  pattern <- paste0("^", param, ":")
  pos <- grep(pattern, cfg)
  cfg <- cfg[!grepl(pattern, cfg)]
  newLine <- yaml::as.yaml(setNames(list(value), param))
  newLine <- strsplit(newLine, "\n")[[1]]
  if (!is.null(comment)) {
    newLine <- paste(newLine, "#", comment)
  }
  cfg <- append(cfg, newLine, after = if (length(pos) == 1) pos - 1 else length(cfg))
  writeLines(cfg, configPath)
}





#' Get configs
#'
#' @param settings named list with bundle settings
#' @returns named list of selected config files

.getConfigs <- function(settings) {
  configFolder <- .getSettingsPath(settings, "configFolder")
  if (!dir.exists(configFolder)) {
    stop("Config folder doesn't exist: ", configFolder)
  }
  configs <- list.files(configFolder, "^.+\\.yaml$", full.names = TRUE)
  configNames <- .getFileName(configs)
  selectedConfigs <- names(settings$run)
  missingConfigs <- setdiff(selectedConfigs, configNames)
  if (length(missingConfigs) > 0) {
    stop("Can't find the following configs in the config folder: ",
         paste(missingConfigs, collapse = ", "),
         if (all(missingConfigs %in% .getPlaceholderConfigNames())) {
           "\nYou might need to specify placeholder config names in the settings."
         } else {
           ""
         })
  }
  setNames(configs, configNames)[selectedConfigs]
}





#' Get placeholder config names
#'
#' @returns vector of config names

.getPlaceholderConfigNames <- function() {
  paste0("<config", 1:3, ">")
}





#' Create new scenario bundle settings file
#'
#' Writes template for settings file to given path. The file might have to be
#' revisited to specify place holders.
#'
#' @param path character, path of file to be written
#' @param configFolderName,outputFolderName character, config and output folders
#'   to look for next to settings file to be written. It is advised to have the
#'   folders and the settings file in the same directory but any sub directory
#'   relative to the settings file is accepted.
#' @returns template (invisibly)
#'
#' @author Robin Hasse

.createNewSettings <- function(path,
                               configFolderName = "config",
                               outputFolderName = "output") {

  configFolder <- file.path(dirname(path), configFolderName)
  configFolderRel <- if (dir.exists(configFolder)) {
    configFolderName
  } else {
    warning("No config folder found. ",
            "Please revisit config folder path in the template.")
    paste0("<", configFolderName, ">")
  }

  configs <- sub("\\..*$", "", list.files(configFolder, "\\.yaml$"))
  if (length(configs) == 0) {
    configs <- .getPlaceholderConfigNames()
    warning("No potential config files found. ",
            "Please revisit run names in the template.")
  }

  template <- c(
    "newRunAlways: no",
    "run:",
    paste0("  ", configs, ": ~"),
    "newRunNext:",
    paste0("  ", configs, ": no"),
    paste0("configFolder: ", configFolderRel),
    paste0("outputFolder: ", outputFolderName)
  )

  writeLines(template, path)
  return(invisible(template))
}





#' Get path from settings file
#'
#' Relative paths are assumed to relate to the directory of the settings file.
#'
#' @param settings named list with bundle settings
#' @param pathName character, name of path in settings file
#' @returns absolute path
#'
#' @author Robin Hasse

.getSettingsPath <- function(settings, pathName) {
  path <- settings[[pathName]]
  if (xfun::is_rel_path(path)) {
    file.path(normalizePath(attr(settings, "dirname")), path)
  }
  path
}





#' Initialise scenarios
#'
#' @param settings named list with bundle settings
#' @param bundleSettings character, path to settings file. If the file doesn't
#'   exist, a new file is written as a template.
#' @returns updated settings
#'
#' @author Robin Hasse

.initialiseScenarios <- function(settings, bundleSettings) {

  if (isTRUE(settings$newRunAlways)) {
    settings$newRunNext[] <- rep(TRUE, length(settings$newNextRun))
    return(settings)
  }

  runs <- .getBundleRunPaths(settings)

  settings$newRunAlways <- FALSE
  settings$newRunNext[!unlist(lapply(runs, .fileExists))] <- TRUE
  yaml::write_yaml(settings, bundleSettings)

  return(settings)
}





#' Read config Switch
#'
#' @param configPath character, path to config file
#' @param switch character, name of config switch
#' @returns named list with switch value and comment. \code{NULL} is switch is
#'   missing in config

.readConfigSwitch <- function(configPath, switch) {
  pattern <- paste0("^", switch, ": ([^#]+)#?(.*)$")

  cfg <- readLines(configPath)
  pos <- grep(pattern, cfg)

  if (length(pos) == 0) {
    return(NULL)
  }

  txt <- cfg[pos]
  value <- yaml::yaml.load(cfg)[[switch]]

  if (is.null(value)) {
    return(NULL)
  }

  comment <- sub(pattern, "\\2", txt)
  comment <- trimws(comment)

  return(list(value = value, comment = comment))
}





#' Update referenced scenario
#'
#' @param configs named character vector of config file paths
#' @param settings named list with bundle settings
#' @param param character, name of config switch
#' @param allowSelfRef logical, if TRUE, the config parameter can reference
#'   itself
#' @returns update settings

.updateRefScens <- function(configs, settings, param, allowSelfRef = TRUE) {
  .changeMsg <- function(x) {
    paste0("changed according to bundle settings [", x, "]")
  }

  scens <- names(settings$run)
  startScens <- names(settings$newRunNext[which(unlist(settings$newRunNext))])
  scensToUpdate <- startScens

  while (length(scensToUpdate) > 0) {
    scen <- scensToUpdate[[1]]
    config <- configs[[scen]]
    switch <- .readConfigSwitch(config, param)

    if (is.null(switch)) {
      scensToUpdate <- setdiff(scensToUpdate, scen)
      next
    }

    previousRun <- settings$run[[switch$value]]
    if (is.null(previousRun)) {
      previousGdx <- NULL
    } else {
      previousRun <- file.path(settings$outputFolder, previousRun)
      previousGdx <- file.path(previousRun, "output.gdx")
    }

    ref <- sub("^.*\\[(.*)\\]$", "\\1", switch$comment)

    if (switch$value == scen) {
      if (.fileExists(previousGdx)) {
        .changeConfig(config, param, previousRun, .changeMsg(switch$value))
      } else {
        message(scen, ": removed self-referencing", param, ".")
        .changeConfig(config, param, NULL, .changeMsg(switch$value))
      }
    } else if (switch$value %in% startScens) {
      .changeConfig(config, param, switch$value, .changeMsg(switch$value))
    } else if (allowSelfRef && ref %in% startScens) {
      # dynamic link to a scenario within start bundle
      .changeConfig(config, param, ref, .changeMsg(ref))
      if (ref != switch$value) {
        message(scen, ": changed ", param, " to run that is started in this bundle: ", ref)
      }
    } else if (switch$value %in% scens) {
      # dynamic link to a scenario outside start bundle
      if (.fileExists(previousGdx)) {
        # make explicit link to previous run
        .changeConfig(config, param, previousRun, .changeMsg(switch$value))
        message(scen, ": changed ", param, " to previous run: ", previousRun)
      } else if (ref %in% scens) {
        # add linked scenario to start bundle
        startScens <- c(startScens, ref)
        scensToUpdate <- c(scensToUpdate, ref)
        settings$run[ref] <- list(NULL)
        message(scen, ": changed ", param, " to previous run: ", previousRun)

        .changeConfig(config, param, ref, .changeMsg(ref))

      }
    }
    scensToUpdate <- setdiff(scensToUpdate, scen)
  }

  # update start scenarios
  settings$newRunNext[startScens] <- TRUE

  return(settings)
}
