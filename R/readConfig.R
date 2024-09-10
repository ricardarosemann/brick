#' Read config file
#'
#' Read config file in yaml format
#'
#' If no argument is given, the default config is used.
#'
#' @param config character, config file, either a path to a yaml file or the
#'   name of the file in \code{configFolder}
#' @param configFolder character, directory to search for configs. If NULL, the
#'   BRICK-internal config folder is used.
#' @param readDirect logical, specify whether \code{config} is a valid path to a
#'   config file that should be read directly.
#' @returns named list with run config
#'
#' @author Robin Hasse

readConfig <- function(config = NULL, configFolder = NULL, readDirect = FALSE) {

  # check config files that are based on the current to avoid circle dependency
  basisOf <- attr(config, "basisOf")
  if (any(duplicated(basisOf))) {
    stop("There is a circle dependency in how configs are based on another: ",
         paste(basisOf, collapse = " -> "))
  }

  # Directly read the file without considering basis configs
  if (readDirect) {
    if (!file.exists(config)) {
      stop("The config", config, "that you want read directly does not exist. ",
           "If this is a restart run, your run folder likely misses the config file.")
    }
    if (!is.null(configFolder)) {
      warning("'configFolder' is ignored as the config is read directly from here: ",
              config)
    }
    return(.readCfg(config))
  }

  # use default in config folder if available, otherwise internal default
  internalConfigFolder <- brick.file("config")
  if (is.null(configFolder)) {
    configFolder <- internalConfigFolder
  } else {
    if (!dir.exists(configFolder)) {
      stop("Directory of 'configFolder' doesn't exist: ", configFolder)
    }
  }
  defaultCfgPath <- file.path(configFolder, "default.yaml")
  if (!file.exists(defaultCfgPath)) {
    defaultCfgPath <- file.path(internalConfigFolder, "default.yaml")
  }

  # use default.yaml by default
  if (is.null(config)) {
    if (is.null(basisOf)) {
      config <- defaultCfgPath
      message("Using default config: ", config)
    }
  }

  # find file to given config and read yaml file
  customCfgPath <- .findCfg(config, configFolder, isFinalCfg = is.null(basisOf))
  customCfg <- .readCfg(customCfgPath)

  # end recursion: default config is not based on another config
  if (file.path(customCfgPath) == file.path(defaultCfgPath)) {
    return(customCfg)
  }

  # read config on which this config is based
  basedOn <- customCfg[["basedOn"]]
  basedOn <- if (is.null(basedOn)) {
    defaultCfgPath
  } else if (length(basedOn) == 1) {
    basedOn
  } else {
    stop("Don't give more than one other config that this config is based on.")
  }

  # read base config and overwrite it with given config
  attr(basedOn, "basisOf") <- c(basisOf, customCfgPath)
  basedOnCfg <- readConfig(basedOn, configFolder)
  cfg <- .overwriteList(x = basedOnCfg, y = customCfg,
                        isFinalCfg = is.null(basisOf),
                        defaultCfgPath = defaultCfgPath)

  # save chain of inheriting config files
  attr(cfg, "file") <- attr(customCfg, "file")
  attr(cfg, "basedOn") <- c(attr(basedOnCfg, "file"),
                            attr(basedOnCfg, "basedOn"))

  return(cfg)
}





#' Read config file
#'
#' Read yaml file from given path, check minimum requirement (title exists) and
#' save the file path as a attribute.
#'
#' @param file character, path to config file
#' @returns named list with config parameters
#'
#' @importFrom yaml read_yaml

.readCfg <- function(file) {
  cfg <- read_yaml(file)
  if (!"title" %in% names(cfg)) {
    stop("There is no title given in the config file: ", file)
  }
  attr(cfg, "file") <- normalizePath(file)
  return(cfg)
}





#' Find config file path
#'
#' Search for config file in multiple steps. The file is found if
#'   * config is a full file path already
#'   * config is the name of a file in the config folder
#'   * there is exactly one file in \code{configFolder} matching the pattern
#'     passed via config
#'
#' @param config character, config file, either a path to a yaml file or the
#'   name of the file in `inst/config/`
#' @param configFolder character, directory to search for configs. If NULL, the
#'   BRICK-internal config folder is used.
#' @param isFinalCfg logical, is this the final config and not an intermediate?
#' @returns file path to config

.findCfg <- function(config, configFolder, isFinalCfg) {
  if (is.character(config)) {
    if (file.exists(config)) {
      customCfgPath <- config
    } else {
      customCfgPath <- file.path(configFolder, config)
      if (!file.exists(customCfgPath)) {
        matchingFiles <- list.files(configFolder, pattern = config,
                                    full.names = TRUE, recursive = TRUE)
        if (length(matchingFiles) == 0) {
          stop("Cannot find a config yaml file matching '", config,
               "' in this configFolder: ", configFolder)
        } else if (length(matchingFiles) == 1) {
          customCfgPath <- matchingFiles
          if (isFinalCfg) {
            message("Using matching config ", matchingFiles)
          }
        } else {
          stop("Be more specific! There is more than one matching config file:\n  ",
               paste(matchingFiles, collapse = "\n  "))
        }
      }
    }
    return(customCfgPath)
  }
  stop("'config' has to be a character object pointing to a config file, ",
       "not a ", class(config))
}





#' Overwrite list with another list
#'
#' Overwrite a named list with another named list. The result corresponds to the
#' overwritten list unless a value is overwritten by the overwriting list.
#'
#' This function is called recursively until the default config is reached.
#' Therefor, the list structure will always correspond to the default config.
#' No config based directly or indirectly on the default can have list keys that
#' the default config doesn't have.
#'
#' @param x named list, provides the structure and default values that can be
#'   overwritten by \code{y}.
#' @param y named list, overwrites \code{x} wherever it has values different
#'   from NULL. Cannot have keys that are not specified in \code{x}.
#' @param isFinalCfg logical, is this the final config and not an intermediate?
#' @param defaultCfgPath character, path to default config. Only used for more
#'   helpful error message.
#' @returns named list with the structure of \code{x} that is (partly)
#'   overwritten by \code{y}.

.overwriteList <- function(x, y, isFinalCfg, defaultCfgPath) {

  stopifnot(`x has to be a list.`       = is.list(x),
            `y has to be a list.`       = is.list(y),
            `x has to be a named list.` = !is.null(names(x)),
            `y has to be a named list.` = !is.null(names(y)))

  missingDefault <- setdiff(names(y), c(names(x), "basedOn"))
  if (length(missingDefault) > 0) {
    stop("The config keys ",
         paste(paste0("'", missingDefault, "'"), collapse = ", "),
         " from your config are not defined in the default config ",
         defaultCfgPath)
  }

  out <- list()
  for (key in names(x)) {
    if (key %in% names(y)) {
      if (is.list(x[[key]])) {
        if (length(x[[key]]) > 0) {
          if (is.list(y[[key]])) {
            out[[key]] <- .overwriteList(x[[key]], y[[key]],
                                         isFinalCfg, defaultCfgPath)
          } else {
            stop("For the key '", key, "', your config has only one value ",
                 "but there is a list in the default config ", defaultCfgPath)
          }
        } else {
          if (is.list(y[[key]]) || length(y[[key]] > 1)) {
            out[[key]] <- y[[key]]
          } else if (is.null(y[[key]])) {
            out[key] <- list(NULL)
          } else {
            stop("For the key '", key, "', your config has a single value ",
                 "where it should have either a list or NULL as there is an ",
                 "empty list in the default config ", defaultCfgPath)
          }
        }
      } else {
        if (is.null(y[[key]])) {
          out[key] <- list(NULL)
        } else if (is.list(y[[key]])) {
          stop("For the key '", key, "', your config has a list of values ",
               "but there is just one value in the default config ",
               defaultCfgPath)
        } else {
          out[[key]] <- y[[key]]
        }
      }
    } else {
      if (is.null(x[[key]])) {
        out[key] <- list(NULL)
      } else {
        if (identical(x[[key]], list()) && isFinalCfg) {
          out[key] <- list(NULL)
        } else {
          out[[key]] <- x[[key]]
        }
      }
    }
  }
  return(out)
}
