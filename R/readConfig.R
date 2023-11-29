#' Read config file
#'
#' Read config file in yaml format
#'
#' If no argument is given, the default config is used.
#'
#' @param config character, config file, either a path to a yaml file or the
#'   name of the file in `inst/config/`
#' @param basisOf character, name of other config that is based on this config
#' @returns named list with run config
#'
#' @author Robin Hasse
#'
#' @importFrom yaml read_yaml

readConfig <- function(config = NULL, basisOf = NULL) {

  configFolder <- brick.file("config")
  defaultCfgPath <- file.path(configFolder, "default.yaml")

  if (!file.exists(defaultCfgPath)) {
    stop("Default config ", defaultCfgPath, " does not exist.")
  }



  # check input ----------------------------------------------------------------

  # list all config files this config is based on
  readCfg <- function(file) {
    cfg <- read_yaml(file)
    if (!"title" %in% names(cfg)) {
      stop("There is no title given in the config file: ", file)
    }
    attr(cfg, "file") <- file
    attr(cfg, "files") <- file
    return(cfg)
  }

  # use default.yaml by default
  if (is.null(config)) {
    if (is.null(basisOf)) {
      message("Using default config: ", config)
    }
    return(readCfg(defaultCfgPath))
  }

  # find file to given config
  if (is.character(config)) {
    if (file.exists(config)) {
      customCfgPath <- config
    } else {
      matchingFiles <- list.files(configFolder, config, full.names = TRUE,
                                  recursive = TRUE)
      if (length(matchingFiles) == 0) {
        stop("Cannot find a config yaml file matching your input: ", config)
      } else if (length(matchingFiles) == 1) {
        customCfgPath <- matchingFiles
        if (is.null(basisOf)) {
          message("Using matching config ", matchingFiles)
        }
      } else {
        stop("Be more specific! There is more than one matching config file:\n  ",
             paste(matchingFiles, collapse = "\n  "))
      }
    }
  } else {
    stop("'config' has to be a character object pointing to a config file, ",
         "not a ", class(config))
  }

  # interrupt circle dependency
  if (!is.null(basisOf)) {
    if (basisOf == customCfgPath) {
      stop("This config is based on itself: ", basisOf)
    }
  }

  # read yaml
  customCfg <- readCfg(customCfgPath)
  if (file.path(customCfgPath) == file.path(defaultCfgPath)) {
    return(readCfg(defaultCfgPath))
  }



  # overwrite default config ---------------------------------------------------

  # function that takes x as default and overwrites with y if specified
  overwriteList <- function(x, y) {
    stopifnot(`x has to be a list.`       = is.list(x),
              `y has to be a list.`       = is.list(y),
              `x has to be a named list.` = !is.null(names(x)),
              `y has to be a named list.` = !is.null(names(y)))
    missingDefault <- setdiff(names(y), c(names(x), "basedOn"))
    if (length(missingDefault) > 0) {
      stop("The config keys ",
           paste(paste0("'", missingDefault, "'"), collapse = ", "),
           "from your config are not defined in the default config ",
           defaultCfgPath)
    }
    out <- list()
    for (key in names(x)) {
      if (key %in% names(y)) {
        if (is.list(x[[key]])) {
          if (length(x[[key]]) > 0) {
            if (is.list(y[[key]])) {
              out[[key]] <- overwriteList(x[[key]], y[[key]])
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
          if (identical(x[[key]], list()) && is.null(basisOf)) {
            # replace empty list with NULL in final output
            out[key] <- list(NULL)
          } else {
            out[[key]] <- x[[key]]
          }
        }
      }
    }
    return(out)
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
  basedOnCfg <- readConfig(basedOn, customCfgPath)
  cfg <- overwriteList(basedOnCfg, customCfg)
  attr(cfg, "file") <- customCfgPath
  attr(cfg, "files") <- c(attr(basedOnCfg, "files"), customCfgPath)

  return(cfg)
}
