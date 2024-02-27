#' Plot Summary of a run
#'
#' Plot an overview of the stock and flows
#'
#' @param path character or character vector, path(s) to the run(s)
#' @param facet character, dimension to resolve as facets
#' @param filtering list of named character vectors in the
#'    format c(var = , val =), for each list entry a plot is produced, where the
#'    variable 'var' is filtered for the value 'val'.
#' @param endyear numeric, last time period to be plotted
#' @param showHistStock logical, show given historic next to the modeled stock
#' @param compareGdx character or character vector, path(s) to gdx(s) that
#'    should be compared with gdx(s) in 'path'
#' @param compRelTo numeric, if set: compute the difference relative to either
#'    the original gdx (if set to 1) or the gdx provided in compareGdx (if set to 2).
#'    Can only be used in combination with compareGdx.
#'    Restricts the plot to the stock only and adjusts appearance of the plot.
#' @param computeRenState logical, display the renovation flow in two separate
#'    panels for original and final state
#' @param scenNames character vector, when passing more than one gdx, provide
#'    names for the given inputs and use these to facet by, if no facet is given
#' @param scenNamesShort character vector, when scenNames is specified, provide
#'    shorter scenario names for the file name
#' @param splitRen logical, plot renovation with identical replacement
#'   transparent
#' @param tMin numeric, first time period to be plotted
#' @param nameAdd character, string to be added to the output file name
#'
#' @author Robin Hasse
#'
#' @importFrom quitte revalue.levels
#' @importFrom tidyr replace_na unite
#' @importFrom dplyr row_number n bind_rows any_of group_by across mutate filter
#'   arrange select left_join rename .data %>% bind_rows summarise
#' @importFrom scales gradient_n_pal brewer_pal
#' @importFrom ggplot2 ggplot geom_col geom_area aes_string scale_x_continuous
#'   scale_y_continuous ggtitle theme_classic theme aes geom_line facet_wrap
#'   element_blank element_line scale_fill_manual labs position_stack geom_hline
#'   expand_limits ggsave geom_point facet_grid margin unit scale_alpha_manual
#' @export

plotSummary <- function(path, facet = "typ", filtering = list(NULL), endyear = NULL,
                        showHistStock = FALSE, compareGdx = NULL, compRelTo = NULL,
                        computeRenState = FALSE,
                        scenNames = NULL, scenNamesShort = NULL, splitRen = FALSE, tMin = NULL,
                        nameAdd = "") {

  configPath <- file.path(path[1], "config", "config.yaml")

  if (is.null(endyear)) {
    if (file.exists(configPath)) {
      config <- readConfig(configPath)
      endyear <- config[["endyear"]]
    } else {
      warning("No endyear given and no config available. Aborting.")
      return(NULL)
    }
  }



  # CHECK INPUT ----------------------------------------------------------------
  gdxOut <- lapply(path, function(pa) {
    if (!dir.exists(pa)) {
      stop("Path does not exist: ", pa)
    }

    # find gdx file in given path
    gdxNames <- c("output.gdx",
                  "abort.gdx")
    gdxFiles <- file.path(pa, gdxNames)
    gdx <- head(gdxFiles[which(file.exists(gdxFiles))], 1)
    if (length(gdx) == 0) {
      warning("No suitable gdx file found to plot in ", pa)
      return(NULL)
    }
    return(gdx)
  })



  # READ DATA ------------------------------------------------------------------
  contL <- list()
  for (i in seq_along(gdxOut)) {
    m <- gamstransfer::Container$new(gdxOut[[i]])
    contL <- c(contL, list(m))

    if (!is.null(compareGdx)) {
      if (length(compareGdx) >= i) {
        n <- gamstransfer::Container$new(compareGdx[i])
      } else {
        n <- gamstransfer::Container$new(compareGdx[1])
      }
      contL <- c(contL, list(n))
    }
  }

  m <- contL[[1]]
  dt <- readSymbol(m, "p_dt") %>%
    select("ttot", dt = "value")

  dtVin <- readSymbol(m, "p_dtVin")
  allVin <- readSymbol(m, "vin")
  t2vin <- dtVin %>%
    group_by(.data[["ttot"]]) %>%
    arrange(-.data[["value"]]) %>%
    filter(row_number() == 1) %>%
    select("ttot", "vin") %>%
    mutate(vin = factor(.data[["vin"]], levels(allVin[["vin"]])))

  if (!is.null(compRelTo)) {
    vars <- c(Stock = "v_stock")
  } else {
    vars <- c(
      Stock = "v_stock",
      # Construction = "v_construction",
      # Demolition = "v_demolition",
      Renovation = "v_renovation"
    )
  }

  data <- list()

  for (co in contL) {
    dataCurr <- lapply(vars, function(v) {
      var <- readSymbol(co, v)

      if (!is.null(tMin)) {
        var <- var %>%
          filter(.data[["ttot"]] >= tMin)
      }

      if (is.null(facet)) {
        var[["facet"]] <- "all"
      } else {
        var <- unite(var, "facet", all_of(facet), sep = " | ")
      }

      var <- var %>%
        mutate(bs = factor(.data[["bs"]], c("0", levels(.data[["bs"]]))),
               hs = factor(.data[["hs"]], c("0", levels(.data[["hs"]]))))

      if (all(c("bsr", "hsr") %in% colnames(var))) {
        # mark identical replacement of heating systems and building shell
        var <- var %>%
          mutate(transparent = paste0(
                  ifelse(as.character(.data[["hs"]]) == as.character(.data[["hsr"]]),
                          "hs", "")))
        if (!splitRen && !computeRenState) {
          var[["transparent"]] <- ""
        }

        if (!computeRenState) {
          var <- rbind(
            var %>%
              filter(!(.data[["bsr"]] == "0" & .data[["hsr"]] == "0")) %>%
              select(-"hs", -"bs") %>%
              rename(hs = "hsr", bs = "bsr") %>%
              mutate(renovation = "to"),
            var %>%
              filter(!(.data[["bsr"]] == "0" & .data[["hsr"]] == "0")) %>%
              select(-"hsr", -"bsr") %>%
              mutate(value = -.data[["value"]],
                    renovation = "from")
          )
        }
      }

      return(var)
    })
    if ("Renovation" %in% names(dataCurr)) {
      if (!is.null(compareGdx) & !computeRenState) {
        dataCurr[["Renovation: Initial"]] <- dataCurr[["Renovation"]] %>%
          filter(.data[["renovation"]] == "from") %>%
          mutate(value = -.data[["value"]])
        dataCurr[["Renovation: Final"]] <- dataCurr[["Renovation"]] %>%
          filter(.data[["renovation"]] == "to")
        dataCurr[["Renovation"]] <- NULL
        allNames <- c(names(vars)[-length(vars)], "Renovation: Initial", "Renovation: Final")
      } else {
        if (computeRenState) {
          dataCurr[["Renovation: Identical"]] <- dataCurr[["Renovation"]] %>%
            filter(.data[["transparent"]] == "hs")
          dataCurr[["Renovation: Different"]] <- dataCurr[["Renovation"]] %>%
            filter(.data[["transparent"]] == "")
          dataCurr[["Renovation"]] <- NULL
          allNames <- c(names(vars)[-length(vars)], "Renovation: Identical", "Renovation: Different")
        } else {
          allNames <- names(vars)
        }
      }
    } else {
      allNames <- names(vars)
    }
    data <- c(data, list(dataCurr))
  }

  if (!is.null(compareGdx)) {
    data <- lapply(allNames, function(v) {
      dataAll <- data.frame()
      for (i in seq_along(path)) {
        diffData <- full_join(data[[2*i-1]][[v]] %>%
                                rename(value_1 = "value"),
                              data[[2*i]][[v]] %>%
                                rename(value_2 = "value")) %>%
          mutate(value = .data[["value_1"]] - .data[["value_2"]])
        # if (!is.null(compRelTo)) {
        #   diffData <- diffData %>%
        #     mutate(value = ifelse(abs(.data[["value_1"]]) > 1E-7
        #                           & abs(.data[["value_2"]]) > 1E-7,
        #                           .data[["value"]] / .data[[paste0("value_", compRelTo)]],
        #                           NaN))
        # }
        if (!is.null(scenNames) & is.null(facet)) {
          diffData <- diffData %>%
            mutate(facet = scenNames[i])
        }
        dataAll <- bind_rows(dataAll, diffData)
      }
      return(dataAll)
    })
    # for (v in vars) {
    #   allData <- full_join(data[[1]][[v]] %>%
    #                          rename(value_1 = "value"),
    #                        data[[2]][[v]] %>%
    #                          rename(value_2 = "value")) %>%
    #     mutate(value = .data[["value_1"]] - .data[["value_2"]])
    # }
  } else {
    data <- lapply(allNames, function(v) {
      dataAll <- data.frame()
      for (i in seq_along(path)) {
        dataOne <- data[[i]][[v]]
        if (!is.null(scenNames) & is.null(facet)) {
          dataOne <- dataOne %>%
            mutate(facet = scenNames[i])
        }
        dataAll <- bind_rows(dataAll, dataOne)
      }
      return(dataAll)
    })
  }
  names(data) <- allNames


  # PLOT STYLE -----------------------------------------------------------------

  fillColours <- list()
  fillLabels  <- list()
  fillTitle   <- list(hs = "Heating System", vin = "Construction cohort")

  ## scales and labels ====

  ### heating system ####

  hsMap <- getBrickMapping("heatingSystem.csv")

  fillColours[["hs"]] <- as.character(hsMap[["colour"]])
  fillLabels[["hs"]]  <- as.character(hsMap[["label"]])
  names(fillColours[["hs"]]) <- names(fillLabels[["hs"]]) <- as.character(hsMap[["hs"]])
  fillColours[["hs"]] <- c(`0` = "black", fillColours[["hs"]])
  fillLabels[["hs"]]  <- c(`0` = "no change", fillLabels[["hs"]])


  ### vintage ####

  vinMap <- getBrickMapping("vintage.csv")
  fillColours[["vin"]] <- as.character(vinMap[["colour"]])
  fillLabels[["vin"]]  <- as.character(vinMap[["label"]])
  names(fillColours[["vin"]]) <- names(fillLabels[["vin"]]) <- as.character(vinMap[["vin"]])

  # rescale vintage map
  vins <- getBrickMapping("vintage.csv") %>%
    filter(.data[["from"]] <= endyear) %>%
    getElement("vin")
  fillColours[["vin"]] <- fillColours[["vin"]][vins]
  fillLabels[["vin"]] <- fillLabels[["vin"]][vins]
  fillColours[["vin"]][] <- gradient_n_pal(brewer_pal("div", "Spectral")(9))(
    seq(0, 1, length.out = length(vins))) # nolint: indentation_linter



  ## plot style ====

  addTheme <- function(p, yLabel, fillDim) {
    pOut <- p +
      scale_y_continuous(yLabel, expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 1, 0, 1)) +
      theme_classic() +
      theme(strip.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey", linewidth = .25),
            axis.title.x = element_blank(),
            text = ggplot2::element_text(size = 20))

    pOut <- pOut +
      scale_fill_manual(values = fillColours[[fillDim]],
                        labels = fillLabels[[fillDim]]) +
      ggplot2::scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.3), guide = "none") +
      labs(fill = fillTitle[[fillDim]])

    return(pOut)
  }



  # PLOT -----------------------------------------------------------------------

  for (filterVars in filtering) {

    for (fillDim in c("hs", "vin")) {

      pData <- do.call(bind_rows, lapply(names(data), function(v) {
        d <- data[[v]] %>%
          filter(.data[["ttot"]] <= endyear)

        if (!"vin" %in% colnames(d)) {
          d <- left_join(d, t2vin, by = "ttot")
        }

        if (!is.null(filterVars)) {
          d <- d %>%
            filter(.data[[filterVars[["var"]]]] == filterVars[["val"]])
        }

        if ("transparent" %in% colnames(d)) {
          d[["transparent"]] <- grepl(fillDim, d[["transparent"]])
        }

        if (is.null(compRelTo)) {
          d <- d %>%
            group_by(across(any_of(c("facet", fillDim, "ttot", "renovation",
                                     "transparent")))) %>%
            summarise(value = sum(.data[["value"]], na.rm = TRUE),
                      .groups = "drop")  %>%
            mutate(quantity = v)
        } else {
          d <- d %>%
            group_by(across(any_of(c("facet", fillDim, "ttot", "renovation",
                                     "transparent")))) %>%
            summarise(value = sum(.data[["value"]], na.rm = TRUE),
                      value_1 = sum(.data[["value_1"]], na.rm = TRUE),
                      value_2 = sum(.data[["value_2"]], na.rm = TRUE)) %>%
            mutate(value = .data[["value"]] / .data[[paste0("value_", compRelTo)]] * 100,
                   quantity = v)

        }

        return(d)
      }))

      if ("transparent" %in% colnames(pData)) {
        pData[["transparent"]] <- factor(replace_na(pData[["transparent"]], FALSE), c(TRUE, FALSE))
      }

      pData <- pData %>%
        left_join(dt, by = "ttot") %>%
        mutate(width = 0.9 * .data[["dt"]],
               pos = .data[["ttot"]] - ifelse(.data[["quantity"]] == "Stock",
                                              0, 0.5 * .data[["dt"]]),
               quantity = factor(.data[["quantity"]], names(data)))
      if (is.null(compRelTo)) {
        pData <- pData %>%
          mutate(width = ifelse(.data[["quantity"]] == "Stock",
                                1.5, .data[["width"]]),
                 value = .data[["value"]] / 1000) # million to billion
      }

      # remove negative renovation values unless relevant dims are chosen for fill
      if (!fillDim %in% c("bs", "hs") & "renovation" %in% colnames(pData)) {
        pData <- pData %>%
          filter(replace_na(.data[["renovation"]], "") != "from")
      }

      # remove irrelevant fill  entries from legend
      if (is.factor(pData[[fillDim]])) {
        pData <- revalue.levels(pData, fillDim)
      }


      # position of flow unit
      # flowUnit <- pData %>%
      #   filter(.data[["quantity"]] != "Stock", .data[["value"]] >= 0) %>%
      #   group_by(across(any_of(c("facet", "ttot", "quantity", "renovation", "renState")))) %>%
      #   summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
      #   group_by(across(all_of(c("quantity")))) %>%
      #   summarise(value = max(.data[["value"]]), .groups = "drop") %>%
      #   mutate(pos = min(pData[["ttot"]]) - dt[1, "dt"] * 1.5,
      #          facet = head(pData[["facet"]], 1))


      ## plot ====

      if (is.null(compRelTo)) {
        p <- pData %>%
          ggplot() +
          suppressWarnings(geom_col(aes(.data[["pos"]], .data[["value"]],
                                        width = .data[["width"]],
                                        alpha = .data[["transparent"]],
                                        fill = .data[[fillDim]]))) +
          facet_grid(.data[["quantity"]] ~ .data[["facet"]], scales = "free") +
          geom_hline(yintercept = 0)
        # geom_text(aes(.data[["pos"]], .data[["value"]]),
        #           flowUnit,
        #           label = "/yr", vjust = 1, hjust = .2, size = 6)

        p <- addTheme(p, expression(paste("Floor space [bn ", m^2, "or bn ", m^2, "/yr]")), fillDim) +
          theme(panel.spacing = unit(4, "mm"))
      } else {
        p <- pData %>%
          ggplot() +
          suppressWarnings(geom_col(aes(.data[["pos"]], .data[["value"]],
                                        width = .data[["width"]],
                                        fill = .data[[fillDim]]), position = "dodge")) +
          facet_grid(.data[["quantity"]] ~ .data[["facet"]], scales = "free") +
          geom_hline(yintercept = 0)
        # geom_text(aes(.data[["pos"]], .data[["value"]]),
        #           flowUnit,
        #           label = "/yr", vjust = 1, hjust = .2, size = 6)

        p <- addTheme(p, expression(paste("Relative change [%]")), fillDim) +
          theme(panel.spacing = unit(4, "mm"))
      }


      ## save plot ====

      plotDir <- file.path(path[1], "plots")
      if (!dir.exists(plotDir)) {
        dir.create(plotDir)
      }
      if (is.null(compRelTo)) {
        pHeight <- 22 / 2.54 * length(allNames) / 4
      } else {
        pHeight <- 22 / 2.54 * length(allNames) / 2
        nameAdd <- paste(nameAdd, paste0("relTo", compRelTo), sep = "_")
      }
      if (!is.null(filterVars)) {
        nameAdd <- paste(nameAdd, filterVars[["val"]], sep = "_")
      }
      if (!is.null(compareGdx)) {
        nameAdd <- paste(nameAdd, "diff", sep = "_")
      }
      if (computeRenState) {
        nameAdd <- paste(nameAdd, "renState", sep = "_")
      }
      if (!is.null(scenNamesShort)) {
        scenNames <- scenNamesShort
      }
      if (!is.null(scenNames)) {
        for (sn in scenNames) {
          nameAdd <- paste(nameAdd, sn, sep = "_")
        }
      }
      plotFile <- file.path(plotDir, paste0(paste("summary", paste(facet, collapse = "_"), fillDim,
                                                  nameAdd, sep = "_"), ".png"))
      ggsave(plotFile, p, height = pHeight, width = 35.74 / 2.54, dpi = 600)

    }
  }

}
