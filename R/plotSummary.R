#' Plot Summary of a run
#'
#' Plot an overview of the stock and flows
#'
#' @param path character, path to the run
#' @param facet character, dimension to resolve as facets
#' @param showHistStock logical, show given historic next to the modeled stock
#' @param splitRen logical, plot renovation with identical replacement
#'   semi-transparent
#'
#' @author Robin Hasse
#'
#' @importFrom quitte revalue.levels
#' @importFrom tidyr replace_na unite
#' @importFrom scales gradient_n_pal brewer_pal
#' @importFrom gamstransfer Container
#' @importFrom dplyr row_number n bind_rows any_of group_by across mutate filter
#'   arrange select left_join rename .data %>% bind_rows summarise pull
#' @importFrom ggplot2 ggplot geom_col geom_area scale_x_continuous geom_point
#'   scale_y_continuous theme_classic theme aes geom_line scale_alpha_manual
#'   element_blank element_line scale_fill_manual labs geom_hline ggsave unit
#'   facet_grid
#' @export

plotSummary <- function(path, facet = "typ", showHistStock = FALSE,
                        splitRen = FALSE) {

  config <- readConfig(file.path(path, "config", CONFIG_COMPILED), readDirect = TRUE)
  endyear <- config[["endyear"]]
  seqRen <- isTRUE(config[["switches"]][["SEQUENTIALREN"]])
  ignoreShell <- isTRUE(config[["ignoreShell"]])



  # CHECK INPUT ----------------------------------------------------------------

  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  # find gdx file in given path
  gdxNames <- c("output.gdx",
                "abort.gdx")
  gdxFiles <- file.path(path, gdxNames)
  gdx <- head(gdxFiles[which(file.exists(gdxFiles))], 1)
  if (length(gdx) == 0) {
    warning("No suitable gdx file found to plot in ", path)
    return(NULL)
  }



  # READ DATA ------------------------------------------------------------------

  m <- Container$new(gdx)


  dt <- readSymbol(m, "p_dt") %>%
    select("ttot", dt = "value")

  dtVin <- readSymbol(m, "p_dtVin")
  t2vin <- dtVin %>%
    group_by(.data[["ttot"]]) %>%
    arrange(-.data[["value"]]) %>%
    filter(row_number() == 1) %>%
    select("ttot", "vin")

  vars <- c(
    Stock = "v_stock",
    Construction = "v_construction",
    Demolition = "v_demolition"
  )
  vars <- if (seqRen) {
    if (ignoreShell) {
      c(vars,
        RenovationHS = "v_renovationHS")
    } else {
      c(vars,
        RenovationBS = "v_renovationBS",
        RenovationHS = "v_renovationHS")
    }
  } else {
    c(vars,
      Renovation = "v_renovation")
  }

  data <- lapply(vars, function(v) {
    var <- readSymbol(m, v)

    if (!is.null(config[["startyear"]])) {
      var <- var[var$ttot >= config[["startyear"]], ]
    }

    if (showHistStock && v == "v_stock") {
      var[["historic"]] <- FALSE
      var <- readSymbol(m, "p_stockHist") %>%
        mutate(historic = TRUE) %>%
        rbind(var)
    }

    if (is.null(facet)) {
      var[["facet"]] <- "all"
    } else {
      var <- unite(var, "facet", all_of(facet), sep = " | ")
    }


    renDims <- intersect(colnames(var), c("bsr", "hsr"))

    if (length(renDims) > 0) {

      # remove entirely untouched buildings
      var <- if (seqRen) {
        filter(var, .data[[renDims]] != "0")
      } else {
        filter(var, !(.data[["bsr"]] == "0" & .data[["hsr"]] == "0"))
      }

      # mark identical replacement of heating systems and building shell
      var$transparent <- character(nrow(var))
      for (to in renDims) {
        from <- switch(to, bsr = "bs", hsr = "hs")

        if (splitRen) {
          var$transparent <- paste0(
            var$transparent,
            ifelse(as.character(var[[from]]) == as.character(var[[to]]), from, "")
          )
        }
      }

      # make origin negative and target state positive
      var <- bind_rows(
        var %>%
          select(-any_of(c("bs", "hs"))) %>%
          rename(any_of(c(hs = "hsr", bs = "bsr"))) %>%
          mutate(renovation = "to"),
        var %>%
          select(-any_of(c("hsr", "bsr"))) %>%
          mutate(value = -.data$value,
                 renovation = "from")
      )
    }

    return(var)
  })



  # PLOT STYLE -----------------------------------------------------------------

  fillColours <- list()
  fillLabels  <- list()
  fillTitle   <- list(bs = "Building shell", hs = "Heating System", vin = "Construction cohort")

  ## scales and labels ====

  ### building shell ####

  bsMap <- getBrickMapping("dim_bs.csv")
  fillLabels[["bs"]]  <- paste(c("Low", "Medium", "High"), "efficiency")
  fillColours[["bs"]] <- mip::plotstyle(fillLabels[["bs"]])
  names(fillLabels[["bs"]]) <- names(fillColours[["bs"]]) <- bsMap$bs
  fillColours[["bs"]] <- c(`0` = "black", fillColours[["bs"]])
  fillLabels[["bs"]]  <- c(`0` = "no change", fillLabels[["bs"]])

  ### heating system ####

  hsMap <- getBrickMapping("dim_hs.csv")
  fillColours[["hs"]] <- pull(hsMap, "colour", "hs")
  fillLabels[["hs"]]  <- pull(hsMap, "label", "hs")
  fillColours[["hs"]] <- c(`0` = "black", fillColours[["hs"]])
  fillLabels[["hs"]]  <- c(`0` = "no change", fillLabels[["hs"]])


  ### vintage ####

  vinMap <- getDimMap("vin", config[["granularity"]])
  fillColours[["vin"]] <- as.character(vinMap[["colour"]])
  fillLabels[["vin"]]  <- as.character(vinMap[["label"]])
  names(fillColours[["vin"]]) <- names(fillLabels[["vin"]]) <- as.character(vinMap[["vin"]])

  # rescale vintage map
  vins <- vinMap %>%
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
      scale_x_continuous(expand = c(0, 0.075)) +
      theme_classic() +
      theme(strip.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey", linewidth = .25),
            axis.title.x = element_blank())

    pOut <- pOut +
      scale_fill_manual(values = fillColours[[fillDim]],
                        labels = fillLabels[[fillDim]]) +
      scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.3), guide = "none") +
      labs(fill = fillTitle[[fillDim]])

    return(pOut)
  }



  # PLOT -----------------------------------------------------------------------

  # Don't create building shell plot if shell renovations are suppressed
  fillDimensions <- if (ignoreShell) c("hs", "vin") else c("bs", "hs", "vin")

  for (fillDim in fillDimensions) {

    pData <- do.call(bind_rows, lapply(names(data), function(v) {
      d <- data[[v]] %>%
        filter(.data[["ttot"]] <= endyear)

      if (!"vin" %in% colnames(d)) {
        d <- left_join(d, t2vin, by = "ttot")
      }

      if ((fillDim == "bs" && v == "RenovationHS") || (fillDim == "hs" && v == "RenovationBS")) {
        return(NULL)
      }

      if ("transparent" %in% colnames(d)) {
        d[["transparent"]] <- grepl(fillDim, d[["transparent"]])
      }

      d <- d %>%
        group_by(across(any_of(c("facet", fillDim, "ttot", "renovation", "historic",
                                 "transparent")))) %>%
        summarise(value = sum(.data[["value"]], na.rm = TRUE),
                  .groups = "drop")  %>%
        filter(.data[["value"]] != 0) %>%
        mutate(quantity = v)

      return(d)
    }))

    pData[["transparent"]] <- factor(replace_na(pData[["transparent"]], FALSE), c(TRUE, FALSE))

    pData <- pData %>%
      left_join(dt, by = "ttot") %>%
      mutate(width = ifelse(.data[["quantity"]] == "Stock",
                            min(.data[["dt"]]) * if (showHistStock) 0.15 else 0.3,
                            0.9 * .data[["dt"]]),
             pos = .data[["ttot"]] - ifelse(.data[["quantity"]] == "Stock",
                                            min(.data[["dt"]]) *
                                              if (showHistStock) {
                                                ifelse(.data[["historic"]], 0.09, -0.09)
                                              } else {
                                                0
                                              },
                                            0.5 * .data[["dt"]]),
             quantity = factor(.data[["quantity"]], names(vars)),
             value = .data[["value"]] / 1000) # million to billion

    # remove negative renovation values unless relevant dims are chosen for fill
    if (!fillDim %in% c("bs", "hs")) {
      pData <- pData %>%
        filter(replace_na(.data[["renovation"]], "") != "from")
    }

    # remove irrelevant fill  entries from legend
    if (is.factor(pData[[fillDim]])) {
      pData <- revalue.levels(pData, fillDim)
    }


    # position of flow unit
    flowUnit <- pData %>%
      filter(.data[["quantity"]] != "Stock") %>%
      group_by(across(all_of(c("facet", "ttot", "quantity", "renovation")))) %>%
      summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
      group_by(across(all_of(c("quantity")))) %>%
      summarise(value = max(.data[["value"]]), .groups = "drop") %>%
      mutate(pos = min(pData[["ttot"]]) - dt[1, "dt"] / 3,
             facet = head(pData[["facet"]], 1))


    ## plot ====
    p <- pData %>%
      ggplot() +
      suppressWarnings(geom_col(aes(.data[["pos"]], .data[["value"]],
                                    width = .data[["width"]],
                                    alpha = .data[["transparent"]],
                                    fill = .data[[fillDim]]))) +
      facet_grid(.data[["quantity"]] ~ .data[["facet"]], scales = "free") +
      geom_hline(yintercept = 0) +
      geom_text(aes(.data[["pos"]], .data[["value"]]),
                flowUnit,
                label = "/yr", vjust = 1, hjust = .2, size = 3)

    # mark historic stock with target sign above
    if (showHistStock) {
      histSignHeight <- pData %>%
        filter(.data[["historic"]]) %>%
        group_by(across(all_of(c("facet", "pos", "quantity")))) %>%
        summarise(value = sum(.data[["value"]]), .groups = "drop")

      p <- p +
        geom_point(aes(.data[["pos"]], .data[["value"]]),
                   histSignHeight,
                   shape = 10)
    }

    p <- addTheme(p, expression(paste("Floor space in billion ", m^2)), fillDim) +
      theme(panel.spacing = unit(4, "mm"))


    ## save plot ====

    plotDir <- file.path(path, "plots")
    if (!dir.exists(plotDir)) {
      dir.create(plotDir)
    }
    plotFile <- file.path(plotDir, paste0(paste("summary", paste(facet, collapse = "_"), fillDim, sep = "_"), ".png"))
    ggsave(plotFile, p, height = 17 / 2.54, width = 34 / 2.54, dpi = 300)

  }

}
