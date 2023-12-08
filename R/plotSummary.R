#' Plot Summary of a run
#'
#' Plot an overview of the stock and flows
#'
#' @param path character, path to the run
#' @param facet character, dimension to resolve as facets
#' @param showHistStock logical, show given historic next to the modeled stock
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
#'   expand_limits ggsave geom_point facet_grid margin unit
#' @export

plotSummary <- function(path, facet = "typ", showHistStock = FALSE) {

  config <- readConfig(file.path(path, "config", "config.yaml"), readDirect = TRUE)
  endyear <- config[["endyear"]]



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

  m <- gamstransfer::Container$new(gdx)


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
    Demolition = "v_demolition",
    Renovation = "v_renovation"
  )

  data <- lapply(vars, function(v) {
    var <- readSymbol(m, v)

    if (is.null(facet)) {
      var[["facet"]] <- "all"
    } else {
      var <- unite(var, "facet", all_of(facet), sep = " | ")
    }

    if (all(c("bsr", "hsr") %in% colnames(var))) {
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

    return(var)
  })



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
            axis.title.x = element_blank())

    pOut <- pOut +
      scale_fill_manual(values = fillColours[[fillDim]],
                        labels = fillLabels[[fillDim]]) +
      labs(fill = fillTitle[[fillDim]])

    return(pOut)
  }



  # PLOT -----------------------------------------------------------------------

  for (fillDim in c("hs", "vin")) {

    pData <- do.call(bind_rows, lapply(names(data), function(v) {
      d <- data[[v]] %>%
        filter(.data[["ttot"]] <= endyear)

      if (!"vin" %in% colnames(d)) {
        d <- left_join(d, t2vin, by = "ttot")
      }

      d <- d %>%
        group_by(across(any_of(c("facet", fillDim, "ttot", "renovation")))) %>%
        summarise(value = sum(.data[["value"]], na.rm = TRUE),
                  .groups = "drop")  %>%
        mutate(quantity = v)

      return(d)
    }))
    pData <- pData %>%
      left_join(dt, by = "ttot") %>%
      mutate(width = ifelse(.data[["quantity"]] == "Stock",
                            1.5, 0.9 * .data[["dt"]]),
             pos = .data[["ttot"]] - ifelse(.data[["quantity"]] == "Stock",
                                            0, 0.5 * .data[["dt"]]),
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
      mutate(pos = min(pData[["ttot"]]) - dt[1, "dt"] / 2,
             facet = head(pData[["facet"]], 1))


    ## plot ====

    p <- pData %>%
      ggplot() +
      suppressWarnings(geom_col(aes(.data[["pos"]], .data[["value"]],
                                    width = .data[["width"]],
                                    fill = .data[[fillDim]]))) +
      facet_grid(.data[["quantity"]] ~ .data[["facet"]], scales = "free") +
      geom_hline(yintercept = 0) +
      geom_text(aes(.data[["pos"]], .data[["value"]]),
                flowUnit,
                label = "/yr", vjust = 1, hjust = .2, size = 3)

    p <- addTheme(p, expression(paste("Floor space in billion ", m^2)), fillDim) +
      theme(panel.spacing = unit(4, "mm"))


    ## save plot ====

    plotDir <- file.path(path, "plots")
    if (!dir.exists(plotDir)) {
      dir.create(plotDir)
    }
    plotFile <- file.path(plotDir, paste0(paste("summary", paste(facet, collapse = "_"), fillDim, sep = "_"), ".png"))
    ggsave(plotFile, p, height = 14.6 / 2.54, width = 25 / 2.54, dpi = 300)

  }

}
