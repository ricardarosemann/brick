#' Plot Summary of a run
#'
#' Plot an overview of the stock and flows
#'
#' @author Robin Hasse
#'
#' @param path character, path to the run
#' @param facet character, dimension to resolve as facets
#' @param showHistStock logical, show given historic next to the modeled stock
#'
#' @importFrom quitte as.quitte calc_addVariable
#' @importFrom gamstransfer Container
#' @importFrom dplyr row_number n
#' @importFrom ggplot2 ggplot geom_col geom_area aes_string scale_x_continuous
#'   scale_y_continuous ggtitle theme_classic theme aes geom_line facet_wrap
#'   element_blank element_line scale_fill_manual labs position_stack geom_hline
#'   expand_limits ggsave geom_point
#' @importFrom ggpubr ggarrange
#' @export
#'
plotSummary <- function(path, facet = "typ", showHistStock = FALSE) {

  config <- readConfig(file.path(path, "config", "config.yaml"), readDirect = TRUE)
  endyear <- config[["endyear"]]

  # PLOT STYLE -----------------------------------------------------------------

  techColour <- list(`0`  = "black",
                     reel = "#ffc000",
                     ehp1 = "#009FDA",
                     libo = "#7030a0",
                     gabo = "#c00000",
                     sobo = "#8E908F",
                     dihe = "#ff6b07",
                     biom = "#69923A")
  vinColour <- list(
    `before 1945` = "#dd3b1b",
    `1945-1969`   = "#e75f05",
    `1970-1979`   = "#ec8000",
    `1980-1989`   = "#ee9f00",
    `1990-1999`   = "#ecbd00",
    `2000-2010`   = "#e6da18",
    `2011-2020`   = "#c7d623",
    `2021-2030`   = "#a8d030",
    `2031-2040`   = "#8bca3d",
    `2041-2050`   = "#6dc249",
    `2051-2060`   = "#4dba54",
    `2061-2070`   = "#00be87",
    `2071-2080`   = "#00bfb4",
    `2081-2090`   = "#00bdd7",
    `2091-2100`   = "#00b8eb",
    `after 2100`  = "#5fb0ef"
  )
  techLabel <- c(`0`  = "no change",
                 reel = "Resitive electric",
                 ehp1 = "Heat pump",
                 libo = "Liquids boiler",
                 gabo = "Gas boiler",
                 sobo = "Solids boiler",
                 dihe = "District heating",
                 biom = "Biomass boiler")
  gap <- 0.5
  addTheme <- function(p, title = "", yLabel = "",
                       removeXLabels = FALSE,
                       removeFacetLabels = TRUE) {

    pOut <- p +
      scale_y_continuous(yLabel, expand = c(0, 0)) +
      scale_x_continuous(limits = xLimits, expand = c(0, 0)) +
      ggtitle(title) +
      theme_classic() +
      theme(strip.background = element_blank(),
            panel.grid.major.y = element_line(colour = "grey", size = .25),
            axis.title.x = element_blank())
    if (removeFacetLabels) {
      pOut <- pOut +
        theme(strip.text = element_blank())
    }
    if (removeXLabels) {
      pOut <- pOut +
        theme(axis.text.x = element_blank())
    }
    switch(fillDim,
           hs = {
             pOut <- pOut +
               scale_fill_manual(values = techColour, labels = techLabel) +
               labs(fill = "Heating system")
             },
           vin = {
             pOut <- pOut +
               scale_fill_manual(values = vinColour) +
               labs(fill = "Construction cohort")
             }
    )
    return(pOut)
  }



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



  # READ DATA ----

  m <- Container$new(gdx)

  dt <- readSymbol(m, "p_dt") %>%
    select("ttot", dt = "value") %>%
    filter(.data[["ttot"]] <= endyear)

  #  bar width
  width <- 0.4 * min(dt[["dt"]])
  if (showHistStock) {
    width <- width / 2
  }

  # axis limits
  xLimits <- dt %>%
    arrange(.data[["ttot"]]) %>%
    filter(row_number() %in% c(1, n())) %>%
    mutate(limit = .data[["ttot"]] + .data[["dt"]] * (row_number() - 1.5)) %>%
    getElement("limit")

  dtVin <- readSymbol(m, "p_dtVin")
  t2vin <- dtVin %>%
    group_by(.data[["ttot"]]) %>%
    arrange(-.data[["value"]]) %>%
    filter(row_number() == 1) %>%
    select("ttot", "vin")

  stock <- rbind(readSymbol(m, "v_stock") %>%
                   mutate(historic = FALSE),
                 readSymbol(m, "p_stockHist") %>%
                   mutate(historic = TRUE)) %>%
    mutate(pos = .data[["ttot"]] + (0.5 - .data[["historic"]]) * width * 1.2) %>%
    filter(.data[["ttot"]] >= 2000,
           .data[["ttot"]] <= endyear)

  construction <- readSymbol(m, "v_construction") %>%
    filter(.data[["ttot"]] <= endyear)

  demolition <- readSymbol(m, "v_demolition") %>%
    filter(.data[["ttot"]] >= 2000,
           .data[["ttot"]] <= endyear)

  renovation <- readSymbol(m, "v_renovation") %>%
    filter(.data[["ttot"]] >= 2000,
           .data[["ttot"]] <= endyear)



  # PLOT ----

  for (fillDim in c("hs", "vin")) {



    ## stock ====

    pStock <- stock %>%
      filter(showHistStock | !.data[["historic"]]) %>%
      group_by(across(all_of(c(facet, fillDim, "ttot", "historic", "pos")))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE),
                .groups = "drop") %>%
      ggplot(aes_string("ttot", "value")) +
      geom_area(aes(fill = .data[[fillDim]]), alpha = 0.2 * !showHistStock) +
      geom_col(aes(x = if (showHistStock) .data[["pos"]] else .data[["ttot"]],
        fill = .data[[fillDim]]),
        width = width) +
      facet_wrap(facet)
    if (showHistStock) {
      pStock <- pStock +
        geom_point(aes(.data[["pos"]]), shape = 13,
                   data = stock %>%
                     filter(.data[["historic"]]) %>%
                     group_by(across(all_of(c("pos", facet)))) %>%
                     summarise(value = sum(.data[["value"]]), .groups = "drop"))
    }
    pStock <- addTheme(pStock, "Stock", "Floor space in million m2", TRUE, FALSE)

    ## construction ====
    pCon <- construction %>%
      left_join(t2vin, by = "ttot") %>%
      group_by(across(all_of(c(facet, fillDim, "ttot")))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      left_join(dt, by = "ttot") %>%
      ggplot(aes(fill = .data[[fillDim]])) +
      geom_col(aes(.data[["ttot"]], .data[["value"]], width = .data[["dt"]] - gap),
               just = 1) +
      facet_wrap(facet)
    pCon <- addTheme(pCon, "Construction", "", TRUE)

    ## demolition ====
    pDem <- demolition %>%
      group_by(across(all_of(c(facet, fillDim, "ttot")))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      left_join(dt, by = "ttot") %>%
      ggplot() +
      geom_col(aes(.data[["ttot"]], .data[["value"]],
                   fill = .data[[fillDim]],
                   width = .data[["dt"]] - gap),
               position = position_stack(reverse = TRUE),
               just = 1) +
      facet_wrap(facet)
    pDem <- addTheme(pDem, "Demolition", "Floor space in million m2/yr", TRUE)

    ## renovation ====
    pRen <- rbind(renovation %>%
                    filter(!(.data[["bsr"]] == "0" & .data[["hsr"]] == "0")) %>%
                    select(-"hs", -"bs") %>%
                    rename(hs = "hsr", bs = "bsr") %>%
                    mutate(renovation = "to"),
                  renovation %>%
                    filter(!(.data[["bsr"]] == "0" & .data[["hsr"]] == "0")) %>%
                    select(-"hsr", -"bsr") %>%
                    mutate(value = -.data[["value"]],
                           renovation = "from")
    ) %>%
      group_by(across(all_of(c(facet, fillDim, "ttot", "renovation")))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      left_join(dt, by = "ttot") %>%
      ggplot() +
      geom_col(aes(.data[["ttot"]], .data[["value"]],
                   fill = .data[[fillDim]],
                   width = .data[["dt"]] - gap),
               position = position_stack(reverse = TRUE),
               just = 1) +
      facet_wrap(facet) +
      geom_hline(yintercept = 0)
    pRen <-  addTheme(pRen, "Renovation", "") +
      expand_limits(y = 0.05)

    # join plots
    print(ggarrange(pStock, pCon, pDem, pRen,
                    ncol = 1, align = "v",
                    legend = "right", common.legend = TRUE))

    # save plot
    plotDir <- file.path(path, "plots")
    if (!dir.exists(plotDir)) {
      dir.create(plotDir)
    }
    plotFile <- file.path(plotDir, paste0("summary_", fillDim, ".png"))
    ggsave(plotFile, height = 14.6 / 2.54, width = 25 / 2.54, dpi = 300)

  }

}
