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
#'   expand_limits ggsave geom_point facet_grid margin unit element_text
#' @export

plotOwnership <- function(path) {

  fileName <- file.path("inst", "extdata", "ownership_Eurostat.csv")

  data <- read.csv(fileName) %>%
    select(tenure, geo, TIME_PERIOD, OBS_VALUE) %>%
    rename(Region = geo, Year = TIME_PERIOD, Value = OBS_VALUE) %>%
    filter(Year == 2022, tenure == "OWN") %>%
    mutate(Region = gsub("DE", "DEU", Region),
           Region = gsub("FI", "FIN", Region),
           Region = gsub("PL", "POL", Region),
           Region = factor(gsub("EU27_2020", "EU27", Region),
                           rev(c("EU27", "DEU", "FIN", "POL"))),
           aggReg = (Region == "EU27")) %>%
    arrange(desc(Region == "EU27"))

  blueMap <- getBrickMapping("heatingSystem.csv") %>%
    filter(hs == "ehp1")
  blueTinted <- "#7fcfec"
  colouring <- c(blueMap[["colour"]], rep.int(blueTinted, 3))

  ownershipPlot <- data %>%
    ggplot(mapping = aes(y = Region, x = Value)) +
    geom_col(fill = colouring) +
    scale_x_continuous("Home Ownership Rate", limits = c(0, 100), expand = c(0, 2)) +
    scale_y_discrete(expand = c(0, 1, 0, 1)) +
    theme_classic() +
    theme(strip.background = element_blank(),
    panel.grid.major.x = element_line(colour = "grey", linewidth = .25,),
    axis.title.y = element_blank(),
    text = ggplot2::element_text(size = 20))

  plotDir <- file.path(path, "plots")
  ggsave(file.path(plotDir, gsub("\\.csv", ".png", basename(fileName))), height = 7.3 / 2.54,
         width = 25 / 2.54, dpi = 600)
}

plotStock <- function(path, regions = c("DEU", "FIN", "POL"), vars = "hs", share = FALSE) {

  EU27 <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GRC", "HRV",
           "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")

  p_stockHist <- readInput("f_buildingStock.cs4r",
                           c("ttot", "reg", "variable", "typ", "loc", "vin", "hs"),
                           "inst/input") %>%
    filter(.data[["variable"]] == "floor") %>%
    select(-"variable") %>%
    filter(reg %in% EU27, ttot == max(ttot)) %>%
    group_by(across(all_of(c(vars, "reg")))) %>%
    summarise(value = sum(value), .groups = "drop")

  p_stockHist <- bind_rows(p_stockHist %>%
                             filter(.data[["reg"]] %in% regions),
                           p_stockHist %>%
                             group_by(across(all_of(vars))) %>%
                             summarise(value = sum(value), .groups = "drop") %>%
                             mutate(reg = "EU27")) %>%
    mutate(reg = factor(.data[["reg"]], rev(c("EU27", regions))))

  if (share) {
    p_stockHist <- p_stockHist %>%
                      group_by(reg) %>%
                      mutate(value = value/sum(value) * 100)
    xName <- "Percentage of heated floor space"
  } else {
    xName <- expression(paste("Floor space in billion ", m^2))
  }

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

  stockPlot <- p_stockHist %>%
    ggplot(mapping = aes(x = value, y = reg, fill = .data[[vars]])) +
    geom_col() +
    scale_x_continuous(xName, expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 1, 0, 1)) +
    theme_classic() +
    theme(strip.background = element_blank(),
          panel.grid.major.x = element_line(colour = "grey", linewidth = .25),
          axis.title.y = element_blank(),
          text = ggplot2::element_text(size = 20)) +
    scale_fill_manual(values = fillColours[[vars]],
                      labels = fillLabels[[vars]]) +
    labs(fill = fillTitle[[vars]])

  plotDir <- file.path(path, "plots")
  ggsave(file.path(plotDir, "historicStock.png"), stockPlot, height = 7.3 / 2.54,
         width = 25 / 2.54, dpi = 600)
}
