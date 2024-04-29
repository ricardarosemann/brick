#' Plot heat map of reference deviation
#'
#' @param path character; directory of output folder
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr mutate left_join case_when full_join
#' @importFrom gamstransfer Container
#' @importFrom ggplot2 ggplot geom_tile facet_wrap aes theme_minimal theme
#'   element_blank geom_text scale_x_discrete scale_y_discrete scale_fill_manual
#'   scale_fill_brewer
#' @export

plotRefDeviation <- function(path) {


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

  m <- Container$new(gdx)

  refs <- readSymbol(m, "r") %>%
    getElement("ref") %>%
    as.character()

  refDeviationVar <- readSymbol(m, "v_refDeviationVar")
  refVals <- readSymbol(m, "p_refVals")

  refDeviationVarRel <- refDeviationVar %>%
    left_join(refVals, by = c("ref", "refVar", "reg", "ttot")) %>%
    filter(.data[["ref"]] %in% refs) %>%
    mutate(value = ifelse(.data[["value.x"]] == 0 & .data[["value.y"]] == 0,
                          0,
                          .data[["value.x"]] / .data[["value.y"]]),
           valueDiscrete = case_when(
             abs(.data[["value"]]) < 0.05                      ~ "< 5 %",
             abs(.data[["value"]]) > 0.15                      ~ "> 15 %",
             abs(.data[["value"]]) <= 0.15                     ~ "5-15 %",
             is.na(.data[["value"]]) & .data[["value.y"]] == 0 ~ "> 0"
           ))

  refDeviation <- readSymbol(m, "v_refDeviation")
  refWeight <- readSymbol(m, "p_refWeight")
  refDeviationTot <- full_join(refDeviation, refWeight,
                               by = c("ref", "reg", "ttot")) %>%
    filter(.data[["ref"]] %in% refs) %>%
    mutate(value = .data[["value.x"]] * .data[["value.y"]])

  # plot dir
  plotDir <- file.path(path, "plots")
  if (!dir.exists(plotDir)) {
    dir.create(plotDir)
  }

  p <- ggplot(refDeviationVarRel,
              aes(.data[["ttot"]], interaction(.data[["ref"]],
                                               .data[["refVar"]],
                                               sep = "  |  "))) +
    geom_tile(aes(fill = .data[["valueDiscrete"]]),
              colour = "white") +
    geom_text(aes(label = paste(ifelse(abs(.data[["value"]]) < 0.0001,
                                       0,
                                       signif(.data[["value"]] * 100, 2)),
                                "%")),
              size = 3) +
    facet_wrap("reg", scales = "free_y") +
    scale_x_discrete("") +
    scale_y_discrete("") +
    scale_fill_manual(values = list(`> 0` = "#db7b2b",
                                    `< 5 %` = "#99c140",
                                    `5-15 %` = "#e7b416",
                                    `> 15 %` = "#cc3232")) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          panel.grid = element_blank())

  ggsave(file.path(plotDir, "refDeviation.png"), p,
         height = 14.6 / 1.5, width = 25 / 1.5, dpi = 300, bg = "white")



  p <- ggplot(refDeviationTot) +
    geom_col(aes(.data[["ttot"]], .data[["value"]], fill = .data[["ref"]])) +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous("total reference deviation") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())

  ggsave(file.path(plotDir, "refDeviationTot.png"), p,
         height = 14.6 / 1.5, width = 25 / 1.5, dpi = 300, bg = "white")


}
