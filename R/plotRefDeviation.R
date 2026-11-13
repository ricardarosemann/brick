#' Plot heat map of reference deviation
#'
#' @param path character, directory of output folder
#' @param dropZeroWeightRefs logical, if TRUE, references with zero weight are
#'   not shown
#' @param dropUnmappedRefVars logical, if TRUE, reference variables that are not
#'   mapped to BRICK variables are not shown
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr mutate left_join case_when full_join
#' @importFrom gamstransfer Container
#' @importFrom utils tail
#' @importFrom ggplot2 ggplot geom_tile facet_wrap aes theme_minimal theme
#'   element_blank geom_text scale_x_discrete scale_y_discrete scale_fill_manual
#'   scale_fill_brewer element_text ggtitle geom_vline
#' @export

plotRefDeviation <- function(path,
                             dropZeroWeightRefs = TRUE,
                             dropUnmappedRefVars = TRUE) {

  linebreak <- function(string, sep = "_") {
    sub(sep, "\n", string)
  }

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

  refs <- readSymbol(m, "ref") %>%
    getElement("reference") %>%
    as.character()
  refDeviation <- readSymbol(m, "v_refDeviation")
  refWeight <- readSymbol(m, "p_refWeight")

  # drop refs without weight
  if (isTRUE(dropZeroWeightRefs)) {
    refWithWeight <- refWeight %>%
      filter(.data$value != 0) %>%
      getElement("reference")
    refs <- intersect(refs, refWithWeight)
  }

  refVars <- if (isTRUE(dropUnmappedRefVars)) {
    # only mapped refVars
    do.call(rbind, lapply(refs, function(ref) {
      readSymbol(m, paste0("refMap_", ref)) %>%
        mutate(reference = ref) %>%
        select("reference", "refVar")
    }))
  } else {
    # all refVars
    readSymbol(m, "refVarConsidered") %>%
      filter(.data$reference %in% refs)
  }


  refDeviationVar <- readSymbol(m, "v_refDeviationVar")
  refVals <- readSymbol(m, "p_refVals")
  refValsBasic <- readSymbol(m, "v_refValsBasic")
  refVarBasic <- readSymbol(m, "refVarBasic")

  refDeviationVarRel <- refDeviationVar %>%
    inner_join(refVars, by = c("reference", "refVar")) %>%
    left_join(refVals, by = c("reference", "refVar", "region", "ttot"),
              suffix = c("", ".ref")) %>%
    left_join(refVarBasic, by = c("reference", "refVar")) %>%
    left_join(refValsBasic, by = c("reference", "refVarGroup", "region", ttot = "t"),
              suffix = c("", ".basic")) %>%
    mutate(value.basic = replace_na(.data$value.basic, 1),
           value = ifelse(abs(.data$value) < 1E-4 & .data$value.ref == 0,
                          0,
                          .data$value / .data$value.ref / .data$value.basic),
           valueDiscrete = case_when(
             (is.na(.data$value) | is.infinite(.data$value)) & .data$value.ref == 0 ~ "> 0",
             abs(.data$value) < 0.1                      ~ "< 10 %",
             abs(.data$value) > 0.3                      ~ "> 30 %",
             abs(.data$value) <= 0.3                     ~ "10-30 %"
           ))

  refDeviationTot <- full_join(refDeviation, refWeight,
                               by = "reference") %>%
    filter(.data$reference %in% refs) %>%
    mutate(value = .data$value.x * .data$value.y)

  # plot dir
  plotDir <- file.path(path, "plots")
  if (!dir.exists(plotDir)) {
    dir.create(plotDir)
  }
  pdfPath <- file.path(plotDir, "refDeviation.pdf")

  regions <- unique(refDeviation[["region"]])
  periods <- sort(unique(refDeviation[["ttot"]]))

  message("writing PDF: ", pdfPath)
  grDevices::pdf(pdfPath,
                 width = 8.5, height = 11.5, paper = "a4",
                 title = "BRICK matching: reference deviation")

  for (reg in regions) {
    p <- refDeviationVarRel %>%
      filter(.data$region %in% reg) %>%
      mutate(reference = linebreak(.data$reference)) %>%
      ggplot(aes(.data$ttot, .data$refVar)) +
      geom_vline(aes(xintercept = .data$period),
                 data.frame(period = (head(periods, -1) + tail(periods, -1)) / 2),
                 colour = "lightgrey", linewidth = 0.1) +
      geom_tile(aes(fill = .data$valueDiscrete),
                colour = "white") +
      geom_text(aes(label = ifelse(is.na(.data$value),
                                   "",
                                   paste(ifelse(abs(.data$value) < 0.0001,
                                                0,
                                                signif(.data$value * 100, 2)),
                                         "%"))),
                size = 1.5) +
      facet_grid(.data$reference ~ .,
                 scales = "free_y", space = "free", switch = "y") +
      scale_y_discrete(NULL) +
      scale_fill_manual(values = list(`> 0`     = "#db7b2b",
                                      `< 10 %`  = "#99c140",
                                      `10-30 %` = "#e7b416",
                                      `> 30 %`  = "#cc3232"),
                        na.value = "lightgrey") +
      ggtitle("Reference deviation", reg) +
      theme_minimal() +
      theme(strip.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_line(colour = "lightgrey", linewidth = 0.1),
            strip.placement = "outside",
            strip.text.y.left = element_text(size = 5, angle = 0, face = "bold", lineheight = 0.75),
            axis.title.x = element_blank(),
            axis.text = element_text(size = 5),
            legend.position = "bottom",
            legend.title = element_blank())
    print(p)
  }


  p <- ggplot(refDeviationTot) +
    geom_col(aes(.data$ttot, .data$value, fill = .data$reference)) +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous("total reference deviation") +
    facet_wrap("region", scales = "free_y") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "bottom")
  print(p)

  # end PDF
  grDevices::dev.off()
  message("PDF written: ", pdfPath)
}
