#' Compute the LCC of the heating.
#'
#' For each heating system, compute the lifecycle costs.
#'
#'
#' @author Ricarda Rosemann
#'
#' @param gdx string, path to gdx to be evaluated
#' @returns data frame of life cycle costs
#'
#' @export
#'
postProcessing <- function(path, gdx = "output.gdx", readCsv = FALSE) {

  logitShare <- function(lcc, priceSensHs) {
    return(exp(-priceSensHs * lcc)
           / sum(exp(-priceSensHs * lcc)))
  }

  m <- gamstransfer::Container$new(file.path(path, gdx))

  ttot <- readSymbol(m, "ttot")[["tall"]]
  typ <- readSymbol(m, "typ")[["typ"]]
  vin <- readSymbol(m, "vin")[["vin"]]

  renAllowed <- readSymbol(m, "renAllowed")
  vinExists <- readSymbol(m, "vinExists")

  dt <- readSymbol(m, "p_dt") %>%
    select("ttot", dt = "value")

  dtVin <- readSymbol(m, "p_dtVin")
  t2vin <- dtVin %>%
    group_by(.data[["ttot"]]) %>%
    arrange(-.data[["value"]]) %>%
    filter(row_number() == 1) %>%
    select("ttot", "vin")

  lambda <- readSymbol(m, "priceSensHs")[["value"]]

  hsMap <- getBrickMapping("heatingSystem.csv")

  fillColours <- as.character(hsMap[["colour"]])
  fillLabels  <- as.character(hsMap[["label"]])
  names(fillColours) <- names(fillLabels) <- as.character(hsMap[["hs"]])

  # Load flow data
  renovationFlow <- readSymbol(m, "v_renovation") %>%
    mutate(vin = factor(.data[["vin"]], levels(vinExists[["vin"]])),
           bsr = factor(.data[["bsr"]], levels(renAllowed[["bsr"]]))) %>%
    filter(.data[["bs"]] == "low", .data[["bsr"]] == "0")

  p_specCostRen <- readSymbol(m, "p_specCostRen") %>%
    mutate(vin = factor(vin, levels(vinExists[["vin"]])),
           bsr = factor(bsr, levels(renAllowed[["bsr"]]))) %>%
    right_join(renAllowed, by = c("bs", "hs", "bsr", "hsr")) %>%
    right_join(vinExists, by = c("ttot", "vin"))
  p_specCostOpe <- readSymbol(m, "p_specCostOpe") %>%
    mutate(vin = factor(vin, levels(vinExists[["vin"]]))) %>%
    right_join(vinExists, by = c("ttot", "vin"))
  p_specCostDem <- readSymbol(m, "p_specCostDem")[["value"]]

  if (isFALSE(readCsv)) {

    lccData <- lccAnalysisHs(ttot, dt[["dt"]], typ, p_specCostRen, p_specCostOpe, p_specCostDem)

    readr::write_csv2(lccData, file.path(path, "lccData.csv"))
  } else {
    lccData <- readr::read_csv2(file.path(path, "lccData.csv"))
  }

  lccData <- lccData %>%
    filter(.data[["bs"]] == "low", .data[["bsr"]] == "0") %>%
    group_by(across(-all_of(c("totalRen", "lccOpe", "lccDem", "lcc", "hsr"))))

  hsShares <- lccData %>%
    mutate(logit = logitShare(.data[["lcc"]], lambda))
  hsShares <- hsShares %>%
    left_join(renovationFlow %>%
                rename(brickRes = value)) %>%
    group_by(across(all_of(c("bs", "hs", "bsr", "vin", "reg", "loc", "typ", "inc", "ttot")))) %>%
    mutate(brickShare = .data[["brickRes"]] / sum(.data[["brickRes"]]))

  vinPlot <- vinExists %>%
    filter(.data[["ttot"]] > ttot[1])
  for (i in 1:nrow(vinPlot)) {
    lccPlot <- lccData %>%
      filter(vin == vinPlot[["vin"]][i], ttot == vinPlot[["ttot"]][i])
    lccPlot <- lccPlot %>%
      ggplot(mapping = aes(x = .data[["hs"]], y = .data[["lcc"]], fill = .data[["hsr"]])) +
      geom_col(position = "dodge") +
      facet_grid(.data[["loc"]]~.data[["typ"]]) +
      scale_fill_manual(values = fillColours,
                        labels = fillLabels)

    ggsave(file.path(path, "plots", paste0("lcc_vin", vinPlot[["vin"]][i], "_t",
                                           vinPlot[["ttot"]][i], ".png")), lccPlot)
    currHsShares <- hsShares %>%
      filter(vin == vinPlot[["vin"]][i], ttot == vinPlot[["ttot"]][i])

    logitPlot <- currHsShares %>%
      ggplot(mapping = aes(x = .data[["hs"]], y = .data[["logit"]], fill = .data[["hsr"]])) +
      geom_col() +
      facet_grid(.data[["loc"]]~.data[["typ"]]) +
      scale_fill_manual(values = fillColours,
                        labels = fillLabels)

    ggsave(file.path(path, "plots", paste0("shareLogit_vin", vinPlot[["vin"]][i],
                                           "_t", vinPlot[["ttot"]][i], ".png")),
           logitPlot)

    brickPlot <- currHsShares %>%
      ggplot(mapping = aes(x = .data[["hs"]], y = .data[["brickShare"]], fill = .data[["hsr"]])) +
      geom_col() +
      facet_grid(.data[["loc"]]~.data[["typ"]])  +
      scale_fill_manual(values = fillColours,
                        labels = fillLabels)

    ggsave(file.path(path, "plots", paste0("shareBrick_vin", vinPlot[["vin"]][i],
                                           "_t", vinPlot[["ttot"]][i], ".png")),
           brickPlot)

    sharePlot <- currHsShares %>%
      rename(l = logit, b = brickShare) %>%
      tidyr::pivot_longer(all_of(c("l", "b")), names_to = "source", values_to = "share") %>%
      unite("hs_source", "hs", "source") %>%
      ggplot(mapping = aes(x = .data[["hs_source"]], y = .data[["share"]], fill = .data[["hsr"]])) +
      geom_col() +
      facet_grid(.data[["loc"]]~.data[["typ"]]) +
      scale_fill_manual(values = fillColours,
                        labels = fillLabels)

    ggsave(file.path(path, "plots", paste0("share_vin", vinPlot[["vin"]][i],
                                           "_t", vinPlot[["ttot"]][i], ".png")),
           sharePlot, width = 14)

  }

}
