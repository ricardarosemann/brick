#' Compute the LCC of the heating.
#'
#' For each heating system, compute the lifecycle costs.
#'
#'
#' @author Ricarda Rosemann
#'
#' @param slurmQOS string, name of the desired QOS (Quality of Service)
#' @param tasks32 boolean, specify whether a node with 32 tasks should be requested
#' @returns data frame of life cycle costs
#'
#' @export
#'
lccAnalysisHs <- function(ttot, dt, typ, p_specCostRen, p_specCostOpe, p_specCostDem) {

    # Compute the operational costs during the lifetime
    lccOperation <- function(ltProb, years, costOpe, typGiven) {
      ltProb <- ltProb %>%
        filter(.data[["typ"]] == typGiven,
        .data[["ttot"]] %in% years)
      names(costOpe) <- as.character(years)
      costOpeData <- vector(mode = "numeric", nrow(ltProb))
      for (i in seq_len(nrow(ltProb))) {
        if (ltProb$ttot2[i] %in% years) {
          costOpeData[i] <- costOpe[[as.character(ltProb[["ttot2"]][i])]]
        } else {
          costOpeData[i] <- costOpe[[length(costOpe)]]
        }
      }
      ltProb <- dplyr::bind_cols(ltProb, data.frame(costOpe = costOpeData)) %>%
        group_by(ttot) %>%
        mutate(cumCostOpe = cumsum(.data[["costOpe"]] * .data[["discount"]] * .data[["dt"]]))
      ltProb <- ltProb %>%
        summarise(lccOpe = sum(.data[["prob"]] * .data[["cumCostOpe"]]))
      return(ltProb[["lccOpe"]])
    }

    # compute the demolition costs during the lifetime
    lccDemolition <- function(ltProb, costDem, typGiven) {
      ltProb <- ltProb %>%
        filter(typ == typGiven) %>%
        group_by(all_of("ttot")) %>%
        summarise(lccDem = sum(.data[["prob"]] * .data[["discount"]]))
      return(ltProb[["lccDem"]] * costDem)
    }

    ttotExt <- c(ttot, seq(from = ttot[length(ttot)] + dt[length(dt)],
                              by = dt[length(dt)], length.out = 6))
    dtExt <- c(dt, rep_len(dt[length(dt)], 6))

    t <- ttot[-1]

    ltProb <- data.frame(ttot = t) %>%
      tidyr::crossing(ttot2 = ttotExt, typ = typ) %>%
      mutate(r = ifelse(typ == "SFH", 0.21, 0.25)) %>%
      filter(ttot2 >= ttot)
    ltProb <- ltProb %>%
      group_by(across(all_of(c("ttot", "typ")))) %>%
      mutate(index = seq(n()),
             dt = ifelse(.data[["ttot"]] > 2020, c(10, diff(.data[["ttot2"]])), c(5, diff(.data[["ttot2"]]))))
    ltProb <- ltProb %>%
      mutate(
             # dt = c(as.numeric(dplyr::first(.data[["ttot"]])) - as.numeric(ttot[which(ttot == dplyr::first(.data[["ttot"]])) - 1]),
             #        diff(.data[["ttot2"]])),
             lt = as.numeric(.data[["ttot2"]]) - as.numeric(.data[["ttot"]]),
             t1 = .data[["lt"]],
             t2 = .data[["lt"]] + .data[["dt"]],
             prob = pweibull(.data[["t2"]], 3, 20) - pweibull(.data[["t1"]], 3, 20),
             discount = 1 / (1 + .data[["r"]])^(.data[["lt"]])) %>%
      ungroup()

    specCostAll <- left_join(p_specCostRen %>%
                               filter(.data[["hsr"]] != "0", .data[["ttot"]] %in% t) %>%
                               rename(costRen = "value"),
                             p_specCostOpe %>%
                               rename(tangible = "value") %>%
                               mutate(intangible = 0) %>%
                               tidyr::pivot_longer(cols = all_of(c("tangible", "intangible")), names_to = "cost",
                                            values_to = "costOpe") %>%
                               rename(hsr = "hs") %>%
                               mutate(hsr = factor(hsr, levels(p_specCostRen[["hsr"]]))) %>%
                               tidyr::crossing(bsr = p_specCostRen[["bsr"]], hs = p_specCostRen[["hs"]]),
                             by = c("cost", "bs", "hs", "bsr", "hsr", "vin", "reg", "loc", "typ", "ttot")) %>%
      # mutate(costOpe = ifelse(is.na(.data[["costOpe"]]) & !(.data[["ttot"]] %in% t$getUELs()), 0, .data[["costOpe"]]),
        mutate(costDem = ifelse(cost == "tangible", p_specCostDem, 0))

    specCostAll <- specCostAll %>%
      group_by(across(-all_of(c("cost", "costRen", "costOpe", "costDem")))) %>%
      summarise(totalRen = sum(.data[["costRen"]]), totalOpe = sum(.data[["costOpe"]]),
                totalDem = sum(.data[["costDem"]]), .groups = "drop")
    specCostAll <- specCostAll %>%
      group_by(across(-all_of(c("ttot", "totalRen", "totalOpe", "totalDem")))) %>%
      mutate(lccOpe = lccOperation(ltProb, .data[["ttot"]], .data[["totalOpe"]],
                                   typ = dplyr::first(.data[["typ"]])),
             lccDem = lccDemolition(ltProb, .data[["totalDem"]],
                                   typ = dplyr::first(.data[["typ"]])),
             lcc = .data[["totalRen"]] + .data[["lccOpe"]] + .data[["lccDem"]]) %>%
      select(-"totalOpe", -"totalDem") %>%
      ungroup()

    return(specCostAll)
}
