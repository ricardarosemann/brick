#' Modify costs as specified in config
#'
#' @param costs data frame, containing costs values
#' @param costMod named list, specification on how to modify costs
#' @param t0 numeric, initial time period
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% across all_of .data group_by left_join mutate select summarise ungroup

modifyCosts <- function(costs, costMod, t0) {

  hsCol <- if ("hsr" %in% colnames(costs)) "hsr" else "hs"

  if (costMod[["method"]] == "scale_constant") {
    costs <- costs %>%
      group_by(across(-all_of(c("ttot", "value")))) %>%
      mutate(value = .data[["value"]][[which(.data[["ttot"]] == t0)]]) %>%
      ungroup()
  }

  if (!"hs" %in% colnames(costs) && "carrier" %in% colnames(costs)) {
    hsCarrier <- unique(getBrickMapping("heatingSystem.csv", "sectoral")[, c("hs", "carrier")]) %>%
      group_by(across("carrier")) %>%
      summarise(modify = any(.data[["hs"]] == costMod[["hs"]]), .groups = "drop")

    costs <- left_join(costs, hsCarrier, by = "carrier")
  } else {
    costs <- mutate(costs, modify = .data[[hsCol]] == costMod[["hs"]])
  }

  if (costMod[["method"]] %in% c("scale", "scale_constant")) {
    costs <- costs %>%
      mutate(value = ifelse(
        .data[["modify"]],
        .data[["value"]] * costMod[["scale"]],
        .data[["value"]]
      ))
  } else if (costMod[["method"]] == "value") {
    costs <- costs %>%
      mutate(value = ifelse(
        .data[["modify"]] & .data[["cost"]] == "tangible",
        costMod[["scale"]],
        .data[["value"]]
      ))
  } else {
    message("You specified an invalid cost modification method. Costs are not modified.")
  }

  return(select(costs, -"modify"))

}
