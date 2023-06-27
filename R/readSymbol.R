#' Read symbol from gams container
#'
#' @author Robin Hasse
#'
#' @param m gams Container
#' @param symbol character, name of gams object
#' @param selectArea logical, select area quantity and remove this dimension
#'
#' @importFrom dplyr %>% filter select .data
#' @export

readSymbol <- function(m, symbol, selectArea = TRUE) {
  if (length(symbol) != 1) {
    stop("Only a single string is allowed as symbol.")
  }
  obj <- m$getSymbols(symbol)[[1]]
  data <- obj$records
  colnames(data) <- sub("_[0-9]*$", "", colnames(data))

  switch(class(obj)[1],
    Variable = {
      data <- data %>%
        select(-"marginal", -"lower", -"upper", -"scale") %>%
        rename(value = "level")},
    Set = {
      data <- data %>%
        select(-"element_text")
    }
  )

  if ("qty" %in% colnames(data) && selectArea) {
    data <- data %>%
      filter(.data[["qty"]] == "area") %>%
      select(-"qty")
  }

  # make temporal dimensions numeric
  tDims <- intersect(colnames(data), c("ttot", "tall"))
  for (tDim in tDims) {
    data[[tDim]] <- as.numeric(as.character(data[[tDim]]))
  }

  return(data)
}
