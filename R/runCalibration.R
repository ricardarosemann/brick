#' Run the brick calibration
#'
#' Two methods for the Brick calibration are available: The logit calibration and the
#' optimization calibration.
#'
#' The general procedure is as follows:
#' We aim to minimize the deviation between historic values and  Brick results.
#' An intangible component of the specific construction and renovation costs is iteratively
#' adjusted to achieve this. Thus the intangible costs serve as the optimization variable \code{x}
#' and a functional evaluation of the deviation represents the objective function \code{f}.
#'
#' Brick is run iteratively and from each run, an adjustment term \code{d} is computed.
#' The optimization variable is adjusted according to \code{x = x + stepSize * d}, where
#' the step size is determined according to the Armijo step size adaptation algorithm.
#'
#' The implementation is centered around the following objects:
#' \itemize{
#' \item The optimization variable \code{x} and its variant(s) required in the step size adaptation
#' are gathered in the data frame \code{optimVarCon} for construction and \code{optimVarRen}
#' for renovation.
#' \item The objective function value \code{f} and its variant(s) are gathered in the data
#' frame \code{outerObjective}
#' \item The adjustment term \code{d} and related variables if applicable are gathered in the
#' data frame \code{deviationCon} for construction and \code{deviationRen} for renovation.
#' \item The step size \code{stepSize} and related parameters required for the step size adaptation
#' algorithm are gathered in the data frame \code{stepSizeParams}.
#' }
#'
#' @param path character vector with folders to run gams in
#' @param parameters named list of calibration parameters
#' @param tcalib numeric, time periods to calibrate on
#' @param gamsOptions named list of GAMS options
#' @param switches named list of model switches
#' @param fileName character vector with gams file names
#' @param gamsCall system command to call gams
#'
#' @author Ricarda Rosemann
#'
#' @importFrom dplyr %>% .data anti_join filter left_join mutate rename right_join select
#' @importFrom tidyr pivot_wider
#'
runCalibration <- function(path,
                           parameters,
                           tcalib,
                           gamsOptions = NULL,
                           switches = NULL,
                           fileName = "main.gms",
                           gamsCall = "gams") {

  .runCalibration <- switch(
    switches[["CALIBRATIONMETHOD"]],
    logit = runCalibrationLogit,
    optimization = runCalibrationOptim,
    stop("You did not provide a valid calibration method Stopping.")
  )
  .runCalibration(
    path,
    parameters,
    tcalib,
    calibTarget = .readCalibTarget(tcalib, modifyData = parameters[["modifyData"]]),
    gamsOptions = gamsOptions,
    switches = switches,
    fileName = fileName,
    gamsCall = gamsCall
  )
}

#' Call the logit brick calibration
#'
#' The adjustment term is computed as if Brick were a pure logit model:
#' \code{d = log(<brick result>/<historic value>)}.
#'
#' Here the armijo step size algorithm may not be applicable; if this is the
#' case, a heuristic step size adaptation is applied.
#'
#' @param path character vector with folders to run gams in
#' @param parameters named list of calibration parameters
#' @param tcalib numeric, time periods to calibrate on
#' @param calibTarget list of data frames of calibration targets
#' @param gamsOptions named list of GAMS options
#' @param switches named list of model switches
#' @param fileName character vector with gams file names
#' @param gamsCall system command to call gams
#'
#' @importFrom dplyr %>% .data anti_join filter mutate rename select
#' @importFrom gamstransfer Container
#' @importFrom utils write.csv
#'
runCalibrationLogit <- function(path,
                                parameters,
                                tcalib,
                                calibTarget,
                                gamsOptions = NULL,
                                switches = NULL,
                                fileName = "main.gms",
                                gamsCall = "gams") {


  # PREPARE ----------------------------------------------------------------------------

  # Store initial input data
  if (!file.exists(file.path(path, "input_init.gdx"))) {
    file.copy(from = file.path(path, "input.gdx"), to = file.path(path, "input_init.gdx"))
  }
  dims <- .getDims(calibTarget)

  # Read in required input data
  gdxInput <- file.path(path, "input.gdx")
  mInput <- Container$new(gdxInput)
  renAllowed <- readSymbol(mInput, symbol = "renAllowed")
  vinExists <- readSymbol(mInput, symbol = "vinExists")
  vinCalib <- readSymbol(mInput, symbol = "vinCalib")
  xinitCon <- filter(readSymbol(mInput, "p_specCostCon"), .data[["cost"]] == "intangible")
  xinitRen <- filter(readSymbol(mInput, "p_specCostRen"), .data[["cost"]] == "intangible")

  # Brick only runs as standard scenario run
  switches[["RUNTYPE"]] <- "scenario"

  # Initialise optimization variable and optimization objective data frames
  optimVarCon <- .initOptimVarCon(mInput, tcalib)
  optimVarRen <- .initOptimVarRen(mInput, tcalib, renAllowed)
  outerObjective <- .initOuterObjective(mInput, tcalib)

  # Initialise objects to store diagnostic parameters over calibration iterations
  diagnostics <- setNames(nm = c("deviationConIter",
                                 "deviationRenIter",
                                 "stepSizeParamsIter")) %>%
    lapply(function(x) data.frame())

  diagnosticsDetail <- setNames(nm = c("stepSizeAllIter",
                                       "armijoStepAllIter",
                                       "heuristicStepAllIter",
                                       "outerObjectiveAllIter")) %>%
    lapply(function(x) data.frame())

  gdxOutput <- file.path(path, "output.gdx")

  .addTargetsToInput(mInput, path, calibTarget)

  p_constructionCalibTarget <- select(calibTarget[["construction"]], -"qty")
  p_renovationCalibTarget <- select(calibTarget[["renovation"]], -"qty")

  # Initial Brick run
  runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)

  # Compute (virtual) objective function
  gdx <- file.path(path, paste0("calibration_0.gdx"))
  file.copy(from = gdxOutput, to = gdx, overwrite = TRUE)

  m <- Container$new(gdx)
  outerObjective <- .combineOuterObjective(m, outerObjective, p_constructionCalibTarget, p_renovationCalibTarget,
                                           tcalib, agg = switches[["AGGREGATEDIM"]])



  # START OF CALIBRATION LOOP ----------------------------------------------------------------------

  for (i in seq_len(parameters[["iterations"]])) {


    # READ IN BRICK RESULTS AND COMPUTE DEVIATION -------------------------------------------------

    deviationCon <- .computeDeviation(m, p_constructionCalibTarget, dims$construction, tcalib, flow = "construction",
                                      agg = switches[["AGGREGATEDIM"]])

    deviationRen <- .computeDeviation(m, p_renovationCalibTarget, dims$renovation, tcalib,
                                      flow = "renovation", renAllowed = renAllowed, vinExists = vinExists,
                                      agg = switches[["AGGREGATEDIM"]])

    # Compute parameters delta and phi-derivative of the step size adaptation
    stepSizeParams <- .combineStepSizeParams(.computeStepSizeParams(deviationCon),
                                             .computeStepSizeParams(deviationRen))



    # PREPARE STEP SIZE ADAPTATION ---------------------------------------------------------------


    ## Set the initial step size for the step size adaptation ====

    stepSizeParams <- .initStepSize(i, stepSizeParams, outerObjective, parameters[["stepSizeInit"]])


    ## Determine the step size adaptation algorithm ====

    # Run Brick for a test point close to the starting point
    optimVarCon <- .updateX(optimVarCon, deviationCon, stepSizeParams, dims$construction,
                            nameTo = "xMin", factorMin = 0.0001)
    optimVarRen <- .updateX(optimVarRen, deviationRen, stepSizeParams, dims$renovation,
                            nameTo = "xMin", factorMin = 0.0001)

    .addSpecCostToInput(mInput, path, optimVarCon, optimVarRen, xinitCon, xinitRen, tcalib, varName = "xMin",
                        vinExists = vinExists, vinCalib = vinCalib)

    runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)

    gdxMin <- file.path(path, "calibrationMin.gdx")
    file.copy(from = gdxOutput, to = gdxMin, overwrite = TRUE)
    mMin <- Container$new(gdxMin)

    # Evaluate the (virtual) objective of the outer optimization
    outerObjective <- .combineOuterObjective(mMin, outerObjective, p_constructionCalibTarget, p_renovationCalibTarget,
                                             tcalib, varName = "fMin", agg = switches[["AGGREGATEDIM"]])

    totalStep <- select(stepSizeParams, "region", "loc", "typ", "inc", "ttot")

    # Check for which combinations the armijo step size algorithm cannot be applied:
    # If a point close to the starting point does not satisfy the Armijo condition,
    # we use a heuristic step size criterion
    heuristicStep <- .checkArmijoStep(totalStep, stepSizeParams, outerObjective, parameters[["sensitivityArmijo"]],
                                      varName = "fMin", factorMin = 0.0001)

    # If the Armijo condition is satisfied for the test point, we apply the armijo step size algorithm
    armijoStep <- totalStep %>%
      anti_join(heuristicStep, by = c("region", "loc", "typ", "inc", "ttot"))



    # ITERATION OF STEP SIZE ADAPTATION -----------------------------------------------------------

    for (j in seq_len(parameters[["iterationsArmijo"]])) {

      ## Adjust the optimization variable according to current step size ====
      optimVarCon <- .updateXSelect(totalStep, optimVarCon, deviationCon,
                                    stepSizeParams, dims$construction)
      optimVarRen <- .updateXSelect(totalStep, optimVarRen, deviationRen,
                                    stepSizeParams, dims$renovation)

      .addSpecCostToInput(mInput, path, optimVarCon, optimVarRen, xinitCon, xinitRen, tcalib, varName = "xA",
                          vinExists = vinExists, vinCalib = vinCalib)

      ## Evaluate outer objective of Brick results ====

      runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)

      gdxA <- file.path(path, "calibrationA.gdx")
      file.copy(from = gdxOutput, to = gdxA, overwrite = TRUE)
      mA <- Container$new(gdxA)

      outerObjective <- .combineOuterObjective(mA, outerObjective, p_constructionCalibTarget, p_renovationCalibTarget,
                                               tcalib, varName = "fA", agg = switches[["AGGREGATEDIM"]])


      ## Check step size conditions and update the step size ====

      # Filter for combinations that do not satisfy the Armijo condition yet
      armijoStep <- .checkArmijoStep(armijoStep, stepSizeParams, outerObjective, parameters[["sensitivityArmijo"]])

      # Filter for combinations where the objective is still larger than in the previous iteration
      heuristicStep <- .checkHeuristicStep(heuristicStep, outerObjective)

      totalStep <- rbind(armijoStep, heuristicStep)

      diagDetObj <- list(stepSizeAllIter = stepSizeParams, armijoStepAllIter = armijoStep,
                         heuristicStepAllIter = heuristicStep, outerObjectiveAllIter = outerObjective)
      diagnosticsDetail <- setNames(lapply(names(diagDetObj), function(nm) {
        rbind(
          diagnosticsDetail[[nm]],
          diagDetObj[[nm]] %>%
            mutate(iteration = i, iterA = j) %>%
            select(-any_of(c("fMin", "fPrev"))) # only required for outerObjective
        )
      }), names(diagDetObj))

      if (nrow(totalStep) == 0) {# All combinations satisfy the Armijo/heuristic condition
        break
      }

      # Update the step size where applicable
      stepSizeParams <- .updateStepSize(totalStep, stepSizeParams, outerObjective, parameters[["stepReduction"]])

    }

    # If the armijo condition is still not satisfied for any combination:
    # Use the step size with the minimum objective function.
    # Print a warning if this minimum does not exist and use the last step size of the iteration.
    if (nrow(totalStep) > 0) {
      stepSizeParams <- .adjustStepSizeAfterArmijo(totalStep, stepSizeParams)

      optimVarCon <- .updateXSelect(totalStep, optimVarCon, deviationCon, stepSizeParams, dims$construction)
      optimVarRen <- .updateXSelect(totalStep, optimVarRen, deviationRen, stepSizeParams, dims$renovation)

      .addSpecCostToInput(mInput, path, optimVarCon, optimVarRen, xinitCon, xinitRen, tcalib, varName = "xA",
                          vinExists = vinExists, vinCalib = vinCalib)

      ## Re-evaluate outer objective of Brick results ====

      # TODO: This is only necessary because in the case of no descent we're using a step size
      # we never computed with before
      # Can be replaced by any previous step size to save this code bit.

      runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)

      gdxA <- file.path(path, "calibrationA.gdx")
      file.copy(from = gdxOutput, to = gdxA, overwrite = TRUE)
      mA <- Container$new(gdxA)

      outerObjective <- .combineOuterObjective(mA, outerObjective, p_constructionCalibTarget, p_renovationCalibTarget,
                                               tcalib, varName = "fA", agg = switches[["AGGREGATEDIM"]])
    }

    # Update optimization variable data
    optimVarCon <- mutate(optimVarCon, x = .data[["xA"]], xA = NULL, xMin = NULL)
    optimVarRen <- mutate(optimVarRen, x = .data[["xA"]], xA = NULL, xMin = NULL)

    # Update optimization objective data
    outerObjective <- mutate(outerObjective, fPrev = .data[["f"]], f = .data[["fA"]], fA = NULL, fMin = NULL)

    # Rename the last output file of the Armijo iteration
    gdx <- file.path(path, paste0("calibration_", i, ".gdx"))
    file.copy(from = gdxA, to = gdx, overwrite = TRUE)
    m <- Container$new(gdx)

    # Store diagnostic variables
    diagObj <- list(deviationConIter = deviationCon, deviationRenIter = deviationRen,
                    stepSizeParamsIter = stepSizeParams)
    diagnostics <- setNames(lapply(names(diagObj), function(nm) {
      rbind(diagnostics[[nm]], mutate(diagObj[[nm]], iteration = i))
    }), names(diagObj))

  }



  # WRITE INTANGIBLE COSTS TO FILE -------------------------------------------------------------

  .writeCostIntang(file.path(path, "costIntangCon.csv"), optimVarCon, xinitCon, dims$construction, tcalib)
  .writeCostIntang(file.path(path, "costIntangRen.csv"), optimVarRen, xinitRen, dims$renovation, tcalib)

  diagnosticsAll <- c(diagnostics, diagnosticsDetail)
  lapply(names(diagnosticsAll), function(nm) {
    write.csv(diagnosticsAll[[nm]], file = file.path(path, paste0(nm, ".csv")), row.names = FALSE)
  })

}

#' Call the optimization brick calibration
#'
#' This implements a gradient descent method to perform the minimization of the deviation.
#' The adjustment term is computed as the negative of the gradient of the objective function.
#'
#' @param path character vector with folders to run gams in
#' @param parameters named list of calibration parameters
#' @param tcalib numeric, time periods to calibrate on
#' @param calibTarget list of data frames of calibration targets
#' @param gamsOptions named list of GAMS options
#' @param switches named list of model switches
#' @param fileName character vector with gams file names
#' @param gamsCall system command to call gams
#'
#' @importFrom dplyr %>% .data filter mutate rename select
#' @importFrom gamstransfer Container
#' @importFrom tidyr pivot_longer
#' @importFrom utils write.csv
#'
runCalibrationOptim <- function(path,
                                parameters,
                                tcalib,
                                calibTarget,
                                gamsOptions = NULL,
                                switches = NULL,
                                fileName = "main.gms",
                                gamsCall = "gams") {


  # PREPARE ----------------------------------------------------------------------------

  # Store initial input data
  file.copy(from = file.path(path, "input.gdx"), to = file.path(path, "input_init.gdx"),
            overwrite = TRUE)
  dims <- .getDims(calibTarget)

  # Read in required input data
  gdxInput <- file.path(path, "input.gdx")
  mInput <- Container$new(gdxInput)
  renAllowed <- readSymbol(mInput, symbol = "renAllowed")
  vinExists <- readSymbol(mInput, symbol = "vinExists")
  vinCalib <- readSymbol(mInput, symbol = "vinCalib")
  xinitCon <- filter(readSymbol(mInput, "p_specCostCon"), .data[["cost"]] == "intangible")
  xinitRen <- filter(readSymbol(mInput, "p_specCostRen"), .data[["cost"]] == "intangible")

  switchesScenRun <- switches
  switchesScenRun[["RUNTYPE"]] <- "scenario"

  # Initialise optimization variable and optimization objective data frames
  optimVarCon <- .initOptimVarCon(mInput, tcalib)
  optimVarRen <- .initOptimVarRen(mInput, tcalib, renAllowed)
  outerObjective <- .initOuterObjective(mInput, tcalib)

  # Initialise objects to store diagnostic parameters over calibration iterations
  diagnostics <- setNames(nm = c("deviationConIter",
                                 "deviationRenIter",
                                 "stepSizeParamsIter")) %>%
    lapply(function(x) data.frame())

  diagnosticsDetail <- setNames(nm = c("stepSizeAllIter",
                                       "armijoStepAllIter",
                                       "outerObjectiveAllIter")) %>%
    lapply(function(x) data.frame())

  outerObjectiveIterComp <- data.frame()

  gdxOutput <- file.path(path, "output.gdx")

  .addTargetsToInput(mInput, path, calibTarget)

  # Initial Brick run
  runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)

  # Compute (virtual) objective function
  gdx <- file.path(path, paste0("calibration_0.gdx"))
  file.copy(from = gdxOutput, to = gdx, overwrite = TRUE)

  m <- Container$new(gdx)
  outerObjective <- .readOuterObjectiveOptim(m, outerObjective)



  # START OF CALIBRATION LOOP ----------------------------------------------------------------------

  for (i in seq_len(parameters[["iterations"]])) {


    # READ IN BRICK RESULTS AND COMPUTE DEVIATION -------------------------------------------------

    deviationCon <- .computeDescentDirection(m, dims$construction, tcalib)

    deviationRen <- .computeDescentDirection(m, dims$renovation, tcalib, flow = "renovation")

    if (switches[["AGGREGATEDIM"]] == "vin") {
      deviationRen <- select(deviationRen, -"vin")
      dims$renovationDeviation <- setdiff(dims$renovation, "vin")
    } else {
      dims$renovationDeviation <- dims$renovation
    }

    # Compute parameters delta and phi-derivative of the step size adaptation
    stepSizeParams <- .combineStepSizeParams(.computeStepSizeParams(deviationCon),
                                             .computeStepSizeParams(deviationRen))



    # PREPARE STEP SIZE ADAPTATION ---------------------------------------------------------------

    stepSizeParams <- .initStepSize(i, stepSizeParams, outerObjective, parameters[["stepSizeInit"]])

    armijoStep <- select(stepSizeParams, "region", "loc", "typ", "inc", "ttot")



    # ITERATION OF STEP SIZE ADAPTATION -----------------------------------------------------------

    for (j in seq_len(parameters[["iterationsArmijo"]])) {

      ## Adjust the optimization variable according to current step size ====
      optimVarCon <- .updateXSelect(armijoStep, optimVarCon, deviationCon,
                                    stepSizeParams, dims$construction)
      optimVarRen <- .updateXSelect(armijoStep, optimVarRen, deviationRen,
                                    stepSizeParams, dims$renovationDeviation)

      .addSpecCostToInput(mInput, path, optimVarCon, optimVarRen, xinitCon, xinitRen, tcalib, varName = "xA",
                          vinExists = vinExists, vinCalib = vinCalib)

      ## Evaluate outer objective of Brick results ====

      runGams(path, gamsOptions = gamsOptions, switches = switchesScenRun, gamsCall = gamsCall)

      gdxA <- file.path(path, paste0("calibrationA_", j, ".gdx"))
      file.copy(from = gdxOutput, to = gdxA, overwrite = TRUE)
      mA <- Container$new(gdxA)

      outerObjective <- .readOuterObjectiveOptim(mA, outerObjective, varName = "fA")


      ## Check step size conditions and update the step size ====

      # Filter for combinations that do not satisfy the Armijo condition yet
      armijoStep <- .checkArmijoStep(armijoStep, stepSizeParams, outerObjective, parameters[["sensitivityArmijo"]])

      diagDetObj <- list(stepSizeAllIter = stepSizeParams, armijoStepAllIter = armijoStep,
                         outerObjectiveAllIter = outerObjective)
      diagnosticsDetail <- setNames(lapply(names(diagDetObj), function(nm) {
        rbind(
          diagnosticsDetail[[nm]],
          diagDetObj[[nm]] %>%
            mutate(iteration = i, iterA = j) %>%
            select(-any_of(c("fMin", "fPrev"))) # only required for outerObjective
        )
      }), names(diagDetObj))

      if (nrow(armijoStep) == 0) {
        break
      }

      # Update the step size where applicable
      stepSizeParams <- .updateStepSize(armijoStep, stepSizeParams, outerObjective, parameters[["stepReduction"]])

    }

    outerObjectiveIterComp <- rbind(
      outerObjectiveIterComp,
      outerObjective %>%
        select(-any_of(c("f", "fPrev"))) %>%
        pivot_longer(names_to = "func", values_to = "value", cols = "fA") %>%
        mutate(iteration = i)
    )

    # If the armijo condition is still not satisfied for any combination:
    # Use the step size with the minimum objective function.
    # Print a warning if this minimum does not exist and use the last step size of the iteration.
    if (nrow(armijoStep) > 0) {
      stepSizeParams <- .adjustStepSizeAfterArmijo(armijoStep, stepSizeParams)
      stepSizeParams <- .adjustStepSizeAfterArmijo(armijoStep, stepSizeParams)

      optimVarCon <- .updateXSelect(armijoStep, optimVarCon, deviationCon, stepSizeParams, dims$construction)
      optimVarRen <- .updateXSelect(armijoStep, optimVarRen, deviationRen, stepSizeParams, dims$renovationDeviation)
    }


    # Update optimization variable data
    optimVarCon <- mutate(optimVarCon, x = .data[["xA"]], xA = NULL, xMin = NULL)
    optimVarRen <- mutate(optimVarRen, x = .data[["xA"]], xA = NULL, xMin = NULL)

    .addSpecCostToInput(mInput, path, optimVarCon, optimVarRen, xinitCon, xinitRen, tcalib,
                        vinExists = vinExists, vinCalib = vinCalib)

    runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)

    # Rename the output file of the Brick run with updated specific costs
    gdx <- file.path(path, paste0("calibration_", i, ".gdx"))
    file.copy(from = gdxOutput, to = gdx, overwrite = TRUE)
    m <- Container$new(gdx)

    outerObjective <- mutate(outerObjective, fPrev = .data[["f"]], f = NULL, fA = NULL)
    outerObjective <- .readOuterObjectiveOptim(m, outerObjective)

    outerObjectiveIterComp <- rbind(
      outerObjectiveIterComp,
      outerObjective %>%
        select(-"fPrev") %>%
        pivot_longer(names_to = "func", values_to = "value", cols = "f") %>%
        mutate(iteration = i)
    )

    # Store diagnostic variables
    diagObj <- list(deviationConIter = deviationCon, deviationRenIter = deviationRen,
                    stepSizeParamsIter = stepSizeParams)
    diagnostics <- setNames(lapply(names(diagObj), function(nm) {
      rbind(diagnostics[[nm]], mutate(diagObj[[nm]], iteration = i))
    }), names(diagObj))

  }



  # WRITE INTANGIBLE COSTS TO FILE -------------------------------------------------------------

  .writeCostIntang(file.path(path, "costIntangCon.csv"), optimVarCon, xinitCon, dims$construction, tcalib)
  .writeCostIntang(file.path(path, "costIntangRen.csv"), optimVarRen, xinitRen, dims$renovation, tcalib)

  diagnosticsAll <- c(diagnostics, diagnosticsDetail, list(outerObjectiveIterComp = outerObjectiveIterComp))
  lapply(names(diagnosticsAll), function(nm) {
    write.csv(diagnosticsAll[[nm]], file = file.path(path, paste0(nm, ".csv")), row.names = FALSE)
  })

}

#' Read calibration targets from input folder
#'
#' @param tcalib numeric, calibration time periods
#' @param modifyData logical, switch to control whether to alter data read from matching
#'
#' @importFrom dplyr %>% .data filter mutate
#'
.readCalibTarget <- function(tcalib, modifyData = FALSE) {
  dims <- list(
    stock        = c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", "ttot"),
    construction = c("qty", "bs", "hs", "region", "loc", "typ", "inc", "ttot"),
    renovation   = c("qty", "bs", "hs", "bsr", "hsr", "vin", "region", "loc", "typ", "inc", "ttot")
  )
  lapply(setNames(nm = names(dims)), function(var) {
    file <- paste0("f_", var, "CalibTarget.cs4r")
    dfTarget <- readInput(file, c(dims[[var]], "target")) %>%
      mutate(across(-all_of(c("target", "ttot")), as.character))
    if (isTRUE(modifyData)) {
      dfTarget <- dfTarget %>%
        mutate(target = ifelse(
          .data$loc == "rural",
          .data$target / 2,
          .data$target
        ))
      if (var == "renovation") {
        dfTarget <- dfTarget %>%
          filter(.data$ttot %in% tcalib) %>%
          mutate(target = ifelse(
            .data$hsr == "0",
            .data$target / 5,
            .data$target
          ))
      }
    }
    return(dfTarget)
  })
}

#' Get dimension names from calibration targets
#'
#' Gives the column names of a list of data frames except the first and the last
#' one, i.e. \code{qty} and \code{value}.
#'
#' @param calibTarget list, calibration targets
#'
.getDims <- function(calibTarget) {
  lapply(calibTarget, function(target) {
    dims <- colnames(target)
    dims[2:(length(dims) - 1)]
  })
}

#' Initialize the data frame for the optimization variable of construction
#'
#' @param mInput Gamstransfer container with the input data
#' @param tcalib numeric, calibration time periods
#'
#' @importFrom dplyr %>% .data filter mutate
#'
.initOptimVarCon <- function(mInput, tcalib) {
  expandSets("bs", "hs", "region", "loc", "typ", "inc", "ttot", .m = mInput) %>%
    filter(.data[["ttot"]] %in% tcalib) %>%
    mutate(x = 0)
}

#' Initialize the data frame for the optimization variable of renovation
#'
#' @param mInput Gamstransfer container with the input data
#' @param tcalib numeric, calibration time periods
#' @param renAllowed data frame with allowed renovation transitions
#'
#' @importFrom dplyr %>% .data filter mutate right_join
#'
.initOptimVarRen <- function(mInput, tcalib, renAllowed) {
  expandSets("bs", "hs", "bsr", "hsr", "vin", "region", "loc", "typ", "inc", "ttot", .m = mInput) %>%
    filter(.data[["ttot"]] %in% tcalib) %>%
    .filter(renAllowed) %>%
    mutate(x = 0)
}

#' Initialize the data frame for the objective function value
#'
#' @param mInput Gamstransfer container with the input data
#' @param tcalib numeric, calibration time periods
#'
#' @importFrom dplyr %>% .data filter
#'
.initOuterObjective <- function(mInput, tcalib) {
  expandSets("region", "loc", "typ", "inc", "ttot", .m = mInput) %>%
    filter(.data[["ttot"]] %in% tcalib)
}

#' Add the calibration targets to the input gdx
#'
#' @param mInput gamstransfer container of the input gdx
#' @param path character, path to output folder of this run
#' @param calibTarget list of data frames of calibration targets
#'
.addTargetsToInput <- function(mInput, path, calibTarget) {

  invisible(mInput$addParameter(
    name = "p_stockCalibTarget",
    domain = c("qty", "bs", "hs", "vin", "region", "loc", "typ", "inc", "ttot"),
    records = rename(calibTarget[["stock"]], value = "target"),
    description = "historic stock of buildings as calibration target in million m2"
  ))

  invisible(mInput$addParameter(
    name = "p_constructionCalibTarget",
    domain = c("qty", "bs", "hs", "region", "loc", "typ", "inc", "ttot"),
    records = rename(calibTarget[["construction"]], value = "target"),
    description = "historic flow of new buildings as calibration target in million m2/yr"
  ))

  invisible(mInput$addParameter(
    name = "p_renovationCalibTarget",
    domain = c("qty", "bs", "hs", "bsr", "hsr", "vin", "region", "loc", "typ", "inc", "ttot"),
    records = rename(calibTarget[["renovation"]], value = "target"),
    description = "historic flow of renovated and untouched buildings as calibration target in million m2/yr"
  ))

  mInput$write(file.path(path, "input.gdx"), compress = TRUE)

  return(invisible(mInput))
}

#' Aggregate data across given dimensions by a given function
#'
#' @param df data frame with data to be aggregated
#' @param agg character, columns with the dimensions to be aggregated
#' @param func function to be used for aggregation
#' @param valueNames character, names of the columns containing the values to aggregate
#'
#' @importFrom dplyr %>% .data across any_of group_by rename_with summarise
#'
.aggregateDim <- function(df, agg, func = sum, valueNames = "value") {
  if (any(agg %in% colnames(df))) {
    df <- df %>%
      group_by(across(-any_of(c(agg, valueNames)))) %>%
      summarise(across(valueNames, func))
  }
  return(df)
}

#' Compute the deviation between historic data and Brick results.
#' Then compute the adjustment term for the calibration \code{d}
#'
#' @param m Gams transfer container with current Brick results
#' @param target data frame of historical data
#' @param dims character, dimensions of historic data and Brick results
#' @param tcalib numeric, time steps to calibrate on
#' @param flow character, either 'construction' or 'renovation'
#' @param renAllowed data frame with allowed renovation transitions
#' @param vinExists data frame with existing vintages for each time period
#' @param agg character, dimensions to aggregate Brick results and target data over
#'
#' @importFrom dplyr %>% .data case_match case_when filter left_join mutate select
#' @importFrom tidyr pivot_wider replace_na
#'
.computeDeviation <- function(m, target, dims, tcalib, flow = c("construction", "renovation"),
                              renAllowed = NULL, vinExists = NULL, agg = NULL) {

  flow <- match.arg(flow)

  varSym <- switch(flow, construction = "v_construction", renovation = "v_renovation")
  costSym <- switch(flow, construction = "p_specCostCon", renovation = "p_specCostRen")

  gamsVar <- readSymbol(m, symbol = varSym)

  cost <- readSymbol(m, symbol = costSym) %>%
    pivot_wider(names_from = .data[["cost"]])

  deviation <- do.call(expandSets, c(as.list(dims), .m = m)) %>%
    .filter(renAllowed, vinExists) %>%
    left_join(gamsVar, by = dims) %>%
    left_join(target, by = dims) %>%
    replace_na(list(value = 0, target = 0)) %>%
    .aggregateDim(agg = agg, func = sum, valueNames = c("value", "target")) %>%
    left_join(cost, by = setdiff(dims, agg)) %>%
    filter(.data[["ttot"]] %in% tcalib)
  if (flow == "renovation") {
    # Set tangible cost of zero renovation to zero
    deviation[deviation$bsr == 0 & deviation$hsr == 0, "tangible"] <- 0
  }
  deviation %>%
    replace_na(list(intangible = 0)) %>% # Set missing intangible costs to 0
    mutate(dev = .data[["value"]] / .data[["target"]],
           # Store case of computing the descent direction d
           dCase = case_when(
             # Non-zero deviation and non-zero historic values
             .data[["target"]] > 1E-6 & .data[["value"]] > 1E-6 ~ "standard",
             # One of historic data or Gams results is zero
             xor(.data[["target"]] <= 1E-6, .data[["value"]] <= 1E-6) ~ "oneZero",
             # None of the above holds
             .default = "bothZero"
           )) %>%
    mutate(d = case_match(
      .data[["dCase"]],
      "standard" ~ log(.data[["dev"]]),
      "oneZero" ~ 0.5 * abs(.data[["intangible"]]) + ifelse(
        .data[["intangible"]] <= 1E-6,
        0.1 * .data[["tangible"]],
        0
      ),
      "bothZero" ~ 0
    )) %>%
    select(dims, "dev", "d")
}

#' Compute the descent direction from Brick results
#'
#' Read in objective function values for slightly shifted optimization variables
#' and compute the gradient of the objective function.
#' Set the adjustment term \code{d} to the negative of the gradient.
#'
#' @param m Gams transfer container with current Brick results
#' @param dims character, dimensions of the desired result object
#' @param tcalib numeric, time steps to calibrate on
#' @param flow character, either 'construction' or 'renovation'
#'
#' @importFrom dplyr %>% .data filter left_join mutate right_join rename select
#'
.computeDescentDirection <- function(m, dims, tcalib, flow = c("construction", "renovation")) {

  flow <- match.arg(flow)

  suffixSymbol <- switch(flow, construction = "Con", renovation = "Ren")

  p_fDiff <- readSymbol(m, symbol = paste0("p_fDiff", suffixSymbol)) %>%
    rename(fDiff = "value")
  p_f <- readSymbol(m, symbol = "p_f") %>%
    rename(f = "value")
  p_diff <- readSymbol(m, symbol = "p_diff")[[1]]

  p_d <- p_fDiff %>%
    left_join(p_f, by = c("region", "loc", "typ", "inc", "ttot")) %>%
    mutate(d = - (.data[["fDiff"]] - .data[["f"]]) / p_diff) %>%
    select(-"f", -"fDiff")

  if (flow == "construction") {
    p_d <- do.call(expandSets, c(as.list(dims), .m = m)) %>%
      right_join(p_d, by = c("bs", "hs", "region", "loc", "typ", "inc", "ttot"))
  } else {
    p_d <- do.call(expandSets, c(as.list(dims), .m = m)) %>%
      right_join(p_d, by = c("bsr", "hsr", "vin", "region", "loc", "typ", "inc", "ttot")) %>%
      filter(
        .data[["renType"]] == "0" & .data[["hsr"]] == "0" # zero renovation
        | .data[["renType"]] == "identRepl" & .data[["hs"]] == .data[["hsr"]] # identical replacement
        | .data[["renType"]] == "newSys" & .data[["hs"]] != .data[["hsr"]] # new system
      ) %>%
      select(-"renType")
  }

  return(p_d)
}

#' Compute the initial step size of the step size iteration algorithm
#'
#' @param i numeric, iteration number of overall calibration procedure
#' @param stepSizeParams data frame with parameters for the step size adaptation
#' @param outerObjective data frame with previous and current values of outer objective function;
#'   needs to contain the columns \code{fPrev} and \code{f}
#' @param stepSizeInit numeric, lower bound for the initial step size
#'
#' @importFrom dplyr %>% .data left_join mutate select
#'
.initStepSize <- function(i, stepSizeParams, outerObjective, stepSizeInit) {
  stepSizeParams %>%
    left_join(outerObjective, by = c("region", "loc", "typ", "inc", "ttot")) %>%
    mutate(stepSize = ifelse(
      i == 1 | .data$delta <= 0.001,
      stepSizeInit,
      pmax(stepSizeInit, (.data$fPrev - .data$f) / .data$delta)
    )) %>%
    mutate(minOuterObj = .data$f,
           minStepSize = 0) %>%
    select(-any_of(c("f", "fPrev")))
}

#' Compute the step size adaptation paramters delta and phi-derivative
#'
#' @param deviation data frame with deviation and adjustment term 'd'
#'
#' @importFrom dplyr %>% .data across all_of group_by mutate summarise
#'
.computeStepSizeParams <- function(deviation) {
  deviation %>%
    group_by(across(all_of(c("region", "loc", "typ", "inc", "ttot")))) %>%
    summarise(delta = sum(.data[["d"]]^2), .groups = "drop") %>%
    mutate(phiDeriv = - .data[["delta"]])
}

#' Combine the step size paramters delta and phi-derivative from construction and renovation flows
#'
#' @param paramsCon data frame with step size parameters from construction
#' @param paramsRen data frame with step size parameters from renovation
#'
#' @importFrom dplyr %>% .data across all_of group_by mutate summarise
#'
.combineStepSizeParams <- function(paramsCon, paramsRen) {
  rbind(paramsCon %>%
          mutate(flow = "construction"),
        paramsRen %>%
          mutate(flow = "renovation")) %>%
    group_by(across(all_of(c("region", "loc", "typ", "inc", "ttot")))) %>%
    summarise(delta = sum(.data[["delta"]]), phiDeriv = sum(.data[["phiDeriv"]]), .groups = "drop")
}

#' Update the optimization variable 'x'
#'
#' Adjust 'x' by adding the adjustment term 'd' multiplied by the step size
#'
#' @param optimVar data frame with the optimization variable 'x' and optionally 'xMin' and 'xA'
#' @param deviation data frame with deviation and adjustment term 'd'
#' @param stepSizeParams data frame with parameters of step size adaptation, including step size 'stepSize'
#' @param dims character, dimensions of the optimization variable
#' @param nameTo character, optimization variable to write the result to
#' @param factorMin numeric, additional factor to multiply the adjustment term with
#'
#' @importFrom dplyr %>% .data left_join mutate rename_with select
#'
.updateX <- function(optimVar, deviation, stepSizeParams, dims, nameTo = "x", factorMin = 1) {
  if (!"dCase" %in% colnames(deviation)) {
    deviation <- mutate(deviation, dCase = "standard")
  }

  optimVar %>%
    left_join(deviation, by = dims) %>%
    left_join(stepSizeParams, by = c("region", "loc", "typ", "inc", "ttot")) %>%
    # Set step size to one if descent direction was computed by alternative approach
    mutate(stepSize = ifelse(.data[["dCase"]] == "oneZero", 1, .data[["stepSize"]]),
           xNew = .data[["x"]] + factorMin * .data[["stepSize"]] * .data[["d"]]) %>%
    select(-any_of(nameTo)) %>%
    rename_with(~ nameTo, .cols = "xNew") %>%
    select(dims, any_of(c("vin", "x", "xMin", "xA")))
}

#' Assemble specific costs from initial specific costs and the optimization variable
#'
#' @param optimVar data frame with the optimization variable 'x' and optionally 'xMin' and 'xA'
#' @param xinit data frame with initial specific intangible costs
#' @param dims character, dimensions of the optimization variable
#' @param tcalib numeric, calibration time steps
#' @param flow character, type of flow, either construction or renovation
#' @param varName character, optimization variable to calculate specific costs from.
#'   Should be one of 'x', 'xA' or 'xMin'.
#' @param vinExists data frame of vintages that exist for each time period
#' @param vinCalib data frame with vintages that exist in calibration periods
#'
#' @importFrom dplyr %>% .data across any_of group_by mutate select ungroup
#'
# TODO: Maybe handle case of zero values differently
.determineSpecCost <- function(optimVar, xinit, dims, tcalib, flow = c("construction", "renovation"),
                               varName = "x", vinExists = NULL, vinCalib = NULL) {

  flow <- match.arg(flow)

  specCost <- xinit %>%
    .filter(vinExists) %>%
    replace_na(list(value = 0)) %>% # replace missing initial values with 0
    left_join(optimVar, by = dims) %>%
    group_by(across(-any_of(c("ttot", "x", "xA", "xMin", "value")))) %>%
    mutate(xProj = mean(.data[[varName]][.data[["ttot"]] %in% tcalib]),
           value = ifelse(
             .data[["ttot"]] %in% tcalib,
             .data[["value"]] + .data[[varName]],
             .data[["value"]] + .data[["xProj"]]
           ))
  if (flow == "renovation") {
    if (all(grepl("all", vinCalib$vin))) {
      vinCalib <- vinCalib %>%
        select(-"vin") %>%
        left_join(vinExists, by = "ttot")
    }
    vinCalibMax <- vinCalib %>%
      mutate(to = as.numeric(sub(".*(\\d{4})$", "\\1", as.character(.data$vin)))) %>%
      filter(.data$to == max(.data$to)) %>%
      dplyr::pull(var = "vin") %>%
      unique()

    specCost <- specCost %>%
      group_by(across(-any_of(c("vin", "x", "xA", "xMin", "xProj", "value")))) %>%
      mutate(value = ifelse(
        vinCalibMax == "all" | .data[["vin"]] %in% vinCalib$vin,
        .data[["value"]],
        .data[["value"]][.data[["vin"]] == vinCalibMax]
      )) %>%
      ungroup()
  }
  specCost %>%
    ungroup() %>%
    select(-"xProj", -any_of(c("x", "xMin", "xA")))
}

#' Write the specific costs to the input.gdx
#'
#' @param m Gams transfer container with previous input data
#' @param path character, path to this run
#' @param optimVarCon data frame with optimization variables of construction
#' @param optimVarRen data frame with optimization variables of renovation
#' @param xinitCon data frame with initial specific intangible costs of construction
#' @param xinitRen data frame with initial specific intangible costs of renovation
#' @param tcalib numeric, calibration time steps
#' @param vinExists data frame of vintages that exist for each time period
#' @param vinCalib data frame with vintages that exist in calibration periods
#' @param varName character, optimization variable to calculate specific costs from.
#'   Needs to be a column of optimVarCon and optimVarRen, should be one of 'x', 'xA' or 'xMin'.
#'
#' @importFrom dplyr filter
#'
.addSpecCostToInput <- function(m, path,
                                optimVarCon, optimVarRen, xinitCon, xinitRen, tcalib, vinExists, vinCalib,
                                varName = "x") {

  p_specCostCon <- m$getSymbols("p_specCostCon")[[1]]
  p_specCostRen <- m$getSymbols("p_specCostRen")[[1]]

  p_specCostConTang <- filter(readSymbol(m, symbol = "p_specCostCon"), .data[["cost"]] == "tangible")
  p_specCostRenTang <- filter(readSymbol(m, symbol = "p_specCostRen"), .data[["cost"]] == "tangible")

  dimsCon <- c("bs", "hs", "region", "loc", "typ", "inc", "ttot")
  dimsRen <- c("bs", "hs", "bsr", "hsr", "vin", "region", "loc", "typ", "inc", "ttot")

  p_specCostConData <- rbind(
    p_specCostConTang,
    .determineSpecCost(optimVarCon, xinitCon, dimsCon, tcalib, flow = "construction", varName = varName)
  )
  p_specCostRenData <- rbind(
    p_specCostRenTang,
    .determineSpecCost(optimVarRen, xinitRen, dimsRen, tcalib, flow = "renovation",
                       varName = varName, vinExists = vinExists, vinCalib = vinCalib)
  )

  p_specCostCon$setRecords(p_specCostConData)
  p_specCostRen$setRecords(p_specCostRenData)

  m$write(file.path(path, "input.gdx"), compress = TRUE)
}

#' Calculate sum of squared differences between results and historic values
#'
#' @param res numeric, result values
#' @param target numeric, historic values
#'
.sumSquare <- function(res, target) {
  sum((res - target)^2)
}

#' Calculate log-Likelihood of historical values
#'
#' @param res numeric, result values
#' @param target numeric, historic values
#'
.logLikelihood <- function(res, target) {
  -sum(target * log(res / sum(res)))
}

#' Evaluate the outer objective function.
#'
#' In case of the logit calibration, this outer objective function is virtual:
#' We are not actually performing an optimization algorithm on this function,
#' but only use it for the step size adaptation.
#'
#' @param m Gams transfer container object to read set values from
#' @param gamsVar data frame with brick results for the given variable
#' @param target data frame with historic data
#' @param dims character, dimensions of data
#' @param tcalib numeric, calibration time steps
#' @param agg character, dimension(s) to aggregate the data over
#'
#' @importFrom dplyr %>% .data across all_of filter group_by left_join summarise
#' @importFrom tidyr replace_na
#'
.computeOuterObjective <- function(m, gamsVar, target, dims, tcalib, agg = NULL) {

  do.call(expandSets, c(as.list(dims), .m = m)) %>%
    left_join(target, by = dims) %>%
    left_join(gamsVar, by = dims) %>%
    replace_na(list(value = 0, target = 0)) %>%
    filter(.data[["ttot"]] %in% tcalib) %>%
    .aggregateDim(agg = agg, valueNames = c("value", "target")) %>%
    group_by(across(all_of(c("region", "loc", "typ", "inc", "ttot")))) %>%
    summarise(value = .sumSquare(.data[["value"]], .data[["target"]]), .groups = "drop")
}


#' Read in brick results and compute the total outer objective function
#' by combining resuls from construction and renovation.
#'
#' @param m Gams transfer container to read brick results from
#' @param outerObjective data frame to write the outer objective to
#' @param p_constructionCalibTarget data frame with historic construction flows
#' @param p_renovationCalibTarget data frame with historic renovation flows
#' @param tcalib numeric, calibration time steps
#' @param varName character, column name in \code{outerObjective} to write the result to.
#'   Should be one of 'f', 'fA', 'fMin'.
#' @param agg character, dimension(s) to aggregate the data over
#'
#' @importFrom dplyr %>% .data across all_of group_by mutate rename_with right_join select summarise
#'
.combineOuterObjective <- function(m, outerObjective, p_constructionCalibTarget, p_renovationCalibTarget, tcalib,
                                   varName = "f", agg = NULL) {
  dimsCon <- setdiff(colnames(p_constructionCalibTarget), "target")
  dimsRen <- setdiff(colnames(p_renovationCalibTarget), "target")

  v_construction <- readSymbol(m, symbol = "v_construction")
  v_renovation <- readSymbol(m, symbol = "v_renovation")

  rbind(
    .computeOuterObjective(m, v_construction, p_constructionCalibTarget, dimsCon, tcalib, agg = agg) %>%
      mutate(flow = "construction"),
    .computeOuterObjective(m, v_renovation, p_renovationCalibTarget, dimsRen, tcalib, agg = agg) %>%
      mutate(flow = "renovation")
  ) %>%
    group_by(across(all_of(c("region", "loc", "typ", "inc", "ttot")))) %>%
    summarise(fNew = sum(.data[["value"]]), .groups = "drop") %>%
    rename_with(~ varName, .cols = "fNew") %>%
    right_join(outerObjective %>%
                 select(-any_of(varName)),
               by = c("region", "loc", "typ", "inc", "ttot"))

}


#' Read the outer objective from a gams calibration run.
#'
#' Only applicable for optimization calibration.
#'
#' @param m Gams transfer container to read brick results from
#' @param outerObjective data frame to write the outer objective to
#' @param varName character, column name in \code{outerObjective} to write the result to.
#'   Should be one of 'f', 'fA', 'fMin'.
#'
#' @importFrom dplyr %>% rename_with right_join select
#'
.readOuterObjectiveOptim <- function(m, outerObjective, varName = "f") {

  readSymbol(m, symbol = "p_f") %>%
    rename_with(~ varName, .cols = "value") %>%
    right_join(outerObjective %>%
                 select(-any_of(varName)),
               by = c("region", "loc", "typ", "inc", "ttot"))

}

#' Check if the Armijo condition holds.
#' Return only data combinations for which it does not hold.
#'
#' @param prevStep data frame with data combinations that did not satisfy the Armijo condition in the previous step
#' @param stepSizeParams data frame with step size and related parameters
#' @param outerObjective data frame containing the value of the outer objective function.
#'   Needs to contain the column \code{f} and the column specified by \code{varName}.
#' @param sensitivityArmijo numeric, parameter of the Armijo condition specifying how strict the condition is
#' @param varName character, outer objective variable to test the Armijo condition on
#' @param factorMin numeric, additional factor to modify the Armijo condition
#'
#' @importFrom dplyr %>% .data filter left_join select
#'
.checkArmijoStep <- function(prevStep, stepSizeParams, outerObjective, sensitivityArmijo,
                             varName = "fA", factorMin = 1) {
  prevStep %>%
    left_join(outerObjective, by = c("region", "loc", "typ", "inc", "ttot")) %>%
    left_join(stepSizeParams, by = c("region", "loc", "typ", "inc", "ttot")) %>%
    filter(.data[[varName]] > (.data[["f"]]
                               + factorMin * sensitivityArmijo * .data[["stepSize"]] * .data[["phiDeriv"]])) %>%
    select("region", "loc", "typ", "inc", "ttot")
}

#' Check if the condition of the heuristic step size adaptation holds:
#' Check if the outer objective does not increase.
#' Return only data combinations for which the condition does not hold, i.e. the outer objective increases
#'
#' @param prevStep data frame with data combinations that did not satisfy the heuristic condition in the previous step
#' @param outerObjective data frame containing the value of the outer objective function.
#'   Needs to contain the columns \code{f} and \code{fA}.
#'
#' @importFrom dplyr %>% .data filter left_join select
#'
.checkHeuristicStep <- function(prevStep, outerObjective) {
  prevStep %>%
    left_join(outerObjective, by = c("region", "loc", "typ", "inc", "ttot")) %>%
    filter(.data[["fA"]] > .data[["f"]]) %>%
    select("region", "loc", "typ", "inc", "ttot")
}

#' Update the optimization variable 'x' for selected combinations only
#'
#' @param totalStep data frame with combinations to perform the adjustment of 'x' on
#' @param optimVar data frame with the optimization variables
#' @param deviation data frame with the deviation from historic data and the adjustment parameter 'd'
#' @param stepSizeParams data frame with the parameters of the step size adaptatation procedure
#' @param dims character, dimensions of the optimization variable
#'
#' @importFrom dplyr %>% anti_join right_join
#'
.updateXSelect <- function(totalStep, optimVar, deviation, stepSizeParams, dims) {
  optimVarSelect <- right_join(optimVar, totalStep, by = c("region", "loc", "typ", "inc", "ttot"))

  optimVarSelect <- .updateX(optimVarSelect, deviation, stepSizeParams, dims, nameTo = "xA")

  optimVar %>%
    anti_join(optimVarSelect, by = dims) %>%
    rbind(optimVarSelect)
}

#' Update the step size for the selected combinations
#'
#' @param totalStep data frame with combinations to perform the adjustment ot the step size on
#' @param stepSizeParams data frame with the parameters of the step size adjustment algorithm
#' @param outerObjective data frame containing the value of the outer objective function.
#' @param stepReduction numeric, factor applied to stepSize to reduce the step size. Should be < 1.
#'
#' @importFrom dplyr %>% .data anti_join mutate right_join
#'
.updateStepSize <- function(totalStep, stepSizeParams, outerObjective, stepReduction) {
  stepSizeParamsSelect <- stepSizeParams %>%
    right_join(totalStep, by = c("region", "loc", "typ", "inc", "ttot")) %>%
    left_join(outerObjective, by = c("region", "loc", "typ", "inc", "ttot")) %>%
    mutate(minStepSize = ifelse(.data$fA < .data$minOuterObj, .data$stepSize, .data$minStepSize),
           minOuterObj = pmin(.data$minOuterObj, .data$fA)) %>%
    mutate(stepSize = .data[["stepSize"]] * stepReduction) %>%
    select(-any_of(c("f", "fA", "fMin", "fPrev")))

  stepSizeParams %>%
    anti_join(stepSizeParamsSelect, by = c("region", "loc", "typ", "inc", "ttot")) %>%
    rbind(stepSizeParamsSelect)
}

#' Handle the case that the step size adaptation condition is not satisfied after the predefined number of iterations.
#'
#' If for any combination the condition is still not satisfied:
#' Use the step size with the minimum objective function.
#' Print a warning if this minimum does not exist and use the last step size of the iteration.
#'
#' @param totalStep data frame with combinations for which the condition is not satisfied
#' @param stepSizeParams data frame with the parameters of the step size adaptation procedure
#'
#'  @importFrom dplyr mutate right_join
#'
.adjustStepSizeAfterArmijo <- function(totalStep, stepSizeParams) {
  stepSizeParamsSelect <- right_join(stepSizeParams, totalStep,
                                     by = c("region", "loc", "typ", "inc", "ttot"))
  if (nrow(stepSizeParamsSelect[stepSizeParamsSelect$minStepSize == 0, ]) > 0) {
    warning("At least one subset seems to have stalled, i.e. no descent has been detected. ",
            "Continuing with the smallest step size of the step size adaptation algorithm")
  }
  stepSizeParamsSelect <- mutate(stepSizeParamsSelect, stepSize = ifelse(
    .data$minStepSize != 0,
    .data$minStepSize,
    .data$stepSize
  ))

  stepSizeParams %>%
    anti_join(stepSizeParamsSelect, by = c("region", "loc", "typ", "inc", "ttot")) %>%
    rbind(stepSizeParamsSelect)
}

#' Write the intangible costs to a .csv file
#'
#' @param file character, path to file to which results should be written
#' @param optimVar data frame with optimization variable, needs to contain column 'x'
#' @param xinit data frame with initial intangible specific costs
#' @param dims character, dimensions of the optimization variable
#' @param tcalib numeric, calibration time steps
#'
#' @importFrom utils write.csv
#'
.writeCostIntang <- function(file, optimVar, xinit, dims, tcalib) {
  specCostIntang <- .determineSpecCost(optimVar, xinit, dims, tcalib)

  write.csv(specCostIntang, file = file, row.names = FALSE)
}
