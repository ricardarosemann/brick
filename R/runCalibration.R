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
#' the step size is determined according to the Armijo step size adaptation algorithm,
#' supplemented with a heuristic that picks the step size resulting in the (locally) minimum outer objective.
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

  dir.create(file.path(path, "failedRuns"))

  dims <- list(
    stock        = c("bs", "hs", "vin", "region", "loc", "typ", "inc", "ttot"),
    construction = c("bs", "hs", "region", "loc", "typ", "inc", "ttot"),
    demolition   = c("bs", "hs", "vin", "region", "loc", "typ", "inc", "ttot")
  )
  if (isTRUE(switches[["SEQUENTIALREN"]])) {
    dims$renovationBS <- c("bs", "hs", "bsr", "vin", "region", "loc", "typ", "inc", "ttot")
    dims$renovationHS <- c("bs", "hs", "hsr", "vin", "region", "loc", "typ", "inc", "ttot")
  } else {
    dims$renovation <- c("bs", "hs", "bsr", "hsr", "vin", "region", "loc", "typ", "inc", "ttot")
  }

  .runCalibration <- switch(
    switches[["CALIBRATIONMETHOD"]],
    logit = runCalibrationLogit,
    optimization = runCalibrationOptim,
    stop("You did not provide a valid calibration method. Stopping.")
  )
  .runCalibration(
    path,
    parameters,
    tcalib,
    dims,
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
#' case, the first local minimum objective is used.
#'
#' @param path character vector with folders to run gams in
#' @param parameters named list of calibration parameters
#' @param tcalib numeric, time periods to calibrate on
#' @param dims list of characters with dimensions of stock and flow variables
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
                                dims,
                                gamsOptions = NULL,
                                switches = NULL,
                                fileName = "main.gms",
                                gamsCall = "gams") {


  # PREPARE ----------------------------------------------------------------------------

  # Store initial input data
  if (!file.exists(file.path(path, "input_init.gdx"))) {
    file.copy(from = file.path(path, "input.gdx"), to = file.path(path, "input_init.gdx"))
  }
  variables <- setdiff(names(dims), c("stock", "demolition"))

  # Read in required input data
  gdxInput <- file.path(path, "input.gdx")
  mInput <- Container$new(gdxInput)
  renAllowedSym <- list(
    renovation = "renAllowed",
    renovationBS = "renAllowedBS",
    renovationHS = "renAllowedHS"
  )
  renAllowed <- lapply(X = renAllowedSym[setdiff(variables, "construction")], FUN = readSymbol, x = mInput)

  vinExists <- readSymbol(mInput, symbol = "vinExists")
  vinCalib <- readSymbol(mInput, symbol = "vinCalib")
  costSym <- list(
    construction = "p_specCostCon",
    renovation = "p_specCostRen",
    renovationBS = "p_specCostRenBS",
    renovationHS = "p_specCostRenHS"
  )
  xinit <- lapply(costSym[variables], function(symb) {
    filter(readSymbol(mInput, symb), .data$cost == "intangible")
  })

  # Brick only runs as standard scenario run
  switches[["RUNTYPE"]] <- "scenario"

  # Initialise optimization variable and optimization objective data frames
  optimVar <- .namedLapply(variables, function(var) {
    .initOptimVar(mInput, tcalib, dims[[var]], renAllowed[[var]])
  })
  outerObjective <- .initOuterObjective(mInput)

  # Initialise objects to store diagnostic parameters over calibration iterations
  diagDev <- list(
    construction = "deviationConIter",
    renovation = "deviationRenIter",
    renovationBS = "deviationRenBSIter",
    renovationHS = "deviationRenHSIter"
  )
  diagnostics <- .createListWithEmptyDf(nm = c(unlist(diagDev[variables], use.names = FALSE),
                                               "stepSizeParamsIter"))

  diagnosticsDetail <- .createListWithEmptyDf(nm = c("stepSizeAllIter",
                                                     "totalStepAllIter",
                                                     "outerObjectiveAllIter"))

  gdxOutput <- file.path(path, "output.gdx")

  p_calibTarget <- .readCalibTarget(dims, mInput)
  .addTargetsToInput(mInput, path, p_calibTarget, dims)

  p_calibTarget <- lapply(p_calibTarget[variables], .pick, qty = "area")

  # Initial Brick run
  runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)
  gamsSuccess <- checkGamsSuccess(path)
  if (isFALSE(all(gamsSuccess$success))) {
    stop("Gams failed for at least one subset in calibration iteration 0, aborting. ",
         "For details, refer to the model and solver summaries.")
  }

  # Compute (virtual) objective function
  gdx <- file.path(path, paste0("calibration_0.gdx"))
  file.copy(from = gdxOutput, to = gdx, overwrite = TRUE)

  m <- Container$new(gdx)
  outerObjective <- .combineOuterObjective(m, outerObjective, p_calibTarget,
                                           tcalib, dims, agg = switches[["AGGREGATEDIM"]])



  # START OF CALIBRATION LOOP ----------------------------------------------------------------------

  for (i in seq_len(parameters[["iterations"]])) {


    # READ IN BRICK RESULTS AND COMPUTE DEVIATION -------------------------------------------------

    deviation <- .namedLapply(variables, function(var) {
      .computeDeviation(m, p_calibTarget[[var]], dims[[var]], tcalib, flow = var,
                        agg = switches[["AGGREGATEDIM"]], calibResolution = switches[["CALIBRESOLUTION"]])
    })


    # Compute parameters delta and phi-derivative of the step size adaptation
    stepSizeParams <- .combineStepSizeParams(deviation)



    # PREPARE STEP SIZE ADAPTATION ---------------------------------------------------------------


    ## Set the initial step size for the step size adaptation ====

    stepSizeParams <- .initStepSize(i, stepSizeParams, outerObjective, parameters[["stepSizeInit"]])

    totalStep <- select(stepSizeParams, "region", "loc", "typ", "inc")



    # ITERATION OF STEP SIZE ADAPTATION -----------------------------------------------------------

    for (j in seq_len(parameters[["iterationsStepSize"]])) {

      ## Adjust the optimization variable according to current step size ====

      optimVar <- .namedLapply(variables, function(var) {
        .updateXSelect(totalStep, optimVar[[var]], deviation[[var]], stepSizeParams, dims[[var]])
      })

      .addSpecCostToInput(mInput, path, optimVar, xinit, tcalib, dims, varName = "xA",
                          vinExists = vinExists, vinCalib = vinCalib, shiftIntang = switches[["SHIFTINTANG"]])


      ## Evaluate outer objective of Brick results ====

      runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)
      gamsSuccess <- checkGamsSuccess(path, silent = TRUE)

      gdxA <- file.path(path, "calibrationA.gdx")
      file.copy(from = gdxOutput, to = gdxA, overwrite = TRUE)
      mA <- Container$new(gdxA)

      outerObjective <- .combineOuterObjective(mA, outerObjective, p_calibTarget,
                                               tcalib, dims,
                                               varName = "fA", agg = switches[["AGGREGATEDIM"]])

      # If Gams was not successful for a subset, reject this step size and save all run files
      outerObjective <- .rejectErrorRun(path, outerObjective, gamsSuccess, i, j)


      ## Check step size conditions and update the step size ====

      # If we found the minimum step size, set this as step size
      stepSizeParams <- .findMinimumStepSize(totalStep, stepSizeParams, outerObjective)

      # Filter for combinations that do not satisfy the Armijo condition or where no minimum has been found yet
      totalStep <- .checkStepCondition(totalStep, stepSizeParams, outerObjective, parameters[["sensitivityArmijo"]])

      diagDetObj <- list(stepSizeAllIter = stepSizeParams, totalStepAllIter = totalStep,
                         outerObjectiveAllIter = outerObjective)
      diagnosticsDetail <- .namedLapply(names(diagDetObj), function(nm) {
        rbind(
          diagnosticsDetail[[nm]],
          diagDetObj[[nm]] %>%
            mutate(iteration = i, iterA = j) %>%
            select(-any_of("fPrev")) # only required for outerObjective
        )
      })

      if (nrow(totalStep) == 0) {# All combinations satisfy the step size condition
        break
      }

      # Update the step size where applicable
      stepSizeParams <- .updateStepSize(totalStep, stepSizeParams, outerObjective, parameters[["stepReduction"]],
                                        j == parameters[["iterationsStepSize"]])

    }

    # Update optimization variable data
    optimVar <- lapply(optimVar[variables], select, -"xA")

    # Update optimization objective data
    outerObjective <- mutate(outerObjective, fPrev = .data$f, f = NULL, fA = NULL)

    # If the step size condition is still not satisfied for any combination:
    # Use the last step size of the iteration if this decreases the outer objective.
    # Print a warning otherwise and set step size to zero.
    if (nrow(totalStep) > 0) {
      stepSizeParams <- .adjustStepSizeAfterIteration(totalStep, stepSizeParams)
    }

    optimVar <- .namedLapply(variables, function(var) {
      .updateX(optimVar[[var]], deviation[[var]], stepSizeParams, dims[[var]])
    })

    .addSpecCostToInput(mInput, path, optimVar, xinit, tcalib, dims,
                        vinExists = vinExists, vinCalib = vinCalib, shiftIntang = switches[["SHIFTINTANG"]])


    ## Re-evaluate outer objective of Brick results ====

    # This is necessary because we may now have a combination of step sizes for the different subsets
    # we never computed with before. To obtain a clean calibration.gdx, we need to recalculate.

    runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)
    gamsSuccess <- checkGamsSuccess(path)

    # Store results with final step sizes in calibration iteration gdx
    gdx <- file.path(path, paste0("calibration_", i, ".gdx"))
    file.copy(from = gdxOutput, to = gdx, overwrite = TRUE)
    m <- Container$new(gdx)

    outerObjective <- .combineOuterObjective(m, outerObjective, p_calibTarget,
                                             tcalib, dims,
                                             agg = switches[["AGGREGATEDIM"]])

    # Store diagnostic variables
    diagObj <- stats::setNames(deviation[variables],
                               unlist(diagDev[variables], use.names = FALSE))
    diagObj$stepSizeParamsIter <- stepSizeParams
    diagnostics <- .namedLapply(names(diagObj), function(nm) {
      rbind(diagnostics[[nm]], mutate(diagObj[[nm]], iteration = i))
    })

    if (isFALSE(all(gamsSuccess$success))) {
      message("Gams failed for at least one subset in calibration iteration", i, ", aborting. ",
              "For details, refer to the model and solver summaries. ",
              "Diagnostics and output is still printed. If convergence is already satisfactory, ",
              "you might want to restart with fewer iterations.")
      break
    }

    if (.checkStoppingCriterion(outerObjective, p_calibTarget, parameters[["threshold"]])) {
      .printConvergenceMessage(i, parameters[["threshold"]])
      break
    }

  }



  # WRITE INTANGIBLE COSTS TO FILE -------------------------------------------------------------

  .writeStockRes(m, path)

  fileNames <- list(
    construction = "costIntangCon.csv",
    renovation = "costIntangRen.csv",
    renovationBS = "costIntangRenBS.csv",
    renovationHS = "costIntangRenHS.csv"
  )
  lapply(variables, function(var) {
    .writeCostIntang(file.path(path, fileNames[[var]]), optimVar[[var]], xinit[[var]], dims[[var]],
                     tcalib, flow = var, vinExists = vinExists, vinCalib = vinCalib)
  })

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
#' @param dims list of characters with dimensions of stock and flow variables
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
                                dims,
                                gamsOptions = NULL,
                                switches = NULL,
                                fileName = "main.gms",
                                gamsCall = "gams") {


  # PREPARE ----------------------------------------------------------------------------

  # Store initial input data
  file.copy(from = file.path(path, "input.gdx"), to = file.path(path, "input_init.gdx"),
            overwrite = TRUE)
  variables <- setdiff(names(dims), "stock")
  varCalib <- switch(
    switches[["CALIBRATIONTYPE"]],
    stocks = "stock",
    stockszero = c("stock", "renovation", "renovationBS", "renovationHS"),
    flows = c("construction", "renovation", "renovationBS", "renovationHS")
  )
  varCalib <- intersect(varCalib, names(dims))

  # Read in required input data
  gdxInput <- file.path(path, "input.gdx")
  mInput <- Container$new(gdxInput)
  renAllowedSym <- list(
    renovation = "renAllowed",
    renovationBS = "renAllowedBS",
    renovationHS = "renAllowedHS"
  )
  renAllowed <- lapply(X = renAllowedSym[setdiff(variables, "construction")], FUN = readSymbol, x = mInput)
  vinExists <- readSymbol(mInput, symbol = "vinExists")
  vinCalib <- readSymbol(mInput, symbol = "vinCalib")
  costSym <- list(
    construction = "p_specCostCon",
    renovation = "p_specCostRen",
    renovationBS = "p_specCostRenBS",
    renovationHS = "p_specCostRenHS"
  )
  xinit <- .namedLapply(variables, function(var) {
    filter(readSymbol(mInput, costSym[[var]]), .data$cost == "intangible")
  })

  switchesScenRun <- switches
  switchesScenRun[["RUNTYPE"]] <- "scenario"

  # Initialise optimization variable and optimization objective data frames
  optimVar <- .namedLapply(variables, function(var) {
    .initOptimVar(mInput, tcalib, dims[[var]], renAllowed[[var]])
  })
  outerObjective <- .initOuterObjective(mInput)

  # Initialise objects to store diagnostic parameters over calibration iterations
  diagDev <- list(
    construction = "deviationConIter",
    renovation = "deviationRenIter",
    renovationBS = "deviationRenBSIter",
    renovationHS = "deviationRenHSIter"
  )
  diagnostics <- .createListWithEmptyDf(nm = c(unlist(diagDev[variables], use.names = FALSE),
                                               "stepSizeParamsIter"))

  diagnosticsDetail <- .createListWithEmptyDf(nm = c("stepSizeAllIter",
                                                     "totalStepAllIter",
                                                     "heuristicStepAllIter",
                                                     "outerObjectiveAllIter"))

  outerObjectiveIterComp <- data.frame()

  gdxOutput <- file.path(path, "output.gdx")

  p_calibTarget <- .readCalibTarget(dims, mInput)
  .addTargetsToInput(mInput, path, p_calibTarget, dims)

  p_calibTarget <- lapply(p_calibTarget[varCalib], .pick, qty = "area")

  # Initial Brick run
  runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)
  gamsSuccess <- checkGamsSuccess(path)
  if (isFALSE(all(gamsSuccess$success))) {
    stop("Gams failed for at least one subset in calibration iteration 0, aborting. ",
         "For details, refer to the model and solver summaries.")
  }

  # Compute (virtual) objective function
  gdx <- file.path(path, paste0("calibration_0.gdx"))
  file.copy(from = gdxOutput, to = gdx, overwrite = TRUE)

  m <- Container$new(gdx)
  outerObjective <- .readOuterObjectiveOptim(m, outerObjective)



  # START OF CALIBRATION LOOP ----------------------------------------------------------------------

  for (i in seq_len(parameters[["iterations"]])) {


    # READ IN BRICK RESULTS AND COMPUTE DEVIATION -------------------------------------------------

    deviation <- .namedLapply(variables, function(var) {
      .computeDescentDirection(m, dims[[var]], tcalib, flow = var)
    })

    for (var in grep("renovation", variables, value = TRUE)) {
      if (switches[["AGGREGATEDIM"]] == "vin") {
        deviation[[var]] <- select(deviation[[var]], -"vin")
        dims[[paste0(var, "Dev")]] <- setdiff(dims[[var]], "vin")
      } else {
        dims[[paste0(var, "Dev")]] <- dims[[var]]
      }
    }

    # Compute parameters delta and phi-derivative of the step size adaptation
    stepSizeParams <- .combineStepSizeParams(deviation)



    # PREPARE STEP SIZE ADAPTATION ---------------------------------------------------------------

    stepSizeParams <- .initStepSize(i, stepSizeParams, outerObjective, parameters[["stepSizeInit"]])

    totalStep <- select(stepSizeParams, "region", "loc", "typ", "inc")



    # ITERATION OF STEP SIZE ADAPTATION -----------------------------------------------------------

    for (j in seq_len(parameters[["iterationsStepSize"]])) {

      ## Adjust the optimization variable according to current step size ====
      optimVar <- .namedLapply(variables, function(var) {
        .updateXSelect(totalStep, optimVar[[var]], deviation[[var]], stepSizeParams,
                       dims[[if (identical(var, "construction")) var else paste0(var, "Dev")]])
      })

      .addSpecCostToInput(mInput, path, optimVar, xinit, tcalib, dims, varName = "xA",
                          vinExists = vinExists, vinCalib = vinCalib, shiftIntang = switches[["SHIFTINTANG"]])

      ## Evaluate outer objective of Brick results ====

      runGams(path, gamsOptions = gamsOptions, switches = switchesScenRun, gamsCall = gamsCall)
      gamsSuccess <- checkGamsSuccess(path, silent = TRUE)

      gdxA <- file.path(path, "calibrationA.gdx")
      file.copy(from = gdxOutput, to = gdxA, overwrite = TRUE)
      mA <- Container$new(gdxA)

      outerObjective <- .readOuterObjectiveOptim(mA, outerObjective, varName = "fA")

      # If Gams was not successful for a subset, reject this step size and save all run files
      outerObjective <- .rejectErrorRun(path, outerObjective, gamsSuccess, i, j)


      ## Check step size conditions and update the step size ====

      # If we found the minimum step size, set this as step size
      stepSizeParams <- .findMinimumStepSize(totalStep, stepSizeParams, outerObjective)

      # Filter for combinations that do not satisfy the Armijo condition or where no minimum has been found yet
      totalStep <- .checkStepCondition(totalStep, stepSizeParams, outerObjective, parameters[["sensitivityArmijo"]])

      diagDetObj <- list(stepSizeAllIter = stepSizeParams, totalStepAllIter = totalStep,
                         outerObjectiveAllIter = outerObjective)
      diagnosticsDetail <- .namedLapply(names(diagDetObj), function(nm) {
        rbind(
          diagnosticsDetail[[nm]],
          diagDetObj[[nm]] %>%
            mutate(iteration = i, iterA = j) %>%
            select(-any_of("fPrev")) # only required for outerObjective
        )
      })

      if (nrow(totalStep) == 0) {
        break
      }

      # Update the step size where applicable
      stepSizeParams <- .updateStepSize(totalStep, stepSizeParams, outerObjective, parameters[["stepReduction"]],
                                        j == parameters[["iterationsStepSize"]])

    }

    outerObjectiveIterComp <- rbind(
      outerObjectiveIterComp,
      outerObjective %>%
        select(-any_of(c("f", "fPrev"))) %>%
        pivot_longer(names_to = "func", values_to = "value", cols = "fA") %>%
        mutate(iteration = i)
    )

    # Remove step size adaptation entry from optimization variable data
    optimVar <- lapply(optimVar[variables], select, -"xA")

    # Store previous outer objective, remove no longer needed columns
    outerObjective <- mutate(outerObjective, fPrev = .data$f, f = NULL, fA = NULL)

    # If the step size condition is still not satisfied for any combination:
    # Use the last step size of the iteration if this decreases the outer objective.
    # Print a warning otherwise and set step size to zero.
    if (nrow(totalStep) > 0) {
      stepSizeParams <- .adjustStepSizeAfterIteration(totalStep, stepSizeParams)
    }

    optimVar <- .namedLapply(variables, function(var) {
      .updateX(optimVar[[var]], deviation[[var]], stepSizeParams,
               dims[[if (identical(var, "construction")) var else paste0(var, "Dev")]])
    })

    .addSpecCostToInput(mInput, path, optimVar, xinit, tcalib, dims,
                        vinExists = vinExists, vinCalib = vinCalib, shiftIntang = switches[["SHIFTINTANG"]])

    runGams(path, gamsOptions = gamsOptions, switches = switches, gamsCall = gamsCall)
    gamsSuccess <- checkGamsSuccess(path)

    # Rename the output file of the Brick run with updated specific costs
    gdx <- file.path(path, paste0("calibration_", i, ".gdx"))
    file.copy(from = gdxOutput, to = gdx, overwrite = TRUE)
    m <- Container$new(gdx)

    outerObjective <- .readOuterObjectiveOptim(m, outerObjective)

    outerObjectiveIterComp <- rbind(
      outerObjectiveIterComp,
      outerObjective %>%
        select(-"fPrev") %>%
        pivot_longer(names_to = "func", values_to = "value", cols = "f") %>%
        mutate(iteration = i)
    )

    # Store diagnostic variables
    diagObj <- stats::setNames(deviation[variables],
                               unlist(diagDev[variables], use.names = FALSE))
    diagObj$stepSizeParamsIter <- stepSizeParams
    diagnostics <- .namedLapply(names(diagObj), function(nm) {
      rbind(diagnostics[[nm]], mutate(diagObj[[nm]], iteration = i))
    })

    if (isFALSE(all(gamsSuccess$success))) {
      message("Gams failed for at least one subset in calibration iteration", i, ", aborting. ",
              "For details, refer to the model and solver summaries. ",
              "Diagnostics and output is still printed. If convergence is already satisfactory, ",
              "you might want to restart with fewer iterations.")
      break
    }

    isConverged <- .checkStoppingCriterion(outerObjective, p_calibTarget, parameters[["threshold"]],
                                           zeroFlow = switches[["CALIBRATIONTYPE"]] == "stockszero")
    if (isConverged) {
      .printConvergenceMessage(i, parameters[["threshold"]])
      break
    }

  }



  # WRITE INTANGIBLE COSTS TO FILE -------------------------------------------------------------

  .writeStockRes(m, path)

  fileNames <- list(
    construction = "costIntangCon.csv",
    renovation = "costIntangRen.csv",
    renovationBS = "costIntangRenBS.csv",
    renovationHS = "costIntangRenHS.csv"
  )
  lapply(variables, function(var) {
    .writeCostIntang(file.path(path, fileNames[[var]]), optimVar[[var]], xinit[[var]], dims[[var]], tcalib)
  })

  diagnosticsAll <- c(diagnostics, diagnosticsDetail, list(outerObjectiveIterComp = outerObjectiveIterComp))
  lapply(names(diagnosticsAll), function(nm) {
    write.csv(diagnosticsAll[[nm]], file = file.path(path, paste0(nm, ".csv")), row.names = FALSE)
  })

}

#' Read calibration targets from input folder
#'
#' @param dims named character, dimensions of each variable of the calibration
#' @param mInput gamstransfer container with the input.gdx
#'
#' @returns list of data frames with calibration targets for each variable
#'
#' @importFrom dplyr %>% .data filter mutate
#'
.readCalibTarget <- function(dims, mInput) {
  .namedLapply(names(dims), function(var) {
    file <- paste0("f_", var, "CalibTarget.cs4r")
    readInput(file, c("qty", dims[[var]], "target")) %>%
      toModelResolution(mInput, value = "target", unfilteredDims = "ttot") %>%
      mutate(across(-all_of(c("target", "ttot")), as.character))
  })
}

#' Initialize the data frame for the optimization variables
#'
#' @param mInput Gamstransfer container with the input data
#' @param tcalib numeric, calibration time periods
#' @param dims character, dimensions to initialize the data with
#' @param renAllowed data frame with allowed renovation transitions
#'
#' @returns data frame of desired dimension with x = 0 (column to contain the optimzation variable)
#'
#' @importFrom dplyr %>% .data filter mutate right_join
#'
.initOptimVar <- function(mInput, tcalib, dims, renAllowed) {
  do.call(expandSets, c(as.list(dims), .m = mInput)) %>%
    filter(.data[["ttot"]] %in% tcalib) %>%
    .filter(renAllowed) %>%
    mutate(x = 0)
}

#' Initialize the data frame for the objective function value
#'
#' @param mInput Gamstransfer container with the input data
#'
#' @returns data frame with required dimensions for the outer objective
#'
#' @importFrom dplyr %>% .data filter
#'
.initOuterObjective <- function(mInput) {
  expandSets("region", "loc", "typ", "inc", .m = mInput)
}

#' Create a list of empty data frames with given names
#'
#' @param nm character, names of the empty data frames
#'
#' @returns list of empty data frames
#'
.createListWithEmptyDf <- function(nm) {
  stats::setNames(nm = nm) %>%
    lapply(function(x) data.frame())
}

#' Add the calibration targets to the input gdx
#'
#' @param mInput gamstransfer container of the input gdx
#' @param path character, path to output folder of this run
#' @param p_calibTarget list of data frames of calibration targets
#' @param dims list of characters, dimensions of data
#'
#' @returns gamstransfer container with the modified input gdx (invisibly)
#'
.addTargetsToInput <- function(mInput, path, p_calibTarget, dims) {

  paramNames <- c(
    stock = "p_stockCalibTarget",
    construction = "p_constructionCalibTarget",
    demolition = "p_demolitionCalibTarget",
    renovation = "p_renovationCalibTarget",
    renovationBS = "p_renovationBSCalibTarget",
    renovationHS = "p_renovationHSCalibTarget"
  )

  descriptions <- c(
    stock = "historic stock of buildings as calibration target in million m2",
    construction = "historic flow of new buildings as calibration target in million m2/yr",
    demolition = "historic flow of demolished buildings as calibration target in million m2/yr",
    renovation = "historic flow of renovated and untouched buildings as calibration target in million m2/yr",
    renovationBS = "historic flow of shell renovation as calibration target in million m2/yr",
    renovationHS = "historic flow of heating system renovation as calibration target in million m2/yr"
  )

  for (var in names(dims)) {
    invisible(mInput$addParameter(
      name = paramNames[[var]],
      domain = c("qty", dims[[var]]),
      records = rename(p_calibTarget[[var]], value = "target"),
      description = descriptions[[var]]
    ))
  }

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
#' @returns data frame with aggregated data
#'
#' @importFrom dplyr %>% .data across any_of group_by rename_with summarise
#'
.aggregateDim <- function(df, agg, func = sum, valueNames = "value") {
  if (any(agg %in% colnames(df))) {
    df <- df %>%
      group_by(across(-any_of(c(agg, valueNames)))) %>%
      mutate(across(valueNames, func)) %>%
      ungroup()
  }
  return(df)
}

#' Compute the deviation between historic data and Brick results as the log ratio.
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
#' @param calibResolution character, resolution of the calibration for renovation flows
#'
#' @returns data frame containing the deviation from the target \code{dev} and the adjustment term \code{d}.
#'
#' @importFrom dplyr %>% .data case_match case_when filter left_join mutate select
#' @importFrom tidyr pivot_wider replace_na
#'
.computeDeviation <- function(m, target, dims, tcalib,
                              flow = c("construction", "renovation", "renovationBS", "renovationHS"),
                              renAllowed = NULL, vinExists = NULL, agg = NULL, calibResolution = "full") {

  flow <- match.arg(flow)

  # Suppress filtering for vintage in case of construction flow
  if (identical(flow, "construction")) vinExists <- NULL

  varSym <- switch(
    flow,
    construction = "v_construction",
    renovation = "v_renovation",
    renovationBS = "v_renovationBS",
    renovationHS = "v_renovationHS"
  )
  costSym <- switch(
    flow,
    construction = "p_specCostCon",
    renovation = "p_specCostRen",
    renovationBS = "p_specCostRenBS",
    renovationHS = "p_specCostRenHS"
  )

  gamsVar <- readSymbol(m, symbol = varSym)

  cost <- readSymbol(m, symbol = costSym) %>%
    pivot_wider(names_from = .data[["cost"]])

  deviation <- do.call(expandSets, c(as.list(dims), .m = m)) %>%
    .filter(renAllowed, vinExists) %>%
    left_join(gamsVar, by = dims) %>%
    left_join(target, by = dims) %>%
    replace_na(list(value = 0, target = 0)) %>%
    .aggregateDim(agg = agg, func = sum, valueNames = c("value", "target"))
  if (identical(calibResolution, "identRepl") && flow %in% c("renovation", "renovationHS")) {
    deviation <- deviation %>%
      mutate(renType = case_when(
        .data$hsr == "0" ~ "0",
        .data$hs == .data$hsr ~ "identRepl",
        .default = "newSys"
      )) %>%
      .aggregateDim("hs", func = sum, valueNames = c("value", "target"))
  }
  deviation <- deviation %>%
    left_join(cost, by = setdiff(dims, agg)) %>%
    filter(.data[["ttot"]] %in% tcalib)
  # Set tangible cost of zero renovation to zero
  if (identical(flow, "renovationBS")) {
    deviation[deviation$bsr == 0, "tangible"] <- 0
  } else if (identical(flow, "renovationHS")) {
    deviation[deviation$hsr == 0, "tangible"] <- 0
  } else if (identical(flow, "renovation")) {
    deviation[deviation$bsr == 0 & deviation$hsr == 0, "tangible"] <- 0
  }
  deviation %>%
    replace_na(list(intangible = 0)) %>% # Set missing intangible costs to 0
    mutate(dev = .data$value / .data$target,
           # Store case of computing the descent direction d
           dCase = case_when(
             # Non-zero deviation and non-zero historic values
             .data$target > 0 & .data$value > 0 ~ "standard",
             # One of historic data or Gams results is zero
             xor(.data$target == 0, .data$value == 0) ~ "oneZero",
             # None of the above holds
             .default = "bothZero"
           )) %>%
    mutate(d = case_match(
      .data$dCase,
      "standard" ~ log(.data$dev),
      "oneZero" ~ (.data$value - .data$target) * ifelse(abs(.data$intangible) > 1E-6,
                                                        0.5 * abs(.data$intangible),
                                                        0.1 * .data$tangible),
      "bothZero" ~ 0
    )) %>%
    select(dims, "dCase", "dev", "d")
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
#' @returns data frame with the adjustment term \code{d}.
#'
#' @importFrom dplyr %>% .data filter left_join mutate right_join rename select
#'
.computeDescentDirection <- function(m, dims, tcalib,
                                     flow = c("construction", "renovation", "renovationBS", "renovationHS")) {

  flow <- match.arg(flow)

  suffixSymbol <- switch(
    flow,
    construction = "Con",
    renovation = "Ren",
    renovationBS = "RenBS",
    renovationHS = "RenHS"
  )

  p_fDiff <- readSymbol(m, symbol = paste0("p_fDiff", suffixSymbol)) %>%
    rename(fDiff = "value")
  p_f <- readSymbol(m, symbol = "p_f") %>%
    rename(f = "value")
  p_diff <- readSymbol(m, symbol = "p_diff")[[1]]

  p_d <- p_fDiff %>%
    left_join(p_f, by = c("region", "loc", "typ", "inc")) %>%
    mutate(d = - (.data[["fDiff"]] - .data[["f"]]) / p_diff) %>%
    select(-"f", -"fDiff")

  if (identical(flow, "construction")) {
    p_d <- do.call(expandSets, c(as.list(dims), .m = m)) %>%
      right_join(p_d, by = dims)
  } else {
    p_d <- do.call(expandSets, c(as.list(dims), .m = m)) %>%
      right_join(p_d, by = setdiff(dims, c("bs", "hs")))
    if (flow %in% c("renovation", "renovationHS")) {
      p_d <- p_d %>%
        filter(
          .data$renType == "0" & .data$hsr == "0" # zero renovation
          | .data$renType == "identRepl" & .data$hs == .data$hsr # identical replacement
          | .data$renType == "newSys" & .data$hs != .data$hsr # new system
        ) %>%
        select(-"renType")
    }
  }

  return(p_d)
}

#' Compute the step size adaptation paramters delta and phi-derivative
#'
#' @param deviation data frame with deviation and adjustment term 'd'
#'
#' @returns data frame with step size adaptation parameters \code{delta} and \code{phiDeriv}.
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
#' @param deviation named list of data frames with deviation and adjustment term \code{d} from all calibration variables
#'
#' @returns data frame with step size adaptation parameters computed from all calibration variables
#'
#' @importFrom dplyr %>% .data across all_of group_by mutate summarise
#'
.combineStepSizeParams <- function(deviation) {
  do.call(rbind, lapply(names(deviation), function(var) {
    .computeStepSizeParams(deviation[[var]]) %>%
      mutate(flow = var)
  })) %>%
    group_by(across(all_of(c("region", "loc", "typ", "inc")))) %>%
    summarise(delta = sum(.data[["delta"]]), phiDeriv = sum(.data[["phiDeriv"]]), .groups = "drop")
}

#' Compute the initial step size of the step size iteration algorithm
#'
#' @param i numeric, iteration number of overall calibration procedure
#' @param stepSizeParams data frame with parameters for the step size adaptation
#' @param outerObjective data frame with previous and current values of outer objective function;
#'   needs to contain the columns \code{fPrev} and \code{f}
#' @param stepSizeInit numeric, lower bound for the initial step size
#'
#' @returns data frame with the initial step size and initial values for the minimum outer objective
#'   and corresponding step size
#'
#' @importFrom dplyr %>% .data left_join mutate select
#'
.initStepSize <- function(i, stepSizeParams, outerObjective, stepSizeInit) {
  stepSizeParams %>%
    left_join(outerObjective, by = c("region", "loc", "typ", "inc")) %>%
    mutate(stepSize = ifelse(
      i == 1 | .data$delta <= 0.001,
      stepSizeInit,
      pmax(stepSizeInit, (.data$fPrev - .data$f) / .data$delta)
    )) %>%
    mutate(minOuterObj = .data$f,
           minStepSize = 0) %>%
    select(-any_of(c("f", "fPrev")))
}

#' Update the optimization variable 'x'
#'
#' Adjust 'x' by adding the adjustment term 'd' multiplied by the step size
#'
#' @param optimVar data frame with the optimization variable 'x' and optionally 'xA'
#' @param deviation data frame with deviation and adjustment term 'd'
#' @param stepSizeParams data frame with parameters of step size adaptation, including step size 'stepSize'
#' @param dims character, dimensions of the optimization variable
#' @param nameTo character, optimization variable to write the result to
#'
#' @returns data frame with the updated optimization variable \code{x} or \code{xA}
#'
#' @importFrom dplyr %>% .data left_join mutate rename_with select
#'
.updateX <- function(optimVar, deviation, stepSizeParams, dims, nameTo = "x") {
  if (!"dCase" %in% colnames(deviation)) {
    deviation <- mutate(deviation, dCase = "standard")
  }

  optimVar %>%
    left_join(deviation, by = dims) %>%
    left_join(stepSizeParams, by = c("region", "loc", "typ", "inc")) %>%
    # Set step size to one if descent direction was computed by alternative approach
    mutate(stepSize = ifelse(.data[["dCase"]] == "oneZero", 1, .data[["stepSize"]]),
           xNew = .data[["x"]] + .data[["stepSize"]] * .data[["d"]]) %>%
    select(-any_of(nameTo)) %>%
    rename_with(~ nameTo, .cols = "xNew") %>%
    select(dims, any_of(c("vin", "x", "xA")))
}

#' Assemble specific costs from initial specific costs and the optimization variable
#'
#' @param optimVar data frame with the optimization variable 'x' and optionally 'xA'
#' @param xinit data frame with initial specific intangible costs
#' @param dims character, dimensions of the optimization variable
#' @param tcalib numeric, calibration time steps
#' @param flow character, type of flow, either construction or renovation
#' @param varName character, optimization variable to calculate specific costs from.
#'   Should be either 'x' or 'xA'.
#' @param vinExists data frame of vintages that exist for each time period
#' @param vinCalib data frame with vintages that exist in calibration periods
#' @param shiftIntang logical indicating whether intangible costs should be shifted to positive range
#'
#' @returns data frame with specific intangible costs for the given flow
#'
#' @importFrom dplyr %>% .data across any_of group_by mutate select ungroup
#'
# TODO: Maybe handle case of zero values differently
.determineSpecCost <- function(optimVar, xinit, dims, tcalib,
                               flow = c("construction", "renovation", "renovationBS", "renovationHS"),
                               varName = "x", vinExists = NULL, vinCalib = NULL, shiftIntang = TRUE) {

  flow <- match.arg(flow)

  if (flow == "construction") {
    finalBs <- "bs"
    finalHs <- "hs"
    # Suppress filtering by vintage
    vinExists <- NULL
  } else {
    finalBs <- "bsr"
    finalHs <- "hsr"
  }

  specCost <- xinit %>%
    .filter(vinExists) %>%
    replace_na(list(value = 0)) %>% # replace missing initial values with 0
    left_join(optimVar, by = dims) %>%
    group_by(across(-any_of(c("ttot", "x", "xA", "value")))) %>%
    mutate(xProj = mean(.data[[varName]][.data[["ttot"]] %in% tcalib]),
           value = .data$value + ifelse(.data$ttot %in% tcalib, .data[[varName]], .data$xProj)) %>%
    ungroup() %>%
    select(-"xProj", -any_of(c("x", "xA")))
  if (grepl("renovation", flow)) {
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
      group_by(across(-all_of(c("vin", "value")))) %>%
      mutate(value = ifelse(
        vinCalibMax == "all" | .data[["vin"]] %in% vinCalib$vin,
        .data[["value"]],
        .data[["value"]][.data[["vin"]] == vinCalibMax]
      ))
  }
  if (isTRUE(shiftIntang)) {
    specCost <- specCost %>%
      group_by(across(-any_of(c(finalBs, finalHs, "value")))) %>%
      mutate(value = ifelse(
        is.na(.data$value),
        NA,
        .data$value - min(.data$value, na.rm = TRUE) + 1E-5
      )) %>%
      ungroup()
  }
  specCost
}

#' Write the specific costs to the input.gdx
#'
#' @param m Gams transfer container with previous input data
#' @param path character, path to this run
#' @param optimVar list of data frames with optimization variables of all flows
#' @param xinit list of data frames with initial specific intangible costs of all flows
#' @param tcalib numeric, calibration time steps
#' @param dims list of characters, dimensions of each flow
#' @param vinExists data frame of vintages that exist for each time period
#' @param vinCalib data frame with vintages that exist in calibration periods
#' @param varName character, optimization variable to calculate specific costs from.
#'   Needs to be a column of optimVarCon and optimVarRen, should be either 'x' or 'xA'.
#' @param shiftIntang logical indicating whether intangible costs should be shifted to positive range
#'
#' @importFrom dplyr filter
#'
.addSpecCostToInput <- function(m, path,
                                optimVar, xinit, tcalib, dims, vinExists, vinCalib,
                                varName = "x", shiftIntang = TRUE) {

  costSym <- list(
    construction = "p_specCostCon",
    renovation = "p_specCostRen",
    renovationBS = "p_specCostRenBS",
    renovationHS = "p_specCostRenHS"
  )
  variables <- names(optimVar)

  p_specCost <- .namedLapply(variables, function(var) {
    m$getSymbols(costSym[[var]])[[1]]
  })

  p_specCostTang <- .namedLapply(variables, function(var) {
    filter(readSymbol(m, symbol = costSym[[var]]), .data$cost == "tangible")
  })

  p_specCostData <- .namedLapply(variables, function(var) {
    rbind(
      p_specCostTang[[var]],
      .determineSpecCost(optimVar[[var]], xinit[[var]], dims[[var]], tcalib,
                         flow = var, varName = varName,
                         vinExists = vinExists, vinCalib = vinCalib, shiftIntang = shiftIntang)
    )
  })

  lapply(variables, function(var) {
    p_specCost[[var]]$setRecords(p_specCostData[[var]])
  })

  m$write(file.path(path, "input.gdx"), compress = TRUE)
}

#' Calculate sum of squared differences between results and historic values
#'
#' @param res numeric, result values
#' @param target numeric, historic values
#'
#' @returns numeric, sum of squared differences
#'
.sumSquare <- function(res, target = 0) {
  sum((res - target)^2)
}

#' Calculate log-Likelihood of historical values
#'
#' @param res numeric, result values
#' @param target numeric, historic values
#'
#' @returns numeric, negative of the log likelihood
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
#' @returns data frame with the outer objective computed from one calibration variable
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
    group_by(across(all_of(c("region", "loc", "typ", "inc")))) %>%
    summarise(value = .sumSquare(.data[["value"]], .data[["target"]]), .groups = "drop")
}


#' Read in brick results and compute the total outer objective function
#' by combining resuls from construction and renovation.
#'
#' @param m Gams transfer container to read brick results from
#' @param outerObjective data frame to write the outer objective to
#' @param p_calibTarget data frame with historic flows
#' @param tcalib numeric, calibration time steps
#' @param dims named character, dimensions of the flows
#' @param varName character, column name in \code{outerObjective} to write the result to.
#'   Should be either 'f' or 'fA'.
#' @param agg character, dimension(s) to aggregate the data over
#'
#' @returns data frame with the outer objective computed from all calibration variables
#'   stored in column \code{f} or \code{fA}.
#'   If applicable, joined with previously computed value for \code{f}.
#'
#' @importFrom dplyr %>% .data across all_of group_by mutate rename_with right_join select summarise
#'
.combineOuterObjective <- function(m, outerObjective, p_calibTarget, tcalib, dims,
                                   varName = "f", agg = NULL) {
  variables <- names(p_calibTarget)

  gamsVar <- .namedLapply(variables, function(var) {
    readSymbol(m, symbol = paste0("v_", var))
  })

  do.call(rbind, lapply(variables, function(var) {
    .computeOuterObjective(m, gamsVar[[var]], p_calibTarget[[var]], dims[[var]], tcalib, agg = agg) %>%
      mutate(flow = var)
  })) %>%
    group_by(across(all_of(c("region", "loc", "typ", "inc")))) %>%
    summarise(fNew = sum(.data[["value"]]), .groups = "drop") %>%
    rename_with(~ varName, .cols = "fNew") %>%
    right_join(outerObjective %>%
                 select(-any_of(varName)),
               by = c("region", "loc", "typ", "inc"))
}


#' Read the outer objective from a gams calibration run.
#'
#' Only applicable for optimization calibration.
#'
#' @param m Gams transfer container to read brick results from
#' @param outerObjective data frame to write the outer objective to
#' @param varName character, column name in \code{outerObjective} to write the result to.
#'   Should be either 'f' or 'fA'.
#'
#' @returns data frame with outer objective read from Gams results stored in column \code{f} or \code{fA}.
#'   If applicable, joined with previously computed value for \code{f}.
#'
#' @importFrom dplyr %>% rename_with right_join select
#'
.readOuterObjectiveOptim <- function(m, outerObjective, varName = "f") {

  readSymbol(m, symbol = "p_f") %>%
    rename_with(~ varName, .cols = "value") %>%
    right_join(outerObjective %>%
                 select(-any_of(varName)),
               by = c("region", "loc", "typ", "inc"))

}

#' Reject step sizes with unsuccesful Gams run
#'
#' If Gams was not successful, set the outer objective to infinity
#' so that it cannot be accepted by the step size adaptation.
#' Copy relevant files to trace the error (gdx, main.log and main.lst)
#'
#' @param path character, run path where all files, in particular Gams files, are stored
#' @param outerObjective data frame with data on outer objective
#' @param gamsSuccess data frame containing whether Gams was successful for each subset
#' @param iteration numeric, current calibration iteration
#' @param iterationStepSize numeric, current iteration of step size adaptation
#' @param varName character, column of outer objective to be modified
#' @param gdxName character, name of the gdx file to be copied.
#'
#' @returns data frame, outer objective with modifications in the column given by \code{varName}
#'
#' @importFrom dplyr %>% .data left_join mutate rename_with
#'
.rejectErrorRun <- function(path, outerObjective, gamsSuccess, iteration, iterationStepSize,
                            varName = "fA", gdxName = "calibrationA.gdx") {
  # Save gdx, main.log and main.lst if gams was not successful for all subsets
  if (isFALSE(all(gamsSuccess$success))) {
    lapply(c(gdxName, "main.log", "main.lst"), function(f) {
      fileSplit <- strsplit(f, ".", fixed = TRUE)[[1]]
      fileName <- paste0(fileSplit[1], "_", iteration, "_A", iterationStepSize, ".", fileSplit[2])
      file.copy(from = file.path(path, f), to = file.path(path, "failedRuns", fileName), overwrite = TRUE)
    })
  }

  # Reject this step size by setting the outer objective to infinity
  outerObjective %>%
    left_join(gamsSuccess, by = c("region", "loc", "typ", "inc")) %>%
    mutate(fNew = ifelse(.data$success, .data[[varName]], Inf), .keep = "unused") %>%
    select(-any_of(varName)) %>%
    rename_with(~ varName, .cols = "fNew")
}

#' Determine the so far minimum step size.
#' If the outerObjective is increasing again after a decrease,
#' set the step size to the value yielding the current minimum.
#'
#' @param totalStep data frame with combinations to perform the adjustment ot the step size on
#' @param stepSizeParams data frame with the parameters of the step size adjustment algorithm
#' @param outerObjective data frame containing the value of the outer objective function.
#'
#' @returns data frame with the updated step size where applicable and allrelevant step size parameters
#'
#' @importFrom dplyr %>% .data anti_join left_join mutate right_join select
#'
.findMinimumStepSize <- function(totalStep, stepSizeParams, outerObjective) {
  stepSizeParamsSelect <- stepSizeParams %>%
    right_join(totalStep, by = c("region", "loc", "typ", "inc")) %>%
    left_join(outerObjective, by = c("region", "loc", "typ", "inc")) %>%
    mutate(stepSize = ifelse(
      .data$fA > .data$minOuterObj + 1E-6 & .data$minStepSize != 0,
      .data$minStepSize,
      .data$stepSize
    )) %>%
    select(-any_of(c("f", "fA", "fPrev")))

  stepSizeParams %>%
    anti_join(stepSizeParamsSelect, by = c("region", "loc", "typ", "inc")) %>%
    rbind(stepSizeParamsSelect)
}

#' Check if the Armijo condition holds or if a local minimum of the outer objective has been identified.
#' Return only data combinations for which it does not hold.
#'
#' @param prevStep data frame with data combinations that did not satisfy the step size condition in the previous step
#' @param stepSizeParams data frame with step size and related parameters
#' @param outerObjective data frame containing the value of the outer objective function.
#'   Needs to contain the columns \code{f} and \code{fA}.
#' @param sensitivityArmijo numeric, parameter of the Armijo condition specifying how strict the condition is
#'
#' @returns data frame of combinations that satisfy neither the Armijo condition
#'   nor the local minimum criterion in the objective function
#'
#' @importFrom dplyr %>% .data filter left_join select
#'
.checkStepCondition <- function(prevStep, stepSizeParams, outerObjective, sensitivityArmijo) {
  prevStep %>%
    left_join(outerObjective, by = c("region", "loc", "typ", "inc")) %>%
    left_join(stepSizeParams, by = c("region", "loc", "typ", "inc")) %>%
    filter(.data$fA > (.data$f
                       + sensitivityArmijo * .data$stepSize * .data$phiDeriv),
           .data$fA <= .data$minOuterObj + 1E-6 | .data$minStepSize == 0) %>%
    select("region", "loc", "typ", "inc")
}

#' Update the optimization variable 'x' for selected combinations only
#'
#' @param totalStep data frame with combinations to perform the adjustment of 'x' on
#' @param optimVar data frame with the optimization variables
#' @param deviation data frame with the deviation from historic data and the adjustment parameter 'd'
#' @param stepSizeParams data frame with the parameters of the step size adaptatation procedure
#' @param dims character, dimensions of the optimization variable
#'
#' @returns data frame with the optimzation variable which was updated for the desired combinations
#'
#' @importFrom dplyr %>% anti_join right_join
#'
.updateXSelect <- function(totalStep, optimVar, deviation, stepSizeParams, dims) {
  optimVarSelect <- right_join(optimVar, totalStep, by = c("region", "loc", "typ", "inc"))

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
#' @param lastIteration logical, if this is the last iteration of the step size adaptation
#'
#' @returns data frame with the current step size and all related parameters.
#'   The step size and the minimum outer objective and minimum step size are updated for selected combinations only.
#'
#' @importFrom dplyr %>% .data anti_join mutate right_join
#'
.updateStepSize <- function(totalStep, stepSizeParams, outerObjective, stepReduction, lastIteration) {
  stepSizeParamsSelect <- stepSizeParams %>%
    right_join(totalStep, by = c("region", "loc", "typ", "inc")) %>%
    left_join(outerObjective, by = c("region", "loc", "typ", "inc")) %>%
    mutate(minStepSize = ifelse(.data$fA < .data$minOuterObj, .data$stepSize, .data$minStepSize),
           minOuterObj = pmin(.data$minOuterObj, .data$fA)) %>%
    # Don't decrease the step size in the last iteration
    mutate(stepSize = if (isTRUE(lastIteration)) .data[["stepSize"]] else .data[["stepSize"]] * stepReduction) %>%
    select(-any_of(c("f", "fA", "fPrev")))

  stepSizeParams %>%
    anti_join(stepSizeParamsSelect, by = c("region", "loc", "typ", "inc")) %>%
    rbind(stepSizeParamsSelect)
}

#' Handle the case that the step size adaptation condition is not satisfied after the predefined number of iterations.
#'
#' If for any combination the condition is still not satisfied:
#' Set the step size to zero and print a warning if no descent has been detected.
#' Use the last step size of the iteration otherwise.
#'
#' @param totalStep data frame with combinations for which the condition is not satisfied
#' @param stepSizeParams data frame with the parameters of the step size adaptation procedure
#'
#' @returns data frame with step size and related paramters, updated if applicable.
#'
#' @importFrom dplyr mutate right_join
#'
.adjustStepSizeAfterIteration <- function(totalStep, stepSizeParams) {
  message("The step size adaptation was not successful for at least one subset.")
  stepSizeParamsSelect <- right_join(stepSizeParams, totalStep,
                                     by = c("region", "loc", "typ", "inc"))
  if (nrow(stepSizeParamsSelect[stepSizeParamsSelect$minStepSize == 0, ]) > 0) {
    warning("At least one subset seems to have stalled, i.e. no descent has been detected. ",
            "Step size is set to zero.")
    stepSizeParamsSelect <- mutate(stepSizeParamsSelect, stepSize = 0)

    stepSizeParams <- stepSizeParams %>%
      anti_join(stepSizeParamsSelect, by = c("region", "loc", "typ", "inc")) %>%
      rbind(stepSizeParamsSelect)
  } else {
    message("For at least one subset: ",
            "The armijo condition is not satisfied in the predefined number of iterations of the step size algorithm",
            "and no minimum has been found.",
            "Since the outer objective has been decreasing throughout the iterations, the last step size is used.")
  }
  stepSizeParams
}

#' Compute the sum of the squares for a calibration target
#'
#' @param target data frame with calibration target data
#' @param zeroFlow logical, whether data should be filtered for the zero flow
#'
#' @returns data frame with the same dimensions as the outer objective, aggregated by computing the sum of the squares
#'
#' @importFrom dplyr across all_of group_by summarise
#'
.computeSumSqTarget <- function(target, zeroFlow = FALSE) {
  if (isTRUE(zeroFlow)) {
    for (column in intersect(c("bsr", "hsr"), colnames(target))) {
      target <- filter(target, .data[[column]] == "0")
    }
  }

  target %>%
    group_by(across(all_of(c("region", "loc", "typ", "inc")))) %>%
    summarise(target = .sumSquare(.data$target), .groups = "drop")
}

#' Check for the stopping criterion of the iteration
#'
#' Returns \code{TRUE} if and only if both the absolute and the relative stopping criterion
#' are satisfied for all subsets.
#'
#' @param outerObjective data frame containing the value of the outer objective function.
#' @param p_calibTarget list of data frames with historic flows
#' @param threshold list of numeric, absolute and relative threshold of the stopping criterion
#' @param zeroFlow logical, whether renovation flow data should be filtered for zero flows
#'
#' @returns logical, whether the stopping criterion is satisfied for all subsets
#'
#' @importFrom dplyr %>% .data across all_of group_by left_join mutate select summarise
#'
.checkStoppingCriterion <- function(outerObjective, p_calibTarget, threshold, zeroFlow = FALSE) {
  sumSqTarget <- do.call(rbind, lapply(names(p_calibTarget), function(var) {
    .computeSumSqTarget(p_calibTarget[[var]], zeroFlow && grepl("renovation", var)) %>%
      mutate(variable = var)
  })) %>%
    group_by(across(all_of(c("region", "loc", "typ", "inc")))) %>%
    summarise(target = sum(.data$target), .groups = "drop")

  outerObjective %>%
    select(-"fPrev") %>%
    left_join(sumSqTarget, by = c("region", "loc", "typ", "inc")) %>%
    mutate(absLevel = sqrt(.data$f),
           .keep = "unused") %>%
    mutate(absCriterion = .data$absLevel <= threshold[["abs"]],
           relCriterion = .data$absLevel <= threshold[["rel"]] * sqrt(.data$target),
           .keep = "none") %>%
    all()
}

#' Print message that the calibration converged.
#'
#' @param iteration numeric, number of current iteration
#' @param threshold list of numeric, absolute and relative threshold of the stopping criterion
#'
.printConvergenceMessage <- function(iteration, threshold) {
  message("The calibration converged after ", iteration, " iterations. ",
          "The absolute deviation is below ", threshold[["abs"]],
          ", the relative deviation is below ", threshold[["rel"]], ".")
}

#' Write the stock of the calibration result to a csv file
#'
#' @param m Gams container with the calibration output.gdx
#' @param path character, path to the calibration run
#'
#' @importFrom utils write.csv
#'
.writeStockRes <- function(m, path) {
  stockRes <- readSymbol(m, symbol = "v_stock")

  write.csv(stockRes, file.path(path, "stockCalibrationRes.csv"), row.names = FALSE)
}

#' Write the intangible costs to a .csv file
#'
#' @param file character, path to file to which results should be written
#' @param optimVar data frame with optimization variable, needs to contain column 'x'
#' @param xinit data frame with initial intangible specific costs
#' @param dims character, dimensions of the optimization variable
#' @param tcalib numeric, calibration time steps
#' @param flow character, specifies flow considered. Must be cosntruction or renovation
#' @param vinExists data frame with existing vintage and time step combinations
#' @param vinCalib data frame with vintage and time step combinations that exist during calibration
#'
#' @importFrom utils write.csv
#'
.writeCostIntang <- function(file, optimVar, xinit, dims, tcalib,
                             flow = c("construction", "renovation", "renovationBS", "renovationHS"),
                             vinExists = NULL, vinCalib = NULL) {
  flow <- match.arg(flow)

  specCostIntang <- .determineSpecCost(optimVar, xinit, dims, tcalib, flow = flow,
                                       vinExists = vinExists, vinCalib = vinCalib)

  write.csv(specCostIntang, file = file, row.names = FALSE)
}
