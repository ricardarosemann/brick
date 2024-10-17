*** models ---------------------------------------------------------------------

model fullSysLP "full system linear optimisation"
  /
  q_totObj
  q_Obj
  q_SysCost
  q_ConCost
  q_RenCost
  q_OpeCost
  q_DemCost
  q_stockBalNext
  q_stockBalPrev
  q_housingDemand
  q_buildingLifeTime
$ifthenE.shell (not(sameas("%ignoreShell%","TRUE")))
  q_buildingShellLifeTime
$endif.shell
  q_heatingSystemLifeTime
  q_SysHeteroPref
  q_zeroHeteroPrefCon
  q_zeroHeteroPrefRen
*  q_minDivConHS
*  q_minDivConBS
*  q_minDivRenBS
*  q_minDivRenHS
*  q_maxRenRate
  /
;

model fullSysNLP "full system linear optimisation"
  /
  q_totObj
  q_Obj
  q_SysCost
  q_ConCost
  q_RenCost
  q_OpeCost
  q_DemCost
  q_stockBalNext
  q_stockBalPrev
  q_housingDemand
  q_buildingLifeTime
$ifthenE.shell (not(sameas("%ignoreShell%","TRUE")))
  q_buildingShellLifeTime
$endif.shell
  q_heatingSystemLifeTime
  q_SysHeteroPref
  q_HeteroPrefCon
  q_HeteroPrefRen
*  q_maxRenRate
  /
;

$ifthen.matching "%RUNTYPE%" == "matching"

model matching "find stock and flows that best match reference sources"
  /
  q_matchingObj
  q_refDeviationTot
  q_refDeviationVar
  q_refDeviation
  q_stockBalNext
  q_stockBalPrev
  q_buildingLifeTime !! TODO: make this a matching target, not a hard constraint
*  q_dwelSizeStock
*  q_dwelSizeConstruction
*  q_dwelSize_Odyssee
  q_renRate_EuropeanCommissionRenovation
  q_heatingShare_Odyssee
  q_heatingShare_IDEES
  q_vinShare_EUBDB
  q_finiteHeatingShareCon
  q_finiteHeatingShareRen
*  q_flowVariation
*  q_flowVariationTot
  /
;
$endif.matching



*** prepare solving ------------------------------------------------------------

* filter subs
$ifthen.filtersubs %FILTERSUBS% == "TRUE"
subs(all_subs) = no;
subs("DEU","rural","SFH","all") = yes;
$endif.filtersubs


* solvers
option lp  = %solverLP%;
option nlp = %solverNLP%;
option qcp = %solverQCP%;

* define macro for solving in parallel mode

* TODO: Maybe I'll need to use a different alias in the macros
$macro solveParallel subs(all_subs) = no; \
fullSysNLP.SolveLink = 3; \
loop(all_subs, \
  subs(all_subs) = yes; \
  solve fullSysNLP minimizing v_totObj using nlp; \
  subs(all_subs) = no; \
  p_handle(all_subs) = fullSysNLP.handle; \
); \
repeat \
  loop(all_subs$handleCollect(p_handle(all_subs)), \
		p_runtime(all_subs) = fullSysNLP.resusd; \
    p_repyFullSysNLP(all_subs,'solvestat') = fullSysNLP.solvestat; \
    p_repyFullSysNLP(all_subs,'modelstat') = fullSysNLP.modelstat; \
    p_repyFullSysNLP(all_subs,'resusd')    = fullSysNLP.resusd; \
    p_repyFullSysNLP(all_subs,'objval')    = fullSysNLP.objval; \
    if(handleStatus(p_handle(all_subs)), \
      fullSysNLP.handle = p_handle(all_subs); \
      display$handleDelete(p_handle(all_subs)) 'trouble deleting handles' ; \
      p_handle(all_subs) = 0; \
    ); \
  ); \
  display$sleep(5) 'sleep some time'; \
until card(p_handle) = 0; \
subs(all_subs) = yes;

$ifThen.targetFunc "%TARGETFUNCTION%" == "minsquare"
$ifThen.calibTarget "%CALIBRATIONTYPE%" == "flows"
$macro func sum(state3$renTarAllowed("construction", state3), \
  power(p_constructionHist("area", state3, subs, tcalib) - v_construction.l("area", state3, subs, tcalib), 2)) \
  + sum((vin3, state3, stateFull3)$renAllowed(state3, stateFull3), \
  power(p_renovationHist("area", state3, stateFull3, vin3, subs, tcalib) - v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib), 2));

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func sum((vin3, state3), \
  power(p_stockHist("area", state3, vin3, subs, tcalib) - v_stock.l("area", state3, vin3, subs, tcalib), 2));

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stockszero"
$macro func sum((vin3, state3), \
  power(p_stockHist("area", state3, vin3, subs, tcalib) - v_stock.l("area", state3, vin3, subs, tcalib), 2)) \
  + sum((vin3, state3, stateFull3)$zeroFlow(state3, stateFull3), \
  power(p_renovationHist("area", state3, stateFull3, vin3, subs, tcalib) - v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib), 2));
$endIf.calibTarget

$elseIf.targetFunc "%TARGETFUNCTION%" == "maxlikely"
$ifThen.calibTarget "%CALIBRATIONTYPE%" == "flows"
$macro func - sum(state3$renTarAllowed("construction", state3), \
  p_constructionHist("area", state3, subs, tcalib) \
  * log(v_construction.l("area", state3, subs, tcalib) \
    / (sum(state4, \
      v_construction.l("area", state4, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  ) \
  - sum((vin3, state3, stateFull3)$renAllowed(state3, stateFull3), \
  p_renovationHist("area", state3, stateFull3, vin3, subs, tcalib) \
  * log(v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib) \
    / (sum((state4, stateFull4), \
      v_renovation.l("area", state4, stateFull4, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  );

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func - sum((vin3, state3), \
  p_stockHist("area", state3, vin3, subs, tcalib) \
  * log(v_stock.l("area", state3, vin3, subs, tcalib) \
    / (sum((state4), \
      v_stock.l("area", state4, vin3, subs, tcalib) \
    ) \
    + epsilonSmall) \
  + epsilonSmall)) \
  - sum((vin3, state3, stateFull3)$zeroFlow(state3, stateFull3), \
  p_renovationHist("area", state3, stateFull3, vin3, subs, tcalib) \
  * log(v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib) \
    / (sum((state4, stateFull4), \
      v_renovation.l("area", state4, stateFull4, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
);

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stockszero"
$macro func - sum((vin3, state3), \
  p_stockHist("area", state3, vin3, subs, tcalib) \
  * log(v_stock.l("area", state3, vin3, subs, tcalib) \
    / (sum((state4), \
      v_stock.l("area", state4, vin3, subs, tcalib) \
    ) \
    + epsilonSmall) \
  + epsilonSmall));
$endIf.calibTarget

$endIf.targetFunc

$macro extrapolateIntang - sum(tcalib, \
  p_specCostCalib(flow, stateAll, stateFull, vinAll, subs, tcalib)) / card(tcalib);




*** scenario / calibration run -------------------------------------------------

$ifthen.fullSys "%RUNTYPE%" == "scenario"

* measure stocks and flows in floor area
q("dwel") = no;
q("area") = yes;



* linear model
* $ifthen.calibration "%RUNTYPE%" == "calibration"
* if(iteration.val eq 1,
* $endif.calibration

$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
solve fullSysLP minimizing v_totObj using lp;
p_repyFullSysLP('solvestat') = fullSysLP.solvestat;
p_repyFullSysLP('modelstat') = fullSysLP.modelstat;
p_repyFullSysLP('resusd')    = fullSysLP.resusd;
p_repyFullSysLP('objval')    = fullSysLP.objval;
$endif.lp


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))or(sameas("%RUNTYPE%","calibration"))

$ifthen.parallel "%PARALLEL%" == "TRUE"

solveParallel

$else.parallel

solve fullSysNLP minimizing v_totObj using nlp;

p_repyFullSysNLP(subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLP(subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLP(subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLP(subs,'objval')    = fullSysNLP.objval;

$endif.parallel



$endif.nlp


$elseif.fullSys "%RUNTYPE%" == "calibrationSimple"

* measure stocks and flows in floor area
q("dwel") = no;
q("area") = yes;

* Write initial specific costs to variable xinit, initialise x as 0
p_xinitCon(state, subs, t) = p_specCostCon("intangible", state, subs, t);
p_xinitRen(ren, vin, subs, t)$renAllowed(ren) = p_specCostRen("intangible", ren, vin ,subs, t);
p_xSimp("construction", "none", "none", state, "none", subs, tcalib) = 0;
p_xSimp("renovation", state, stateFull, vin, subs, tcalib)$renAllowed(state, stateFull) = 0;

p_fPrev(subs, tcalib) = 0; !! Unused initialisation to avoid compilation error

loop(iteration,
$ifthen.parallel "%PARALLEL%" == "TRUE"

solveParallel

$else.parallel

solve fullSysNLP minimizing v_totObj using nlp;

$endif.parallel

p_repyFullSysNLPIter(iteration,all_subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLPIter(iteration,all_subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLPIter(iteration,all_subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLPIter(iteration,all_subs,'objval')    = fullSysNLP.objval;

p_f(subs, tcalib) = func

p_renovation("area", state, stateFull, vin, subs, ttot) = v_renovation.l("area", state, stateFull, vin, subs, ttot);
p_construction("area", state, subs, ttot) = v_construction.l("area", state, subs, ttot);
p_stock("area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);

*** check calibration deviation
p_calibDeviationCon(iteration,state,subs,tcalib)$(abs(p_constructionHist("area",state,subs,tcalib)) > eps) =
  v_construction.l("area",state,subs,tcalib)
  / p_constructionHist("area",state,subs,tcalib)
;
$ifThen.aggPrev "%CALIBRATIONRESOLUTION%" == "aggregate"
p_calibDeviationRenTest(iteration,state, stateFull,vin,subs,tcalib)$(abs(p_renovationHist("area",state, stateFull,vin,subs,tcalib)) > eps
                                                                 and renAllowed(state, stateFull)) =
  sum(state2$renAllowed(state2, stateFull), p_renovationHist("area",state2, stateFull,vin,subs,tcalib));
p_calibDeviationRen(iteration,state, stateFull,vin,subs,tcalib)$(abs(sum(state2$renAllowed(state2, stateFull), p_renovationHist("area",state2, stateFull,vin,subs,tcalib))) > eps
                                                                 and renAllowed(state, stateFull)) =
  sum(state2$renAllowed(state2, stateFull), v_renovation.l("area",state2, stateFull,vin,subs,tcalib))
  / sum(state2$renAllowed(state2, stateFull), p_renovationHist("area",state2, stateFull,vin,subs,tcalib))
;
execute_unload "test.gdx";
$elseIf.aggPrev "%CALIBRATIONRESOLUTION%" == "identicalRep"
p_calibDeviationRen(iteration,state(bs,hs),stateFull(bsr,hsr),vin,subs,tcalib)$(sameas(hs, hsr)
                                                              and (abs(sum(state2$renAllowed(state2, stateFull), p_renovationHist("area",state2, stateFull,vin,subs,tcalib))) > eps)
                                                              and renAllowed(state, stateFull)) =
  sum((bs2, hs2)$(renAllowed(bs2, hs2, bsr, hsr) and sameas(hs2, hsr)), v_renovation.l("area",bs2, hs2, bsr, hsr,vin,subs,tcalib))
  / sum((bs2, hs2)$(renAllowed(bs2, hs2, bsr, hsr) and sameas(hs2, hsr)), p_renovationHist("area",bs2, hs2, bsr, hsr,vin,subs,tcalib))
;
p_calibDeviationRen(iteration,state(bs,hs),stateFull(bsr,hsr),vin,subs,tcalib)$(not sameas(hs, hsr) and not sameas(hsr, "0")
                                                              and (abs(sum(state2$renAllowed(state2, stateFull), p_renovationHist("area",state2, stateFull,vin,subs,tcalib))) > eps)
                                                              and renAllowed(state, stateFull)) =
  sum((bs2, hs2)$(renAllowed(bs2, hs2, bsr, hsr) and not sameas(hs2, hsr) and not sameas(hsr, "0")), v_renovation.l("area",bs2, hs2, bsr, hsr,vin,subs,tcalib))
  / sum((bs2, hs2)$(renAllowed(bs2, hs2, bsr, hsr) and not sameas(hs2, hsr) and not sameas(hsr, "0")), p_renovationHist("area",bs2, hs2, bsr, hsr,vin,subs,tcalib))
;
p_calibDeviationRen(iteration,state(bs,hs),stateFull(bsr,hsr),vin,subs,tcalib)$(sameas("0", hsr)
                                                              and (abs(sum(state2$renAllowed(state2, stateFull), p_renovationHist("area",state2, stateFull,vin,subs,tcalib))) > eps)
                                                              and renAllowed(state, stateFull)) =
  sum((bs2, hs2)$(renAllowed(bs2, hs2, bsr, hsr) and sameas("0", hsr)), v_renovation.l("area",bs2, hs2, bsr, hsr,vin,subs,tcalib))
  / sum((bs2, hs2)$(renAllowed(bs2, hs2, bsr, hsr) and sameas("0", hsr)), p_renovationHist("area",bs2, hs2, bsr, hsr,vin,subs,tcalib))
;
$else.aggPrev
$ifThen.aggVin "%AGGREGATEVIN%" == "TRUE"
p_renovation("area", ren, "none", subs, tcalib) = sum(vin, v_renovation.l("area", ren, vin, subs, tcalib));
vinVar(vinAll) = no;
vinVar("none") = yes;
$endIf.aggVin
p_calibDeviationRen(iteration,ren(bs, hs, bsr, hsr),vinVar,subs,tcalib)$(abs(p_renovationHist("area",ren,vinVar,subs,tcalib))$(p_renovationHist("area",ren,vinVar,subs,tcalib) ne NA) > eps
                                                    and not sameas(hsr, "0"))
                                                    = p_renovation("area",ren,vinVar,subs,tcalib)
                                                      / p_renovationHist("area",ren,vinVar,subs,tcalib)
;
$endIf.aggPrev

*** Calculate the change in intangible costs: Sufficiently large deviation and data
p_dSimp("construction", "none", "none", state, "none", subs, tcalib)$(p_constructionHist("area",state,subs,tcalib) > eps
                                          and p_calibDeviationCon(iteration,state,subs,tcalib) > eps)
                                          = log(p_calibDeviationCon(iteration,state,subs,tcalib));
p_dSimp("renovation", ren(bs, hs, bsr, hsr), vinVar, subs, tcalib)$(p_renovationHist("area",ren,vinVar,subs,tcalib)$(p_renovationHist("area",ren,vinVar,subs,tcalib) ne NA) > eps
                                               and p_calibDeviationRen(iteration,ren,vinVar,subs,tcalib) > eps
                                               and not sameas(hsr, "0"))
                                          = log(p_calibDeviationRen(iteration,ren,vinVar,subs,tcalib));

*** Calculate the change in intangible costs: Deviation or data - but not both - is close to zero
p_dSimp("construction", "none", "none", state, "none", subs, tcalib)$(    (p_constructionHist("area",state,subs,tcalib) <= eps)
                                          xor (v_construction.l("area",state,subs,tcalib) <= eps))
  = sign(p_calibDeviationCon(iteration,state,subs,tcalib) - 1)
  * (
      0.5 * abs(p_specCostCon("intangible",state,subs,tcalib))
      + 0.1 * p_specCostCon("tangible",state,subs,tcalib)$(abs(p_specCostCon("intangible",state,subs,tcalib)) <= eps)
    );
$ifThen.aggVin "%AGGREGATEVIN%" == "TRUE"
p_dSimp("renovation", ren(bs, hs, bsr, hsr), vinVar, subs, tcalib)$((    (p_renovationHist("area",ren,vinVar,subs,tcalib)$(p_renovationHist("area",ren,vinVar,subs,tcalib) ne NA) <= eps)
                                            xor (sum(vin, v_renovation.l("area",ren,vin,subs,tcalib)) / card(vin) <= eps))
                                            and not sameas(hsr, "0"))
  = sign(p_calibDeviationRen(iteration,ren,vinVar,subs,tcalib) - 1)
  * (
      0.5 * abs(sum(vin, p_specCostRen("intangible",ren,vin,subs,tcalib)) / card(vin);
      + 0.1 * (sum(vin, p_specCostRen("tangible",ren,vin,subs,tcalib)) / card(vin))$(abs(sum(vin, p_specCostRen("intangible",ren,vin,subs,tcalib)) / card(vin)) <= eps)
    )
;
$else.aggVin
p_dSimp("renovation", ren(bs, hs, bsr, hsr), vin, subs, tcalib)$((    (p_renovationHist("area",ren,vin,subs,tcalib)$(p_renovationHist("area",ren,vin,subs,tcalib) ne NA) <= eps)
                                            xor (v_renovation.l("area",ren,vin,subs,tcalib) <= eps))
                                            and not sameas(hsr, "0"))
  = sign(p_calibDeviationRen(iteration,ren,vin,subs,tcalib) - 1)
  * (
      0.5 * abs(p_specCostRen("intangible",ren,vin,subs,tcalib))
      + 0.1 * p_specCostRen("tangible",ren,vin,subs,tcalib)$(abs(p_specCostRen("intangible",ren,vin,subs,tcalib)) <= eps)
    )
;
$endIf.aggVin

*** If both the deviation and the data are small: Set change in intangible costs to zero (Possibly not necessary?)
* p_dSimp("construction", "none", "none", state, "none", subs, tcalib)$(    (p_constructionHist("area",state,subs,tcalib) <= eps)
*                                           and (v_construction.l("area",state,subs,tcalib) <= eps)) = 0;
* p_dSimp("renovation", ren, vinVar, subs, tcalib)$(    (p_renovationHist("area",ren,vinVar,subs,tcalib) <= eps)
*                                             and (v_renovation.l("area",ren,vin,subs,tcalib) <= eps)) = 0;

p_delta(subs, tcalib) = sum(simpleCalibVars(flow, renAll, vinAll),
  p_dSimp(flow, renAll, vinAll, subs, tcalib) * p_dSimp(flow, renAll, vinAll, subs, tcalib)
);

*** Set the step size
$ifThen.stepSize "%CALIBRATIONSTEP%" == "priceSensHS"
loop(tcalib, p_alpha(subs, tcalib) = sum(flow, 1 /priceSensHS(flow, subs);) / card(flow););
$elseIf.stepSize "%CALIBRATIONSTEP%" == "adaptive"
loop(tcalib,
  p_alpha(subs, tcalib)$(iteration.val > 1 and p_delta(subs, tcalib) > 0.001)
    = max(p_alphaL(subs),
      (p_fPrev(subs, tcalib) - p_f(subs, tcalib))
        / p_delta(subs, tcalib));
  p_alpha(subs, tcalib)$(iteration.val = 1 or p_delta(subs, tcalib) le 0.001) = p_alphaL(subs);
);
$else.stepSize
loop(tcalib, p_alpha(subs, tcalib) = p_alphaL(subs););
$endIf.stepSize


$ifThen.stepSize "%CALIBRATIONSTEP%" == "adaptive"

*** Compute the derivative of the step functional
p_phiDeriv(subs, tcalib) = sum(simpleCalibVars(flow, renAll, vinAll),
    - p_dSimp(flow, renAll, vinAll, subs, tcalib) * p_dSimp(flow, renAll, vinAll, subs, tcalib));

*** Test the Armijo-condition for a very small step size and follow a different heuristic if this is not satisfied
p_xMin(flow, renAll, vinAll, subs, tcalib)$simpleCalibVars(flow, renAll, vinAll)
       = p_xSimp(flow, renAll, vinAll, subs, tcalib) + 1/1000 * p_alpha(subs, tcalib) * p_dSimp(flow, renAll, vinAll, subs, tcalib);

v_stock.l("area", state, vin, subs, ttot) = 0;
v_construction.l("area", state, subs, ttot) = 0;
v_renovation.l("area", state, stateFull, vin, subs, ttot) = 0;

p_specCostConFut(state, subs) = sum(tcalib, p_xMin("construction", "none", "none", state, "none", subs, tcalib) / card(tcalib));
p_specCostRenFut(ren, vinVar, subs) = sum(tcalib, p_xMin("renovation", ren, vinVar, subs, tcalib) / card(tcalib));

*TODO: Handle time dimension here
p_specCostCon("intangible", state, subs, t)$tcalib(t) = p_xinitCon(state, subs, t) + p_xMin("construction", "none", "none", state, "none", subs, t);
p_specCostCon("intangible", state, subs, t)$(not tcalib(t)) = p_xinitCon(state, subs, t) + p_specCostConFut(state, subs);

loop(vinVar,
  p_specCostRen("intangible", ren, vin, subs, t)$(tcalib(t) and sameas(vin, vinVar)) = p_xinitRen(ren, vin, subs, t) + p_xMin("renovation", ren, vinVar, subs, t);
  p_specCostRen("intangible", ren, vin, subs, t)$(not tcalib(t) and sameas(vin, vinVar)) = p_xinitRen(ren, vin, subs, t) + p_specCostRenFut(ren, vinVar, subs);

  p_specCostRen("intangible", ren, vin, subs, t)$(tcalib(t) and sameas(vinVar, "none")) = p_xinitRen(ren, vin, subs, t) + p_xMin("renovation", ren, vinVar, subs, t);
  p_specCostRen("intangible", ren, vin, subs, t)$(not tcalib(t) and sameas(vinVar, "none")) = p_xinitRen(ren, vin, subs, t) + p_specCostRenFut(ren, vinVar, subs);
);

solveParallel

p_fMin(subs, tcalib) = func

p_fArmijoRHMin(subs, tcalib) = p_f(subs, tcalib) + p_sigma * 1/1000 * p_alpha(subs, tcalib) * p_phiDeriv(subs, tcalib);

*** Quick fix: Long term alpha should not depend on the flow (maybe?)
armijoStep(subs, tcalib)$(p_fMin(subs, tcalib) le p_f(subs, tcalib) + p_sigma * 1/1000 * p_alpha(subs, tcalib) * p_phiDeriv(subs, tcalib)) = yes;
heuristicStep(subs, tcalib)$(not armijoStep(subs, tcalib)) = yes;

armijoStepIter(iteration, subs, tcalib)$armijoStep(subs, tcalib) = yes;
heuristicStepIter(iteration, subs, tcalib)$heuristicStep(subs, tcalib) = yes;

*** Solve the model only for the subsets which do not satisfy the Armijo condition yet
loop(iterA,

  p_xASimp(flow, stateAll, stateFull, vinAll, subs, tcalib)$(simpleCalibVars(flow, stateAll, stateFull, vinAll) and totalStep(subs, tcalib))
            = p_xSimp(flow, stateAll, stateFull, vinAll, subs, tcalib) + p_alpha(subs, tcalib) * p_dSimp(flow, stateAll, stateFull, vinAll, subs, tcalib);

  v_stock.l("area", state, vin, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vin, subs, ttot) = 0;

  p_specCostConFut(state, subs) = sum(tcalib2, p_xASimp("construction", "none", "none", state, "none", subs, tcalib2)) / card(tcalib2);
  p_specCostRenFut(ren, vinVar, subs) = sum(tcalib2, p_xASimp("renovation", ren, vinVar, subs, tcalib2)) / card(tcalib2);

  p_specCostCon("intangible", state, subs, tcalib) = p_xinitCon(state, subs, tcalib) + p_xASimp("construction", "none", "none", state, "none", subs, tcalib);
  p_specCostCon("intangible", state, subs, t)$(not tcalib(t)) = p_xinitCon(state, subs, t) + p_specCostConFut(state, subs);

  loop(vinVar,
    p_specCostRen("intangible", ren, vin, subs, t)$(tcalib(t) and sameas(vin, vinVar)) = p_xinitRen(ren, vin, subs, t) + p_xASimp("renovation", ren, vinVar, subs, t);
    p_specCostRen("intangible", ren, vin, subs, t)$(not tcalib(t) and sameas(vin, vinVar)) = p_xinitRen(ren, vin, subs, t) + p_specCostRenFut(ren, vinVar, subs);

    p_specCostRen("intangible", ren, vin, subs, t)$(tcalib(t) and sameas(vinVar, "none")) = p_xinitRen(ren, vin, subs, t) + p_xASimp("renovation", ren, vinVar, subs, t);
    p_specCostRen("intangible", ren, vin, subs, t)$(not tcalib(t) and sameas(vinVar, "none")) = p_xinitRen(ren, vin, subs, t) + p_specCostRenFut(ren, vinVar, subs);
  );

  solveParallel

  p_fA(subs, tcalib) = func

*** Stopping criterion in all dimensions
  loop((subs, tcalib)$armijoStep(subs, tcalib),
    if (p_fA(subs, tcalib) le p_f(subs, tcalib) + p_sigma * p_alpha(subs, tcalib) * p_phiDeriv(subs, tcalib),
      armijoStep(subs, tcalib) = no;
      p_iterA(subs, tcalib) = iterA.val;
      );
  );
  loop((subs, tcalib)$heuristicStep(subs, tcalib),
    if (p_fA(subs, tcalib) le p_f(subs, tcalib),
      heuristicStep(subs, tcalib) = no;
      p_iterA(subs, tcalib) = iterA.val;
    );
  );
  totalStep(subs, tcalib)$(not heuristicStep(subs, tcalib) and not armijoStep(subs, tcalib)) = no;
  subs(all_subs) = no;
  loop(tcalib,
    subs(all_subs)$(totalStep(all_subs, tcalib)) = yes;
  );
  if(card(subs) = 0,
    p_alphaIterA(iterA, subs, tcalib) = p_alpha(subs, tcalib);
    break;
  );
*** Update alpha
  p_alpha(subs, tcalib)$(totalStep(subs, tcalib)) = p_alpha(subs, tcalib) * p_beta;
  p_alphaIterA(iterA, subs, tcalib)$(totalStep(subs, tcalib)) = p_alpha(subs, tcalib);

  p_fAIterA(iterA, subs, tcalib) = p_fA(subs, tcalib);
  p_fArmijoRHIterA(iterA, subs, tcalib) = p_f(subs, tcalib) + p_sigma * p_alpha(subs, tcalib) * p_phiDeriv(subs, tcalib);
);
subs(all_subs) = yes;
totalStep(subs, tcalib) = yes;

p_xSimp(flow, renAll, vinAll, subs, tcalib)$simpleCalibVars(flow, renAll, vinAll) = p_xASimp(flow, renAll, vinAll, subs, tcalib);
* p_f(subs) = p_fA(subs);
p_fPrev(subs, tcalib) = p_f(subs, tcalib);

$else.stepSize
*** Update p_x
p_xSimp(flow, stateAll, stateFull, vinAll, subs, tcalib)$simpleCalibVars(flow, stateAll, stateFull, vinAll)
           = p_xSimp(flow, stateAll, stateFull, vinAll, subs, tcalib) + p_alpha(subs, tcalib) * p_dSimp(flow, stateAll, stateFull, vinAll, subs, tcalib);

$endIf.stepSize

p_specCostConFut(state, subs) = sum(tcalib, p_xSimp("construction", "none", "none", state, "none", subs, tcalib) / card(tcalib));
p_specCostRenFut(ren, vinVar, subs) = sum(tcalib, p_xSimp("renovation", ren, vinVar, subs, tcalib) / card(tcalib));

* finite deviation
p_specCostCon("intangible",state,subs,t)$(    p_constructionHist("area",state,subs,t) > eps
                                          and p_calibDeviationCon(iteration,state,subs,t) > eps
                                          and tcalib(t)) =
  p_xinitCon(state, subs, t)
  +
  p_xSimp("construction", "none", "none", state, "none", subs, t)
;

p_specCostCon("intangible",state,subs,t)$(    p_constructionHist("area",state,subs,t) > eps
                                          and p_calibDeviationCon(iteration,state,subs,t) > eps
                                          and not tcalib(t)) =
  p_xinitCon(state, subs, t)
  +
  p_specCostConFut(state, subs)
;

loop(vinVar,
  p_specCostRen("intangible",ren,vin,subs,t)$(    p_renovationHist("area",ren,vinVar,subs,t) > eps
                                            and p_calibDeviationRen(iteration,ren,vinVar,subs,t) > eps
                                            and tcalib(t)
                                            and sameas(vin, vinVar)) =
    p_xinitRen(ren, vin, subs, t)
    +
    p_xSimp("renovation", ren, vinVar, subs, t)
  ;

  p_specCostRen("intangible",ren,vin,subs,t)$(    p_renovationHist("area",ren,vinVar,subs,t) > eps
                                            and p_calibDeviationRen(iteration,ren,vinVar,subs,t) > eps
                                            and not tcalib(t)
                                            and sameas(vin, vinVar)) =
    p_xinitRen(ren, vin, subs, t)
    +
    p_specCostRenFut(ren, vinVar, subs)
  ;

  p_specCostRen("intangible",ren,vin,subs,t)$(    p_renovationHist("area",ren,vinVar,subs,t) > eps
                                            and p_calibDeviationRen(iteration,ren,vinVar,subs,t) > eps
                                            and tcalib(t)
                                            and sameas(vinVar, "none")) =
    p_xinitRen(ren, vin, subs, t)
    +
    p_xSimp("renovation", ren, vinVar, subs, t)
  ;

  p_specCostRen("intangible",ren,vin,subs,t)$(    p_renovationHist("area",ren,vinVar,subs,t) > eps
                                            and p_calibDeviationRen(iteration,ren,vinVar,subs,t) > eps
                                            and not tcalib(t)
                                            and sameas(vinVar, "none")) =
    p_xinitRen(ren, vin, subs, t)
    +
    p_specCostRenFut(ren, vinVar, subs)
;
);

* might need to adapt this for tcalib of more than one time step
p_specCostConFutZero(state, subs) = sum(tcalib, p_dSimp("construction", "none", "none", state, "none", subs, tcalib) / card(tcalib));
p_specCostRenFutZero(ren, vinVar, subs) = sum(tcalib, p_dSimp("renovation", ren, vinVar, subs, tcalib) / card(tcalib));

* zero targets or zero actual value-
p_specCostCon("intangible",state,subs,t)$((    (p_constructionHist("area",state,subs,t) <= eps)
                                          xor (v_construction.l("area",state,subs,t) <= eps))
                                          and tcalib(t))
  =
  p_specCostCon("intangible",state,subs,t)
  + p_dSimp("construction", "none", "none", state, "none", subs, t)
;

p_specCostCon("intangible",state,subs,t)$((    (p_constructionHist("area",state,subs,t) <= eps)
                                          xor (v_construction.l("area",state,subs,t) <= eps))
                                          and not tcalib(t))
  =
  p_specCostCon("intangible",state,subs,t)
  + p_specCostConFutZero(state, subs)
;

loop(vinVar,
  p_specCostRen("intangible",ren,vin,subs,t)$((    (p_renovationHist("area",ren,vinVar,subs,t) <= eps)
                                            xor (v_renovation.l("area",ren,vin,subs,t) <= eps))
                                            and tcalib(t)
                                            and sameas(vin, vinVar))
    =
    p_specCostRen("intangible",ren,vin,subs,t)
    + p_dSimp("renovation", ren, vinVar, subs, t)
  ;

  p_specCostRen("intangible",ren,vin,subs,t)$((    (p_renovationHist("area",ren,vinVar,subs,t) <= eps)
                                            xor (v_renovation.l("area",ren,vin,subs,t) <= eps))
                                            and not tcalib(t)
                                            and sameas(vin, vinVar))
    =
    p_specCostRen("intangible",ren,vin,subs,t)
    + p_specCostRenFutZero(ren, vinVar, subs)
  ;

  p_specCostRen("intangible",ren,vin,subs,t)$((    (p_renovationHist("area",ren,vinVar,subs,t) <= eps)
                                            xor (v_renovation.l("area",ren,vin,subs,t) <= eps))
                                            and tcalib(t)
                                            and sameas(vinVar, "none"))
    =
    p_specCostRen("intangible",ren,vin,subs,t)
    + p_dSimp("renovation", ren, vinVar, subs, t)
  ;

  p_specCostRen("intangible",ren,vin,subs,t)$((    (p_renovationHist("area",ren,vinVar,subs,t) <= eps)
                                            xor (v_renovation.l("area",ren,vin,subs,t) <= eps))
                                            and not tcalib(t)
                                            and sameas(vinVar, "none"))
    =
    p_specCostRen("intangible",ren,vin,subs,t)
    + p_specCostRenFutZero(ren, vinVar, subs)
  ;
);

*** Save results of current iteration
execute_unload "calibration.gdx";
!! retain gdxes of intermediate iterations by copying them using shell
!! commands
put_utility "shell" / "cp calibration.gdx calibration_" iteration.val:0:0 ".gdx";



*** end iteration loop
);


$elseif.fullSys "%RUNTYPE%" == "calibration"
*********************************************************************************
*** Preparation of the calibration
*********************************************************************************

* measure stocks and flows in floor area
q("dwel") = no;
q("area") = yes;

$ifThen.stepSize "%CALIBRATIONSTEP%" == "priceSensHS"
p_alpha(subs, tcalib) = sum(flow, 1 / priceSensHS(flow, subs);) / card(flow);
$else.stepSize
p_alpha(subs, tcalib) = p_alphaL(subs);
$endIf.stepSize
p_fPrev(subs, tcalib) = 0; !! unused initialization to avoid compilation error

* p_specCostCon("intangible", bs, hs, subs, t) = p_x("construction", bs, hs, "2000-2010", subs);
* p_specCostRen("intangible", state, stateFull, vinCalib, subs, t) = p_x("renovation", stateFull, vinCalib, subs);
* v_stock.l(qty,state,vinCalib,subs,t) = p_stockHist(qty,state,vinCalib,subs,t);

solveParallel

*** Compute the functional value
p_f(subs, tcalib) = func
p_f0(subs, tcalib) = p_f(subs, tcalib);

*** Store renovation and construction values
p_construction("area", state, subs, t) = v_construction.l("area", state, subs, t);
p_renovation("area", state, stateFull, vinCalib, subs, t) = v_renovation.l("area", state, stateFull, vinCalib, subs, t);
p_stock("area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);

*** Save the model statistics of the previous iteration. This means that the iteration counter is off by one in some sense.
p_repyFullSysNLPIter("0",all_subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLPIter("0",all_subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLPIter("0",all_subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLPIter("0",all_subs,'objval')    = fullSysNLP.objval;


************************************************************************************
*** Calibration of price sensitivity
************************************************************************************
$ifThen.calPrice "%CALIBRATEPRICESENS%" == "NORMAL"
*** TODO: Only makes sense with one-dimensional tcalib

*** Compute the gradient
p_xPS(flow, subs) = priceSensHS(flow, subs);

loop(flow2, 
  p_xDiffPS(flow, subs)$(sameAs(flow, flow2)) = p_xPS(flow, subs) + p_diff;
  p_xDiffPS(flow, subs)$(not sameAs(flow, flow2)) = p_xPS(flow, subs);
  priceSensHS(flow, subs) = p_xDiffPS(flow, subs);

  solveParallel

  p_fDiffPS(flow2, subs) = func
);

p_rPS(flow, subs) = (p_fDiffPS(flow, subs) - p_f(subs)) / p_diff;
p_dPS(flow, subs) = - p_rPS(flow, subs);
p_deltaPS(subs) = sum(flow,
  -p_rPS(flow, subs) * p_dPS(flow, subs));

execute_unload "calibrationPS_0.gdx";

loop(iteration,

*** Armijo backtracking method
$ifThen.stepSize "%CALIBRATIONSTEP%" == "adaptive"
p_alpha(subs, tcalib)$(iteration.val > 1 and p_deltaPS(subs) > 0.001) = min(abs(0.5 * p_f(subs, tcalib)),
  max(p_alphaL(subs, tcalib),
    (p_fPrev(subs, tcalib) - p_f(subs, tcalib))
      / p_deltaPS(subs))
  );
p_alpha(subs, tcalib)$(p_deltaPS(subs) le 0.001) = p_alphaL(subs, tcalib);

loop(iterA,
*** Solve the model only for the subsets which do not satisfy the Armijo condition yet
  p_xAPS(flow, subs) = sum(tcalib, p_xPS(flow, subs) + p_alpha(subs, tcalib) * p_dPS(flow, subs)) / card(tcalib);

  v_stock.l("area", state, vinCalib, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

  priceSensHS(flow, subs) = p_xAPS(flow, subs);

  solveParallel

  p_fA(subs, tcalib) = func

  loop(tcalib,
    p_phiDeriv(subs, tcalib) = sum(flow,
      p_rPS(flow, subs) * p_dPS(flow, subs));
  );

*** Stopping criterion in all dimensions
  loop((flow, all_subs, tcalib),
    if (p_fA(all_subs, tcalib) le p_f(all_subs, tcalib) + p_sigma * p_alpha(all_subs, tcalib) * p_phiDeriv(all_subs, tcalib),
      subs(all_subs) = no;
      p_iterA(all_subs) = iterA.val;
      );
    p_fAIterA(iterA, all_subs, tcalib) = p_fA(all_subs, tcalib);
    p_fArmijoRHIterA(iterA, all_subs, tcalib) = p_f(all_subs, tcalib) + p_sigma * p_alpha(all_subs, tcalib) * p_phiDeriv(all_subs, tcalib);
  );
  if(card(subs) = 0,
    p_alphaIterA(iterA, all_subs, tcalib) = p_alpha(all_subs, tcalib);
    break;
  );
*** Update alpha
  p_alpha(subs, tcalib) = p_alpha(subs, tcalib) * p_beta;
  p_alphaIterA(iterA, all_subs, tcalib) = p_alpha(all_subs, tcalib);
);
subs(all_subs) = yes;

p_xPS(flow, subs) = p_xAPS(flow, subs);
p_fPrev(subs, tcalib) = p_f(subs, tcalib);
p_f(subs, tcalib) = p_fA(subs, tcalib);
$else.stepSize
*** Update p_xPS and recompute p_f
p_xPS(flow, subs) = sum(tcalib, p_xPS(flow, subs) + p_alpha(subs, tcalib) * p_dPS(flow, subs)) / card(tcalib);

v_stock.l("area", state, vinCalib, subs, ttot) = 0;
v_construction.l("area", state, subs, ttot) = 0;
v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

priceSensHS(flow, subs) = p_xPS(flow, subs);

solveParallel

*** Update p_f
p_fPrev(subs, tcalib) = p_f(subs, tcalib);
p_f(subs, tcalib) = func
$endIf.stepSize

p_renovation("area", state, stateFull, vinCalib, subs, ttot) = v_renovation.l("area", state, stateFull, vinCalib, subs, ttot);
p_construction("area", state, subs, ttot) = v_construction.l("area", state, subs, ttot);
p_stock("area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);

*** Save the model statistics of the previous iteration. This means that the iteration counter is off by one in some sense.
p_repyFullSysNLPIter(iteration,all_subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLPIter(iteration,all_subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLPIter(iteration,all_subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLPIter(iteration,all_subs,'objval')    = fullSysNLP.objval;

*** Compute the gradient
loop(flow2, 
  p_xDiffPS(flow, subs)$(sameAs(flow, flow2)) = p_xPS(flow, subs) + p_diff;
  p_xDiffPS(flow, subs)$(not sameAs(flow, flow2)) = p_xPS(flow, subs);
  priceSensHS(flow, subs) = p_xDiffPS(flow, subs);

  solveParallel

  p_fDiffPS(flow2, subs, tcalib) = func
);

p_rPS(flow, subs) = sum(tcalib, (p_fDiffPS(flow, subs) - p_f(subs)) / p_diff);
p_dPS(flow, subs) = - p_rPS(flow, subs);
p_deltaPS(subs) = sum(flow,
  -p_rPS(flow, subs) * p_dPS(flow, subs));

*** Save results of current iteration
execute_unload "calibrationPS.gdx";
!! retain gdxes of intermediate iterations by copying them using shell
!! commands
put_utility "shell" / "cp calibrationPS.gdx calibrationPS_" iteration.val:0:0 ".gdx";

*** end iteration loop
);

$endIf.calPrice

**************************************************************************************
*** Calibration of intangible costs
**************************************************************************************

*** TODO: Still need to check the proper time dimension!
*** TODO: Fix sets: What do I mean by subs? Where do I need to consider the vintage?
*** TODO: Fix: For construction I need to vary bs and hs, for renovation, costs primarily depend on bsr and hsr! (Although for more precision, reflecting both makes sens)
*** TODO: IMPORTANT! The implementation below relies on having constant intangible renovation costs for bs and hs, and assumes that the calibration is solely carried out for 2010!!!
*** Done (Mostly): Figure out whether we want to calibrate flows or stocks; if they are stocks: Also need to treat p_specCostRen in a similar way! (Then c(p_specCostCon, p_specCostRen) serve the function of x)
p_xinitCon(state, subs, tcalib)$renTarAllowed("construction", state) = p_specCostCon("intangible", state, subs, tcalib);
p_xinitRen(state, stateFull, vinCalib, subs, tcalib)$renTarAllowed("renovation", stateFull) = p_specCostRen("intangible", state, stateFull, vinCalib, subs, tcalib);
p_x(flow, renType, bsr, hsr, vinCalib, subs, tcalib)$gradientVars(flow, renType, bsr, hsr, vinCalib, tcalib) = 0;
$ifThen.stepSize "%CALIBRATIONSTEP%" == "priceSensHS"
p_alpha(subs, tcalib) = sum(flow, 1 / priceSensHS(flow, subs)) / card(flow);
$else.stepSize
p_alpha(subs, tcalib) = p_alphaL(subs);
$endIf.stepSize
p_fPrev(subs, tcalib) = 0; !! unused initialization to avoid compilation error

p_f0(subs, tcalib) = p_f(subs, tcalib);

*** Compute the gradient
loop(gradientVars(flow2, renType2, bsr3, hsr3, vin2, tcalib2),
  p_xDiff(flow, renType, bsr, hsr, vinCalib, subs, tcalib)$(gradientVars(flow, renType, bsr, hsr, vinCalib, tcalib)
                                                            and (not sameas(flow, flow2) or not sameas(renType, renType2)
                                                                 or not sameas(bsr, bsr3) or not sameas(hsr, hsr3)
                                                                 or not sameas(vinCalib, vin2) or not sameas(tcalib, tcalib2)))
                                                          = p_x(flow, renType, bsr, hsr, vinCalib, subs, tcalib);
  p_xDiff(flow, renType, bsr, hsr, vinCalib, subs, tcalib)$(gradientVars(flow, renType, bsr, hsr, vinCalib, tcalib)
                                                            and (sameas(flow, flow2) and sameas(renType, renType2)
                                                                 and sameas(bsr, bsr3) and sameas(hsr, hsr3)
                                                                 and sameas(vinCalib, vin2) and sameas(tcalib, tcalib2)))
                                                          = p_x(flow, renType, bsr, hsr, vinCalib, subs, tcalib) + p_diff;
  p_specCostCalib("construction", "none", "none", state, "none", subs, tcalib)$renTarAllowed("construction", state) = p_xDiff("construction", "newSys", state, "none", subs, tcalib);
  loop(renAllowed(bs, hs, bsr, hsr),
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$sameas(hs, hsr)
                                                                           = p_xDiff("renovation", "identRepl", bsr, hsr, vinCalib, subs, tcalib);
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$(not sameas(hs, hsr) and not sameas(hsr, "0"))
                                                                           = p_xDiff("renovation", "newSys", bsr, hsr, vinCalib, subs, tcalib);
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$sameas(hsr, "0") = p_xDiff("renovation", "0", bsr, hsr, vinCalib, subs, tcalib);
  );

  p_specCostCalib(flow, stateAll, stateFull, vinAll, subs, t)$(not tcalib(t) and calibStates(flow, stateAll, stateFull, vinAll))
                                                             = extrapolateIntang

  p_specCostCon("intangible", state, subs, t)$renTarAllowed("construction", state)
                                                  = p_xinitCon(state, subs, t)
                                                  + p_specCostCalib("construction", "none", "none", state, "none", subs, t);
  p_specCostRen("intangible", state, stateFull, vinCalib, subs, t)$renAllowed(state, stateFull)
                                                  = p_xinitRen(state, stateFull, vinCalib, subs, t)
                                                  + p_specCostCalib("renovation", state, stateFull, vinCalib, subs, t);

  v_stock.l("area", state, vinCalib, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

  solveParallel

  p_fDiff(flow2, renType2, bsr3, hsr3, vin2, subs, tcalib) = func
);

p_r(flow, renType, stateFull, vinCalib, subs, tcalib)$gradientVars(flow, renType, stateFull, vinCalib, tcalib) = (p_fDiff(flow, renType, stateFull, vinCalib, subs, tcalib) - p_f(subs, tcalib)) / p_diff;
p_d(flow, renType, stateFull, vinCalib, subs, tcalib)$gradientVars(flow, renType, stateFull, vinCalib, tcalib) = - p_r(flow, renType, stateFull, vinCalib, subs, tcalib);
p_delta(subs, tcalib) = sum((flow, renType, stateFull, vinCalib)$gradientVars(flow, renType, stateFull, vinCalib, tcalib),
  -p_r(flow, renType, stateFull, vinCalib, subs, tcalib) * p_d(flow, renType, stateFull, vinCalib, subs, tcalib));

execute_unload "calibration_0.gdx";

loop(iteration,

*** TODO: Include proper stopping criterion! Handle multi-dimensionality

$ifThen.stepSize "%CALIBRATIONSTEP%" == "adaptive"
*** Armijo backtracking method
p_alpha(subs, tcalib)$(iteration.val > 1 and p_delta(subs, tcalib) > 0.001) 
  = max(p_alphaL(subs),
    (p_fPrev(subs, tcalib) - p_f(subs, tcalib))
      / p_delta(subs, tcalib));
p_alpha(subs, tcalib)$(p_delta(subs, tcalib) le 0.001) = p_alphaL(subs);

loop(iterA,
*** Solve the model only for the subsets which do not satisfy the Armijo condition yet
  p_xA(flow, renType, bsr, hsr, vinCalib, subs, tcalib)$gradientVars(flow, renType, bsr, hsr, vinCalib, tcalib)
                                  = p_x(flow, renType, bsr, hsr, vinCalib, subs, tcalib) + p_alpha(subs, tcalib) * p_d(flow, renType, bsr, hsr, vinCalib, subs, tcalib);
  p_specCostCalib("construction", "none", "none", state, "none", subs, tcalib)$renTarAllowed("construction", state) = p_xA("construction", "newSys", state, "none", subs, tcalib);
  loop(renAllowed(bs, hs, bsr, hsr),
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$sameas(hs, hsr)
                                                                           = p_xA("renovation", "identRepl", bsr, hsr, vinCalib, subs, tcalib);
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$(not sameas(hs, hsr) and not sameas(hsr, "0"))
                                                                           = p_xA("renovation", "newSys", bsr, hsr, vinCalib, subs, tcalib);
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$sameas(hsr, "0") = p_xA("renovation", "0", bsr, hsr, vinCalib, subs, tcalib);
  );

  p_specCostCalib(flow, stateAll, stateFull, vinAll, subs, t)$(not tcalib(t) and calibStates(flow, stateAll, stateFull, vinAll))
                                                             = extrapolateIntang

  p_specCostCon("intangible", state, subs, t)$renTarAllowed("construction", state)
                                                  = p_xinitCon(state, subs, t)
                                                  + p_specCostCalib("construction", "none", "none", state, "none", subs, t);
  p_specCostRen("intangible", state, stateFull, vinCalib, subs, t)$renAllowed(state, stateFull)
                                                  = p_xinitRen(state, stateFull, vinCalib, subs, t)
                                                  + p_specCostCalib("renovation", state, stateFull, vinCalib, subs, t);

  v_stock.l("area", state, vinCalib, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

  solveParallel

  p_fA(subs, tcalib) = func

  p_phiDeriv(subs, tcalib) = sum((flow, renType, stateFull, vinCalib)$gradientVars(flow, renType, stateFull, vinCalib, tcalib),
    p_r(flow, renType, stateFull, vinCalib, subs, tcalib) * p_d(flow, renType, stateFull, vinCalib, subs, tcalib));

*** Stopping criterion in all dimensions
  loop((all_subs, tcalib),
    if (p_fA(all_subs, tcalib) le p_f(all_subs, tcalib) + p_sigma * p_alpha(all_subs, tcalib) * p_phiDeriv(all_subs, tcalib),
      subs(all_subs) = no;
      p_iterA(all_subs, tcalib) = iterA.val;
      );
    p_fAIterA(iterA, all_subs, tcalib) = p_fA(all_subs, tcalib);
    p_fArmijoRHIterA(iterA, all_subs, tcalib) = p_f(all_subs, tcalib) + p_sigma * p_alpha(all_subs, tcalib) * p_phiDeriv(all_subs, tcalib);
  );
  if(card(subs) = 0,
    p_alphaIterA(iterA, all_subs, tcalib) = p_alpha(all_subs, tcalib);
    break;
  );
*** Update alpha
  p_alpha(subs, tcalib) = p_alpha(subs, tcalib) * p_beta;
  p_alphaIterA(iterA, all_subs, tcalib) = p_alpha(all_subs, tcalib);
);
subs(all_subs) = yes;

p_x(flow, renType, stateFull, vinCalib, subs, tcalib)$gradientVars(flow, renType, stateFull, vinCalib, tcalib)
                                = p_xA(flow, renType, stateFull, vinCalib, subs, tcalib);
p_fPrev(subs, tcalib) = p_f(subs, tcalib);
p_f(subs, tcalib) = p_fA(subs, tcalib);
$else.stepSize
*** Update p_x and recompute p_f
p_x(flow, renType, bsr, hsr, vinCalib, subs, tcalib)$gradientVars(flow, renType, bsr, hsr, vinCalib, tcalib)
                                  = p_x(flow, renType, bsr, hsr, vinCalib, subs, tcalib) + p_alpha(subs, tcalib) * p_d(flow, renType, bsr, hsr, vinCalib, subs, tcalib);
p_specCostCalib("construction", "none", "none", state, "none", subs, tcalib)$renTarAllowed("construction", state) = p_x("construction", "newSys", state, "none", subs, tcalib);
  loop(renAllowed(bs, hs, bsr, hsr),
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$sameas(hs, hsr)
                                                                           = p_x("renovation", "identRepl", bsr, hsr, vinCalib, subs, tcalib);
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$(not sameas(hs, hsr) and not sameas(hsr, "0"))
                                                                           = p_x("renovation", "newSys", bsr, hsr, vinCalib, subs, tcalib);
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$sameas(hsr, "0") = p_x("renovation", "0", bsr, hsr, vinCalib, subs, tcalib);
  );

p_specCostCalib(flow, stateAll, stateFull, vinAll, subs, t)$(not tcalib(t) and calibStates(flow, stateAll, stateFull, vinAll))
                                                             = extrapolateIntang

p_specCostCon("intangible", state, subs, t)$renTarAllowed("construction", state)
                                                  = p_xinitCon(state, subs, t)
                                                  + p_specCostCalib("construction", "none", "none", state, "none", subs, t);
p_specCostRen("intangible", state, stateFull, vinCalib, subs, t)$renAllowed(state, stateFull)
                                                  = p_xinitRen(state, stateFull, vinCalib, subs, t)
                                                  + p_specCostCalib("renovation", state, stateFull, vinCalib, subs, t);

v_stock.l("area", state, vinCalib, subs, ttot) = 0;
v_construction.l("area", state, subs, ttot) = 0;
v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

solveParallel

*** Update p_f
p_fPrev(subs, tcalib) = p_f(subs, tcalib);
p_f(subs, tcalib) = func

$endif.stepSize

p_renovation("area", state, stateFull, vinCalib, subs, ttot) = v_renovation.l("area", state, stateFull, vinCalib, subs, ttot);
p_construction("area", state, subs, ttot) = v_construction.l("area", state, subs, ttot);
p_stock("area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);

*** Save the model statistics of the previous iteration. This means that the iteration counter is off by one in some sense.
p_repyFullSysNLPIter(iteration,all_subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLPIter(iteration,all_subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLPIter(iteration,all_subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLPIter(iteration,all_subs,'objval')    = fullSysNLP.objval;

*** Compute the gradient
loop(gradientVars(flow2, renType2, bsr3, hsr3, vin2, tcalib2),
  p_xDiff(flow, renType, bsr, hsr, vinCalib, subs, tcalib)$(gradientVars(flow, renType, bsr, hsr, vinCalib, tcalib)
                                      and (not sameas(flow, flow2) or not sameas(renType, renType2)
                                           or not sameas(bsr, bsr3) or not sameas(hsr, hsr3)
                                           or not sameas(vinCalib, vin2) or not sameas(tcalib, tcalib2)))
                                      = p_x(flow, renType, bsr, hsr, vinCalib, subs, tcalib);
  p_xDiff(flow, renType, bsr, hsr, vinCalib, subs, tcalib)$(gradientVars(flow, renType, bsr, hsr, vinCalib, tcalib)
                                    and (sameas(flow, flow2) and sameas(renType, renType2)
                                         and sameas(bsr, bsr3) and sameas(hsr, hsr3)
                                         and sameas(vinCalib, vin2) and sameas(tcalib, tcalib2)))
                                    = p_x(flow, renType, bsr, hsr, vinCalib, subs, tcalib) + p_diff;
  p_specCostCalib("construction", "none", "none", state, "none", subs, tcalib)$renTarAllowed("construction", state) = p_xDiff("construction", "newSys", state, "none", subs, tcalib);
  loop(renAllowed(bs, hs, bsr, hsr),
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$sameas(hs, hsr)
                                                                           = p_xDiff("renovation", "identRepl", bsr, hsr, vinCalib, subs, tcalib);
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$(not sameas(hs, hsr) and not sameas(hsr, "0"))
                                                                           = p_xDiff("renovation", "newSys", bsr, hsr, vinCalib, subs, tcalib);
    p_specCostCalib("renovation", bs, hs, bsr, hsr, vinCalib, subs, tcalib)$sameas(hsr, "0") = p_xDiff("renovation", "0", bsr, hsr, vinCalib, subs, tcalib);
  );

  p_specCostCalib(flow, stateAll, stateFull, vinAll, subs, t)$(not tcalib(t) and calibStates(flow, stateAll, stateFull, vinAll))
                                                             = extrapolateIntang

  p_specCostCon("intangible", state, subs, t)$renTarAllowed("construction", state)
                                                  = p_xinitCon(state, subs, t)
                                                  + p_specCostCalib("construction", "none", "none", state, "none", subs, t);
  p_specCostRen("intangible", state, stateFull, vinCalib, subs, t)$renAllowed(state, stateFull)
                                                  = p_xinitRen(state, stateFull, vinCalib, subs, t)
                                                  + p_specCostCalib("renovation", state, stateFull, vinCalib, subs, t);

  v_stock.l("area", state, vinCalib, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

  solveParallel

  p_fDiff(flow2, renType2, bsr3, hsr3, vin2, subs, tcalib) = func
);

p_r(flow, renType, stateFull, vinCalib, subs, tcalib)$gradientVars(flow, renType, stateFull, vinCalib, tcalib) = (p_fDiff(flow, renType, stateFull, vinCalib, subs, tcalib) - p_f(subs, tcalib)) / p_diff;
p_d(flow, renType, stateFull, vinCalib, subs, tcalib)$gradientVars(flow, renType, stateFull, vinCalib, tcalib) = - p_r(flow, renType, stateFull, vinCalib, subs, tcalib);
p_delta(subs, tcalib) = sum(gradientVars(flow, renType, stateFull, vinCalib, tcalib),
  -p_r(flow, renType, stateFull, vinCalib, subs, tcalib) * p_d(flow, renType, stateFull, vinCalib, subs, tcalib));

*** Save results of current iteration
execute_unload "calibration.gdx";
!! retain gdxes of intermediate iterations by copying them using shell
!! commands
put_utility "shell" / "cp calibration.gdx calibration_" iteration.val:0:0 ".gdx";

*** end iteration loop
);


$endif.fullSys



*** matching run ---------------------------------------------------------------

$ifthen.matching "%RUNTYPE%" == "matching"

* measure stocks and flows in both floor area and number of dwellings
q(qty) = yes;

* measure stocks and flows in floor area
q("dwel") = no;
q("area") = yes;

solve matching minimizing v_matchingObj using qcp;

$endif.matching
