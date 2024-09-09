*** models ---------------------------------------------------------------------

model fullSysLP "full system linear optimisation"
  /
  q_totSysCost
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
  q_totSysCost
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
  solve fullSysNLP minimizing v_totSysCost using nlp; \
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
$macro func sum((state3, tcalib)$renTarAllowed("con", state3), \
  power(p_constructionHist("area", state3, subs, tcalib) - v_construction.l("area", state3, subs, tcalib), 2)) \
  + sum((vin3, state3, stateFull3, tcalib)$renAllowed(state3, stateFull3), \
  power(p_renovationHist("area", state3, stateFull3, vin3, subs, tcalib) - v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib), 2));

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func sum((vin3, state3, tcalib), \
  power(p_stockHist("area", state3, vin3, subs, tcalib) - v_stock.l("area", state3, vin3, subs, tcalib), 2));

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stockszero"
$macro func sum((vin3, state3, tcalib), \
  power(p_stockHist("area", state3, vin3, subs, tcalib) - v_stock.l("area", state3, vin3, subs, tcalib), 2)) \
  + sum((vin3, state3, stateFull3, tcalib)$zeroFlow(state3, stateFull3), \
  power(p_renovationHist("area", state3, stateFull3, vin3, subs, tcalib) - v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib), 2));
$endIf.calibTarget

$elseIf.targetFunc "%TARGETFUNCTION%" == "maxlikely"
$ifThen.calibTarget "%CALIBRATIONTYPE%" == "flows"
$macro func - sum((state3, tcalib)$renTarAllowed("con", state3), \
  p_constructionHist("area", state3, subs, tcalib) \
  * log(v_construction.l("area", state3, subs, tcalib) \
    / (sum(state4, \
      v_construction.l("area", state4, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  ) \
  - sum((vin3, state3, stateFull3, tcalib)$renAllowed(state3, stateFull3), \
  p_renovationHist("area", state3, stateFull3, vin3, subs, tcalib) \
  * log(v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib) \
    / (sum((state4, stateFull4), \
      v_renovation.l("area", state4, stateFull4, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  );

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func - sum((vin3, state3, tcalib), \
  p_stockHist("area", state3, vin3, subs, tcalib) \
  * log(v_stock.l("area", state3, vin3, subs, tcalib) \
    / (sum((state4), \
      v_stock.l("area", state4, vin3, subs, tcalib) \
    ) \
    + epsilonSmall) \
  + epsilonSmall)) \
  - sum((vin3, state3, stateFull3, tcalib)$zeroFlow(state3, stateFull3), \
  p_renovationHist("area", state3, stateFull3, vin3, subs, tcalib) \
  * log(v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib) \
    / (sum((state4, stateFull4), \
      v_renovation.l("area", state4, stateFull4, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
);

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stockszero"
$macro func - sum((vin3, state3, tcalib), \
  p_stockHist("area", state3, vin3, subs, tcalib) \
  * log(v_stock.l("area", state3, vin3, subs, tcalib) \
    / (sum((state4), \
      v_stock.l("area", state4, vin3, subs, tcalib) \
    ) \
    + epsilonSmall) \
  + epsilonSmall));
$endIf.calibTarget

$endIf.targetFunc



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
solve fullSysLP minimizing v_totSysCost using lp;
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

solve fullSysNLP minimizing v_totSysCost using nlp;

p_repyFullSysNLP(subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLP(subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLP(subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLP(subs,'objval')    = fullSysNLP.objval;

$endif.parallel



$endif.nlp


$elseif.fullSys "%RUNTYPE%" == "calibration"
*********************************************************************************
*** Preparation of the calibration
*********************************************************************************

* measure stocks and flows in floor area
q("dwel") = no;
q("area") = yes;

p_alpha(subs) = p_alphaL;
p_fPrev(subs) = 0; !! unused initialization to avoid compilation error

* p_specCostCon("intangible", bs, hs, subs, t) = p_x("con", bs, hs, "2000-2010", subs);
* p_specCostRen("intangible", state, stateFull, vinCalib, subs, t) = p_x("ren", stateFull, vinCalib, subs);
* v_stock.l(qty,state,vinCalib,subs,t) = p_stockHist(qty,state,vinCalib,subs,t);

solveParallel

*** Compute the functional value
p_f(subs) = func
p_f0(subs) = p_f(subs);

$ifThen.calLog "%CALIBRATIONLOG%" == "TRUE"

*** Store renovation and construction values
p_construction("area", state, subs, t) = v_construction.l("area", state, subs, t);
p_renovation("area", state, stateFull, vinCalib, subs, t) = v_renovation.l("area", state, stateFull, vinCalib, subs, t);
$else.calLog
abort "Did not start full logging";
$endIf.calLog
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

*** Compute the gradient
p_xPS(subs) = priceSensHS(subs);
p_xDiffPS(subs) = p_xPS(subs) + p_diff;
priceSensHS(subs) = p_xDiffPS(subs);

solveParallel

p_fDiffPS(subs) = func

p_rPS(subs) = (p_fDiffPS(subs) - p_f(subs)) / p_diff;
p_dPS(subs) = - p_rPS(subs);
p_deltaPS(subs) = -p_rPS(subs) * p_dPS(subs);

execute_unload "calibrationPS_0.gdx";

loop(iteration,

*** Armijo backtracking method
p_alpha(subs)$(iteration.val > 1 and p_deltaPS(subs) > 0.001) = min(abs(0.5 * p_f(subs)),
  max(p_alphaL,
    (p_fPrev(subs) - p_f(subs))
      / p_deltaPS(subs))
  );
p_alpha(subs)$(p_deltaPS(subs) le 0.001) = p_alphaL;

loop(iterA,
*** Solve the model only for the subsets which do not satisfy the Armijo condition yet
  p_xAPS(subs) = p_xPS(subs) + p_alpha(subs) * p_dPS(subs);

  v_stock.l("area", state, vinCalib, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

  priceSensHS(subs) = p_xAPS(subs);

  solveParallel

  p_fA(subs) = func

  p_phiDeriv(subs) = p_rPS(subs) * p_dPS(subs);

*** Stopping criterion in all dimensions
  loop((all_subs),
    if (p_fA(all_subs) le p_f(all_subs) + p_sigma * p_alpha(all_subs) * p_phiDeriv(all_subs),
      subs(all_subs) = no;
      p_iterA(all_subs) = iterA.val;
      );
    p_fAIterA(iterA, all_subs) = p_fA(all_subs);
    p_fArmijoRHIterA(iterA, all_subs) = p_f(all_subs) + p_sigma * p_alpha(all_subs) * p_phiDeriv(all_subs);
  );
  if(card(subs) = 0,
    p_alphaIterA(iterA, all_subs) = p_alpha(all_subs);
    break;
  );
*** Update alpha
  p_alpha(subs) = p_alpha(subs) * p_beta;
  p_alphaIterA(iterA, all_subs) = p_alpha(all_subs);
);
subs(all_subs) = yes;

p_xPS(subs) = p_xAPS(subs);
p_fPrev(subs) = p_f(subs);
p_f(subs) = p_fA(subs);

$ifThen.calLog "%CALIBRATIONLOG%" == "TRUE"
p_renovation("area", state, stateFull, vinCalib, subs, ttot) = v_renovation.l("area", state, stateFull, vinCalib, subs, ttot);
p_construction("area", state, subs, ttot) = v_construction.l("area", state, subs, ttot);
$endIf.calLog
p_stock("area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);

*** Save the model statistics of the previous iteration. This means that the iteration counter is off by one in some sense.
p_repyFullSysNLPIter(iteration,all_subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLPIter(iteration,all_subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLPIter(iteration,all_subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLPIter(iteration,all_subs,'objval')    = fullSysNLP.objval;

*** Compute the gradient
p_xDiffPS(subs) = p_xPS(subs) + p_diff;
priceSensHS(subs) = p_xDiffPS(subs);
solveParallel

p_fDiffPS(subs) = func

p_rPS(subs) = (p_fDiffPS(subs) - p_f(subs)) / p_diff;
p_dPS(subs) = - p_rPS(subs);
p_deltaPS(subs) = -p_rPS(subs) * p_dPS(subs);

*** Save results of current iteration
execute_unload "calibrationPS.gdx";
!! retain gdxes of intermediate iterations by copying them using shell
!! commands
put_utility "shell" / "cp calibrationPS.gdx calibrationPS_" iteration.val:0:0 ".gdx";

*** end iteration loop
);

$elseIf.calPrice "%CALIBRATEPRICESENS%" == "SIMPLE"

execute_unload "calibrationPS_0.gdx";

loop(iteration,

  priceSensHS(subs) = allPriceSensHS(iteration);

  solveParallel

  p_renovation("area", state, stateFull, vinCalib, subs, ttot) = v_renovation.l("area", state, stateFull, vinCalib, subs, ttot);
  p_construction("area", state, subs, ttot) = v_construction.l("area", state, subs, ttot);
  p_stock("area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);

  p_f(subs) = func

  execute_unload "calibrationPS.gdx";

  !! retain gdxes of intermediate iterations by copying them using shell
  !! commands
  put_utility "shell" / "cp calibrationPS.gdx calibrationPS_" iteration.val:0:0 ".gdx";


);

abort "Stop here";

$endIf.calPrice

**************************************************************************************
*** Calibration of intangible costs
**************************************************************************************

*** TODO: Still need to check the proper time dimension!
*** TODO: Fix sets: What do I mean by subs? Where do I need to consider the vintage?
*** TODO: Fix: For construction I need to vary bs and hs, for renovation, costs primarily depend on bsr and hsr! (Although for more precision, reflecting both makes sens)
*** TODO: IMPORTANT! The implementation below relies on having constant intangible renovation costs for bs and hs, and assumes that the calibration is solely carried out for 2010!!!
*** Done (Mostly): Figure out whether we want to calibrate flows or stocks; if they are stocks: Also need to treat p_specCostRen in a similar way! (Then c(p_specCostCon, p_specCostRen) serve the function of x)
p_xinitCon(state, subs)$renTarAllowed("con", state) = p_specCostCon("intangible", state, subs, "2010");
p_xinitRen(state, stateFull, vinCalib, subs)$renTarAllowed("ren", stateFull) = p_specCostRen("intangible", state, stateFull, vinCalib, subs, "2010");
p_x("con", state, "2000-2010", subs)$renTarAllowed("con", state) = 0;
p_x("ren", stateFull, vinCalib, subs)$renTarAllowed("ren", stateFull) = 0;
p_alpha(subs) = p_alphaL;
p_fPrev(subs) = 0; !! unused initialization to avoid compilation error

p_f0(subs) = p_f(subs);

*** Compute the gradient
loop((flow2, bsr3, hsr3, vin2)$renTarAllowed(flow2, bsr3, hsr3),
  p_xDiff(flow, bsr, hsr, vinCalib, subs)$((renTarAllowed(flow, bsr, hsr) and (sameas(flow, "ren") or sameas(vinCalib, "2000-2010")))
                                      and (not sameas(bsr, bsr3) or not sameas(hsr, hsr3) or not sameas(flow, flow2) or not sameas(vinCalib, vin2)))
                                      = p_x(flow, bsr, hsr, vinCalib, subs);
  p_xDiff(flow, bsr, hsr, vinCalib, subs)$((renTarAllowed(flow, bsr, hsr) and (sameas(flow, "ren") or sameas(vinCalib, "2000-2010")))
                                    and (sameas(bsr, bsr3) and sameas(hsr, hsr3) and sameas(flow, flow2) and sameas(vinCalib, vin2)))
                                    = p_x(flow, bsr, hsr, vinCalib, subs) + p_diff;
  p_specCostCon("intangible", state, subs, t)$renTarAllowed("con", state) = p_xinitCon(state, subs) + p_xDiff("con", state, "2000-2010", subs);
  p_specCostRen("intangible", state, stateFull, vinCalib, subs, t)$renAllowed(state, stateFull) = p_xinitRen(state, stateFull, vinCalib, subs) + p_xDiff("ren", stateFull, vinCalib, subs);

  v_stock.l("area", state, vinCalib, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

  solveParallel

* $ifThen.calLog "%CALIBRATIONLOG%" == "TRUE"
*   p_constructionDiffIter("0", flow2, bsr3, hsr3, vin2, state, subs, t) = v_construction.l("area", state, subs, t);
*   p_renovationDiffIter("0", flow2, bsr3, hsr3, vin2, state, stateFull, vinCalib, subs, t) = v_renovation.l("area", state, stateFull, vinCalib, subs, t);
* $endIf.calLog

  p_fDiff(flow2, bsr3, hsr3, vin2, subs) = func
);

p_r(flow, stateFull, vinCalib, subs)$(renTarAllowed(flow, stateFull) and (sameas(flow, "ren") or sameas(vinCalib, "2000-2010"))) = (p_fDiff(flow, stateFull, vinCalib, subs) - p_f(subs)) / p_diff;
p_d(flow, stateFull, vinCalib, subs)$(renTarAllowed(flow, stateFull) and (sameas(flow, "ren") or sameas(vinCalib, "2000-2010"))) = - p_r(flow, stateFull, vinCalib, subs);
p_delta(subs) = sum((flow, stateFull, vinCalib)$renTarAllowed(flow, stateFull),
  -p_r(flow, stateFull, vinCalib, subs) * p_d(flow, stateFull, vinCalib, subs));

execute_unload "calibration_0.gdx";

loop(iteration,

*** TODO: Include proper stopping criterion! Handle multi-dimensionality

*** Armijo backtracking method
p_alpha(subs)$(iteration.val > 1 and p_delta(subs) > 0.001) = min(abs(0.5 * p_f(subs)),
  max(p_alphaL,
    (p_fPrev(subs) - p_f(subs))
      / p_delta(subs))
  );
p_alpha(subs)$(p_delta(subs) le 0.001) = p_alphaL;

loop(iterA,
*** Solve the model only for the subsets which do not satisfy the Armijo condition yet
  p_xA(flow, bsr, hsr, vinCalib, subs)$(renTarAllowed(flow, bsr, hsr) and (sameas(flow, "ren") or sameas(vinCalib, "2000-2010")))
                                  = p_x(flow, bsr, hsr, vinCalib, subs) + p_alpha(subs) * p_d(flow, bsr, hsr, vinCalib, subs);
  p_specCostCon("intangible", state, subs, t)$renTarAllowed("con", state) = p_xinitCon(state, subs) + p_xA("con", state, "2000-2010", subs);
  p_specCostRen("intangible", state, stateFull, vinCalib, subs, t)$renAllowed(state, stateFull) = p_xinitRen(state, stateFull, vinCalib, subs) + p_xA("ren", stateFull, vinCalib, subs);

  v_stock.l("area", state, vinCalib, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

  solveParallel

  p_fA(subs) = func

  p_phiDeriv(subs) = sum((flow, stateFull, vinCalib)$renTarAllowed(flow, stateFull),
    p_r(flow, stateFull, vinCalib, subs) * p_d(flow, stateFull, vinCalib, subs));

*** Stopping criterion in all dimensions
  loop((all_subs),
    if (p_fA(all_subs) le p_f(all_subs) + p_sigma * p_alpha(all_subs) * p_phiDeriv(all_subs),
      subs(all_subs) = no;
      p_iterA(all_subs) = iterA.val;
      );
    p_fAIterA(iterA, all_subs) = p_fA(all_subs);
    p_fArmijoRHIterA(iterA, all_subs) = p_f(all_subs) + p_sigma * p_alpha(all_subs) * p_phiDeriv(all_subs);
  );
  if(card(subs) = 0,
    p_alphaIterA(iterA, all_subs) = p_alpha(all_subs);
    break;
  );
*** Update alpha
  p_alpha(subs) = p_alpha(subs) * p_beta;
  p_alphaIterA(iterA, all_subs) = p_alpha(all_subs);
);
subs(all_subs) = yes;

p_x(flow, stateFull, vinCalib, subs)$(renTarAllowed(flow, stateFull) and (sameas(flow, "ren") or sameas(vinCalib, "2000-2010")))
                                = p_xA(flow, stateFull, vinCalib, subs);
p_fPrev(subs) = p_f(subs);
p_f(subs) = p_fA(subs);

$ifThen.calLog "%CALIBRATIONLOG%" == "TRUE"
p_renovation("area", state, stateFull, vinCalib, subs, ttot) = v_renovation.l("area", state, stateFull, vinCalib, subs, ttot);
p_construction("area", state, subs, ttot) = v_construction.l("area", state, subs, ttot);
$endIf.calLog
p_stock("area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);

*** Save the model statistics of the previous iteration. This means that the iteration counter is off by one in some sense.
p_repyFullSysNLPIter(iteration,all_subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLPIter(iteration,all_subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLPIter(iteration,all_subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLPIter(iteration,all_subs,'objval')    = fullSysNLP.objval;

*** Compute the gradient
loop((flow2, bsr3, hsr3, vin2)$renTarAllowed(flow2, bsr3, hsr3),
  p_xDiff(flow, bsr, hsr, vinCalib, subs)$((renTarAllowed(flow, bsr, hsr) and (sameas(flow, "ren") or sameas(vinCalib, "2000-2010")))
                                      and (not sameas(bsr, bsr3) or not sameas(hsr, hsr3) or not sameas(flow, flow2) or not sameas(vinCalib, vin2)))
                                      = p_x(flow, bsr, hsr, vinCalib, subs);
  p_xDiff(flow, bsr, hsr, vinCalib, subs)$((renTarAllowed(flow, bsr, hsr) and (sameas(flow, "ren") or sameas(vinCalib, "2000-2010")))
                                    and (sameas(bsr, bsr3) and sameas(hsr, hsr3) and sameas(flow, flow2) and sameas(vinCalib, vin2)))
                                    = p_x(flow, bsr, hsr, vinCalib, subs) + p_diff;
  p_specCostCon("intangible", state, subs, t)$renTarAllowed("con", state) = p_xinitCon(state, subs) + p_xDiff("con", state, "2000-2010", subs);
  p_specCostRen("intangible", state, stateFull, vinCalib, subs, t)$renAllowed(state, stateFull) = p_xinitRen(state, stateFull, vinCalib, subs) + p_xDiff("ren", stateFull, vinCalib, subs);

  v_stock.l("area", state, vinCalib, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vinCalib, subs, ttot) = 0;

  solveParallel

* $ifThen.calLog "%CALIBRATIONLOG%" == "TRUE"
*   p_constructionDiffIter(iteration, flow2, bsr3, hsr3, vin2, state, subs, t) = v_construction.l("area", state, subs, t);
*   p_renovationDiffIter(iteration, flow2, bsr3, hsr3, vin2, state, stateFull, vinCalib, subs, t) = v_renovation.l("area", state, stateFull, vinCalib, subs, t);
* $endIf.calLog

  p_fDiff(flow2, bsr3, hsr3, vin2, subs) = func
);

p_r(flow, stateFull, vinCalib, subs)$(renTarAllowed(flow, stateFull) and (sameas(flow, "ren") or sameas(vinCalib, "2000-2010"))) = (p_fDiff(flow, stateFull, vinCalib, subs) - p_f(subs)) / p_diff;
p_d(flow, stateFull, vinCalib, subs)$(renTarAllowed(flow, stateFull) and (sameas(flow, "ren") or sameas(vinCalib, "2000-2010")))= - p_r(flow, stateFull, vinCalib, subs);
p_delta(subs) = sum((flow, stateFull, vinCalib)$renTarAllowed(flow, stateFull),
  -p_r(flow, stateFull, vinCalib, subs) * p_d(flow, stateFull, vinCalib, subs));

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
