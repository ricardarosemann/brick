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
*  q_buildingShellLifeTime  !! rule out building shell dimension
  q_heatingSystemLifeTime
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
*  q_buildingShellLifeTime  !! rule out building shell dimension
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
$macro func sum((state3, t), \
  power(p_constructionHist("area", state3, subs, t) - v_construction.l("area", state3, subs, t), 2)) \
  + sum((state3, stateFull3, t), \
  power(p_renovationHist("area", state3, stateFull3, vinCalib, subs, t) - v_renovation.l("area", state3, stateFull3, vinCalib, subs, t), 2));

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func sum((state3, t), \
  power(p_stockHist("area", state3, vinCalib, subs, t) - v_stock.l("area", state3, vinCalib, subs, t), 2));
$endIf.calibTarget

$elseIf.targetFunc "%TARGETFUNCTION%" == "maxlikely"
$ifThen.calibTarget "%CALIBRATIONTYPE%" == "flows"
$macro func - sum((state3, t), \
  p_constructionHist("area", state3, subs, t) \
  * log(v_construction.l("area", state3, subs, t) \
    / (sum(state4, \
      v_construction.l("area", state4, subs, t) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  ) \
  + sum((state3, stateFull3, t), \
  p_renovationHist("area", state3, stateFull3, vinCalib, subs, t) \
  * log(v_renovation.l("area", state3, stateFull3, vinCalib, subs, t) \
    / (sum((state4, stateFull4), \
      v_renovation.l("area", state4, stateFull4, vinCalib, subs, t) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  );

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func - sum((state3, t), \
  p_stockHist("area", state3, vinCalib, subs, t) \
  * log(v_stock.l("area", state3, vinCalib, subs, t) \
    / (sum(state4, \
      v_stock.l("area", state4, vinCalib, subs, t) \
    ) \
    + epsilonSmall) \
  + epsilonSmall) \
);
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
* measure stocks and flows in floor area
q("dwel") = no;
q("area") = yes;

*** TODO: Still need to check the proper time dimension!
*** TODO: Fix sets: What do I mean by subs? Where do I need to consider the vintage?
*** TODO: Fix: For construction I need to vary bs and hs, for renovation, costs primarily depend on bsr and hsr! (Although for more precision, reflecting both makes sens)
*** Done (Mostly): Figure out whether we want to calibrate flows or stocks; if they are stocks: Also need to treat p_specCostRen in a similar way! (Then c(p_specCostCon, p_specCostRen) serve the function of x)
p_x("con", state, "2000-2010", subs) = p_specCostCon("intangible", state, subs, "2000");
p_x("ren", stateFull, vinCalib, subs) = p_specCostRen("intangible", "original", "biom", stateFull, vinCalib, subs, "2000");
p_alpha(vinCalib, subs) = p_alphaL;
p_fPrev(vinCalib, subs) = 0; !! unused initialization to avoid compilation error

* p_specCostCon("intangible", bs, hs, subs, t) = p_x("con", bs, hs, "2000-2010", subs);
* p_specCostRen("intangible", state, stateFull, vinCalib, subs, t) = p_x("ren", stateFull, vinCalib, subs);
* v_stock.l(qty,state,vinCalib,subs,t) = p_stockHist(qty,state,vinCalib,subs,t);

solveParallel

*** Compute the functional value
p_f(vinCalib, subs) = func
p_f0(vinCalib, subs) = p_f(vinCalib, subs);
p_fIter("0", vinCalib, subs) = p_f0(vinCalib, subs);

*** Store renovation and construction values
p_constructionIter("0", "area", state, subs, t) = v_construction.l("area", state, subs, t);
p_renovationIter("0", "area", state, stateFull, vinCalib, subs, t) = v_renovation.l("area", state, stateFull, vinCalib, subs, t);
p_stockIter("0", "area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);

loop(iteration,

*** Save the model statistics of the previous iteration. This means that the iteration counter is off by one in some sense.
p_repyFullSysNLPIter(iteration,all_subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLPIter(iteration,all_subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLPIter(iteration,all_subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLPIter(iteration,all_subs,'objval')    = fullSysNLP.objval;

*** Compute the gradient
loop((flow2, bsr3, hsr3),
  p_xDiff(flow, bsr, hsr, vinCalib, subs)$((sameas(flow, "ren") or (sameas(vinCalib, "2000-2010") and not sameas(bsr, "0") and not sameas(hsr, "0")))
                                      and (not sameas(bsr, bsr3) or not sameas(hsr, hsr3) or not sameas(flow, flow2)))
                                      = p_x(flow, bsr, hsr, vinCalib, subs);
  p_xDiff(flow, bsr, hsr, vinCalib, subs)$((sameas(flow, "ren") or (sameas(vinCalib, "2000-2010") and not sameas(bsr, "0") and not sameas(hsr, "0")))
                                    and (sameas(bsr, bsr3) and sameas(hsr, hsr3) and sameas(flow, flow2)))
                                    = p_x(flow, bsr, hsr, vinCalib, subs) + p_diff;
  p_xDiffAll(iteration, flow2, bsr3, hsr3, flow, stateFull, vinCalib, subs) = p_xDiff(flow, stateFull, vinCalib, subs);
  p_specCostCon("intangible", state, subs, t) = p_xDiff("con", state, "2000-2010", subs);
  p_specCostRen("intangible", state, stateFull, vinCalib, subs, t) = p_xDiff("ren", stateFull, vinCalib, subs);

  solveParallel

  p_fDiff(flow2, bsr3, hsr3, vinCalib, subs) = func
);

p_r(flow, stateFull, vinCalib, subs) = (p_fDiff(flow, stateFull, vinCalib, subs) - p_f(vinCalib, subs)) / p_diff;
p_d(flow, stateFull, vinCalib, subs) = - p_r(flow, stateFull, vinCalib, subs);
p_delta(vinCalib, subs) = sum((flow, stateFull),
  -p_r(flow, stateFull, vinCalib, subs) * p_d(flow, stateFull, vinCalib, subs));

*** TODO: Include proper stopping criterion! Handle multi-dimensionality

*** Armijo backtracking method
p_alpha(vinCalib, subs)$(iteration.val > 1 and p_delta(vinCalib, subs) > 0.001) = min(abs(0.5 * p_f(vinCalib, subs)),
  max(p_alphaL,
    (p_fPrev(vinCalib, subs) - p_f(vinCalib, subs))
      / p_delta(vinCalib, subs))
  );
p_alpha(vinCalib, subs)$(p_delta(vinCalib, subs) le 0.001) = p_alphaL;

loop(iterA,
*** Solve the model only for the subsets which do not satisfy the Armijo condition yet
  p_xA(flow, bsr, hsr, vinCalib, subs)$(sameas(flow, "ren") or (sameas(vinCalib, "2000-2010") and not sameas(bsr, "0") and not sameas(hsr, "0")))
                                  = p_x(flow, bsr, hsr, vinCalib, subs) + p_alpha(vinCalib, subs) * p_d(flow, bsr, hsr, vinCalib, subs);
  p_specCostCon("intangible", state, subs, t) = p_xA("con", state, "2000-2010", subs);
  p_specCostRen("intangible", state, stateFull, vinCalib, subs, t) = p_xA("ren", stateFull, vinCalib, subs);

  solveParallel

  p_fA(vinCalib, subs) = func

  p_phiDeriv(vinCalib, subs) = sum((flow, stateFull),
    p_r(flow, stateFull, vinCalib, subs) * p_d(flow, stateFull, vinCalib, subs));

*** Stopping criterion in all dimensions
  loop((vin, all_subs),
    if (p_fA(vin, all_subs) le p_f(vin, all_subs) + p_sigma * p_alpha(vin, all_subs) * p_phiDeriv(vin, all_subs),
      subs(all_subs) = no;
      vinCalib(vin) = no;
      p_iterA(iteration, vin, all_subs) = iterA.val;
      );
    p_fAIter(iteration, iterA, vin, all_subs) = p_fA(vin, all_subs);
    p_fArmijoRHIter(iteration, iterA, vin, all_subs) = p_f(vin, all_subs) + p_sigma * p_alpha(vin, all_subs) * p_phiDeriv(vin, all_subs);
  );
  if(card(subs) = 0 and card(vinCalib) = 0,
    p_alphaIter(iteration, iterA, vin, all_subs) = p_alpha(vin, all_subs);
    break;
  );
*** Update alpha
  p_alpha(vinCalib, subs) = p_alpha(vinCalib, subs) * p_beta;
  p_alphaIter(iteration, iterA, vin, all_subs) = p_alpha(vin, all_subs);
);
loop(t,
  vinCalib(vin)$vinExists(t, vin) = yes;
);
subs(all_subs) = yes;

p_x(flow, stateFull, vinCalib, subs)$(sameas(flow, "ren") or sameas(vinCalib, "2000-2010"))
                                = p_xA(flow, stateFull, vinCalib, subs);
p_fPrev(vinCalib, subs) = p_f(vinCalib, subs);
p_f(vinCalib, subs) = p_fA(vinCalib, subs);

p_xIter(iteration, flow, stateFull, vinCalib, subs)$(sameas(flow, "ren") or sameas(vinCalib, "2000-2010")) = p_x(flow, stateFull,vinCalib, subs);
p_fIter(iteration, vinCalib, subs) = p_f(vinCalib, subs);
p_renovationIter(iteration, "area", state, stateFull, vinCalib, subs, ttot) = v_renovation.l("area", state, stateFull, vinCalib, subs, ttot);
p_constructionIter(iteration, "area", state, subs, ttot) = v_construction.l("area", state, subs, ttot);
p_stockIter(iteration, "area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);

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
