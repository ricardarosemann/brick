$offOrder

*** models ---------------------------------------------------------------------

model fullSysLP "full system linear optimisation"
  /
  q_totObj
  q_obj
  q_sysCost
  q_conCost
  q_renCost
  q_opeCost
  q_demCost
$ifthen.sequentialRen  "%SEQUENTIALREN%" == "TRUE"
  q_stockBal1
  q_stockBal2
  q_stockBal3
$else.sequentialRen
  q_stockBalNext
  q_stockBalPrev
  q_renovationBS
  q_renovationHS
$endif.sequentialRen
$ifthen.notFixedBuildings not "%FIXEDBUILDINGS%" == "TRUE"
  q_housingDemand
  q_buildingLifeTime
$endif.notFixedBuildings
$ifthen.shell not "%ignoreShell%" == "TRUE"
  q_lifeTimeBS
$endif.shell
  q_lifeTimeHS
  q_sysHeteroPref
  q_zeroHeteroPrefCon
  q_zeroHeteroPrefRen
  q_statusQuoPref
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
  q_obj
  q_sysCost
  q_conCost
  q_renCost
  q_opeCost
  q_demCost
$ifthen.sequentialRen  "%SEQUENTIALREN%" == "TRUE"
  q_stockBal1
  q_stockBal2
  q_stockBal3
$else.sequentialRen
  q_stockBalNext
  q_stockBalPrev
  q_renovationBS
  q_renovationHS
  q_entropyRenFrom
$endif.sequentialRen
$ifthen.notFixedBuildings not "%FIXEDBUILDINGS%" == "TRUE"
  q_housingDemand
  q_buildingLifeTime
$endif.notFixedBuildings
$ifthen.shell not "%ignoreShell%" == "TRUE"
  q_lifeTimeBS
$endif.shell
  q_lifeTimeHS
  q_entropyRenToBS
  q_entropyRenToHS
  q_entropyRenFromBS
  q_entropyRenFromHS
  q_sysHeteroPref
  q_statusQuoPref
  q_heteroPrefCon
  q_heteroPrefRen
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
  q_refVals
  q_refValsBasic
$ifthen.sequentialRen  "%SEQUENTIALREN%" == "TRUE"
  q_stockBal1
  q_stockBal2
  q_stockBal3
  q_flowVariationRenBS
  q_flowVariationRenHS
  q_testRenBS
  q_testRenHS
$else.sequentialRen
  q_stockBalNext
  q_stockBalPrev
  q_renovationBS
  q_renovationHS
  q_flowVariationRen
  q_testRen
$endif.sequentialRen
$ifthen.notFixedBuildings not "%FIXEDBUILDINGS%" == "TRUE"
  q_housingDemand
  q_buildingLifeTime !! TODO: make this a matching target, not a hard constraint
$endif.notFixedBuildings
$ifthen.shell not "%ignoreShell%" == "TRUE"
  q_lifeTimeBS
$endif.shell
  q_lifeTimeHS
*  q_dwelSizeStock
*  q_dwelSizeConstruction
*  q_dwelSize_Odyssee
*  q_renRate_EuropeanCommissionRenovation
  q_replacementDeviation
  q_flowVariation
  q_flowVariationTot
  q_flowVariationCon
  q_flowVariationDem
*  q_test
  q_testCon

  /
;
$endif.matching

$ifthen.renCorrect "%RUNTYPE%" == "renCorrect"
model renCorrect "Find renovation flows that best matches other run"
  /
  q_renCorrectObj
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
  q_stockBal1
  q_stockBal2
  q_stockBal3
$else.sequentialRen
  q_stockBalNext
  q_stockBalPrev
  q_renovationBS
  q_renovationHS
$endif.sequentialRen
$ifthen.shell not "%ignoreShell%" == "TRUE"
  q_lifeTimeBS
$endif.shell
  q_lifeTimeHS
  /
;
$endif.renCorrect



*** prepare solving ------------------------------------------------------------

* solvers
option lp  = %solverLP%;
option nlp = %solverNLP%;
option qcp = %solverQCP%;

* define macro for solving in parallel mode

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

$ifthen.optimCalibration "%CALIBRATIONMETHOD%" == "optimization"

$ifThen.aggregateDim "%AGGREGATEDIM%" == "FALSE"
$ifThen.targetFunc "%TARGETFUNCTION%" == "minsquare"
$ifThen.calibTarget "%CALIBRATIONTYPE%" == "flows"
$macro func sum((state3, tcalib)$(p_constructionCalibTarget("area", state3, subs, tcalib)), \
  power(p_constructionCalibTarget("area", state3, subs, tcalib) - v_construction.l("area", state3, subs, tcalib), 2)) \
$ifThen.sequentialRen  "%SEQUENTIALREN%" == "TRUE"
  + sum((vin3, state3, bsr4, tcalib)$(renAllowedBS(state3, bsr4) and p_renovationBSCalibTarget("area", state3, bsr4, vin3, subs, tcalib) ne NA), \
  power(p_renovationBSCalibTarget("area", state3, bsr4, vin3, subs, tcalib) - v_renovationBS.l("area", state3, bsr4, vin3, subs, tcalib), 2)) \
  + sum((vin3, state3, hsr4, tcalib)$(renAllowedHS(state3, hsr4) and p_renovationHSCalibTarget("area", state3, hsr4, vin3, subs, tcalib) ne NA), \
  power(p_renovationHSCalibTarget("area", state3, hsr4, vin3, subs, tcalib) - v_renovationHS.l("area", state3, hsr4, vin3, subs, tcalib), 2));
$else.sequentialRen
  + sum((vin3, state3, stateFull3, tcalib)$(renAllowed(state3, stateFull3) and p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib) ne NA), \
  power(p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib) - v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib), 2));
$endIf.sequentialRen

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func sum((vin3, state3, tcalib), \
  power(p_stockCalibTarget("area", state3, vin3, subs, tcalib) - v_stock.l("area", state3, vin3, subs, tcalib), 2));

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stockszero"
$macro func sum((vin3, state3, tcalib), \
  power(p_stockCalibTarget("area", state3, vin3, subs, tcalib) - v_stock.l("area", state3, vin3, subs, tcalib), 2)) \
$ifThen.sequentialRen  "%SEQUENTIALREN%" == "TRUE"
  + sum((vin3, state3, bsr4, tcalib)$zeroFlowBS(state3, bsr4), \
  power(p_renovationBSCalibTarget("area", state3, bsr4, vin3, subs, tcalib) - v_renovationBS.l("area", state3, bsr4, vin3, subs, tcalib), 2)) \
  + sum((vin3, state3, hsr4, tcalib)$zeroFlowHS(state3, hsr4), \
  power(p_renovationHSCalibTarget("area", state3, hsr4, vin3, subs, tcalib) - v_renovationHS.l("area", state3, hsr4, vin3, subs, tcalib), 2));
$else.sequentialRen
  + sum((vin3, state3, stateFull3, tcalib)$zeroFlow(state3, stateFull3), \
  power(p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib) - v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib), 2));
$endIf.sequentialRen
$endIf.calibTarget

$elseIf.targetFunc "%TARGETFUNCTION%" == "maxlikely"
$ifThen.calibTarget "%CALIBRATIONTYPE%" == "flows"
$macro func - sum((state3, tcalib), \
  p_constructionCalibTarget("area", state3, subs, tcalib) \
  * log(v_construction.l("area", state3, subs, tcalib) \
    / (sum(state4, \
      v_construction.l("area", state4, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  ) \
$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
- sum((vin3, state3, bsr4, tcalib)$renAllowedBS(state3, bsr4), \
  p_renovationBSCalibTarget("area", state3, bsr4, vin3, subs, tcalib) \
  * log(v_renovationBS.l("area", state3, bsr4, vin3, subs, tcalib) \
    / (sum((state4, bsr5), \
      v_renovationBS.l("area", state4, bsr5, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  ) \
  - sum((vin3, state3, hsr4, tcalib)$renAllowedHS(state3, hsr4), \
  p_renovationHSCalibTarget("area", state3, hsr4, vin3, subs, tcalib) \
  * log(v_renovationHS.l("area", state3, hsr4, vin3, subs, tcalib) \
    / (sum((state4, hsr5), \
      v_renovationHS.l("area", state4, hsr5, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  );
$else.sequentialRen
  - sum((vin3, state3, stateFull3, tcalib)$renAllowed(state3, stateFull3), \
  p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib) \
  * log(v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib) \
    / (sum((state4, stateFull4), \
      v_renovation.l("area", state4, stateFull4, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  );
$endIf.sequentialRen

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func - sum((vin3, state3, tcalib), \
  p_stockCalibTarget("area", state3, vin3, subs, tcalib) \
  * log(v_stock.l("area", state3, vin3, subs, tcalib) \
    / (sum((state4), \
      v_stock.l("area", state4, vin3, subs, tcalib) \
    ) \
    + epsilonSmall) \
  + epsilonSmall));
$endIf.calibTarget

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stockszero"
$macro func - sum((vin3, state3, tcalib), \
  p_stockCalibTarget("area", state3, vin3, subs, tcalib) \
  * log(v_stock.l("area", state3, vin3, subs, tcalib) \
    / (sum((state4), \
      v_stock.l("area", state4, vin3, subs, tcalib) \
    ) \
    + epsilonSmall) \
  + epsilonSmall)) \
$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
  - sum((vin3, state3, bsr4, tcalib)$zeroFlowBS(state3, bsr4), \
  p_renovationBSCalibTarget("area", state3, bsr4, vin3, subs, tcalib) \
  * log(v_renovationBS.l("area", state3, bsr4, vin3, subs, tcalib) \
    / (sum((state4, bsr5), \
      v_renovationBS.l("area", state4, bsr5, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
  ) \
  - sum((vin3, state3, hsr4, tcalib)$zeroFlowHS(state3, hsr4), \
  p_renovationHSCalibTarget("area", state3, hsr4, vin3, subs, tcalib) \
  * log(v_renovationHS.l("area", state3, hsr4, vin3, subs, tcalib) \
    / (sum((state4, hsr5), \
      v_renovationHS.l("area", state4, hsr5, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
);
$else.sequentialRen
  - sum((vin3, state3, stateFull3, tcalib)$zeroFlow(state3, stateFull3), \
  p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib) \
  * log(v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib) \
    / (sum((state4, stateFull4), \
      v_renovation.l("area", state4, stateFull4, vin3, subs, tcalib) \
      ) \
      + epsilonSmall) \
    + epsilonSmall) \
);
$endIf.sequentialRen

$endIf.targetFunc

$elseIf.aggregateDim "%AGGREGATEDIM%" == "vin"

$ifThen.targetFunc "%TARGETFUNCTION%" == "minsquare"
$ifThen.calibTarget "%CALIBRATIONTYPE%" == "flows"
$macro func sum((state3, tcalib)$(p_constructionCalibTarget("area", state3, subs, tcalib)), \
  power(p_constructionCalibTarget("area", state3, subs, tcalib) - v_construction.l("area", state3, subs, tcalib), 2)) \
$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
  + sum((state3, bsr4, tcalib)$(renAllowedBS(state3, bsr4) and sum(vin3, p_renovationBSCalibTarget("area", state3, bsr4, vin3, subs, tcalib)) ne NA), \
  power(sum(vin3, p_renovationBSCalibTarget("area", state3, bsr4, vin3, subs, tcalib)) - sum(vin3, v_renovationBS.l("area", state3, bsr4, vin3, subs, tcalib)), 2)) \
  + sum((state3, hsr4, tcalib)$(renAllowedHS(state3, hsr4) and sum(vin3, p_renovationHSCalibTarget("area", state3, hsr4, vin3, subs, tcalib)) ne NA), \
  power(sum(vin3, p_renovationHSCalibTarget("area", state3, hsr4, vin3, subs, tcalib)) - sum(vin3, v_renovationHS.l("area", state3, hsr4, vin3, subs, tcalib)), 2));
$else.sequentialRen
  + sum((state3, stateFull3, tcalib)$(renAllowed(state3, stateFull3) and sum(vin3, p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib)) ne NA), \
  power(sum(vin3, p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib)) - sum(vin3, v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib)), 2));
$endIf.sequentialRen

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stocks"
$macro func sum((state3, tcalib), \
  power(sum(vin3, p_stockCalibTarget("area", state3, vin3, subs, tcalib)) - sum(vin3, v_stock.l("area", state3, vin3, subs, tcalib)), 2));

$elseIf.calibTarget "%CALIBRATIONTYPE%" == "stockszero"
$macro func sum((state3, tcalib), \
  power(sum(vin3, p_stockCalibTarget("area", state3, vin3, subs, tcalib)) - sum(vin3, v_stock.l("area", state3, vin3, subs, tcalib)), 2)) \
$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
$else.sequentialRen
  + sum((state3, stateFull3)$zeroFlow(state3, stateFull3), \
  power(sum(vin3, p_renovationCalibTarget("area", state3, stateFull3, vin3, subs, tcalib)) - sum(vin3, v_renovation.l("area", state3, stateFull3, vin3, subs, tcalib)), 2));
$endIf.sequentialRen
$endIf.calibTarget

$endIf.targetFunc

$endIf.aggregateDim

$endIf.optimCalibration


*** scenario / calibration run -------------------------------------------------

$ifthen.fullSys "%RUNTYPE%" == "scenario"

* measure stocks and flows in floor area
q("num") = no;
q("area") = yes;


$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
solve fullSysLP minimizing v_totObj using lp;
p_repyFullSysLP('solvestat') = fullSysLP.solvestat;
p_repyFullSysLP('modelstat') = fullSysLP.modelstat;
p_repyFullSysLP('resusd')    = fullSysLP.resusd;
p_repyFullSysLP('objval')    = fullSysLP.objval;
$endif.lp


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))

$ifthen.parallel "%PARALLEL%" == "TRUE"

solveParallel

$else.parallel

solve fullSysNLP minimizing v_totObj using nlp;

p_repyFullSysNLP(subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLP(subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLP(subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLP(subs,'objval')    = fullSysNLP.objval;

$endif.parallel

$ifthen.calibrationOptimization "%CALIBRATIONMETHOD%" == "optimization"

p_f(subs) = func

$endif.calibrationOptimization

$endif.nlp


*** scenario / calibration run -------------------------------------------------

$elseIfE.fullSys (sameas("%RUNTYPE%","calibration"))and(sameas("%CALIBRATIONMETHOD%","optimization"))

*********************************************************************************
*** Preparation of the calibration
*********************************************************************************

* measure stocks and flows in floor area
q("num") = no;
q("area") = yes;

$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
solve fullSysLP minimizing v_totObj using lp;
p_repyFullSysLP('solvestat') = fullSysLP.solvestat;
p_repyFullSysLP('modelstat') = fullSysLP.modelstat;
p_repyFullSysLP('resusd')    = fullSysLP.resusd;
p_repyFullSysLP('objval')    = fullSysLP.objval;
$endif.lp


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))

solveParallel

$endif.nlp

*** Compute the functional value
p_f(subs) = func

*** Store renovation and construction values
p_stock("area", state, vin, subs, t) = v_stock.l("area", state, vin, subs, t);
p_construction("area", state, subs, t) = v_construction.l("area", state, subs, t);

$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
p_renovationBS("area", state, bsr, vin, subs, t) = v_renovationBS.l("area", state, bsr, vin, subs, t);
p_renovationHS("area", state, hsr, vin, subs, t) = v_renovationHS.l("area", state, hsr, vin, subs, t);
$else.sequentialRen
p_renovation("area", state, stateFull, vin, subs, t) = v_renovation.l("area", state, stateFull, vin, subs, t);
$endIf.sequentialRen


*** Evaluate changes in the construction after price changes --------------------

p_xinitCon(state, subs, t) = p_specCostCon("intangible", state, subs, t);

* Compute the gradient for construction
loop((bs3, hs3, tcalib2),
  p_xDiffCon(bs, hs, subs, tcalib)$(gradientVarsCon(bs, hs, tcalib)
                                    and (not sameas(bs, bs3) or not sameas(hs, hs3) or not sameas(tcalib, tcalib2)))
                                    = 0;
  p_xDiffCon(bs, hs, subs, tcalib)$(gradientVarsCon(bs, hs, tcalib)
                                    and (sameas(bs, bs3) and sameas(hs, hs3) and sameas(tcalib, tcalib2)))
                                    = p_diff;
  p_specCostCalibCon(state, subs, tcalib) = p_xDiffCon(state, subs, tcalib);

  p_specCostCon("intangible", state, subs, t) = p_xinitCon(state, subs, t) + p_specCostCalibCon(state, subs, t);

  v_stock.l("area", state, vin, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vin, subs, ttot) = 0;

$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
  solve fullSysLP minimizing v_totObj using lp;
  p_repyFullSysLP('solvestat') = fullSysLP.solvestat;
  p_repyFullSysLP('modelstat') = fullSysLP.modelstat;
  p_repyFullSysLP('resusd')    = fullSysLP.resusd;
  p_repyFullSysLP('objval')    = fullSysLP.objval;
$endif.lp


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))

  solveParallel

$endif.nlp

  p_fDiffCon(bs3, hs3, subs, tcalib2) = func
);

*** Evaluate changes in the construction after price changes --------------------

$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"

p_xinitRenBS(state, bsr, vin, subs, t)$renAllowedBS(state, bsr) = p_specCostRenBS("intangible", state, bsr, vin, subs, t);
p_xinitRenHS(state, hsr, vin, subs, t)$renAllowedHS(state, hsr) = p_specCostRenHS("intangible", state, hsr, vin, subs, t);

* Compute the gradient for building shell renovation
loop(gradientVarsRenBS(bsr3, vin2, tcalib2),
  p_xDiffRenBS(bsr, vin, subs, tcalib)$(gradientVarsRenBS(bsr, vin, tcalib)
                                                            and (not sameas(bsr, bsr3)
                                                                 or not sameas(vin, vin2) or not sameas(tcalib, tcalib2)))
                                                          = 0;
  p_xDiffRenBS(bsr, vin, subs, tcalib)$(gradientVarsRenBS(bsr, vin, tcalib)
                                                            and (sameas(bsr, bsr3)
                                                                 and sameas(vin, vin2) and sameas(tcalib, tcalib2)))
                                                          = p_diff;
  p_specCostCalibRenBS(bs, hs, bsr, vin, subs, tcalib)$(vinCalib(tcalib, vin)) = p_xDiffRenBS(bsr, vin, subs, tcalib);

  p_specCostRenBS("intangible", renAllowedBS, vin, subs, t) = p_xinitRenBS(renAllowedBS, vin, subs, t) + p_specCostCalibRenBS(renAllowedBS, vin, subs, t);

  v_stock.l("area", state, vin, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovationBS.l("area", state, bsr, vin, subs, ttot) = 0;
  v_renovationHS.l("area", state, hsr, vin, subs, ttot) = 0;

$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
  solve fullSysLP minimizing v_totObj using lp;
  p_repyFullSysLP('solvestat') = fullSysLP.solvestat;
  p_repyFullSysLP('modelstat') = fullSysLP.modelstat;
  p_repyFullSysLP('resusd')    = fullSysLP.resusd;
  p_repyFullSysLP('objval')    = fullSysLP.objval;
$endif.lp


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))

  solveParallel

$endif.nlp

  p_fDiffRenBS(bsr3, vin2, subs, tcalib2) = func
);

* Compute the gradient for heating system renovation
loop(gradientVarsRenHS(renType2, hsr3, vin2, tcalib2),
  p_xDiffRenHS(renType, hsr, vin, subs, tcalib)$(gradientVarsRenHS(renType, hsr, vin, tcalib)
                                                            and (not sameas(renType, renType2)
                                                                 or not sameas(hsr, hsr3)
                                                                 or not sameas(vin, vin2) or not sameas(tcalib, tcalib2)))
                                                          = 0;
  p_xDiffRenHS(renType, hsr, vin, subs, tcalib)$(gradientVarsRenHS(renType, hsr, vin, tcalib)
                                                            and (sameas(renType, renType2)
                                                                 and sameas(hsr, hsr3)
                                                                 and sameas(vin, vin2) and sameas(tcalib, tcalib2)))
                                                          = p_diff;
  loop(renAllowedHS(bs, hs, hsr),
    p_specCostCalibRenHS(bs, hs, hsr, vin, subs, tcalib)$(vinCalib(tcalib, vin) and sameas(hs, hsr))
                                                                           = p_xDiffRenHS("identRepl", hsr, vin, subs, tcalib);
    p_specCostCalibRenHS(bs, hs, hsr, vin, subs, tcalib)$(vinCalib(tcalib, vin) and not sameas(hs, hsr) and not sameas(hsr, "0"))
                                                                           = p_xDiffRenHS("newSys", hsr, vin, subs, tcalib);
    p_specCostCalibRenHS(bs, hs, hsr, vin, subs, tcalib)$(vinCalib(tcalib, vin) and sameas(hsr, "0")) = p_xDiffRenHS("0", hsr, vin, subs, tcalib);
  );
  display p_xDiffRenHS, p_specCostCalibRenHS;

  p_specCostRenHS("intangible", renAllowedHS, vin, subs, t) = p_xinitRenHS(renAllowedHS, vin, subs, t) + p_specCostCalibRenHS(renAllowedHS, vin, subs, t);

  v_stock.l("area", state, vin, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovationBS.l("area", state, bsr, vin, subs, ttot) = 0;
  v_renovationHS.l("area", state, hsr, vin, subs, ttot) = 0;

$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
  solve fullSysLP minimizing v_totObj using lp;
  p_repyFullSysLP('solvestat') = fullSysLP.solvestat;
  p_repyFullSysLP('modelstat') = fullSysLP.modelstat;
  p_repyFullSysLP('resusd')    = fullSysLP.resusd;
  p_repyFullSysLP('objval')    = fullSysLP.objval;
$endif.lp


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))

  solveParallel

$endif.nlp

  p_fDiffRenHS(renType2, hsr3, vin2, subs, tcalib2) = func
);

$else.sequentialRen
p_xinitRen(state, stateFull, vin, subs, t)$renAllowed(state, stateFull) = p_specCostRen("intangible", state, stateFull, vin, subs, t);

* Compute the gradient
loop(gradientVarsRen(renType2, bsr3, hsr3, vin2, tcalib2),
  p_xDiffRen(renType, bsr, hsr, vin, subs, tcalib)$(gradientVarsRen(renType, bsr, hsr, vin, tcalib)
                                                            and (not sameas(renType, renType2)
                                                                 or not sameas(bsr, bsr3) or not sameas(hsr, hsr3)
                                                                 or not sameas(vin, vin2) or not sameas(tcalib, tcalib2)))
                                                          = 0;
  p_xDiffRen(renType, bsr, hsr, vin, subs, tcalib)$(gradientVarsRen(renType, bsr, hsr, vin, tcalib)
                                                            and (sameas(renType, renType2)
                                                                 and sameas(bsr, bsr3) and sameas(hsr, hsr3)
                                                                 and sameas(vin, vin2) and sameas(tcalib, tcalib2)))
                                                          = p_diff;
  loop(renAllowed(bs, hs, bsr, hsr),
    p_specCostCalibRen(bs, hs, bsr, hsr, vin, subs, tcalib)$(vinCalib(tcalib, vin) and sameas(hs, hsr))
                                                                           = p_xDiffRen("identRepl", bsr, hsr, vin, subs, tcalib);
    p_specCostCalibRen(bs, hs, bsr, hsr, vin, subs, tcalib)$(vinCalib(tcalib, vin) and not sameas(hs, hsr) and not sameas(hsr, "0"))
                                                                           = p_xDiffRen("newSys", bsr, hsr, vin, subs, tcalib);
    p_specCostCalibRen(bs, hs, bsr, hsr, vin, subs, tcalib)$(vinCalib(tcalib, vin) and sameas(hsr, "0")) = p_xDiffRen("0", bsr, hsr, vin, subs, tcalib);
  );
  display p_xDiffRen, p_specCostCalibRen;

  p_specCostRen("intangible", renAllowed, vin, subs, t) = p_xinitRen(renAllowed, vin, subs, t) + p_specCostCalibRen(renAllowed, vin, subs, t);

  v_stock.l("area", state, vin, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vin, subs, ttot) = 0;

$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
  solve fullSysLP minimizing v_totObj using lp;
  p_repyFullSysLP('solvestat') = fullSysLP.solvestat;
  p_repyFullSysLP('modelstat') = fullSysLP.modelstat;
  p_repyFullSysLP('resusd')    = fullSysLP.resusd;
  p_repyFullSysLP('objval')    = fullSysLP.objval;
$endif.lp


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))

  solveParallel

$endif.nlp

  p_fDiffRen(renType2, bsr3, hsr3, vin2, subs, tcalib2) = func
);

$endIf.sequentialRen

$endif.fullSys

*** matching run ---------------------------------------------------------------

$ifthen.matching "%RUNTYPE%" == "matching"

* measure stocks and flows in both floor area and number of dwellings
q(qty) = yes;

* measure stocks and flows in floor area
q("num") = no;
*q("area") = yes;

reg(region) = no;
subs(all_subs) = no;

loop(region,
  reg(region) = yes;
  subs(region,loc,typ,inc) = yes;

  solve matching minimizing v_matchingObj using qcp;

  reg(region) = no;
  subs(region,loc,typ,inc) = no;

  p_repyMatchingQCP(region, 'solvestat') = matching.solvestat;
  p_repyMatchingQCP(region, 'modelstat') = matching.modelstat;
  p_repyMatchingQCP(region, 'resusd')    = matching.resusd;
  p_repyMatchingQCP(region, 'objval')    = matching.objval;
);

reg(region) = yes;
subs(all_subs) = yes;

$endif.matching



*** renovation correction ------------------------------------------------------

$ifthen.renCorrect "%RUNTYPE%" == "renCorrect"

q(qty) = yes;
q("num") = no;

solve renCorrect minimizing v_renCorrectObj using qcp;

p_repyRenCorrectQCP('solvestat') = renCorrect.solvestat;
p_repyRenCorrectQCP('modelstat') = renCorrect.modelstat;
p_repyRenCorrectQCP('resusd')    = renCorrect.resusd;
p_repyRenCorrectQCP('objval')    = renCorrect.objval;

$endif.renCorrect


$onOrder
