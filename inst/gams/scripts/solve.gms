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



*** scenario / calibration run -------------------------------------------------

$ifthenE.fullSys (sameas("%RUNTYPE%","scenario"))or(sameas("%RUNTYPE%","calibration"))

* measure stocks and flows in floor area
q("dwel") = no;
q("area") = yes;


$ifthen.calibration "%RUNTYPE%" == "calibration"

* start iteration loop
loop(iteration,

$endif.calibration


* linear model
$ifthen.calibration "%RUNTYPE%" == "calibration"
if(iteration.val eq 1,
$endif.calibration

$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
solve fullSysLP minimizing v_totObj using lp;
p_repyFullSysLP('solvestat') = fullSysLP.solvestat;
p_repyFullSysLP('modelstat') = fullSysLP.modelstat;
p_repyFullSysLP('resusd')    = fullSysLP.resusd;
p_repyFullSysLP('objval')    = fullSysLP.objval;
$endif.lp

$ifthen.calibration "%RUNTYPE%" == "calibration"
);
$endif.calibration


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))or(sameas("%RUNTYPE%","calibration"))

$ifthen.parallel "%PARALLEL%" == "TRUE"

subs(all_subs) = no;
fullSysNLP.SolveLink = 3;

loop(all_subs,
  subs(all_subs) = yes;
  solve fullSysNLP minimizing v_totObj using nlp;

  subs(all_subs) = no;
  p_handle(all_subs) = fullSysNLP.handle;
);

repeat
  loop(all_subs$handleCollect(p_handle(all_subs)),
		p_runtime(all_subs) = fullSysNLP.resusd;

  p_repyFullSysNLP(all_subs,'solvestat') = fullSysNLP.solvestat;
  p_repyFullSysNLP(all_subs,'modelstat') = fullSysNLP.modelstat;
  p_repyFullSysNLP(all_subs,'resusd')    = fullSysNLP.resusd;
  p_repyFullSysNLP(all_subs,'objval')    = fullSysNLP.objval;

    if(handleStatus(p_handle(all_subs)),
      fullSysNLP.handle = p_handle(all_subs);
      display$handleDelete(p_handle(all_subs)) 'trouble deleting handles' ;
      p_handle(all_subs) = 0;
    );
  );
  display$sleep(5) 'sleep some time';
until card(p_handle) = 0;

subs(all_subs) = yes;

$else.parallel

solve fullSysNLP minimizing v_totObj using nlp;

p_repyFullSysNLP(subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLP(subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLP(subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLP(subs,'objval')    = fullSysNLP.objval;

$endif.parallel



$endif.nlp


$ifthen.calibration "%RUNTYPE%" == "calibration"

p_repyFullSysNLPIter(iteration,all_subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLPIter(iteration,all_subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLPIter(iteration,all_subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLPIter(iteration,all_subs,'objval')    = fullSysNLP.objval;

*** check calibration deviation
p_calibDeviationCon(iteration,state,subs,t)$(abs(p_constructionHist(state,subs,t)) > eps) =
  v_construction.l("area",state,subs,t)
  / p_constructionHist(state,subs,t)
;
p_calibDeviationRen(iteration,ren,vin,subs,t)$(abs(p_renovationHist(ren,vin,subs,t)) > eps) =
  v_renovation.l("area",ren,vin,subs,t)
  / p_renovationHist(ren,vin,subs,t)
;

*** update intangible cost

* finite deviation
p_specCostCon("intangible",state,subs,t)$(    p_constructionHist(state,subs,t) > eps
                                          and p_calibDeviationCon(iteration,state,subs,t) > eps) =
  p_specCostCon("intangible",state,subs,t)
  +
  p_calibSpeed("construction")
  * log(p_calibDeviationCon(iteration,state,subs,t))
;

p_specCostRen("intangible",ren,vin,subs,t)$(    p_renovationHist(ren,vin,subs,t) > eps
                                            and p_calibDeviationRen(iteration,ren,vin,subs,t) > eps) =
  p_specCostRen("intangible",ren,vin,subs,t)
  +
  p_calibSpeed("renovation")
  * log(p_calibDeviationRen(iteration,ren,vin,subs,t))
;

* zero targets or zero acual value-
p_specCostCon("intangible",state,subs,t)$(    (p_constructionHist(state,subs,t) <= eps)
                                          xor (v_construction.l("area",state,subs,t) <= eps))
  =
  p_specCostCon("intangible",state,subs,t)
  + sign(p_calibDeviationCon(iteration,state,subs,t) - 1)
  * (
      0.5 * abs(p_specCostCon("intangible",state,subs,t))
      + 0.1 * p_specCostCon("tangible",state,subs,t)$(abs(p_specCostCon("intangible",state,subs,t)) <= eps)
    )
;

p_specCostRen("intangible",ren,vin,subs,t)$(    (p_renovationHist(ren,vin,subs,t) <= eps)
                                            xor (v_renovation.l("area",ren,vin,subs,t) <= eps)) =
  p_specCostRen("intangible",ren,vin,subs,t)
  + sign(p_calibDeviationRen(iteration,ren,vin,subs,t) - 1)
  * (
      0.5 * abs(p_specCostRen("intangible",ren,vin,subs,t))
      + 0.1 * p_specCostRen("tangible",ren,vin,subs,t)$(abs(p_specCostRen("intangible",ren,vin,subs,t)) <= eps)
    )
;


*** end iteration loop
);

$endif.calibration


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
