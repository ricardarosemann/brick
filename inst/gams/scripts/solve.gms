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


model matching "find stock and flows that best match reference sources"
  /
  q_matchingObj
  q_refDeviationTot
  q_refDeviationVar
  q_refDeviation
  q_stockBalNext
  q_stockBalPrev
*  q_dwelSizeStock
*  q_dwelSizeConstruction
*  q_dwelSize_Odyssee
  q_renRate_EuropeanCommissionRenovation
  q_heatingShare_Odyssee
  q_heatingShare_IDEES
*  q_vinShare_EUBDB
*  q_flowVariation
*  q_flowVariationTot
  /
;



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



*** scenario run ---------------------------------------------------------------

$ifthen.scenario "%RUNTYPE%" == "scenario"

* measure stocks and flows in floor area
q("dwel") = no;
q("area") = yes;

* linear model
$ifthenE.lp (sameas("%SOLVEPROBLEM%","lp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
solve fullSysLP minimizing v_totSysCost using lp;
p_repyFullSysLP('solvestat') = fullSysLP.solvestat;
p_repyFullSysLP('modelstat') = fullSysLP.modelstat;
p_repyFullSysLP('resusd')    = fullSysLP.resusd;
p_repyFullSysLP('objval')    = fullSysLP.objval;
$endif.lp


* non-linear model
$ifthenE.nlp (sameas("%SOLVEPROBLEM%","nlp"))or(sameas("%SOLVEPROBLEM%","lpnlp"))
$ifthen.parallel "%PARALLEL%" == "TRUE"
subs(all_subs) = no;
fullSysNLP.SolveLink = 3;

loop(all_subs,
  subs(all_subs) = yes;

  solve fullSysNLP minimizing v_totSysCost using nlp;

  p_repyFullSysNLP(subs,'solvestat') = fullSysNLP.solvestat;
  p_repyFullSysNLP(subs,'modelstat') = fullSysNLP.modelstat;
  p_repyFullSysNLP(subs,'resusd')    = fullSysNLP.resusd;
  p_repyFullSysNLP(subs,'objval')    = fullSysNLP.objval;

  subs(all_subs) = no;

  p_handle(all_subs) = fullSysNLP.handle;
);

repeat
  loop(all_subs$handleCollect(p_handle(all_subs)),
		p_runtime(all_subs) = fullSysNLP.resusd;
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
solve fullSysNLP minimizing v_totSysCost using nlp;
p_repyFullSysNLP(subs,'solvestat') = fullSysNLP.solvestat;
p_repyFullSysNLP(subs,'modelstat') = fullSysNLP.modelstat;
p_repyFullSysNLP(subs,'resusd')    = fullSysNLP.resusd;
p_repyFullSysNLP(subs,'objval')    = fullSysNLP.objval;
$endif.parallel

$endif.nlp

$endif.scenario



*** calibration run ------------------------------------------------------------

$ifthen.calibration "%RUNTYPE%" == "calibration"

*** find historic flows that match given stock

p_constructionHist(state,subs,ttot) = v_construction.l("area",state,subs,ttot);
p_renovationHist(ren,vin,subs,ttot) = v_renovation.l("area",ren,vin,subs,ttot);
p_demolitionHist(state,vin,subs,ttot) = v_demolition.l("area",state,vin,subs,ttot);


$endif.calibration



*** matching run ---------------------------------------------------------------

$ifthen.matching "%RUNTYPE%" == "matching"

* measure stocks and flows in both floor area and number of dwellings
q(qty) = yes;

* measure stocks and flows in floor area
q("dwel") = no;
q("area") = yes;

solve matching minimizing v_matchingObj using qcp;

$endif.matching
