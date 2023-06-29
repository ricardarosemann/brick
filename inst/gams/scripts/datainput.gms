*** load parameter values ------------------------------------------------------

$gdxin input.gdx
$load p_dt p_dtVin t0
$load vinExists
$load p_specCostCon p_specCostRen p_specCostOpe p_specCostDem
$load p_discountFac
$load p_population
$ifthen.noCalib not "%RUNTYPE%" == "calibration"
$load p_stockHist
$endif.noCalib
$load p_shareDem p_shareRenBS p_shareRenHS p_shareRenBSinit p_shareRenHSinit
$load p_floorPerCap
$gdxin

$ifthen.matching "%RUNTYPE%" == "matching"
$gdxin references.gdx
$load p_refVals p_refValsMed
$gdxin
$endif.matching

$ifthen.calibration "%RUNTYPE%" == "calibration"
$gdxin calibrationTarget.gdx
$load p_stockHist p_constructionHist p_renovationHist p_demolitionHist
$gdxin
$endif.calibration


*** starting point -------------------------------------------------------------

v_stock.l(qty,state,vin,subs,ttot)$vinExists(ttot,vin) =
  p_stockHist(qty,state,vin,subs,ttot);
$if exist "start.gdx" execute_loadpoint "start";



*** history --------------------------------------------------------------------

$ifthen.history exist "history.gdx"
execute_load "history", p_stockHist =        v_stock.l
                        p_constructionHist = v_construction.l
                        p_renovationHist =   v_renovation.l
                        p_demolitionHist =   v_demolition.l
;
$endif.history



*** temp -----------------------------------------------------------------------

* calibration speed
p_calibSpeed("construction") = 1;
p_calibSpeed("renovation") = 1;

$ifthen.matching "%RUNTYPE%" == "matching"

* temporary fix assuming a heating system life time of 20 years
p_refVals(refVarExists("EuropeanCommissionRenovation",refVar,reg,t)) = 0.05;


p_refWeight(ref,reg,t) = 1;
*p_refWeight("EUBDB_stock",reg,t) = 0.2;
p_refWeight("mredgebuildings_heating",reg,t) = 2E-5;
p_refWeight("mredgebuildings_location",reg,t) = 1E-4;
p_refWeight("mredgebuildings_buildingType",reg,t) = 1E-4;
p_refWeight("Odyssee_constructionFloor",reg,t) = 1E-1;
p_refWeight("Odyssee_heatingShare",reg,t) = 5E1;
p_refWeight("IDEES_heatingShare",reg,t) = 1E2;
p_refWeight("EUBDB_vintage",reg,t) = 5E1;
p_refWeight("EuropeanCommissionRenovation",reg,t) = 1E2;

p_flowVariationWeight = 1E-5;
p_flowVariationWeight = 0;

p_refValsMed(ref,reg) = 1;  !! TODO: remove or rewrite normalisation of references

$endif.matching
