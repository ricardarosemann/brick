*** load parameter values ------------------------------------------------------

$gdxin input.gdx
$load p_dt p_dtVin t0
$load vinExists
$load p_specCostCon p_specCostRen p_specCostOpe p_specCostDem
$load p_discountFac
$load p_population p_stockHist
$load p_shareDem p_shareRenBS p_shareRenHS p_shareRenBSinit p_shareRenHSinit
$load p_floorPerCap
$gdxin

$ifthen.matching "%RUNTYPE%" == "matching"
$gdxin references.gdx
$load p_refVals p_refValsMed
$gdxin
$endif.matching


*** starting point -------------------------------------------------------------

v_stock.l("area",state,vin,subs,ttot)$vinExists(ttot,vin) = p_stockHist(state,vin,subs,ttot);
$if exist "start.gdx" execute_loadpoint "start";


*** temp

* temporary fix assuming a heating system life time of 20 years
p_refVals(refVarExists("EuropeanCommissionRenovation",refVar,reg,t)) = 0.05;


p_refWeight(ref,reg,t) = 1;
*p_refWeight("EUBDB_stock",reg,t) = 0.2;
p_refWeight("mredgebuildings_heating",reg,t) = 0.0002;
p_refWeight("mredgebuildings_location",reg,t) = 0.001;
p_refWeight("mredgebuildings_buildingType",reg,t) = 0.001;
p_refWeight("Odyssee_constructionFloor",reg,t) = 5;
p_refWeight("Odyssee_heatingShare",reg,t) = 500;
p_refWeight("IDEES_heatingShare",reg,t) = 500;
p_refWeight("EuropeanCommissionRenovation",reg,t) = 500;

p_flowVariationWeight = 1E-5;
p_flowVariationWeight = 0;

p_refValsMed(ref,reg) = 1;