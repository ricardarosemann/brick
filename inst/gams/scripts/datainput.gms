*** load parameter values ------------------------------------------------------

$gdxin input.gdx
$load p_dt p_dtVin t0
$load p_specCostCon p_specCostDem
$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
$load p_specCostRenBS p_specCostRenHS
$else.sequentialRen
$load p_specCostRen
$endif.sequentialRen
$load priceSensBS priceSensHS p_statusQuoPref
$load p_carbonPrice p_carrierPrice p_carrierEmi p_ueDemand p_eff p_renDepth
$load p_discountRate
$load p_population
$load p_stockHist
$load p_shareDem p_shareRenBS p_shareRenHS p_shareRenBSinit p_shareRenHSinit
$load p_floorPerCap
$load p_probDem p_lifeTimeBS p_lifeTimeHS
$gdxin

$ifthen.matching "%RUNTYPE%" == "matching"
$gdxin references.gdx
$load p_refVals p_refValsMed p_refWeight
$gdxin
$endif.matching

$ifthen.renCorrect "%RUNTYPE%" == "renCorrect"
$gdxin input.gdx
$load p_stock p_construction p_demolition
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
$load p_renovationBS p_renovationHS
$else.sequentialRen
$load p_renovation
$endif.sequentialRen
$gdxin
$endif.renCorrect

$ifthenE.calibration (sameas("%CALIBRATIONMETHOD%","optimization"))or(sameas("%CALIBRATIONMETHOD%","logit"))
$gdxin input.gdx
$load p_stockCalibTarget p_constructionCalibTarget p_demolitionCalibTarget
$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
$load p_renovationBSCalibTarget p_renovationHSCalibTarget
$else.sequentialRen
$load p_renovationCalibTarget
$endIf.sequentialRen
$gdxin
$endif.calibration

$ifthenE.calibrationOptimization (sameas("%RUNTYPE%","calibration"))and(sameas("%CALIBRATIONMETHOD%","optimization"))
$gdxin input.gdx
$load p_diff
$gdxin
$endif.calibrationOptimization

$ifThen.lowop not "%CALIBRATIONLOWOP%" == "FALSE"
$gdxin input.gdx
$load p_specCostOpe
display "Reading operational costs from input.gdx";
$gdxin
$endIf.lowop



*** starting point -------------------------------------------------------------

v_stock.l(qty,state,vin,subs,thist)$vinExists(thist,vin) =
  p_stockHist(qty,state,vin,subs,thist);
$if exist "start.gdx" execute_loadpoint "start";



*** history --------------------------------------------------------------------

$ifthen.history exist "history.gdx"
execute_loadpoint "history",
  p_stockHist        = v_stock.l
  p_constructionHist = v_construction.l
  p_renovationHist   = v_renovation.l
  p_renovationHistBS = v_renovationBS.l
  p_renovationHistHS = v_renovationHS.l
  p_demolitionHist   = v_demolition.l
  p_carbonPrice
  p_carrierEmi
  p_carrierPrice
  p_eff
  p_floorPerCap
  p_discountRate
  p_population
  p_probDem
  p_shareDem
  p_specCostCon
  p_specCostOpe
  p_specCostRen;
$endif.history



*** derived parameters ---------------------------------------------------------

* floor-space specific final energy demand
p_feDemand(hs,bs,vin,reg,typ,ttot) =
  p_ueDemand(bs,vin,reg,typ)
  / p_eff(hs,reg,typ,ttot)
;

* renovation cost (hierarchical renovation)
$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
p_specCostRen(cost,state,bsr,hsr,vin,subs,ttot) =
    p_specCostRenBS(cost,state,bsr,vin,subs,ttot)
  + p_specCostRenHS(cost,state,hsr,vin,subs,ttot)
;
$endIf.sequentialRen

$ifThen.lowop "%CALIBRATIONLOWOP%" == "FALSE"
* floor-space specific operation cost
p_specCostOpe(bs,hs,vin,reg,loc,typ,ttot) =
  p_feDemand(hs,bs,vin,reg,typ,ttot)
  * sum(hsCarrier(hs,carrier),
      p_carrierPrice(carrier,reg,ttot)
      + p_carbonPrice(carrier,ttot) * p_carrierEmi(carrier,reg,ttot)
    ) * 1.2 !! coarse accounting for VAT
;
display "Compute operational costs in GAMS code";
$endIf.lowop

* discount factor
p_discountFac(typ,ttot) =
  1 / (1 + p_discountRate(typ,ttot))**(ttot.val - p_dt(ttot) / 2 - t0)
;

* LCC of housing related to a construction decision under various assumptions:
* - discounted to time of construction
* - expectation value considering distributed lifetime of the building
* - future renovation is driven by technology life time
* - future renovation only maintains the constructed state
* - individual renovations for building shell and heating system respectively
* - average across relevant vintages

p_lccCon(cost,var,bs,hs,reg,loc,typ,inc,ttot) =
  sum(ttot2$(ttot2.val ge ttot.val),
    p_discountFac(typ,ttot2) / p_discountFac(typ,ttot)
    *
    (
      p_specCostCon(cost,bs,hs,reg,loc,typ,inc,ttot2)$(    sameas(var,"construction")
                                                     and sameas(ttot,ttot2))
      + sum(vin$vinExists(ttot2,vin),
          p_dtVin(ttot,vin)
          / p_dt(ttot)
          * (
              p_specCostOpe(bs,hs,vin,reg,loc,typ,ttot)$(    sameas(var,"stock")
                                                       and sameas(cost,"tangible"))
            + p_specCostRen(cost,bs,hs,bs,"0",vin,reg,loc,typ,inc,ttot)$sameas(var,"renovation")
              / p_lifeTimeBS(reg)
            + p_specCostRen(cost,bs,hs,"0",hs,vin,reg,loc,typ,inc,ttot)$sameas(var,"renovation")
              / p_lifeTimeHS(hs,reg,typ)
          )
          * (1 - p_probDem(reg,typ,ttot2,ttot))
      ) * p_dt(ttot2)
      + p_specCostDem$(    sameas(var,"demolition")
                       and sameas(cost,"tangible"))
        * (p_probDem(reg,typ,ttot2,ttot) - p_probDem(reg,typ,ttot2-1,ttot))
    )
  )
;



*** temp -----------------------------------------------------------------------



$ifthen.matching "%RUNTYPE%" == "matching"



p_flowVariationWeight = 7E3;
p_slackRenWeight = 1E4;

* p_refValsMed(reference,reg) = 1;  !! TODO: remove or rewrite normalisation of references

$endif.matching
