*** fix variables for historic periods

$ifthen.matching "%RUNTYPE%" == "calibration"
v_stock.fx("area",state,vin,subs,thist) = p_stockHist(state,vin,subs,thist);
v_construction.fx(qty,state,subs,thist)             = 0;
v_renovation.fx(qty,state,stateFull,vin,subs,thist) = 0;
v_demolition.fx(qty,state,vin,subs,thist)           = 0;
$endif.matching
