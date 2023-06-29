*** fix variables for historic periods

$ifthen.matching not "%RUNTYPE%" == "matching"

v_stock.fx(qty,state,vin,subs,thist) = p_stockHist(qty,state,vin,subs,thist);
v_construction.fx(qty,state,subs,thist)             = 0;
v_renovation.fx(qty,state,stateFull,vin,subs,thist) = 0;
v_demolition.fx(qty,state,vin,subs,thist)           = 0;

$ifthen.history exist "history.gdx"
v_construction.fx(qty,state,subs,thist)             = p_constructionHist(qty,state,subs,thist);
v_renovation.fx(qty,state,stateFull,vin,subs,thist) = p_renovationHist(qty,state,stateFull,vin,subs,thist);
v_demolition.fx(qty,state,vin,subs,thist)           = p_demolitionHist(qty,state,vin,subs,thist);
$endif.history

$endif.matching


*** boiler ban

v_renovation.fx(qty,bs,hs,bsr,hs2,vin,reg,loc,typ,inc,t)$hsBan("renovation",reg,t,hs2) = 0;
v_construction.fx(qty,bs,hs,reg,loc,typ,inc,t)$hsBan("construction",reg,t,hs) = 0;
v_stock.fx(qty,bs,hs,vin,reg,loc,typ,inc,t)$hsBan("stock",reg,t,hs) = 0;
