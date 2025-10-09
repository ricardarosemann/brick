*** fix variables for historic periods

$ifthenE.notMatching (sameas("%RUNTYPE%","scenario"))or(sameas("%RUNTYPE%","calibration"))

v_stock.fx(qty,state,vin,subs,thist)$vinExists(thist,vin) = p_stockHist(qty,state,vin,subs,thist);
v_construction.fx(qty,state,subs,thist)                                  = 0;
v_demolition.fx(qty,state,vin,subs,thist)$vinExists(thist,vin)           = 0;
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
v_renovationBS.fx(qty,state,bsr,vin,subs,thist)$vinExists(thist,vin) = 0;
v_renovationHS.fx(qty,state,hsr,vin,subs,thist)$vinExists(thist,vin) = 0;
$else.sequentialRen
v_renovation.fx(qty,renAllowed,vin,subs,thist)$vinExists(thist,vin) = 0;
$endif.sequentialRen

$ifthen.history exist "history.gdx"
v_construction.fx(qty,state,subs,thist)                                  = p_constructionHist(qty,state,subs,thist);
v_demolition.fx(qty,state,vin,subs,thist)$vinExists(thist,vin)           = p_demolitionHist(qty,state,vin,subs,thist);
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
v_renovationBS.fx(qty,renAllowedBS,vin,subs,thist)$vinExists(thist,vin) = p_renovationBSHist(qty,renAllowedBS,vin,subs,thist);
v_renovationHS.fx(qty,renAllowedHS,vin,subs,thist)$vinExists(thist,vin) = p_renovationHSHist(qty,renAllowedHS,vin,subs,thist);
$else.sequentialRen
v_renovation.fx(qty,renAllowed,vin,subs,thist)$vinExists(thist,vin) = p_renovationHist(qty,renAllowed,vin,subs,thist);
$endif.sequentialRen
$endif.history

$endif.notMatching


$ifthenE.calibration (sameas("%CALIBRATIONMETHOD%","optimization"))or(sameas("%CALIBRATIONMETHOD%","logit"))
v_stock.fx(qty,state,vin,subs,thist)$vinExists(thist,vin) = p_stockCalibTarget(qty,state,vin,subs,thist);
v_construction.fx(qty,bs,hs,region,loc,typ,inc,thist) = p_constructionCalibTarget(qty,bs,hs,region,loc,typ,inc,thist);
v_demolition.fx(qty,state,vin,subs,thist) = p_demolitionCalibTarget(qty,state,vin,subs,thist);
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
v_renovationBS.fx(qty,state,bsr,vin,subs,thist) = p_renovationBSCalibTarget(qty,state,bsr,vin,subs,thist);
v_renovationHS.fx(qty,state,hsr,vin,subs,thist) = p_renovationHSCalibTarget(qty,state,hsr,vin,subs,thist);
$else.sequentialRen
v_renovation.fx(qty,renAllowed,vin,subs,thist) = p_renovationCalibTarget(qty,renAllowed,vin,subs,thist);
$endif.sequentialRen
$endif.calibration


*** building shell and heating system replacement

$ifthen.notMatching not "%RUNTYPE%" == "matching"
* technologies have to be replaced at least as much as life time requires
v_slackRenBS.lo(bs,vin,subs,ttot) = 0;
v_slackRenHS.lo(hs,vin,subs,ttot) = 0;
$endif.notMatching


*** boiler ban

v_renovation.fx(qty,bs,hs,bsr,hs2,vin,region,loc,typ,inc,t)$(hsBan("renovation",region,t,hs2) and vinExists(t,vin)) = 0;
v_construction.fx(qty,bs,hs,region,loc,typ,inc,t)$hsBan("construction",region,t,hs) = 0;
v_stock.fx(qty,bs,hs,vin,region,loc,typ,inc,t)$(hsBan("stock",region,t,hs) and vinExists(t,vin)) = 0;


*** fixed buildings

$ifthen.fixedBuildings "%FIXEDBUILDINGS%" == "TRUE"
v_construction.fx(qty,bs,hs,region,loc,typ,inc,ttot) = 0;
v_demolition.fx(qty,bs,hs,vin,region,loc,typ,inc,ttot) = 0;
$endif.fixedBuildings


*** renovation correction

$ifthen.renCorrect "%RUNTYPE%" == "renCorrect"
v_stock.fx(qty,bs,hs,vin,region,loc,typ,inc,thist) = p_stock(qty,bs,hs,vin,region,loc,typ,inc,thist);
v_construction.fx(qty,bs,hs,region,loc,typ,inc,thist) = p_construction(qty,bs,hs,region,loc,typ,inc,thist);
v_demolition.fx(qty,state,vin,subs,thist) = p_demolition(qty,state,vin,subs,thist);
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
v_renovationBS.fx(qty,state,bsr,vin,subs,thist) = p_renovationBS(qty,state,bsr,vin,subs,thist);
v_renovationHS.fx(qty,state,hsr,vin,subs,thist) = p_renovationHS(qty,state,hsr,vin,subs,thist);
$else.sequentialRen
v_renovation.fx(qty,renAllowed,vin,subs,thist) = p_renovation(qty,renAllowed,vin,subs,thist);
$endif.sequentialRen
$endif.renCorrect


*** temp

v_slackRenBS.lo(bs,vin,subs,ttot) = 0;
v_slackRenHS.lo(hs,vin,subs,ttot) = 0;
