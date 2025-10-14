*** check for unwanted variable values
* ErrStock(state,vin,subs,ttot)$(    not(vinExists(ttot,vin))
*                                and sum(q, v_stock.l(q,state,vin,subs,ttot) > 0)) = yes;

* ErrConstruction(state,subs,ttot) = no;


* $ifthen.sequentialRen  "%SEQUENTIALREN%" == "TRUE"
* ErrRenovationBS(state,bsr,vin,subs,ttot)$(    (   not(vinExists(ttot,vin))
*                                                or not(renAllowedBS(state,bsr)))
*                                           and sum(q, v_renovationBS.l(q,state,bsr,vin,subs,ttot) > 0)) = yes;
* ErrRenovationHS(state,hsr,vin,subs,ttot)$(    (   not(vinExists(ttot,vin))
*                                                or not(renAllowedHS(state,hsr)))
*                                           and sum(q, v_renovationHS.l(q,state,hsr,vin,subs,ttot) > 0)) = yes;
* $else.sequentialRen
* ErrRenovation(state,stateFull,vin,subs,ttot)$(    (   not(vinExists(ttot,vin))
*                                                    or not(renAllowed(state,stateFull)))
*                                               and sum(q, v_renovation.l(q,state,stateFull,vin,subs,ttot) > 0)) = yes;
* $endif.sequentialRen
* ErrDemolition(state,vin,subs,ttot)$(    not(vinExists(ttot,vin))
*                                     and sum(q, v_demolition.l(q,state,vin,subs,ttot) > 0)) = yes;


* if(card(ErrStock) + card(ErrConstruction) + card(ErrRenovation) + card(ErrRenovationBS) + card(ErrRenovationHS) + card(ErrDemolition) > 0,
*   execute_unload "abort.gdx";
*   abort "Variable entries that should not exist are greater zero. abort.gdx written";
* );

$ifThen.renCorrect "%RUNTYPE%" == "renCorrect"
p_stockDiff("area",state,vin,subs,ttot) = v_stock.l("area",state,vin,subs,ttot) - p_stock("area",state,vin,subs,ttot);
p_constructionDiff("area",state,subs,ttot) = v_construction.l("area",state,subs,ttot) - p_construction("area",state,subs,ttot);
p_demolitionDiff("area",state,vin,subs,ttot) = v_demolition.l("area",state,vin,subs,ttot) - p_demolition("area",state,vin,subs,ttot);
$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
p_renovationBSDiff("area",renAllowedBS,vin,subs,t) = v_renovationBS.l("area",renAllowedBS,vin,subs,t)
                                                      - p_renovationBS("area",renAllowedBS,vin,subs,t);
p_renovationHSDiff("area",renAllowedHS,vin,subs,t) = v_renovationHS.l("area",renAllowedHS,vin,subs,t)
                                                      - p_renovationHS("area",renAllowedHS,vin,subs,t);
$else.sequentialRen
p_renovationDiff("area",renAllowed,vin,subs,t) = v_renovation.l("area",renAllowed,vin,subs,t)
                                                  - p_renovation("area",renAllowed,vin,subs,t);
$endIf.sequentialRen
$endIf.renCorrect



*** write results
execute_unload "output.gdx";
