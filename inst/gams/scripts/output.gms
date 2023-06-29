*** check for unwanted variable values
ErrStock(state,vin,subs,ttot)$(    not(vinExists(ttot,vin))
                               and sum(q, v_stock.l(q,state,vin,subs,ttot) > 0)) = yes;

ErrConstruction(state,subs,ttot) = no;

ErrRenovation(state,stateFull,vin,subs,ttot)$(    (   not(vinExists(ttot,vin))
                                                   or not(renAllowed(state,stateFull)))
                                              and sum(q, v_renovation.l(q,state,stateFull,vin,subs,ttot) > 0)) = yes;

ErrDemolition(state,vin,subs,ttot)$(    not(vinExists(ttot,vin))
                                    and sum(q, v_demolition.l(q,state,vin,subs,ttot) > 0)) = yes;


if(card(ErrStock) + card(ErrConstruction) + card(ErrRenovation) + card(ErrDemolition) > 0,
  execute_unload "abort.gdx";
  abort "Variable entries that should not exist are greater zero. abort.gdx written";
);



*** write results
execute_unload "output.gdx";