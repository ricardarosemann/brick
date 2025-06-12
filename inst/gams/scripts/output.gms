*** compute right-hand side of heating system lifetime inequality
p_rhsLifetimHS(q,hs,vin,subs(reg,loc,typ,inc),ttot)$(    vinExists(ttot,vin)
                                                            and t(ttot))
= sum(bsr,
  sum(ttot2$(    ttot2.val le ttot.val
              !!and p_shareRenHS(hs,reg,typ,ttot2 + 1,ttot) < 1
              and vinExists(ttot2,vin)),
    p_shareRenHS(hs,reg,typ,ttot2,ttot)
    * (
      sum(bs(bsr),
          v_construction.l(q,bs,hs,subs,ttot2))
      * p_dtVin(ttot2,vin)
      +
      sum(state$renAllowed(state,bsr,hs),
          v_renovation.l(q,state,bsr,hs,vin,subs,ttot2))
      * p_dt(ttot2)
    )
    +
    p_shareRenHSinit(hs,reg,typ,ttot2,ttot)
    * sum(bs(bsr), v_stock.l(q,bs,hs,vin,subs,ttot2)$(tinit(ttot2)))
  )
);

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