*** total system cost ----------------------------------------------------------

* objective function: full system cost with benefit for heterogeneity in
* contruction and renovation choices discounted to t0

q_totObj..
  v_totObj
  =e=
  sum(subs,
    v_obj(subs)
  )
;


*** system cost for each subset ------------------------------------------------

* sum all cost components and benefit for heterogeneity in contruction and
* renovation choices and discount them to t0 for each subset

q_obj(subs(reg,loc,typ,inc))..
  v_obj(subs)
  =e=
  sum(t,
    p_discountFac(typ,t)
    * p_dt(t)
    * (  v_sysCost(subs,t)
       + v_sysHeteroPref(subs,t)
       + v_statusQuoPref(subs,t))
  )
;

q_sysCost(subs,t)..
  v_sysCost(subs,t)
  =e=
    v_conCost(subs,t)
  + v_renCost(subs,t)
  + v_opeCost(subs,t)
  + v_demCost(subs,t)
;

q_sysHeteroPref(subs,t)..
  v_sysHeteroPref(subs,t)
  =e=
    v_heteroPrefCon(subs,t)
  + v_heteroPrefRen(subs,t)
;


*** construction cost ----------------------------------------------------------

* calculate cash flow of construction cost with area-specific cost

q_conCost(subs,t)..
  v_conCost(subs,t)
  =e=
  sum(state,
    v_construction("area",state,subs,t)
    * sum(cost,
        p_specCostCon(cost,state,subs,t)
      )
  )
;


*** renovation cost ------------------------------------------------------------

* calculate cash flow of renovation cost with area-specific cost

q_renCost(subs,t)..
  v_renCost(subs,t)
  =e=
  sum(vin$vinExists(t,vin),
$ifthen.sequentialRen  "%SEQUENTIALREN%" == "TRUE" !! TODO: this might be generalisable
    sum(cost,
      sum(renAllowedBS,
        v_renovationBS("area",renAllowedBS,vin,subs,t)
        * p_specCostRenBS(cost,renAllowedBS,vin,subs,t)
      )
      +
      sum(renAllowedHS,
        v_renovationHS("area",renAllowedHS,vin,subs,t)
        * p_specCostRenHS(cost,renAllowedHS,vin,subs,t)
      )
    )
$else.sequentialRen
    sum((renAllowed, cost),
      v_renovation("area",renAllowed,vin,subs,t)
      * p_specCostRen(cost,renAllowed,vin,subs,t)
    )
$endif.sequentialRen
  )
;


*** operation cost -------------------------------------------------------------

* calculate cash flow of operation cost with area-specific cost

* we assume a linear transition from the previous to the current stock and
* therefore take the average stock between the two for the operation cost

q_opeCost(subs(reg,loc,typ,inc),ttot)$(t(ttot))..
  v_opeCost(subs,ttot)
  =e=
  sum((state,vinExists(ttot,vin)),
    sum(ttot2$((sameas(ttot2,ttot) or sameas(ttot2,ttot-1)) and vinExists(ttot2,vin)),
      v_stock("area",state,vin,subs,ttot2)
    ) / 2
    * p_specCostOpe(state,vin,reg,loc,typ,ttot)
  )
;


*** demolition cost ------------------------------------------------------------

* calculate cash flow of demolition cost with area-specific cost

q_demCost(subs,t)..
  v_demCost(subs,t)
  =e=
  sum(state, sum(vin$vinExists(t,vin),
    v_demolition("area",state,vin,subs,t)
    * p_specCostDem
  ))
;


*** heterogeneity preference ---------------------------------------------------

* heterogeneity of the choice alternatives
* (reaches maximum for a given total quantity with equal shares of alternatives)

* construction
q_heteroPrefCon(subs,t)..
  v_heteroPrefCon(subs,t)
  =e=
  1 / priceSensBS("construction", subs)
  * sum(bs,
      sum(hs, v_construction("area",bs,hs,subs,t))
      * (
        log(
          sum(hs,
            v_construction("area",bs,hs,subs,t)
          )
          + epsilon
        )
        - 1
      )
    )
  + 1 / priceSensHS("construction", subs)
  * (
    sum(bs,
      sum(hs,
        v_construction("area",bs,hs,subs,t)
        * (
          log(
            v_construction("area",bs,hs,subs,t)
            + epsilon
          )
        - 1
        )
      )
    )
    - sum(bs,
        sum(hs,
          v_construction("area",bs,hs,subs,t)
        )
        * (
          log(
            sum(hs,
              v_construction("area",bs,hs,subs,t)
            )
            + epsilon
          )
          - 1
        )
      )
  )
;

* renovation

* summing renovation entropies for each initial state over those states would
* also benefits heterogeneity in the initial state. To avoid this, we subtract
* the heterogeneity in the inital state.
q_heteroPrefRen(subs,t)..
  v_heteroPrefRen(subs,t)
  =e=
  sum(vinExists(t,vin),
      (sum(state, v_entropyRenToBS(state,vin,subs,t)) - v_entropyRenFromBS(vin,subs,t)) / priceSensBS("renovation",subs)
    + (sum(state, v_entropyRenToHS(state,vin,subs,t)) - v_entropyRenFromHS(vin,subs,t)) / priceSensHS("renovation",subs)
  )
;


q_entropyRenToBS(state,vin,subs,t)$vinExists(t,vin)..
  v_entropyRenToBS(state,vin,subs,t)
  =e=
  sum(renAllowedBS(state,bsr),
    v_renovationBS("area",state,bsr,vin,subs,t)
    * (
      log(
        v_renovationBS("area",state,bsr,vin,subs,t)
        + epsilon
      )
      - 1
    )
  )
;


q_entropyRenToHS(state,vin,subs,t)$vinExists(t,vin)..
  v_entropyRenToHS(state,vin,subs,t)
  =e=
$ifthen.sequentialRen  "%SEQUENTIALREN%" == "TRUE"
  sum(renAllowedHS(state,hsr),
    v_renovationHS("area",state,hsr,vin,subs,t)
    * (
      log(
        v_renovationHS("area",state,hsr,vin,subs,t)
        + epsilon
      )
      - 1
    )
  )
$else.sequentialRen
  sum(renAllowed(state,stateFull),
    v_renovation("area",renAllowed,vin,subs,t)
    * (
      log(
        v_renovation("area",renAllowed,vin,subs,t)
        + epsilon
      )
      - 1
    )
  )
  - 
  v_entropyRenToBS(state,vin,subs,t) !! removes heterogeneity in bsr AND (when summed) state
$endif.sequentialRen
;


q_entropyRenFromBS(vin,subs,t)$vinExists(t,vin)..
  v_entropyRenFromBS(vin,subs,t)
  =e=
$ifthen.sequentialRen  "%SEQUENTIALREN%" == "TRUE"
  sum(state,
    sum(bsr$renAllowedBS(state,bsr),
      v_renovationBS("area",state,bsr,vin,subs,t)
    )
    * (
      log(
        sum(bsr$renAllowedBS(state,bsr),
          v_renovationBS("area",state,bsr,vin,subs,t)
        )
        + epsilon
      )
      - 1
    )
  )
$else.sequentialRen
  v_entropyRenFrom(vin,subs,t)
$endif.sequentialRen
;


q_entropyRenFromHS(vin,subs,t)$vinExists(t,vin)..
  v_entropyRenFromHS(vin,subs,t)
  =e=
$ifthen.sequentialRen  "%SEQUENTIALREN%" == "TRUE"
  sum(state,
    sum(hsr$renAllowedHS(state,hsr),
      v_renovationHS("area",state,hsr,vin,subs,t)
    )
    * (
      log(
        sum(hsr$renAllowedHS(state,hsr),
          v_renovationHS("area",state,hsr,vin,subs,t)
        )
        + epsilon
      )
      - 1
    )
  )
$else.sequentialRen
  0 !! no extra correction for initial state needed as this is already done by subtracting BS entropy
$endif.sequentialRen
;


q_entropyRenFrom(vin,subs,t)$vinExists(t,vin)..
  v_entropyRenFrom(vin,subs,t)
  =e=
  sum(state,
    sum(stateFull$renAllowed(state,stateFull),
      v_renovation("area",state,stateFull,vin,subs,t)
    )
    * (
      log(
        sum(stateFull$renAllowed(state,stateFull),
            v_renovation("area",state,stateFull,vin,subs,t)
          )
        + epsilon
      )
      - 1
    )
  )
;

q_renovationBS(q,state,bsr,vin,subs,ttot)$vinExists(ttot,vin)..
  v_renovationBS(q,state,bsr,vin,subs,ttot)
  =e=
  sum(renAllowed(state,bsr,hsr),
    v_renovation(q,state,bsr,hsr,vin,subs,ttot)
  )
;


q_renovationHS(q,state,hsr,vin,subs,ttot)$vinExists(ttot,vin)..
  v_renovationHS(q,state,hsr,vin,subs,ttot)
  =e=
  sum(renAllowed(state,bsr,hsr),
    v_renovation(q,state,bsr,hsr,vin,subs,ttot)
  )
;


* fix heterogeneity to zero for linear problem (lp)
q_zeroHeteroPrefCon(subs,t)..
  v_heteroPrefCon(subs,t)
  =e=
  0
;

q_zeroHeteroPrefRen(subs,t)..
  v_heteroPrefRen(subs,t)
  =e=
  0
;



*** status quo preference ------------------------------------------------------

q_statusQuoPref(subs,t)..
  v_statusQuoPref(subs,t)
  =e=
  p_statusQuoPref
  * sum((bs,hs,hsr,vin)$(    not(sameas(hs,hsr))
                         and not(sameas(hsr,"0"))
                         and vinExists(t,vin)
                         and renAllowedHS(bs,hs,hsr)),
    v_renovationHS("area",bs,hs,hsr,vin,subs,t)
  )
;


*** building stock balance -----------------------------------------------------

* relation between stocks and flows of floor area:
* (Renovation flow includes also buildings that are untouched, newly constructed
* or demolished in the current time step.)

* Previous stock and new construction within current time step t go into
* renovation flow

q_stockBalPrev(q,state,vin,subs,ttot)$(    vinExists(ttot,vin)
                                       and t(ttot))..
  v_stock(q,state,vin,subs,ttot-1)$vinExists(ttot-1,vin)
  + v_construction(q,state,subs,ttot) * p_dtVin(ttot,vin)
  =e=
  p_dt(ttot)
  * sum(stateFull$renAllowed(state,stateFull),
      v_renovation(q,state,stateFull,vin,subs,ttot)
    )
;


* Renovation flow in time step t either go into stock of next time step or
* demolition

q_stockBalNext(q,state(bs,hs),vin,subs,ttot)$(    vinExists(ttot,vin)
                                              and t(ttot))..
  v_stock(q,state,vin,subs,ttot)
  + v_demolition(q,state,vin,subs,ttot) * p_dt(ttot)
  =e=
  p_dt(ttot)
  * sum(state2(bs2,hs2),
      sum(stateFull2(bsr2,hsr2)$(    renAllowed(state2,stateFull2)
                                 and (sameas(bs,bsr2) or (sameas(bs,bs2) and sameas(bsr2,"0")))
                                 and (sameas(hs,hsr2) or (sameas(hs,hs2) and sameas(hsr2,"0")))),
        v_renovation(q,state2,stateFull2,vin,subs,ttot)
      )
    )
;



*** building stock balance (sequential renovation) -----------------------------

* Previous stock and new construction within current time step t go into flow of
* building shell renovation

q_stockBal1(q,state,vin,subs,ttot)$(    vinExists(ttot,vin)
                                    and t(ttot))..
  v_stock(q,state,vin,subs,ttot-1)$vinExists(ttot-1,vin)
  + v_construction(q,state,subs,ttot) * p_dtVin(ttot,vin)
  =e=
  sum(renAllowedBS(state,bsr),
      v_renovationBS(q,state,bsr,vin,subs,ttot)
  ) * p_dt(ttot)
;


* Flow of building shell renovation goes into flow of heating system replacement

q_stockBal2(q,bs,hs,vin,subs,t)$vinExists(t,vin)..
  sum(renAllowedBS(bs2,hs,bsr)$(   sameas(bs,bsr)
                                or (sameas(bs,bs2) and sameas(bsr,"0"))),
    v_renovationBS(q,bs2,hs,bsr,vin,subs,t)
  )
  =e=
  sum(renAllowedHS(bs,hs,hsr),
    v_renovationHS(q,bs,hs,hsr,vin,subs,t)
  )
;


* Flow of heating system replacement either goes into stock of next time step or
* demolition

q_stockBal3(q,bs,hs,vin,subs,t)$vinExists(t,vin)..
  sum(renAllowedHS(bs,hs2,hsr)$(   sameas(hs,hsr)
                                or (sameas(hs,hs2) and sameas(hsr,"0"))),
    v_renovationHS(q,bs,hs2,hsr,vin,subs,t)
  ) * p_dt(t)
  =e=
  v_stock(q,bs,hs,vin,subs,t)
  + v_demolition(q,bs,hs,vin,subs,t) * p_dt(t)
;



*** floor space demand ---------------------------------------------------------

* calculation of floor space demand
* TODO: move this calculation to mredgebuildings

q_housingDemand(subs(reg,loc,typ,inc),t)..
  p_population(subs,t) * p_floorPerCap(subs,t)
  =l=
  sum(state,
    sum(vinExists(t,vin),
      v_stock("area",state,vin,subs,t)
    )
  )
;


*** building life time ---------------------------------------------------------

* A share of the previous stock has to be demolished as it reaches its end of
* life. This share is calculated from a Weibull distribution of the building
* life time. If the switch EARLYDEMOLITION is TRUE, the model is free to
* demolish more than the share requires.

q_buildingLifeTime(q,state,vin,subs(reg,loc,typ,inc),ttot)$(    vinExists(ttot,vin)
                                                            and t(ttot))..
  v_demolition(q,state,vin,subs,ttot)
$ifthen.earlydemolition  "%EARLYDEMOLITION%" == "TRUE"
  =g=
$else.earlydemolition
  =e=
$endif.earlydemolition
  v_stock(q,state,vin,subs,ttot-1)$vinExists(ttot-1,vin)
  * p_shareDem(vin,reg,typ,ttot)
;


*** building shell life time ---------------------------------------------------

* The cummulated outflows from a particular building shell / heating system
* (demolition or change of building shell / heating system  through renovation)
* until time step t has to be at least as big as a Weibull life time
* distribution together with the cummulated inflows (construction or
* installation of the particular building shell / heating system through
* renovation) until t requires.

* building shell
q_lifeTimeBS(q,bs,vin,subs(reg,loc,typ,inc),ttot)$(    vinExists(ttot,vin)
                                                   and t(ttot))..
  sum(hs,
    sum(ttotOut$(    ttotOut.val le ttot.val
                 !!and p_shareRenBS(reg,ttotOut + 1,ttot) < 1
                 and vinExists(ttotOut,vin)),
      p_dt(ttotOut)
      * (
        v_demolition(q,bs,hs,vin,subs,ttotOut)
        + sum(renAllowedBS(bs,hs,bsr)$(not sameas(bsr,"0")),
            v_renovationBS(q,bs,hs,bsr,vin,subs,ttotOut)
          )
        )
    )
  )
  =e=
  v_slackRenBS(bs,vin,subs,ttot)
  +
  sum(ttotIn$(    ttotIn.val le ttot.val
              !!and p_shareRenBS(reg,ttotIn + 1,ttot) < 1
              and vinExists(ttotIn,vin)),
    p_shareRenBS(reg,ttotIn,ttot)
    * (
      sum(hs,
        v_construction(q,bs,hs,subs,ttotIn)
      ) * p_dtVin(ttotIn,vin)
      + 
      sum(renAllowedBS(state,bs),
        v_renovationBS(q,state,bs,vin,subs,ttotIn) * p_dt(ttotIn)
      )
    )
    +
    p_shareRenBSinit(reg,ttotIn,ttot)
    * sum(hs, v_stock(q,bs,hs,vin,subs,ttotIn)$(tinit(ttotIn)))
  )
;


* heating system
q_lifeTimeHS(q,hs,vin,subs(reg,loc,typ,inc),ttot)$(    vinExists(ttot,vin)
                                                   and t(ttot))..
  sum(bs,
    sum(ttotOut$(    ttotOut.val le ttot.val
                 !!and p_shareRenHS(hs,reg,typ,ttotOut + 1,ttot) < 1
                 and vinExists(ttotOut,vin)),
      p_dt(ttotOut)
      * (
        v_demolition(q,bs,hs,vin,subs,ttotOut)
        + sum(renAllowedHS(bs,hs,hsr)$(not sameas(hsr,"0")),
          v_renovationHS(q,bs,hs,hsr,vin,subs,ttotOut)
        )
      )
    )
  )
  =e=
  v_slackRenHS(hs,vin,subs,ttot)
  +
  sum(ttotIn$(    ttotIn.val le ttot.val
              !!and p_shareRenHS(hs,reg,typ,ttotIn + 1,ttot) < 1
              and vinExists(ttotIn,vin)),
    p_shareRenHS(hs,reg,typ,ttotIn,ttot)
    * (
      sum(bs,
        v_construction(q,bs,hs,subs,ttotIn)
      )
      * p_dtVin(ttotIn,vin)
      +
      sum(renAllowedHS(state,hs),
        v_renovationHS(q,state,hs,vin,subs,ttotIn)
      )
      * p_dt(ttotIn)
    )
    +
    p_shareRenHSinit(hs,reg,typ,ttotIn,ttot)
    * sum(bs, v_stock(q,bs,hs,vin,subs,ttotIn)$(tinit(ttotIn)))
  )
;


*** minimum diversity ----------------------------------------------------------

* require minimum share of each choice alternative (linear formulation)

* building shell choice in construction
q_minDivConBS(bs,hs,subs,t)..
  v_construction("area",bs,hs,subs,t)
  =g=
  sum(bs2,
    v_construction("area",bs2,hs,subs,t)
  )
  * 0.1 / card(bs)
;

* heating system choice in construction
q_minDivConHS(bs,hs,subs,t)..
  v_construction("area",bs,hs,subs,t)
  =g=
  sum(hs2,
    v_construction("area",bs,hs2,subs,t)
  )
  * 0.1 / card(hs)
;


* building shell choice in renovation
q_minDivRenBS(state,bsr,hsr,vin,subs,t)$(    vinExists(t,vin)
                                         and renAllowed(state,bsr,hsr))..
  v_renovation("area",state,bsr,hsr,vin,subs,t)
  =g=
  sum(bsr2,
    v_renovation("area",state,bsr2,hsr,vin,subs,t)
  )
  * 0.1 / card(bsr)
;

* heating system choice in renovation
q_minDivRenHS(state,bsr,hsr,vin,subs,t)$(    vinExists(t,vin)
                                         and renAllowed(state,bsr,hsr))..
  v_renovation("area",state,bsr,hsr,vin,subs,t)
  =g=
  sum(hsr2,
    v_renovation("area",state,bsr,hsr2,vin,subs,t)
  )
  * 0.1 / card(hsr)
;


*** maximum renovation rate ----------------------------------------------------

* limit renovation rate (increasing over time)

q_maxRenRate(reg,t)..
  sum(state,
    sum(stateFull(bsr,hsr)$(    not(sameas(bsr,"0")
                            and sameas(hsr,"0"))
                            and renAllowed(state,stateFull)),
      sum(vin$vinExists(t,vin),
        sum(loc, sum(typ, sum(inc,
          v_renovation("area",state,stateFull,vin,reg,loc,typ,inc,t)
        )))
      )
    )
  )
  =l=
  (0.01 + 0.01 * (t.val - 2020) / (2100 - 2020))
  * sum(state,
      sum(vinExists(t,vin),
        sum(loc, sum(typ, sum(inc,
          v_stock("area",state,vin,reg,loc,typ,inc,t)
        )))
      )
    )
;



*** dwelling size --------------------------------------------------------------

* average dwelling size converts number of dwellings to floor space

q_dwelSizeStock(vin,subs,ttot)$vinExists(ttot,vin)..
  sum(state, v_stock("area",state,vin,subs,ttot))
  =e=
  v_dwelSizeStock(vin,subs,ttot)
  * sum(state, v_stock("num",state,vin,subs,ttot))
;

q_dwelSizeConstruction(subs,ttot)..
  sum(state, v_construction("area",state,subs,ttot))
  =e=
  v_dwelSizeConstruction(subs,ttot)
  * sum(state, v_construction("num",state,subs,ttot))
;

q_dwelSizeRenovationBS(vin,subs,ttot)$vinExists(ttot,vin)..
  sum(renAllowedBS(state,bsr)$(not sameas(bsr,"0")), 
    v_renovationBS("area",renAllowedBS,vin,subs,ttot)
  )
  =e=
  v_dwelSizeRenovationBS(vin,subs,ttot)
  * sum(renAllowedBS(state,bsr)$(not sameas(bsr,"0")), 
    v_renovationBS("num",renAllowedBS,vin,subs,ttot)
  )
;

q_dwelSizeRenovationHS(vin,subs,ttot)$vinExists(ttot,vin)..
  sum(renAllowedHS(state,hsr)$(not sameas(hsr,"0")), 
    v_renovationHS("area",renAllowedHS,vin,subs,ttot)
  )
  =e=
  v_dwelSizeRenovationHS(vin,subs,ttot)
  * sum(renAllowedHS(state,hsr)$(not sameas(hsr,"0")), 
    v_renovationHS("num",renAllowedHS,vin,subs,ttot)
  )
;

q_dwelSizeDemolition(vin,subs,ttot)$vinExists(ttot,vin)..
  sum(state, v_demolition("area",state,vin,subs,ttot))
  =e=
  v_dwelSizeDemolition(vin,subs,ttot)
  * sum(state, v_demolition("num",state,vin,subs,ttot))
;



$ifthen.matching "%RUNTYPE%" == "matching"

*** variation of flows ---------------------------------------------------------

* sum all temporal variation in the flow variables to one number

q_flowVariationTot..
  v_flowVariationTot
  =e=
  sum(varFlow$(not(sameas(varFlow,"demolition"))),
    sum(subs,
      sum(t$((ord(t) lt card(t))),
          v_flowVariation(varFlow,"area",subs,t)
      )
    )
  )
;


* Square of temporal variations in each flow

q_flowVariation(varFlow,q,subs,t)$(ord(t) lt card(t))..
  v_flowVariation(varFlow,q,subs,t)
  =e=
  sum(state,
    sqr(v_flowVariationCon(q,state,subs,t))
  )$sameas(varFlow,"construction")
  +
  sum(ren(renAllowed),
    sqr(v_flowVariationRen(q,ren,subs,t) / 8)  !! rescale to roughly match order of magnitude of other flows
  )$sameas(varFlow,"renovation")
  +
  sum(state,
    sqr(v_flowVariationDem(q,state,subs,t))
  )$sameas(varFlow,"demolition")
;


* temporal variations of each flow

q_flowVariationCon(q,state,subs,t)$(ord(t) lt card(t))..
  v_flowVariationCon(q,state,subs,t)
  =e=
  (  v_construction(q,state,subs,t+1)
   - v_construction(q,state,subs,t))
  / p_dt(t+1)
;

q_flowVariationRen(q,ren,subs,t)$(    (ord(t) lt card(t))
                                  and renAllowed(ren))..
  v_flowVariationRen(q,ren,subs,t)
  =e=
  (  sum(vinExists(t,vin),   v_renovation(q,ren,vin,subs,t))
   - sum(vinExists(t+1,vin), v_renovation(q,ren,vin,subs,t+1)))
  / p_dt(t+1)
;

q_flowVariationRenBS(q,renAllowedBS,subs,t)$(ord(t) lt card(t))..
  v_flowVariationRenBS(q,renAllowedBS,subs,t)
  =e=
  (  sum(vinExists(t,vin),   v_renovationBS(q,renAllowedBS,vin,subs,t))
   - sum(vinExists(t+1,vin), v_renovationBS(q,renAllowedBS,vin,subs,t+1)))
  / p_dt(t+1)
;

q_flowVariationRenHS(q,renAllowedHS,subs,t)$(ord(t) lt card(t))..
  v_flowVariationRenHS(q,renAllowedHS,subs,t)
  =e=
  (  sum(vinExists(t,vin),   v_renovationHS(q,renAllowedHS,vin,subs,t))
   - sum(vinExists(t+1,vin), v_renovationHS(q,renAllowedHS,vin,subs,t+1)))
  / p_dt(t+1)
;

q_flowVariationDem(q,state,subs,t)$(ord(t) lt card(t))..
  v_flowVariationDem(q,state,subs,t)
  =e=
  (  sum(vinExists(t,vin),   v_demolition(q,state,vin,subs,t))
   - sum(vinExists(t+1,vin), v_demolition(q,state,vin,subs,t+1)))
  / p_dt(t+1)
;



*** minimum technology replacement ---------------------------------------------

* deviation from replacement according to life time
q_replacementDeviation..
  v_replacementDeviation
  =e=
  sum((vin,subs,t)$(    vinExists(t,vin)
                    and ord(t) gt 1),
    (  sum(bs, sqr(v_slackRenBS(bs,vin,subs,t)))
     + sum(hs, sqr(v_slackRenHS(hs,vin,subs,t))))
    * p_dt(t)
  )
;



*** matching objective ---------------------------------------------------------

q_matchingObj..
  v_matchingObj
  =e=
  v_refDeviationTot
  +
  p_flowVariationWeight
  * v_flowVariationTot
  +
  p_slackRenWeight
  * v_replacementDeviation
;



*** reference deviation --------------------------------------------------------

* total weighted deviation from reference sources

q_refDeviationTot..
  v_refDeviationTot
  =e=
  sum((ref,reg,t),
    v_refDeviation(ref,reg,t)
    * p_refWeight(ref)
  )
;


* summed squared deviation from reference sources

q_refDeviation(ref,reg,t)..
  v_refDeviation(ref,reg,t)
  =e=
  sum(refVarExists(ref,refVar,reg,t)$(refVarConsidered(ref,refVar)),
    sqr(
      v_refDeviationVar(ref,refVar,reg,t)
!!      / p_refValsMed(ref,reg)
    )
  )
;


* deviation from each variable in reference sources

q_refDeviationVar(ref,refVar,reg,t)$(    refVarExists(ref,refVar,reg,t)
                                     and refVarConsidered(ref,refVar))..
  v_refDeviationVar(ref,refVar,reg,t)
  =e=
  v_refVals(ref,refVar,reg,t)
  - p_refVals(ref,refVar,reg,t)
  * (
    1$refAbs(ref)
    +
    sum(refVarBasic(ref,refVar,refVarGroup),
      v_refValsBasic(ref,refVarGroup,reg,t)
    )$refRel(ref)
  )
;


* aggregate reference variables of share references to basic value

q_refValsBasic(ref,refVarGroup,reg,t)$refRel(ref)..
  v_refValsBasic(ref,refVarGroup,reg,t)
  =e=
  sum(refVarBasic(ref,refVar,refVarGroup)$refVarRef(ref,refVar),
    v_refVals(ref,refVar,reg,t)
  )
;


* aggregate BRICK variables to reference variable

q_refVals(ref,refVar,reg,t)$refVarRef(ref,refVar)..
  v_refVals(ref,refVar,reg,t)
  =e=
!! AUTOCODE.insertRefValEqn
!! Don't edit by hand, code is generated by R script
!! AUTOCODE_end.insertRefValEqn
;




*** matching variables ---------------------------------------------------------

* dwelling size at Odyssee reporting aggregation
q_dwelSize_Odyssee(refVar,reg,t)$refVarExists("Odyssee_dwelSize",refVar,reg,t)..
  sum(refMap_Odyssee_dwelSize(refVar,var,typ),
    sum((state,loc,inc),
      sum(vinExists(t,vin),
        v_stock("area",state,vin,reg,loc,typ,inc,t)
      )$sameas(var,"stock")
      +
      v_construction("area",state,reg,loc,typ,inc,t)$sameas(var,"construction")
    )
  )
  =e=
  v_dwelSize_Odyssee(refVar,reg,t)
  *
  sum(refMap_Odyssee_dwelSize(refVar,var,typ),
    sum((state,loc,inc),
      sum(vinExists(t,vin),
        v_stock("num",state,vin,reg,loc,typ,inc,t)
      )$sameas(var,"stock")
      +
      v_construction("num",state,reg,loc,typ,inc,t)$sameas(var,"construction")
    )
  )
;








* subsectoral renovation rate
* the reference source reports an average across 2014-2017. We temporarely
* assume this value for all periods here
q_renRate_EuropeanCommissionRenovation(refVar,reg,t)$refVarExists("EuropeanCommissionRenovation",refVar,reg,t)..
  sum(refMap_EuropeanCommissionRenovation(refVar,typ),
    sum((state,bsr,vin,loc,inc)$(    vinExists(t,vin)
                                 and not sameas(bsr,"0")),
      v_renovationBS("area",state,bsr,vin,reg,loc,typ,inc,t) !! EuropeanCommissionRenovation references dwellings, temporary change
    )
  )
  =e=
  v_renRate_EuropeanCommissionRenovation(refVar,reg,t)
  *
  sum(refMap_EuropeanCommissionRenovation(refVar,typ),
    sum((state,vin,loc,inc)$vinExists(t,vin),
      v_stock("area",state,vin,reg,loc,typ,inc,t) !! EuropeanCommissionRenovation references dwellings, temporary change
    )
  )
;


*** TEST -----------------------------------------------------------------------

equation q_testCon(qty,bs,hs,region,loc,typ,inc);
q_testCon(q,state,subs)..
  sum(tinit, v_construction(q,state,subs,tinit))
  =l=
  sum(tinit, v_construction(q,state,subs,tinit+1))
;

equation q_testRen(qty,bs,hs,bsr,hsr,vin,region,loc,typ,inc);
q_testRen(q,ren,vin,subs)$renAllowed(ren)..
  sum(tinit, v_renovation(q,ren,vin,subs,tinit))
  =l=
  sum(tinit, v_renovation(q,ren,vin,subs,tinit+1))
;

equation q_testRenBS(qty,bs,hs,bsr,vin,region,loc,typ,inc);
q_testRenBS(q,renAllowedBS,vin,subs)..
  sum(tinit, v_renovationBS(q,renAllowedBS,vin,subs,tinit))
  =l=
  sum(tinit, v_renovationBS(q,renAllowedBS,vin,subs,tinit+1))
;

equation q_testRenHS(qty,bs,hs,hsr,vin,region,loc,typ,inc);
q_testRenHS(q,renAllowedHS,vin,subs)..
  sum(tinit, v_renovationHS(q,renAllowedHS,vin,subs,tinit))
  =l=
  sum(tinit, v_renovationHS(q,renAllowedHS,vin,subs,tinit+1))
;

$endif.matching



*** renovation correction ------------------------------------------------------

$ifthen.renCorrect "%RUNTYPE%" == "renCorrect"
q_renCorrectObj..
  v_renCorrectObj
  =e=
  sum((state,vin,subs,t)$vinExists(t,vin),
    sqr(
        v_stock("area",state,vin,subs,t)
      - p_stock("area",state,vin,subs,t)
    )
  )
  +
  sum((state,subs,t),
    p_dt(t) * sqr(
        v_construction("area",state,subs,t)
      - p_construction("area",state,subs,t)
    )
  )
  +
$ifthen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
  sum((renAllowedBS(state,bsr),vin,subs,t)$(    not(sameas(bsr,"0"))
                                            and vinExists(t,vin)),
    p_dt(t) * sqr(
        v_renovationBS("area",renAllowedBS,vin,subs,t)
      - p_renovationBS("area",renAllowedBS,vin,subs,t)
    )
  )
  +
  sum((renAllowedHS(state,hsr),vin,subs,t)$(    not(sameas(hsr,"0"))
                                            and vinExists(t,vin)),
    p_dt(t) * sqr(
        v_renovationHS("area",renAllowedHS,vin,subs,t)
      - p_renovationHS("area",renAllowedHS,vin,subs,t)
    )
  )
$else.sequentialRen
  sum((ren(renAllowed(state,bsr,hsr)),vin,subs,t)$(not(    sameas(hsr,"0")
                                                       and sameas(bsr,"0"))
                                                   and vinExists(t,vin)),
    p_dt(t) * sqr(
        v_renovation("area",ren,vin,subs,t)
      - p_renovation("area",ren,vin,subs,t)
    )
  )
;
$endif.sequentialRen
$endIf.renCorrect
