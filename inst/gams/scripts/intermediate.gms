p_fArmijoRHMin(subs, tcalib) = p_f(subs, tcalib) + p_sigma * 1/1000 * p_alpha(subs, tcalib) * p_phiDeriv(subs, tcalib);

*** Quick fix: Long term alpha should not depend on the flow (maybe?)
armijoStep(subs, tcalib)$(p_fMin(subs, tcalib) le p_f(subs, tcalib) + p_sigma * 1/1000 * p_alpha(subs, tcalib) * p_phiDeriv(subs, tcalib)) = yes;
heuristicStep(subs, tcalib)$(not armijoStep(subs, tcalib)) = yes;

armijoStepIter(iteration, subs, tcalib)$armijoStep(subs, tcalib) = yes;
heuristicStepIter(iteration, subs, tcalib)$heuristicStep(subs, tcalib) = yes;

*** Solve the model only for the subsets which do not satisfy the Armijo condition yet
loop(iterA,

  p_xASimp(flow, stateAll, stateFull, vinAll, subs, tcalib)$(simpleCalibVars(flow, stateAll, stateFull, vinAll) and totalStep(subs, tcalib))
            = p_xSimp(flow, stateAll, stateFull, vinAll, subs, tcalib) + p_alpha(subs, tcalib) * p_dSimp(flow, stateAll, stateFull, vinAll, subs, tcalib);

  v_stock.l("area", state, vin, subs, ttot) = 0;
  v_construction.l("area", state, subs, ttot) = 0;
  v_renovation.l("area", state, stateFull, vin, subs, ttot) = 0;

  p_specCostConFut(state, subs) = sum(tcalib2, p_xASimp("construction", "none", "none", state, "none", subs, tcalib2)) / card(tcalib2);
  p_specCostRenFut(ren, vin, subs) = sum(tcalib2, p_xASimp("renovation", ren, vin, subs, tcalib2)) / card(tcalib2);

  p_specCostCon("intangible", state, subs, tcalib) = p_xinitCon(state, subs, tcalib) + p_xASimp("construction", "none", "none", state, "none", subs, tcalib);
  p_specCostRen("intangible", ren, vin, subs, tcalib) = p_xinitRen(ren, vin, subs, tcalib) + p_xASimp("renovation", ren, vin, subs, tcalib);
  p_specCostCon("intangible", state, subs, t)$(not tcalib(t)) = p_xinitCon(state, subs, t) + p_specCostConFut(state, subs);
  p_specCostRen("intangible", ren, vin, subs, t)$(not tcalib(t)) = p_xinitRen(ren, vin, subs, t) + p_specCostRenFut(ren, vin, subs);

  solveParallel

  p_fA(subs, tcalib) = func

*** Stopping criterion in all dimensions
  loop((subs, tcalib)$armijoStep(subs, tcalib),
    if (p_fA(subs, tcalib) le p_f(subs, tcalib) + p_sigma * p_alpha(subs, tcalib) * p_phiDeriv(subs, tcalib),
      armijoStep(subs, tcalib) = no;
      p_iterA(subs, tcalib) = iterA.val;
      );
  );
  loop((subs, tcalib)$heuristicStep(subs, tcalib),
    if (p_fA(subs, tcalib) le p_f(subs, tcalib),
      heuristicStep(subs, tcalib) = no;
      p_iterA(subs, tcalib) = iterA.val;
    );
  );
  totalStep(subs, tcalib)$(not heuristicStep(subs, tcalib) and not armijoStep(subs, tcalib)) = no;
  subs(all_subs) = no;
  loop(tcalib,
    subs(all_subs)$(totalStep(all_subs, tcalib)) = yes;
  );
  if(card(subs) = 0,
    p_alphaIterA(iterA, subs, tcalib) = p_alpha(subs, tcalib);
    break;
  );
*** Update alpha
  p_alpha(subs, tcalib)$(totalStep(subs, tcalib)) = p_alpha(subs, tcalib) * p_beta;
  p_alphaIterA(iterA, subs, tcalib)$(totalStep(subs, tcalib)) = p_alpha(subs, tcalib);