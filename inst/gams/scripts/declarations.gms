parameters
p_dt(ttot)        "length of time step in yr"
p_dtVin(ttot,vin) "intersection of time step and vintage cohort in yr"

p_householdSize(reg,loc,typ,inc,ttot) "household size in cap"
p_floorPerCap(reg,loc,typ,inc,ttot)   "average floor space per capita in stock in m2/cap"

p_specCostCon(cost,bs,hs,reg,loc,typ,inc,ttot)             "floor-space specific construction cost in USD/m2"
p_specCostRen(cost,bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "floor-space specific renovation cost in USD/m2"
p_specCostOpe(bs,hs,vin,reg,loc,typ,ttot)                  "floor-space specific operation cost in USD/(m2.yr)"
p_specCostDem                                              "floor-space specific demolition cost in USD/m2"

p_carbonPrice(ttot)                "Carbon price in USD/t_CO2eq"
p_carrierPrice(carrier,reg,ttot)   "final energy carrier price in USD/kWh"
p_carrierEmi(carrier,reg,ttot)     "energy carrier emission intensity in t_CO2/kWh"
p_ueDemand(bs,vin,reg,typ)         "floor-space specific useful energy demand for space heating in kWh/yr/m2"
p_feDemand(hs,bs,vin,reg,typ,ttot) "floor-space specific final energy demand for space heating in kWh/yr/m2"
p_eff(hs,reg,typ,ttot)             "technical efficiency of space heating technologies"

p_lccCon(cost,var,bs,hs,reg,loc,typ,inc,ttot) "Estimate of life cycle cost of constructed housing in USD/m2"
p_probDem(reg,typ,ttot2,ttot)                 "probability of a building having reached its end of life"
p_LifeTimeBS(reg)                             "life time of building shell system in yr"
p_LifeTimeHS(hs,reg,typ)                      "life time of heating system in yr"

p_population(reg,loc,typ,inc,ttot)          "number of people in million"
p_floorPerCap(reg,loc,typ,inc,ttot)         "floor space per capita in m2"

p_stockHist(qty,bs,hs,vin,reg,loc,typ,inc,ttot)              "historic stock of buildings in million m2"
p_constructionHist(qty,bs,hs,reg,loc,typ,inc,ttot)           "historic flow of new buildings in million m2/yr"
p_renovationHist(qty,bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "historic flow of renovated and untouched buildings in million m2/yr"
p_demolitionHist(qty,bs,hs,vin,reg,loc,typ,inc,ttot)         "historic flow of demolished buildings in million m2/yr"

p_shareDem(vin,reg,typ,ttot)           "minimum share of demolition at end of life"
p_shareRenBS(reg,ttot,ttot)            "minimum share of renovation from the building shell reaching end of life"
p_shareRenHS(hs,reg,typ,ttot,ttot)     "minimum share of renovation from the heating system reaching end of life"
p_shareRenBSinit(reg,ttot,ttot)        "minimum share of renovation from the building shell of initial stock reaching end of life"
p_shareRenHSinit(hs,reg,typ,ttot,ttot) "minimum share of renovation from the heating system of initial stock reaching end of life"

p_interestRate(typ,ttot) "interest rate (incl. implicit) w.r.t. t0 in 1/yr"
p_discountFac(typ,ttot)  "discount factor w.r.t. t0"

p_runtime(reg,loc,typ,inc)                  "model runtime"
p_handle(reg,loc,typ,inc)                   "parallel model handle parameter"
p_repyFullSysLP(solveinfo)                  "model and solver summary: fullSysLP"
p_repyFullSysNLP(reg,loc,typ,inc,solveinfo) "model and solver summary: fullSysNLP"
p_repyFullSysNLPIter(iteration,reg,loc,typ,inc,solveinfo) "model and solver summary in every iteration: fullSysNLP"

p_refWeight(ref,reg,ttot) "weight of reference source in input matching"
p_flowVariationWeight     "weight of flow variation in matching objective"

p_refVals(ref,refVar,reg,ttot) "reference values to match"
p_refValsMed(ref,reg)          "median non-zero reference value to normalise deviations"

p_calibSpeed(varFLow)                                                 "Control of the step size in the calibration iteration"
p_calibDeviationCon(iteration,bs,hs,reg,loc,typ,inc,ttot)             "Ratio of actual value and calibration target for construction (should converge to 1)"
p_calibDeviationRen(iteration,bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "Ratio of actual value and calibration target for renovation (should converge to 1)"

priceSensBS(var) "price sensitivity of building shell choice" / construction 10000, renovation 10000 /
priceSensHS(var) "price sensitivity of heating system choice" / construction 0.1, renovation 0.08 /
;

scalars
t0 "reference year for discounting"

epsilon "offset to avoid log(0)" /1E-5/
;

variables
v_totObj               "total objective value"
v_Obj(reg,loc,typ,inc) "objective value: discounted system cost + heterogeneity preference"

v_SysHeteroPref(reg,loc,typ,inc,ttot) "system-wide heterogeneity preference"
v_HeteroPrefCon(reg,loc,typ,inc,ttot) "diversity preference for construction"
v_HeteroPrefRen(reg,loc,typ,inc,ttot) "diversity preference for renovation"

$ifthen.matching "%RUNTYPE%" == "matching"
v_flowVariationTot         "total temporal variation of flows"
v_refDeviationTot          "total weighted squared deviation of quantities from reference sources"
v_refDeviationVar(ref,refVar,reg,ttot) "deviation from each variable in reference sources"
v_matchingObj              "matching objective: reference deviation and flow variation"
$endif.matching

v_SysCost(reg,loc,typ,inc,ttot) "system cost cost cash flow in USD/yr"
v_ConCost(reg,loc,typ,inc,ttot) "construction cost cash flow in USD/yr"
v_RenCost(reg,loc,typ,inc,ttot) "renovation cost cash flow in USD/yr"
;

positive variables
v_OpeCost(reg,loc,typ,inc,ttot) "operational cost cash flow in USD/yr"
v_DemCost(reg,loc,typ,inc,ttot) "demolition cost cash flow in USD/yr"

v_stock(qty,bs,hs,vin,reg,loc,typ,inc,ttot)              "stock of buildings in million m2"
v_construction(qty,bs,hs,reg,loc,typ,inc,ttot)           "flow of new buildings in million m2/yr"
v_renovation(qty,bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "flow of renovated and untouched buildings in million m2/yr"
v_demolition(qty,bs,hs,vin,reg,loc,typ,inc,ttot)         "flow of demolished buildings in million m2/yr"

v_dwelSizeStock(vin,reg,loc,typ,inc,ttot)              "average dwelling size of the stock in m2/dwel"
v_dwelSizeConstruction(reg,loc,typ,inc,ttot)           "average dwelling size of newly constructed buildings in m2/dwel"
v_dwelSizeRenovation(vin,reg,loc,typ,inc,ttot)         "average dwelling size of renovated buildings in m2/dwel"
v_dwelSizeDemolition(vin,reg,loc,typ,inc,ttot)         "average dwelling size of demolished buildings in m2/dwel"

$ifthen.matching "%RUNTYPE%" == "matching"
v_dwelSize_Odyssee(refVar,reg,ttot) "dwelling size at the aggregation of Odyssee_dwelSize in m2/dwel"
v_vinShare_EUBDB(refVar,reg,ttot)   "vintage shares at the aggregation of EUBDB_vintage"
v_renRate_EuropeanCommissionRenovation(refVar,reg,ttot)
v_heatingShare_Odyssee(refVar,reg,ttot) "share of heating systems in the stock"
v_heatingShare_IDEES(refVar,reg,ttot) "share of heating systems in the stock"
v_flowVariation(varFLow,qty,reg,loc,typ,inc,ttot) "temporal variation of flows"
v_refDeviation(ref,reg,ttot)        "summed squared deviation from reference sources"
$endif.matching
;

equations

q_totObj               "total objective"
q_Obj(reg,loc,typ,inc) "objective: discounted system cost + heterogeneity preference"

q_SysCost(reg,loc,typ,inc,ttot) "system cost (con + ren + ope + dem)"
q_ConCost(reg,loc,typ,inc,ttot) "construction cost"
q_RenCost(reg,loc,typ,inc,ttot) "renovation cost"
q_OpeCost(reg,loc,typ,inc,ttot) "operation cost"
q_DemCost(reg,loc,typ,inc,ttot) "demolition cost"

q_SysHeteroPref(reg,loc,typ,inc,ttot) "system-wide heterogeneity preference"
q_HeteroPrefCon(reg,loc,typ,inc,ttot) "diversity preference for construction"
q_HeteroPrefRen(reg,loc,typ,inc,ttot) "diversity preference for renovation"
q_zeroHeteroPrefCon(reg,loc,typ,inc,ttot) "zero diversity preference for construction (lp)"
q_zeroHeteroPrefRen(reg,loc,typ,inc,ttot) "zero diversity preference for renovation (lp)"

q_stockBalNext(qty,bs,hs,vin,reg,loc,typ,inc,ttot)  "building stock balance: flows into next time step"
q_stockBalPrev(qty,bs,hs,vin,reg,loc,typ,inc,ttot)  "building stock balance: flows from previous time step"
q_housingDemand(reg,loc,typ,inc,ttot)                "demand for floor space"
q_buildingLifeTime(qty,bs,hs,vin,reg,loc,typ,inc,ttot)   "minimum demolition from builing life time"
q_buildingShellLifeTime(qty,bs,vin,reg,loc,typ,inc,ttot) "minimum renovation from building shell life time"
q_heatingSystemLifeTime(qty,hs,vin,reg,loc,typ,inc,ttot) "minimum renovation from heating system life time"

q_dwelSizeStock(vin,reg,loc,typ,inc,ttot)      "dwelling size of the stock in m2/dwel"
q_dwelSizeConstruction(reg,loc,typ,inc,ttot)   "dwelling size of newly constructed buildings in m2/dwel"
q_dwelSizeRenovation(vin,reg,loc,typ,inc,ttot) "dwelling size of renovated buildings in m2/dwel"
q_dwelSizeDemolition(vin,reg,loc,typ,inc,ttot) "dwelling size of demolished buildings in m2/dwel"


q_minDivConBS(bs,hsr,reg,loc,typ,inc,t)             "minimum building shell diversity in construction"
q_minDivConHS(bs,hsr,reg,loc,typ,inc,t)             "minimum heating system diversity in construction"
q_minDivRenBS(bs,hsr,bsr,hsr,vin,reg,loc,typ,inc,t) "minimum building shell diversity in renovation"
q_minDivRenHS(bs,hsr,bsr,hsr,vin,reg,loc,typ,inc,t) "minimum heating system diversity in renovation"

q_maxRenRate(reg,ttot) "Maximum renovation rate"

q_flowVariationTot                                   "total temporal variation of flows"
q_flowVariation(varFLow,qty,reg,loc,typ,inc,ttot) "temporal variation of flows"

$ifthen.matching "%RUNTYPE%" == "matching"
q_dwelSize_Odyssee(refVar,reg,ttot) "dwelling size at the aggregation of Odyssee_dwelSize in m2/dwel"
q_vinShare_EUBDB(refVar,reg,ttot)   "vintage shares at the aggregation of EUBDB_vintage"
q_renRate_EuropeanCommissionRenovation(refVar,reg,ttot)
q_heatingShare_Odyssee(refVar,reg,ttot) "share of heating systems in the stock"
q_heatingShare_IDEES(refVar,reg,ttot) "share of heating systems in the stock"

q_refDeviationTot                   "total squared deviation of quantities from reference source"
q_refDeviation(ref,reg,ttot)        "summed squared deviation from reference sources"
q_refDeviationVar(ref,refVar,reg,t) "deviation from each variable in reference sources"

q_matchingObj "matching objective: reference deviation and flow variation"
q_finiteHeatingShareCon(bs,hs,reg,loc,typ,inc,ttot)
q_finiteHeatingShareRen(bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot)
$endif.matching
;
