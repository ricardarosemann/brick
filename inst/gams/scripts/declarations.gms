parameters
p_dt(ttot)        "length of time step in yr"
p_dtVin(ttot,vin) "intersection of time step and vintage cohort in yr"

p_householdSize(region,loc,typ,inc,ttot) "household size in cap"
p_floorPerCap(region,loc,typ,inc,ttot)   "average floor space per capita in stock in m2/cap"

p_specCostCon(cost,bs,hs,region,loc,typ,inc,ttot)             "floor-space specific construction cost [USD/m2]"
p_specCostRen(cost,bs,hs,bsr,hsr,vin,region,loc,typ,inc,ttot) "floor-space specific renovation cost [USD/m2]"
p_specCostRenBS(cost,bs,hs,bsr,vin,region,loc,typ,inc,ttot)   "floor-space specific building shell retrofit cost [USD/m2]"
p_specCostRenHS(cost,bs,hs,hsr,vin,region,loc,typ,inc,ttot)   "floor-space specific heating system replacement cost [USD/m2]"
p_specCostOpe(bs,hs,vin,region,loc,typ,ttot)                  "floor-space specific operation cost [USD/m2/yr]"
p_specCostDem                                                 "floor-space specific demolition cost [USD/m2]"

p_carbonPrice(ttot)                   "Carbon price in USD/t_CO2eq"
p_carrierPrice(carrier,region,ttot)   "final energy carrier price in USD/kWh"
p_carrierEmi(carrier,region,ttot)     "energy carrier emission intensity in t_CO2/kWh"
p_ueDemand(bs,vin,region,typ)         "floor-space specific useful energy demand for space heating in kWh/yr/m2"
p_feDemand(hs,bs,vin,region,typ,ttot) "floor-space specific final energy demand for space heating in kWh/yr/m2"
p_eff(hs,region,typ,ttot)             "technical efficiency of space heating technologies"
p_renDepth(bs,bsr)                    "renovation depth"

p_lccCon(cost,var,bs,hs,region,loc,typ,inc,ttot) "Estimate of life cycle cost of constructed housing in USD/m2"
p_probDem(region,typ,ttot2,ttot)                 "probability of a building having reached its end of life"
p_lifeTimeBS(region)                             "life time of building shell system in yr"
p_lifeTimeHS(hs,region,typ)                      "life time of heating system in yr"

p_population(region,loc,typ,inc,ttot)          "number of people in million"
p_floorPerCap(region,loc,typ,inc,ttot)         "floor space per capita in m2"

p_stockHist(qty,bs,hs,vin,region,loc,typ,inc,ttot)              "historic stock of buildings in [million m2]"
p_constructionHist(qty,bs,hs,region,loc,typ,inc,ttot)           "historic flow of new buildings in [million m2/yr]"
p_renovationHist(qty,bs,hs,bsr,hsr,vin,region,loc,typ,inc,ttot) "historic flow of renovated and untouched buildings in [million m2/yr]"
p_renovationHistBS(qty,bs,hs,bsr,vin,region,loc,typ,inc,ttot)   "historic flow of buildings with building shell retrofit [million m2/yr]"
p_renovationHistHS(qty,bs,hs,hsr,vin,region,loc,typ,inc,ttot)   "historic flow of buildings with heating system repacement [million m2/yr]"
p_demolitionHist(qty,bs,hs,vin,region,loc,typ,inc,ttot)         "historic flow of demolished buildings [million m2/yr]"

p_shareDem(vin,region,typ,ttot)           "minimum share of demolition at end of life"
p_shareRenBS(region,ttot,ttot)            "minimum share of renovation from the building shell reaching end of life"
p_shareRenHS(hs,region,typ,ttot,ttot)     "minimum share of renovation from the heating system reaching end of life"
p_shareRenBSinit(region,ttot,ttot)        "minimum share of renovation from the building shell of initial stock reaching end of life"
p_shareRenHSinit(hs,region,typ,ttot,ttot) "minimum share of renovation from the heating system of initial stock reaching end of life"

p_discountRate(typ,ttot) "discount rate (incl. implicit discount) in 1/yr"
p_discountFac(typ,ttot)  "discount factor w.r.t. t0"

p_runtime(region,loc,typ,inc)                  "model runtime"
p_runtime2(region)                             "model runtime"
p_handle(region,loc,typ,inc)                   "parallel model handle parameter"
p_handle2(region)                              "parallel model handle parameter"
p_repyFullSysLP(solveinfo)                     "model and solver summary: fullSysLP"
p_repyFullSysNLP(region,loc,typ,inc,solveinfo) "model and solver summary: fullSysNLP"
p_repyMatchingQCP(region,solveinfo)            "model and solver summary: matching"
p_repyRenCorrectQCP(solveinfo)                 "model and solver summary: renCorrect"

p_flowVariationWeight               "weight of flow variation in matching objective"
p_slackRenWeight                    "weight of building shell and heating system replacement slack in matching objective"

p_refVals(reference,refVar,region,ttot) "reference values to match"
p_refValsMed(reference,region)          "median non-zero reference value to normalise deviations"
p_refWeight(reference)                  "Weight of reference"

priceSensBS(var, region, loc, typ, inc) "price sensitivity of building shell choice"
priceSensHS(var, region, loc, typ, inc) "price sensitivity of heating system choice"

p_statusQuoPref "preference for replacehing a heating system with the same technology in USD/m2"
;

scalars
t0 "reference year for discounting"

epsilon "offset to avoid log(0)" /1E-5/
;

variables
v_totObj                  "total objective value"
v_obj(region,loc,typ,inc) "objective value: discounted system cost + heterogeneity preference"

v_sysHeteroPref(region,loc,typ,inc,ttot)              "system-wide heterogeneity preference"
v_heteroPrefCon(region,loc,typ,inc,ttot)              "diversity preference for construction"
v_heteroPrefRen(region,loc,typ,inc,ttot)              "diversity preference for renovation"
v_entropyRenToBS(bs,hs,vin,region,loc,typ,inc,ttot)   "diversity in building shell retrofit"
v_entropyRenToHS(bs,hs,vin,region,loc,typ,inc,ttot)   "diversity in heating system replacement"
v_entropyRenFromBS(vin,region,loc,typ,inc,t)          "diversity in state before building shell retrofit"
v_entropyRenFromHS(vin,region,loc,typ,inc,t)          "diversity in state before heating system replacement"
v_entropyRenFrom(vin,region,loc,typ,inc,t)            "diversity in state before renovation"
v_statusQuoPref(region,loc,typ,inc,ttot)              "status quo preference when replacing heating systems"

v_slackRenBS(bs,vin,region,loc,typ,inc,ttot) "difference between actual and min required building shell replacement"
v_slackRenHS(hs,vin,region,loc,typ,inc,ttot) "difference between actual and min required heating system replacement"

$ifthen.matching "%RUNTYPE%" == "matching"
v_replacementDeviation "total deviation from technology replacement according to life time"

v_flowVariationTot                              "total temporal variation of flows"
v_refVals(reference,refVar,region,t)            "model variable values at reference aggregation"
v_refValsBasic(reference,refVarGroup,region,t)  "v_refVals summed to basic value of reference shares"
v_refDeviation(reference,region,ttot)           "summed squared deviation from reference sources"
v_refDeviationTot                               "total weighted squared deviation of quantities from reference sources"
v_refDeviationVar(reference,refVar,region,ttot) "deviation from each variable in reference sources"
v_matchingObj                                   "matching objective: reference deviation and flow variation"

v_flowVariation(varFLow,qty,region,loc,typ,inc,ttot)       "temporal variation of flows [million m2/yr/yr]"
v_flowVariationCon(qty,bs,hs,region,loc,typ,inc,t)         "temporal variation of construction flow [million m2/yr/yr]"
v_flowVariationRen(qty,bs,hs,bsr,hsr,region,loc,typ,inc,t) "temporal variation of renovation flow [million m2/yr/yr]"
v_flowVariationRenBS(qty,bs,hs,bsr,region,loc,typ,inc,ttot)   "temporal variation of building shell retrofit flow"
v_flowVariationRenHS(qty,bs,hs,hsr,region,loc,typ,inc,ttot)   "temporal variation of heating system replacement flow"
v_flowVariationDem(qty,bs,hs,region,loc,typ,inc,t)         "temporal variation of demolition flow [million m2/yr/yr]"
$endif.matching

v_sysCost(region,loc,typ,inc,ttot) "system cost cost cash flow in USD/yr"
v_conCost(region,loc,typ,inc,ttot) "construction cost cash flow in USD/yr"
v_renCost(region,loc,typ,inc,ttot) "renovation cost cash flow in USD/yr"

$ifthen.renCorrect "%RUNTYPE%" == "renCorrect"
v_renCorrectObj "correction objective: renovation deviation"
$endif.renCorrect
;

positive variables
v_opeCost(region,loc,typ,inc,ttot) "operational cost cash flow in USD/yr"
v_demCost(region,loc,typ,inc,ttot) "demolition cost cash flow in USD/yr"

v_stock(qty,bs,hs,vin,region,loc,typ,inc,ttot)              "stock of buildings in [million m2/yr]"
v_construction(qty,bs,hs,region,loc,typ,inc,ttot)           "flow of new buildings in [million m2/yr]"
v_renovation(qty,bs,hs,bsr,hsr,vin,region,loc,typ,inc,ttot) "flow of renovated buildings (incl. untouched) [million m2/yr]"
v_renovationBS(qty,bs,hs,bsr,vin,region,loc,typ,inc,ttot)   "flow of buildings with building shell retrofit (incl. untouched) [million m2/yr]"
v_renovationHS(qty,bs,hs,hsr,vin,region,loc,typ,inc,ttot)   "flow of buildings with heating system repacement (incl. untouched) [million m2/yr]"
v_demolition(qty,bs,hs,vin,region,loc,typ,inc,ttot)         "flow of demolished buildings in million m2/yr"

v_dwelSizeStock(vin,region,loc,typ,inc,ttot)        "average dwelling size of the stock [m2/dwel]"
v_dwelSizeConstruction(region,loc,typ,inc,ttot)     "average dwelling size of newly constructed buildings [m2/dwel]"
v_dwelSizeRenovationBS(vin,region,loc,typ,inc,ttot) "average of dwelling size of buildings with building shell retrofit [m2/dwel]"
v_dwelSizeRenovationHS(vin,region,loc,typ,inc,ttot) "average of dwelling size of buildings with heating system replacement [m2/dwel]"
v_dwelSizeDemolition(vin,region,loc,typ,inc,ttot)   "average dwelling size of demolished buildings [m2/dwel]"

$ifthen.matching "%RUNTYPE%" == "matching"
v_dwelSize_Odyssee(refVar,region,ttot) "dwelling size at the aggregation of Odyssee_dwelSize in m2/dwel"
v_renRate_EuropeanCommissionRenovation(refVar,region,ttot)
$endif.matching
;

equations

q_totObj                  "total objective"
q_obj(region,loc,typ,inc) "objective: discounted system cost + heterogeneity preference"

q_sysCost(region,loc,typ,inc,ttot) "system cost (con + ren + ope + dem)"
q_conCost(region,loc,typ,inc,ttot) "construction cost"
q_renCost(region,loc,typ,inc,ttot) "renovation cost"
q_opeCost(region,loc,typ,inc,ttot) "operation cost"
q_demCost(region,loc,typ,inc,ttot) "demolition cost"

q_renovationBS(qty,bs,hs,bsr,vin,region,loc,typ,inc,ttot) "aggregate renovation to building shell retrofit"
q_renovationHS(qty,bs,hs,hsr,vin,region,loc,typ,inc,ttot) "aggregate renovation to heating system replacement"

q_sysHeteroPref(region,loc,typ,inc,ttot)              "system-wide heterogeneity preference"
q_heteroPrefCon(region,loc,typ,inc,ttot)              "diversity preference for construction"
q_heteroPrefRen(region,loc,typ,inc,ttot)              "diversity preference for renovation"
q_entropyRenToBS(bs,hs,vin,region,loc,typ,inc,ttot)   "diversity in building shell retrofit"
q_entropyRenToHS(bs,hs,vin,region,loc,typ,inc,ttot)   "diversity in heating system replacement"
q_entropyRenFromBS(vin,region,loc,typ,inc,t)          "diversity in state before building shell retrofit"
q_entropyRenFromHS(vin,region,loc,typ,inc,t)          "diversity in state before heating system replacement"
q_entropyRenFrom(vin,region,loc,typ,inc,t)            "diversity in state before renovation"
q_statusQuoPref(region,loc,typ,inc,ttot)              "status quo preference"
q_zeroHeteroPrefCon(region,loc,typ,inc,ttot)          "zero diversity preference for construction (lp)"
q_zeroHeteroPrefRen(region,loc,typ,inc,ttot)          "zero diversity preference for renovation (lp)"

q_stockBalNext(qty,bs,hs,vin,region,loc,typ,inc,ttot)       "building stock balance: flows into next time step"
q_stockBalPrev(qty,bs,hs,vin,region,loc,typ,inc,ttot)       "building stock balance: flows from previous time step"
q_stockBal1(qty,bs,hs,vin,region,loc,typ,inc,ttot)          "building stock balance: flows from previous stock into building shell retrofit"
q_stockBal2(qty,bs,hs,vin,region,loc,typ,inc,ttot)          "building stock balance: flows from building shell retrofit into heating system replacement"
q_stockBal3(qty,bs,hs,vin,region,loc,typ,inc,ttot)          "building stock balance: flows from heating system replacement into next stock"
q_housingDemand(region,loc,typ,inc,ttot)                    "demand for floor space"
q_buildingLifeTime(qty,bs,hs,vin,region,loc,typ,inc,ttot)   "minimum demolition from builing life time"
q_lifeTimeBS(qty,bs,vin,region,loc,typ,inc,ttot)            "minimum renovation from building shell life time"
q_lifeTimeHS(qty,hs,vin,region,loc,typ,inc,ttot)            "minimum renovation from heating system life time"

q_dwelSizeStock(vin,region,loc,typ,inc,ttot)      "dwelling size of the stock [m2/dwel]"
q_dwelSizeConstruction(region,loc,typ,inc,ttot)   "dwelling size of newly constructed buildings [m2/dwel]"
q_dwelSizeRenovationBS(vin,region,loc,typ,inc,ttot) "dwelling size of buildings with building shell retrofit [m2/dwel]"
q_dwelSizeRenovationHS(vin,region,loc,typ,inc,ttot) "dwelling size of buildings with heating system replacement [m2/dwel]"
q_dwelSizeDemolition(vin,region,loc,typ,inc,ttot) "dwelling size of demolished buildings [m2/dwel]"

q_minDivConBS(bs,hsr,region,loc,typ,inc,t)             "minimum building shell diversity in construction"
q_minDivConHS(bs,hsr,region,loc,typ,inc,t)             "minimum heating system diversity in construction"
q_minDivRenBS(bs,hsr,bsr,hsr,vin,region,loc,typ,inc,t) "minimum building shell diversity in renovation"
q_minDivRenHS(bs,hsr,bsr,hsr,vin,region,loc,typ,inc,t) "minimum heating system diversity in renovation"

q_maxRenRate(region,ttot) "Maximum renovation rate"

q_replacementDeviation "total deviation from technology replacement according to life time"

q_flowVariationTot                                            "total temporal variation of flows"
q_flowVariation(varFLow,qty,region,loc,typ,inc,ttot)          "temporal variation of flows"
q_flowVariationCon(qty,bs,hs,region,loc,typ,inc,ttot)         "temporal variation of construction flow"
q_flowVariationRen(qty,bs,hs,bsr,hsr,region,loc,typ,inc,ttot) "temporal variation of renovation flow"
q_flowVariationRenBS(qty,bs,hs,bsr,region,loc,typ,inc,ttot)   "temporal variation of building shell retrofit flow"
q_flowVariationRenHS(qty,bs,hs,hsr,region,loc,typ,inc,ttot)   "temporal variation of heating system replacement flow"
q_flowVariationDem(qty,bs,hs,region,loc,typ,inc,ttot)         "temporal variation of demolition flow"


$ifthen.matching "%RUNTYPE%" == "matching"
q_dwelSize_Odyssee(refVar,region,ttot) "dwelling size at the aggregation of Odyssee_dwelSize in m2/dwel"
q_renRate_EuropeanCommissionRenovation(refVar,region,ttot)

q_refDeviationTot                              "total squared deviation of quantities from reference source"
q_refDeviation(reference,region,ttot)          "summed squared deviation from reference sources"
q_refDeviationVar(reference,refVar,region,t)   "deviation from each variable in reference sources"
q_refVals(reference,refVar,region,t)           "aggregate model variable to reference granularity"
q_refValsBasic(reference,refVarGroup,region,t) "sum v_refVals to basic value of reference shares"

q_matchingObj "matching objective: reference deviation and flow variation"
$endif.matching

$ifthen.renCorrect "%RUNTYPE%" == "renCorrect"
q_renCorrectObj "correction objective: renovation deviation"
$endif.renCorrect
;

$ifthenE.calibration (sameas("%CALIBRATIONMETHOD%","optimization"))or(sameas("%CALIBRATIONMETHOD%","logit"))
parameters
p_stockCalibTarget(qty,bs,hs,vin,region,loc,typ,inc,ttot)              "historic stock of buildings in million m2 as calibration target"
p_constructionCalibTarget(qty,bs,hs,region,loc,typ,inc,ttot)           "historic flow of new buildings as calibration target in million m2/yr"
p_demolitionCalibTarget(qty,bs,hs,vin,region,loc,typ,inc,ttot)         "historic flow of demolished buildings as calibration target in million m2/yr"
$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
p_renovationBSCalibTarget(qty,bs,hs,bsr,vin,region,loc,typ,inc,ttot) "historic flow of shell renovation as calibration target in million m2/yr"
p_renovationHSCalibTarget(qty,bs,hs,hsr,vin,region,loc,typ,inc,ttot) "historic flow of heating system renovation as calibration target in million m2/yr"
$else.sequentialRen
p_renovationCalibTarget(qty,bs,hs,bsr,hsr,vin,region,loc,typ,inc,ttot) "historic flow of renovated and untouched buildings as calibration target in million m2/yr"
$endIf.sequentialRen
p_f(region, loc, typ, inc)                                       "value of the objective function in the outer optimization of the calibration; unit depends on target choice"
;
$endif.calibration

$ifthenE.calibrationOptimization (sameas("%RUNTYPE%","calibration"))and(sameas("%CALIBRATIONMETHOD%","optimization"))
parameters
p_diff                                                              "Difference in the argument of the outer objective function used in the difference quotient"

p_xinitCon(bs, hs, region, loc, typ, inc, ttot)                        "Intangible costs of construction at the beginning of this calibration iteration"
p_specCostCalibCon(bs, hs, region, loc, typ, inc, ttot)                "Intangible costs of construction after adding p_diff to one entry of the vectorized costs; duplicate of p_xDiffCon"
p_xDiffCon(bs, hs, region, loc, typ, inc, ttot)                        "Intangible costs of construction after adding p_diff to one entry of the vectorized costs"
p_fDiffCon(bs, hs, region, loc, typ, inc, ttot)                        "objective value computed with p_diff added to the respective entry of intangible construction costs"
p_stock(qty, bs, hs, vin, region, loc, typ, inc, ttot)                 "stock of buildings in million m2; used to store v_stock after the initial model solve"
p_construction(qty,bs,hs,region,loc,typ,inc,ttot)                      "flow of new buildings in million m2/yr; used to store v_construction after the initial model solve"
$ifThen.sequentialRen "%SEQUENTIALREN%" == "TRUE"
p_renovationBS(qty,bs,hs,bsr,vin,region,loc,typ,inc,ttot)            "flow of renovated and untouched buildings in million m2/yr; used to store v_renovation after the initial model solve"
p_renovationHS(qty,bs,hs,hsr,vin,region,loc,typ,inc,ttot)            "flow of renovated and untouched buildings in million m2/yr; used to store v_renovation after the initial model solve"
p_xinitRenBS(bs, hs, bsr, vin, region, loc, typ, inc, ttot)         "Intangible costs of shell renovation at the beginning of this calibration iteration"
p_xinitRenHS(bs, hs, hsr, vin, region, loc, typ, inc, ttot)         "Intangible costs of heating renovation at the beginning of this calibration iteration"
p_specCostCalibRenBS(bs, hs, bsr, vin, region, loc, typ, inc, ttot) "Intangible costs of shell renovation after adding p_diff to one entry of the vectorized costs at full resolution"
p_specCostCalibRenHS(bs, hs, hsr, vin, region, loc, typ, inc, ttot) "Intangible costs of heating renovation after adding p_diff to one entry of the vectorized costs at full resolution"
p_xDiffRenBS(bsr, vin, region, loc, typ, inc, ttot)                 "Intangible costs of shell renovation after adding p_diff to one entry of the vectorized costs"
p_xDiffRenHS(renType, hsr, vin, region, loc, typ, inc, ttot)        "Intangible costs of heating renovation after adding p_diff to one entry of the vectorized costs"
p_fDiffRenBS(bsr, vin, region, loc, typ, inc, ttot)        "objective value computed with p_diff added to the respective entry of intangible shell renovation costs"
p_fDiffRenHS(renType, hsr, vin, region, loc, typ, inc, ttot)        "objective value computed with p_diff added to the respective entry of intangible heating renovation costs"
$else.sequentialRen
p_renovation(qty,bs,hs,bsr,hsr,vin,region,loc,typ,inc,ttot)            "flow of renovated and untouched buildings in million m2/yr; used to store v_renovation after the initial model solve"
p_xinitRen(bs, hs, bsr, hsr, vin, region, loc, typ, inc, ttot)         "Intangible costs of renovation at the beginning of this calibration iteration"
p_specCostCalibRen(bs, hs, bsr, hsr, vin, region, loc, typ, inc, ttot) "Intangible costs of renovation after adding p_diff to one entry of the vectorized costs at full resolution"
p_xDiffRen(renType, bsr, hsr, vin, region, loc, typ, inc, ttot)        "Intangible costs of renovation after adding p_diff to one entry of the vectorized costs"
p_fDiffRen(renType, bsr, hsr, vin, region, loc, typ, inc, ttot)        "objective value computed with p_diff added to the respective entry of intangible renovation costs"
$endIf.sequentialRen
;
$endif.calibrationOptimization

$ifThen.renCorrect "%RUNTYPE%" == "renCorrect"
parameters
p_renovation(qty,bs,hs,bsr,hsr,vin,region,loc,typ,inc,ttot) "target flow of renovated and untouched buildings [million m2/yr]"
p_renovationBS(qty,bs,hs,bsr,vin,region,loc,typ,inc,ttot)   "target flow of building shell retrofitted and untouched buildings [million m2/yr]"
p_renovationHS(qty,bs,hs,hsr,vin,region,loc,typ,inc,ttot)   "target flow of heating system replacement and untouched buildings [million m2/yr]"
p_construction(qty,bs,hs,region,loc,typ,inc,ttot)           "target flow of new buildings [million m2/yr]"
p_demolition(qty,bs,hs,vin,region,loc,typ,inc,ttot)         "target flow of demolished buildings [million m2/yr]"
p_stock(qty, bs, hs, vin, region, loc, typ, inc, ttot)      "target stock of buildings [million m2]"
p_renovationHSDiff(qty,bs,hs,hsr,vin,region,loc,typ,inc,ttot)   "difference of heating system replacement and untouched buildings [million m2/yr]"
p_stockDiff(qty, bs, hs, vin, region, loc, typ, inc, ttot)      "difference in stock of buildings [million m2]"
;
$endif.renCorrect
