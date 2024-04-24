*** declare one-dimensional sets -----------------------------------------------

sets
*** building state dimensions
bsr     "renovated building shell"
hsr     "renovated heating system"
bs(bsr) "building shell"
hs(hsr) "heating system"

*** vintages
vin "construction vintage cohort"
*$ifthen.calibration "%RUNTYPE%" == "calibration"
vinCalib(vin)  "Dynamic vintages in calibration"
*$endif.calibration

*** stock subset dimesions
reg "regions"
loc "location of building (rural, urban)"
typ "type of residential building (SFH, MFH)"
inc "income quantile"

*** temporal sets
tall        "all time steps"
ttot(tall)  "all modelling time steps"
t(ttot)     "modelled time steps"
tcalib(ttot) "time steps for calibration"
thist(ttot) "historic time steps"
tinit(ttot) "initial modelling time step"

*** model fundamentals
cost "type of cost"
  /
  tangible   "tangible cost (exogenous)"
  intangible "intangible cost (identified in calibration)"
  markup     "markup costs (to represent individual preferences)"
  /
var "mayor variables of the model"
  / stock, construction, renovation, demolition /
varFlow(var) "flow variables of the model"
  / construction, renovation, demolition /

*** For calibration: Flows used in calibration
flow  "flow variables for calibration"

*** model analytics
solveinfo	"model and solver stats"
  /
  solvestat "solver termination condition"
  modelstat "model status"
  resusd    "time the solver used to solve the model in seconds"
  objval    "objective function value"
  /

*** calibration iteration
iterationAll "calibration iteration including zero"
iteration(iterationAll) "calibration iteration"
iterA  "Armijo backtracking iteration" / 1*50 /

*** matching reference sources
qty    "quantity unit to measure stocks and flows in"
  /
  area "floor area in million m2 or million m2/yr"
  dwel "number of dwellings in million or million/yr"
  /
q(qty) "quantity unit used to measure stocks and flows in"

ref                   "reference sources that historic quantities can be calibrated to"
r(ref)                "reference sources that historic quantities are calibrated to"
refVar                "variables of reference sources"
!! TODO: automatic code generation for reference-specific sets
refMap_mredgebuildings_location(refVar,loc)
refMap_mredgebuildings_heating(refVar,hs)
refMap_mredgebuildings_buildingType(refVar,typ)
refMap_mredgebuildings_vintage(refVar,vin)
refMap_Odyssee_stock(refVar,typ,hs,qty)
refMap_Odyssee_construction(refVar,typ)
refMap_Odyssee_constructionFloor(refVar,typ)
refMap_Odyssee_dwelSize(refVar,var,typ)
refMap_Odyssee_heatingShare(refVar,typ,hs)
refMap_IDEES_heatingShare(refVar,hs)
refMap_EUBDB_stock(refVar,hs,typ,qty)
refMap_EUBDB_vintage(refVar,vin,typ)
refMap_EuropeanCommissionRenovation(refVar,typ)
;

*** aliases
alias(bsr,bsr2,bsr3)
alias(hsr,hsr2,hsr3)
alias(bs,bs2)
alias(hs,hs2)
alias(vin,vin2,vin3)
alias(ttot,ttot2)
alias(t,t2)
alias(flow, flow2)
;


*** initialise one-dimensional sets --------------------------------------------

*** load fundamental sets
$gdxin input.gdx
$load bsr hsr bs hs
$load reg loc typ inc
$load tall ttot t thist tinit tcalib
$load vin
$ifthen.calibration "%RUNTYPE%" == "calibration"
$load flow iterationAll iteration
$endif.calibration
$gdxin

* *** Adjust bsr for ignore shell runs
* $ifthenE.shell (not(sameas("%ignoreShell%","TRUE")))
* sets
* bsr
* /
* 0
* /
* ;
* $endif.shell

*** load reference sets
$ifthen.matching "%RUNTYPE%" == "matching"
!! TODO: automatic code generation
$gdxin references.gdx
$load ref refVar r
$load refMap_mredgebuildings_location
$load refMap_mredgebuildings_heating
$load refMap_mredgebuildings_buildingType
$load refMap_mredgebuildings_vintage
$load refMap_Odyssee_stock
$load refMap_Odyssee_dwelSize
$load refMap_Odyssee_construction
$load refMap_Odyssee_constructionFloor
$load refMap_Odyssee_heatingShare
$load refMap_IDEES_heatingShare
$load refMap_EUBDB_stock
$load refMap_EUBDB_vintage
$load refMap_EuropeanCommissionRenovation
$gdxin
$endif.matching

*** declare multi-dimensional sets ---------------------------------------------

sets
*** building subset
all_subs(reg,loc,typ,inc) "all building stock subsets"
subs(reg,loc,typ,inc)     "building stock subsets in the solution process"

*** building state
stateFull(bsr,hsr)      "building state incl 0 for no renovation"
state(bs,hs)            "building state"
ren(bs,hs,bsr,hsr)      "renovation alternatives"

*** mappings to filter unwanted combinations
vinExists(ttot,vin)                "Can this vintage cohort exist i.e. ttot cannot be before cohort starts"
renAllowed(bs,hs,bsr,hsr)          "Is this renovation transition allowed"
renTarAllowed(flow, bsr, hsr)      "Is this renovation/construction target allowed"
sameState(bs,hs,bsr,hsr)           "Is the state after the renovation the same as before"
renEffective(bs,hs,bsr,hsr)        "Renovations without untouched buildings"
conAllowed(bsr, hsr, vin)  "Is this construcion allowed, i.e. exclude zero status and vintages other than the default"
refVarExists (ref,refVar,reg,ttot) "There is a value for this combination of reference, variable, region and period"

*** Temporary: To check whether the stock calibration uses the right flow
zeroFlow(bs, hs, bsr, hsr)

*** control sets (should be empty)
ErrStock(bs,hs,vin,reg,loc,typ,inc,ttot)              "Error in stock of buildings"
ErrConstruction(bs,hs,reg,loc,typ,inc,ttot)           "Error in flow of new buildings"
ErrRenovation(bs,hs,bsr,hsr,vin,reg,loc,typ,inc,ttot) "Error in flow of renovated and untouched buildings"
ErrDemolition(bs,hs,vin,reg,loc,typ,inc,ttot)         "Error in flow of demolished buildings"

refVarRef(ref,refVar) "mapping references to reference variables"

*** heating system ban
hsBan(var,reg,ttot,hs) "heating systems are forbidden in the respective variable after given period"
;

*** aliases
alias(state,state2,state3,state4);
alias(stateFull,stateFull2,stateFull3,stateFull4);
alias(renAllowed,renAllowed2);


*** initialise multi-dimensional sets ------------------------------------------

*** load fundamental sets
$gdxin input.gdx
$load renAllowed
$load vinExists
$load hsBan
$gdxin

$ifthen.matching "%RUNTYPE%" == "matching"
$gdxin references.gdx
$load refVarRef
$load refVarExists
$gdxin
$endif.matching

all_subs(reg,loc,typ,inc) = yes;
subs(all_subs)            = yes;
stateFull(bsr,hsr)        = yes;
state(bs,hs)              = yes;
ren(state,stateFull)      = yes;
$ifthen.calibration "%RUNTYPE%" == "calibration"
*** TODO: Adapt this so that only the vintages relevant for the calibration are taken into account (i.e. vintages that already exist for the calibration years)
loop(tcalib,
  vinCalib(vin)$vinExists(tcalib, vin) = yes;
);

loop(renAllowed(bs, hs, bsr, hsr),
  renTarAllowed("ren", bsr, hsr) = yes;
);
loop((bsr, hsr)$(bs(bsr) and hs(hsr)),
  renTarAllowed("con", bsr, hsr) = yes;
);

***Determine sets of flows which are included in the stock calibration
$ifthenE.shell (sameas("%ignoreShell%","TRUE"))
 loop((bs,hs,bsr,hsr),
   zeroFlow(bs,hs,bsr,hsr)$(renAllowed(bs,hs, bsr, hsr) and sameas(hsr,"0")) = YES;
 );
$else.shell
loop((bs,hs,bsr,hsr),
   zeroFlow(bs,hs,bsr,hsr)$(renAllowed(bs,hs, bsr, hsr) and (sameas(bsr, "0") or sameas(hsr,"0"))) = YES;
 );
$endIf.shell
$endif.calibration
* sets
* vinCalib(vin)  "Dynamic vintages in calibration"
* /1980-1989, 1990-1999, 2000-2010, 2011-2020 /
* ;

*** TODO: initialise mappings with loaded data
sameState(bs,hs,bsr,hsr)$(    (sameas(bsr,bs) or sameas(bsr,"0"))
                          and (sameas(hsr,hs) or sameas(hsr,"0"))) = yes;
renEffective(bs,hs,bsr,hsr)$(not(sameas(bsr,"0") and sameas(hsr,"0"))) = yes;


*** temporal fixes, to be checked ---------------------------------------------

$ifthen.matching "%RUNTYPE%" == "matching"

* define refs here to avoid recalculating matching data
r(ref) = no;
r("mredgebuildings_location") = yes;
r("mredgebuildings_buildingType") = yes;
r("mredgebuildings_heating") = yes;
r("mredgebuildings_vintage") = no;
r("EUBDB_vintage") = yes;
r("Odyssee_constructionFloor") = yes;
r("Odyssee_heatingShare") = yes;
r("IDEES_heatingShare") = yes;
r("EuropeanCommissionRenovation") = yes;

$endif.matching
