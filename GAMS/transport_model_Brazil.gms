$title Transport Optimization of Soy Products between Brazilian Municipalities

$onText
$offText

* enable end-of line comments
$onEOLCom

Set 	a 'municpalities'
$call GDXXRW MUN_codes.xlsx trace=3 set=a rng=MUN_codes!A1:B5572 rDim=1
$GDXIN MUN_codes.gdx
$LOADDC a
$GDXIN
;
Sets mode 'transport mode'
    	   /truck, train, ship/
    product 'type of soy product' 
       /bean, oil, cake /;	
;

Alias (a,b); 

Parameter ctransport(a,b,mode) table of pair-wise transport cost for each MU pair and mode in $ per ton
$call CSV2GDX  .\transport_cost.csv output = .\transport_cost.gdx id=ctransport index = 1,2 values = 3..lastCol useHeader= y trace = 3
$GDXIN .\transport_cost.gdx
$LOADDC ctransport
$GDXIN

Parameter demand(a,product) 'demand for each soy product in each MU (t)'
$call GDXXRW .\excess_demand.xlsx output=.\demand.gdx par=demand rng=demand!A1:D5573 Rdim=1 Cdim=1
$GDXIN .\demand.gdx
$LOADDC demand
$GDXIN

Parameter supply(a, product) 'supply of each soy product in each MU (t)'
$call GDXXRW .\excess_supply.xlsx output=.\supply.gdx par=supply rng=supply!A1:D5573 Rdim=1 Cdim=1
$GDXIN .\supply.gdx
$LOADDC supply
$GDXIN

Parameter  modal_split(mode) 'modal split (share of each mode on total soy transport kilometers in Brazil)' 
*NOTE: to start, assume that the modal split as well as transport cost per ton are the same for all three products
            /truck 0.7,
            train 0.2,
            ship  0.1 /
          report1(product,*) 'report sheet to summarize results for each product'
          report2(*,*,*) 'report sheet to summarize transport amounts between MUs across all transport modes'
;

Free variable
    xtotalcost 'total transport cost'
    xtotalsoytransport(product) 'total amount of ton-kilometers transported'
;

Positive variable
    xsoytransport(a,b,mode,product) 'transport from a to b in tons by mode'
;

Equation
    cost 'objective function to be minimized'
    demand_const(a,product) 'constraint to ensure demand of all municipalities is met'
    supply_const(a,product) 'constraint to ensure that transport from all municipalities does not exceed their supply'
    totaltransport(product) 'total amount of ton-kilometers transported'
    modal_const(mode,product) 'constraint to ensure that modal split is maintained on aggregate level'
 
;

cost.. 
    SUM((a,b,mode,product), ctransport(a,b,mode) *  xsoytransport(a,b,mode,product)$(supply(a,product) and demand(b,product))) =E= xtotalcost  ;

demand_const(b, product)..
    SUM((a,mode), xsoytransport(a,b,mode,product)$(supply(a,product) and demand(b,product))) =G= demand(b,product);

supply_const(a, product)..
    SUM((b,mode), xsoytransport(a,b,mode,product)$(supply(a,product) and demand(b,product))) =L= supply(a,product) ;
  
totaltransport(product)..
    SUM((a,b,mode), xsoytransport(a,b,mode,product)$(supply(a,product) and demand(b,product))) =E= xtotalsoytransport(product);

* impose modal split on overall tranport, not differentiating between products  
*modal_const(mode)..
*    SUM((a,b,product), xsoytransport(a,b,mode,product)$(supply(a,product) and demand(b,product))) =E= SUM(product, xtotalsoytransport(product) * modal_split(mode));

* impose modal split seperately on each product
modal_const(mode, product)..
    SUM((a,b), xsoytransport(a,b,mode,product)$(supply(a,product) and demand(b,product))) =E= xtotalsoytransport(product) * modal_split(mode);



Model transport_model / all /;


*use CPLEX and define some options for it (see bottom of script)
option LP=CPLEX;
transport_model.optfile=1;

* some options to save memory according to https://support.gams.com/solver:error_1001_out_of_memory and https://yetanothermathprogrammingconsultant.blogspot.com/2009/06/large-easy-lps-are-modeling-systems.html
option limrow=0;
option limcol=0;
option iterlim=1000000;
option solvelink = 0;
transport_model.solprint=0;
transport_model.dictfile=0;
transport_model.holdfixed=1;


* solve model for each product
solve transport_model using lp minimizing xtotalcost;


* compile report of main results for each product 
report1(product, "total cost") = SUM((a,b,mode), ctransport(a,b,mode) *  xsoytransport.l(a,b,mode,product));
report1(product, "tons") = xtotalsoytransport.l(product);
report1(product, "tons_truck") = SUM((a,b), xsoytransport.l(a,b,"truck",product));
report1(product, "tons_train") = SUM((a,b), xsoytransport.l(a,b,"train",product));
report1(product, "tons_ship") = SUM((a,b), xsoytransport.l(a,b,"ship",product));
display report1

$ontext
* compile matrix of transport volumes across all modes 
report2(product,a,b) =  SUM(mode, xsoytransport.l(a,b,mode,product))
display report2

$offtext


execute_unload 'transport_sol.gdx',xsoytransport,xtotalsoytransport,xtotalcost;


* options for cplex solver recommendet in https://support.gams.com/solver:error_1001_out_of_memory
$onecho > cplex.opt
lpmethod 0
names no
threads 1
memoryemphasis 1
$offecho




