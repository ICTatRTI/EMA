$title Electricity Dispatch Partial Equilibrium Model
* Copyright RTI International 1/1/2011

* printing options
$onSymList
$offinclude
$onInLine


**--- Include sets ---**
$include '..\Model\sets_STATE.gms'


**--- Include data ---**
$include '..\Model\data_STATE.gms'

**--- Variables for Model ---**
positive variables
        DemandSeg(r,t)          Electricity demand by load segment
        GEN(r,u,v,l,t)          Generation by units
        CAP(r,u,v,t)            Capacity of units
        Fuel(r,f,t)             Fuel use
        Biomass(r,biostep,t)    Biomass fuel use
        Transmit(r,rr,l,t)      Transmission
        Emis(r,p,t)             Emissions
;

variables
        obj                     Objective (producer surplus)
        Demand(r,t)             Total annual demand index by region;

* Model equations *
equations
        objdef                  Objective function definition
        c_DemandSeg(r,l,t)      Electricity demand by load segment
        c_Demand(r,t)           Total electricity demand
        c_GEN(r,u,v,l,t)        Generation
        c_Fuel(r,f,t)           Fuel use
        c_Biomass(r,t)          Biomass use
        c_Emis(r,p,t)           Emissions
;

**--- Objective Function ---**
objdef..
        obj =e= sum(t, sum(r,

* variable O&M costs (TWh times $/MWh equals $ million)
        + sum((feasible(r,u,v,t),l), GEN(r,u,v,l,t) * vomcost(r,u) )

* fixed O&M costs (GW times $/kW-yr equals $ million)
        + sum(feasible(r,u,v,t), CAP(r,u,v,t) * fomcost(r,u))

* fuel costs - without biomass (TWh times MMBtu/GWh times $/MMBtu divided by 1e+3 equals $ million)
        + sum((feasible(r,u,v,t),l), GEN(r,u,v,l,t) * sum(f$(not bio(f)), pf(r,f,t) * fueltype(u,f) * heatrate(r,u,v)) * 1e-3 )  /* scaling prices to put everything in $ million */

* biomass costs by supply step
        + sum(biostep, biomass(r,biostep,t) * pbio(r,biostep,t) * 1e-3 ) /* scaling prices to put everything in $ million */

* wheeling charges (TWh times $/kWh * 1e-3 equals $million)
* (demand in TWh) - 2.9 cents per kWh = $2,900,000 per TWh, 2.9 mills per kWh is $0.0029/kWh
        + sum((rr,l), Transmit(r,rr,l,t) * 0.029 * 1e-3)
))
;

**--- Constraints ---**
* Total annual demand
c_Demand(r,t)..
        Demand(r,t) =e= sum(l, DemandSeg(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t));

* Demand by load segment
c_DemandSeg(r,l,t)..
          sum(feasible(r,u,v,t), GEN(r,u,v,l,t))
        + sum(rr, Transmit(r,rr,l,t) * (1-0.02)) /* transmission losses = 2 percent of imports */
        - sum(rr, Transmit(rr,r,l,t))
          =g=
        DemandSeg(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t);

* Generation (GW times hours per load segment)
c_GEN(r,u,v,l,t)$(feasible(r,u,v,t) and hours(r,l) and not ps(u))..
        GEN(r,u,v,l,t) =l= CAP(r,u,v,t) * hours(r,l) * maxCF(r,u,l) * 1e-3 /* scaling constant: GEN in TWh*/
;

** Constraints for calculating output variables
* Fuel use (trillion Btu)
c_Fuel(r,f,t)..
        Fuel(r,f,t) =e= sum((u,v,l), GEN(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,f)) ;

* Biomass use - aggregated across supply steps (trillion Btu)
c_Biomass(r,t)..
        sum(biostep, biomass(r,biostep,t)) =g= fuel(r,"bio",t);

* CO2 Emissions in MMTC (GWh times MMBtu/GWh times kg/MMBtu divided by 1 million = million metric tons)
c_Emis(r,p,t)$CO2(p)..
        Emis(r,p,t)
          =e=
        sum((u,v,l), GEN(r,u,v,l,t) * heatrate(r,u,v) * emis_factor(r,u,p)) / 1e+6; /* scaling constant*/

model elec /all/;

** Some bonus constraints!

* upper bound on transmission
transmit.up(r,rr,l,t)                                   = transmit_limit(r,rr) * hours(r,l) * 1e-3; /* scaling constant */

* set capacities from data
*CAP.up(r,u,"2020",t)                                    = capacity(r,u,"2020");
* I changed this from an upper bound to an equality constraint */
CAP.fx(r,u,"2020",t)                                    = capacity(r,u,"2020");
* anything else must have 0 capacity
CAP.fx(r,u,"2020",t)$(not capacity(r,u,"2020"))         = 0;

* generation = 0 for infeasible combinations
GEN.fx(r,u,v,l,t)$(not feasible(r,u,v,t) and not ps(u)) = 0;
GEN.fx(r,u,"2020",l,t)$(not capacity(r,u,"2020"))       = 0;

GEN.fx(r,u,v,l,t)$(ps(u))                               = 0;

* New constraint to keep the demand multiplier from being suppressed to zero with the absense of the Consumer Surplus constraint
DemandSeg.fx(r,t)                                       = 1;

* upper bound on biomass use to equal supply
biomass.up(r,biostep,t)         = biosupply(r,biostep,t);

* Solve the model
elec.holdfixed  = 1;
elec.iterlim = 250000;
option QCP=CPLEX
solve elec using qcp minimizing obj;

* save results
$include '..\Model\findings_STATE.gms'
