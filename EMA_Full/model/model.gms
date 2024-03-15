$title Electricity Market Analysis model (EMA)
* Copyright RTI International 1/1/2011

$if not set scn $set scn ces
$if not set carbtax $set carbtax 20

*--- next steps ---*
* add nuclear uprates and retirements


**--- Include sets ---**
$include sets.gms


**--- Include data ---**
$include data.gms

parameter
        deflator;

deflator        = 1 / 1.105783;


**--- Add EMF limits ---**
$ontext
Electricity Regulatory Policy: Renewable Portfolio Standard (RES):
The RES applies only to the electricity sector. In this case, renewable energy includes all hydroelectric power and bioenergy.
The RES is defined as 20% by 2020, 30% by 2030, 40% by 2040, and 50% by 2050.
If modelers are unable to meet these requirements within their model, they should create a scenario that includes a less aggressive RES,
but one that can be met by the model.

Electricity Regulatory Policy: Clean Electricity Standard (in the optional policy scenarios only):
This policy is similar to the RES, but also includes nuclear power, fossil electricity with carbon capture and storage (credited at 90%),
and natural gas (credited at 50%) in the portfolio.  Because many additional sources are allowed to receive credit,
the targets are defined as 50% by 2020, 70% by 2030, 80% by 2035, 90% by 2040, and constant thereafter
(note that the current share of clean energy in the U.S., as defined here, is 42.5%).  All other characteristics are identical to the RES.
$offtext

parameter
        RESlvl          level of RES
        RESswtch        switch on RES policy
        CESlvl          level of CES
        CESswtch        switch on CES policy
        minGENlvl       minimum generation levels;

minGENlvl(colu)         = 0.5;
*minGENlvl(stog)        = 0.25;

RESswtch(t)             = no;
CESswtch(t)             = no;

RESlvl("2020")          = 0.20;
RESlvl("2025")          = 0.25;
RESlvl("2030")          = 0.30;
RESlvl("2035")          = 0.35;
RESlvl("2040")          = 0.40;
RESlvl("2045")          = 0.45;
RESlvl("2050")          = 0.50;
RESlvl("2075")          = RESlvl("2050");

$if %scn%==res  RESswtch(t)$RESlvl(t)   = yes;
display RESswtch;

CESlvl("2020")          = 0.50;
CESlvl("2025")          = 0.55;
CESlvl("2030")          = 0.70;
CESlvl("2035")          = 0.80;
CESlvl("2040")          = 0.90;
CESlvl("2045")          = 0.90;
CESlvl("2050")          = 0.90;
CESlvl("2075")          = CESlvl("2050");

$if %scn%==ces  CESswtch(t)$CESlvl(t)   = yes;
display CESswtch;

* IPM joint limits on new construction of nuclear/CCS

table IPMlimit
        2020    2025    2030    2035    2040    2045    2050    2075
nuc     7.50    18.45   29.40   57.92   86.44   156.16  225.89  451.77
CCS     9.75    23.99   38.22   75.29   112.37  203.01  293.65  587.30
*nuc    12      24      40      60      90      140     210     420
*CCS    27      48      80      120     200     325     450     900
;

parameter IPMratio      ratio of nuclear to CCS;

IPMratio = IPMlimit("CCS","2020") / IPMlimit("nuc","2020");


** add limits to solar to avoid huge increases **

table SLRlimit
        2020    2025    2030    2035    2040    2045    2050    2075
slrPV_N
slrCSP_N                                                                ;

cap_limit("slrPV_N",t)          = sum(r,capacity(r,"slr_X","2010")) * (1.15)**(t.val-2010);
cap_limit("slrCSP_N",t)         = sum(r,capacity(r,"slr_X","2010")) * (1.15)**(t.val-2010);

cap_limit("wnd_N",t)            = sum(r, capacity(r,"wnd_X","2010")) * (1.10)**(t.val-2010);

display cap_limit;

parameter chk_carb_prc;

* setting carbon tax after final BAU solve of CGE
$if %scn%==carbtax      carbtax(t)$(t.val ge 2015)      = %carbtax% * (1.05)**(t.val - 2015);
$if %scn%==carbtax      carbtax("2075")                 = carbtax("2050")*(1.025)**25;



**--- Variables for Model ---**
positive variables
        DemandSeg(r,t)          Electricity demand by load segment
        GEN(r,u,v,l,t)          Generation by units
        CAP(r,u,v,t)            Capacity of units
        INV(r,u,v,t)            Investment in new capacity
        Retire(r,u,v,t)         Retirement of capacity
        Fuel(r,f,t)             Fuel use
        Biomass(r,biostep,t)    Biomass fuel use
        Transmit(r,rr,l,t)      Transmission
        Emis(r,p,t)             Emissions
        Emis_Saved(p,t)         Emission allowances saved
        Emis_Withdrawn(p,t)     Emission allowances withdrawn
        Emis_Bank(p,t)          Emissions bank
        rm_wind(r,rm_step,u,t)
        rm_slr(r,rm_step,u,t)
        wind_cap(r,u,cc)        New wind capacity (GW)
;

variables
        obj                     Objective (producer surplus)
        Demand(r,t)             Total annual demand index by region;

* Model equations *
equations
        objdef                  Objective function definition
        c_DemandSeg(r,l,t)      Electricity demand by load segment
        c_Demand(r,t)           Total electricity demand
        c_RsrvMargin(r,t)       Reserve margins
        c_GEN(r,u,v,l,t)        Generation
        c_Fuel(r,f,t)           Fuel use
        c_Biomass(r,t)          Biomass use
        c_INV(r,u,v,t)          Investment in new capacity
        c_CapLimit(u,t)         Capacity limits on new construction
        c_WindLimit(r,u,t)      Limit on wind resources (by wind class and cost class - as part of "u" definition)
        c_GeoLimit(r,u,t)       IPM limits on new geothermal resources
        c_LFGlimit(r,u,t)       Limit on availability of landfill gas (by class)
        c_Emis(r,p,t)           Emissions
        c_EmisCap(p,t)          Emissions caps
        c_EmisBank(p,t)         Emissions banking
        c_RES(t)                RES level
        c_CES(t)                CES level
        c_IPMlimit              IPM limits on joint construction of nuclear and CCS
        c_windCV(r,u,t)
        c_windstep(r,rm_step,t)
        c_slrCV(r,u,t)
        c_slrstep(r,rm_step,t)
        c_minGEN(r,u,v,l,t)
        c_Retire(r,u,v,t)
        c_totwind(t)
;

*wind_cost(r,u,cc,t) = 0.5*wind_cost(r,u,cc,t) ;

**--- Objective Function ---**
objdef..
        obj =e= sum(t, pv(t) * sum(r,

* variable O&M costs (TWh times $/MWh equals $ million)
        + sum((feasible(r,u,v,t),l), GEN(r,u,v,l,t) * pvom(r,t) * vomcost(r,u) )

* fixed O&M costs (GW times $/kW-yr equals $ million)
        + sum(feasible(r,u,v,t), CAP(r,u,v,t) * pfom(r,t) * fomcost(r,u))

* fuel costs - without biomass (TWh times MMBtu/GWh times $/MMBtu divided by 1e+3 equals $ million)
        + sum((feasible(r,u,v,t),l), GEN(r,u,v,l,t) * sum(f$(not bio(f)), pf(r,f,t) * fueltype(u,f) * heatrate(r,u,v)) * 1e-3 )

* biomass costs by supply step
        + sum(biostep, biomass(r,biostep,t) * pbio(r,biostep,t) * 1e-3 )

* wind costs from NREL ($ per MWh)
*        + sum((u,l,cc)$wndn(u), (wind_cap(r,u,cc) * hours(r,l) * maxCF(r,u,l) * 1e-3) * pvom(r,t) * wind_cost(r,u,cc,t))

* carbon taxes
        + (carbtax(t) * sum((u,v,l), GEN(r,u,v,l,t) * heatrate(r,u,v) * emis_factor(r,u,"CO2")) * 1e-6 )

* carbon sequestration costs
        + (carbseq(t) * sum((u,v,l)$ccsu(u), GEN(r,u,v,l,t) * heatrate(r,u,v) * emis_factor(r,u,"CO2")) * 1e-6 )$carbtax(t)

* wheeling charges (TWh times $/kWh * 1e-3 equals $million)
* (demand in TWh) - 2.9 cents per kWh = $2,900,000 per TWh, 2.9 mills per kWh is $0.0029/kWh
        + sum((rr,l), Transmit(r,rr,l,t) * 0.029 * 1e-3)

))

* capital costs for new units (GW times $/kW equals $ million)
+ sum(t, delta(t) * (
          sum(payback, ( (1/(1+rho))**(ord(payback)-1) ) * (
*       + sum((r,u,v)$newu(u), INV(r,u,v,t) * (pcap(r,t)/ypp(t)) * capcost(r,u,t))
        + sum((r,u,v)$(newu(u)), INV(r,u,v,t) * pcap(r,t) * k_chrg(u) * (1+IDC) * capcost(r,u,t))
* PTS wind transmission costs ($/MW) from NREL: intra-region getting-to-grid supply curve
*        + sum((r,u,cc)$wndn(u), wind_cap(r,u,cc) * pcap(r,t) * k_chrg(u) * (1+IDC) * wind_cost(r,u,cc,t))
        + sum((r,u,cc)$wndn(u), wind_cap(r,u,cc) * pcap(r,t) * k_chrg(u) * (wind_cost(r,u,cc,t)-50))
        ))
))

**- consumer surplus -**

        - sum(t, pv(t) * sum(r, (lnr_coef(r,t) * Demand(r,t) + qdr_coef(r,t) * Demand(r,t) * Demand(r,t)) ))
;

* Total annual demand
c_Demand(r,t)..
        Demand(r,t) =e= sum(l, DemandSeg(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t));

* Demand by load segment
c_DemandSeg(r,l,t)..
          sum(feasible(r,u,v,t), GEN(r,u,v,l,t))
        + sum(rr, Transmit(r,rr,l,t) * (1-0.02))
        - sum(rr, Transmit(rr,r,l,t))
*       + ( capacity(r,"ps_X","2010") * hours(r,l) * 1e-3 * maxCF(r,"ps_X",l))$daytime(l)
*       - ( capacity(r,"ps_X","2010") * hours(r,l) * 1e-3 * maxCF(r,"ps_X",l) * (1+(1-PS_effic)) )$night(l)
*       - ( PS_frac(l) * capacity(r,"ps_X","2010") * 1e-3 * hours(r,l) * maxCF(r,"ps_X",l) * (1+(1-PS_effic)) )$mrnevn(l)
          =g=
        DemandSeg(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t);

* Generation (GW times hours per load segment)
c_GEN(r,u,v,l,t)$(feasible(r,u,v,t) and hours(r,l) and not ps(u))..
        GEN(r,u,v,l,t) =l= CAP(r,u,v,t) * hours(r,l) * maxCF(r,u,l) * 1e-3
* PTS moved curtailments from load constraint to generation-tracking constraint to get RES & CES right
         - sum(rm_step, rm_wind(r,rm_step,u,t)*maxCF(r,u,l)*hours(r,l)*w_curtail(rm_step,l,t) * 1e-3)$wndu(u)
         - sum(rm_step, rm_slr(r,rm_step,u,t)*maxCF(r,u,l)*hours(r,l)*s_curtail(rm_step,l) * 1e-3)$slrpvu(u)
;


* Minimum generation
c_minGEN(r,u,v,l,t)$(feasible(r,u,v,t) and minGENlvl(u))..
        (GEN(r,u,v,l,t) / hours(r,l))   =g= (minGENlvl(u) * GEN(r,u,v,"peak",t) / hours(r,"peak") );

* Investment in new capacity
c_INV(r,u,v,t)$(newv(v) and newu(u))..
        CAP(r,u,v,t) =e= CAP(r,u,v,t-1) + INV(r,u,v,t);

c_Retire(r,u,v,t)$(feasible(r,u,v,t) and (colx(u) or ngcc(u) or ngtbx(u)) and not tv(v,t) )..
        CAP(r,u,v,t)    =e= CAP(r,u,v,t-1) - Retire(r,u,v,t);

* Reserve Margin over peak load
c_RsrvMargin(r,t)$(t.val gt 2010)..
          sum(feasible(r,u,v,t)$(not (wndu(u) or slrpvu(u))), CAP(r,u,v,t) * rsrv_factor(r,u))
        + sum((rm_step,u)$wndu(u), rm_wind(r,rm_step,u,t)*wind_cv(r,rm_step) * maxCF(r,u,"sum_day"))
        + sum((rm_step,u)$slrpvu(u), rm_slr(r,rm_step,u,t)*slr_cv(r,rm_step) * maxCF(r,u,"sum_day"))
          =g=
        Peak(r,t) * (1+RsrvMargin(r));

c_windCV(r,u,t)$wndu(u)..
        sum(rm_step, rm_wind(r,rm_step,u,t)) =g= sum(v$vt(v,t), CAP(r,u,v,t));

c_windstep(r,rm_step,t)..
        sum(u$wndu(u), rm_wind(r,rm_step,u,t) * sum(l, hours(r,l) * maxCF(r,u,l) * 1e-3))
*       =l= sum((feasible(r,u,v,t),l), GEN(r,u,v,l,t))*wind_bin(rm_step);
        =l= sum(l, Demand(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t) * wind_bin(rm_step));

c_slrCV(r,u,t)$slrpvu(u)..
        sum(rm_step, rm_slr(r,rm_step,u,t)) =g= sum(v$vt(v,t), CAP(r,u,v,t));

c_slrstep(r,rm_step,t)..
        sum(u$wndu(u), rm_slr(r,rm_step,u,t) * sum(l, hours(r,l) * maxCF(r,u,l) * 1e-3))
*       =l= sum((feasible(r,u,v,t),l), GEN(r,u,v,l,t))*wind_bin(rm_step);
        =l= sum(l, Demand(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t) * slr_bin(rm_step));

* Limits on total capacity by type
c_CapLimit(u,t)$cap_limit(u,t)..
        sum((r,v), CAP(r,u,v,t)) =l= cap_limit(u,t);

* Wind capacity limits
c_WindLimit(r,wnd,t)..
        sum(v, CAP(r,wnd,v,t)) =l= sum(cc, wind_cap(r,wnd,cc));

c_totwind(t)..
        sum((r,wnd,v), CAP(r,wnd,v,t))  =l= cap_limit("wnd_N",t);

* Geothermal capacity limits
c_GeoLimit(r,geo,t)..
        sum(v, CAP(r,geo,v,t)) =l= geo_limit(r,geo);

* LFG capacity limits
c_LFGlimit(r,lfg,t)..
        sum(v, CAP(r,lfg,v,t)) =l= lfg_limit(r,lfg);

* Fuel use (trillion Btu)
c_Fuel(r,f,t)..
        Fuel(r,f,t) =e= sum((u,v,l), GEN(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,f)) ;

* Biomass use - aggregated across supply steps (trillion Btu)
c_Biomass(r,t)..
        sum(biostep, biomass(r,biostep,t)) =g= fuel(r,"bio",t);

* IPM limits on construction
c_IPMlimit(t)..
        sum((r,u,v)$nucN(u), IPMratio * CAP(r,u,v,t)) + sum((r,u,v)$ccsu(u), CAP(r,u,v,t))
                =l= IPMlimit("CCS",t);

* RES policy
c_RES(t)$RESswtch(t)..
        sum((r,u,v,l)$resu(u), GEN(r,u,v,l,t))
                =g= RESlvl(t) * sum((r,u,v,l), GEN(r,u,v,l,t));

* CES policy
c_CES(t)$CESswtch(t)..
        sum((r,u,v,l)$(cesu(u) and not (ccsu(u) or gasu(u))), GEN(r,u,v,l,t)) +
        sum((r,u,v,l)$ccsu(u), 0.9 * GEN(r,u,v,l,t)) +
        sum((r,u,v,l)$gasu(u), 0.5 * GEN(r,u,v,l,t))
                =g= CESlvl(t) * sum((r,u,v,l), GEN(r,u,v,l,t));

* CO2 Emissions in MMTC (GWh times MMBtu/GWh times kg/MMBtu divided by 1000 divided by 1 million = million metric tons)
c_Emis(r,p,t)$CO2(p)..
        Emis(r,p,t)
          =e=
        sum((u,v,l), GEN(r,u,v,l,t) * heatrate(r,u,v) * emis_factor(r,u,p)) / 1e+6;

* Caps on emissions
c_EmisCap(p,t)$CO2(p)..
        sum(r, Emis(r,p,t)) =l= Emis_Cap(p,t) - Emis_Saved(p,t) + Emis_Withdrawn(p,t);

* Banking/Borrowing of emissions allowances
c_EmisBank(p,t)$CO2(p)..
        Emis_Bank(p,t) =e=
                + Emis_Bank(p,t-1)$(ord(t) ne 1)
                + ypp(t) * Emis_Saved(p,t)
                - ypp(t) * Emis_Withdrawn(p,t);

model elec /all/;
*model elec /all-c_RsrvMargin/;

*demandseg.fx(r,t) = 1;

transmit.up(r,rr,l,t)                                   = transmit_limit(r,rr) * hours(r,l) * 1e-3;

CAP.up(r,u,"2010",t)                                    = capacity(r,u,"2010");
CAP.fx(r,u,"2010",t)$(not capacity(r,u,"2010"))         = 0;

GEN.fx(r,u,v,l,t)$(not feasible(r,u,v,t) and not ps(u)) = 0;
GEN.fx(r,u,"2010",l,t)$(not capacity(r,u,"2010"))       = 0;

GEN.fx(r,u,v,l,t)$(ps(u))                               = 0;

GEN.up(r,baseu,"2010",l,t)$feasible(r,baseu,"2010",t)   = capacity(r,baseu,"2010") * hours(r,l) * maxCF(r,baseu,l) * 1e-3;

INV.fx(r,u,v,t)$(not newu(u))                           = 0;
INV.fx(r,u,"2010",t)$(not ngtbN(u))                     = 0;
* PTS no new nuclear
INV.fx(r,nuc_n,v,t)                           = 0;
* PTS no new CCS
INV.fx(r,ccsu,v,t)                           = 0;

* force in new capacity from IPM (fix "_X" notation)
INV.lo(r,u,"2015",t)$capacity(r,u,"2015")               = capacity(r,u,"2015");

INV.fx(r,u,v,t)$(ord(v) ne ord(t))                      = 0;

Emis_Saved.fx(p,t)$(ord(t) eq card(t))                  = 0;
Emis_Saved.fx(p,t)$(ord(t) eq 1)                        = 0;

Retire.l(r,u,v,t)                                       = 0;

Retire.up(r,u,v,"2075")                                 = 0;
Retire.up(r,u,v,t)$(not (colx(u) or ngcc(u) or ngtbx(u) or stog(u))) = 0;

* upper bound on biomass use to equal supply
biomass.up(r,biostep,t)         = biosupply(r,biostep,t);

* upper bound on wind capacity from NREL
wind_cap.up(r,u,cc)             = wind_limit(r,u,cc);


* eliminate nuclear & CCS for some scenarios
*$if %scn%==US1  INV.fx(r,nucN,v,t)                     = 0;
*$if %scn%==US1  INV.fx(r,ccsu,v,t)                     = 0;



elec.holdfixed  = 1;
elec.iterlim = 250000;
option QCP=CPLEX
solve elec using qcp minimizing obj;

display wind_cap.l, wind_cap.m, INV.l, INV.m ;
* save results
$include findings.gms
* save results
$batinclude output\report %scn%


execute_unload 'output\EMF_ELEC_results_%scn%.gdx',EMF_elec;

execute_unload 'output\results_%scn%.gdx', USAphy%scn%,USAdlr%scn%;

parameter
        carb_results;

carb_results(t,"emis")          = sum(r, emis.l(r,"co2",t));
carb_results(t,"carbtax")       = carbtax(t);
carb_results(t,"price")         = ( sum((r,l),(c_demandseg.m(r,l,t) * DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) / sum((r,l),(DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) ) / pv(t);
carb_results(t,"nuc")           = sum((r,nucu,v,l), gen.l(r,nucu,v,l,t));
carb_results(t,"CCS")           = sum((r,u,v,l)$ccsu(u), GEN.l(r,u,v,l,t));

option carb_results:1;
display carb_results;
