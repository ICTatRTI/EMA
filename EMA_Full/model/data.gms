$title Data for Electricity Market Analysis model (EMA)

* data
parameters
        capacity(r,u,v)         capacity of existing units
        heatrate(r,u,v)         heat rate of existing units
        size(r,u,v)             size of existing units
        count(r,u)              number of existing units
        emis_factor(r,u,p)      pollutant emissions factors (CO2 SO2 NOx Hg)

        capcost(r,u,v)          capital costs ($ per kW)
        fomcost(r,u)            fixed O&M ($ per kW)
        vomcost(r,u)            variable O&M ($ per MWh)
        pf(r,f,t)               fuel prices
        fueltype(u,f)           type of fuel used by a unit

        pele(r,t)               electricity price ($ per MWh)
        dele(r,t)               annual demand (TWh)
        loadpct(r,l)            percentage of demand by load segment
        dadj(r,t)               demand adjustment to turn demand into NEL (including CHP and trade)
        hours(r,l)              hours per load segment
        maxCF(r,u,l)            maximum capacity factor
        peak(r,t)               peak demand
        RsrvMargin(r)           reserve margin
        rsrv_factor(r,u)        reserve margin contribution factor for wind-solar

        transmit_limit(r,rr)    transmission limits
*       wind_limit              maximum wind capacity by wind class and cost class
        geo_limit               maximum geothermal
        lfg_limit               maximum landfill gas

        wind_limit(r,u,cc)      Limits on wind resource (GW)
        wind_cost_nrel(r,u,cc)  Cost of wind transmission ($ per kW)

        pbio(r,biostep,t)       price of biomass by step function
        biosupply(r,biostep,t)  supply of biomass by step function
        chk_bio
        chk_wind
;

$gdxin '..\data\output\model_data.gdx'
$loaddc capacity heatrate size count maxCF capcost fomcost vomcost hours peak transmit_limit RsrvMargin rsrv_factor dele pele dadj loadpct pf fueltype geo_limit lfg_limit emis_factor pbio biosupply wind_limit wind_cost_nrel=wind_cost

biosupply(r,biostep,t)  = biosupply(r,biostep,t) * 1e3;

* adjust


* additional parameters
$set rho 0.05
parameter
        ypp(t)                  years per period
        rho                     interest rate per year          /%rho%/
        delta(yr)               depreciation
        pv(yr)                  present value of period t

        fuelcoef(r,u,f)         fuel use coefficient (for biomass cofiring)
        pvom(r,t)               price index for variable O&M
        pfom(r,t)               price index for fixed O&M
        pcap(r,t)               price index for capital costs
*       pfuel(r,f,t)            price index for fuel costs
        emis_cap(p,t)           emissions cap
        cap_limit(*,t)          limit on new capacity

        pelas(r,t)              demand elasticity for electricity
        lnr_coef                Demand coefficient for consumer surplus (linear)
        qdr_coef                Demand coefficient for consumer surplus (quadratic)

        PS_effic                pump storage efficiency
        PS_frac(l)              fraction of morning-evening hours needed to get full day-peak availability

        K_chrg                  capital charge rate (from IPM)
        IDC                     interest during construction (approx from old model)
;

delta(yr)                       = (1/(1+rho))**(yr.val-2010);

loop(yr$(yr.val lt 2050),
pv(yr)  = delta(yr) + delta(yr+1) + delta(yr+2) + delta(yr+3) + delta(yr+4);
);

loop(yr$(yr.val ge 2050),
pv(yr)
        = delta(yr) + delta(yr+1) + delta(yr+2) + delta(yr+3) + delta(yr+4)
        + delta(yr+5) + delta(yr+6) + delta(yr+7) + delta(yr+8) + delta(yr+9)
        + delta(yr+10) + delta(yr+11) + delta(yr+12) + delta(yr+13) + delta(yr+14)
        + delta(yr+15) + delta(yr+16) + delta(yr+17) + delta(yr+18) + delta(yr+19)
        + delta(yr+20) + delta(yr+21) + delta(yr+22) + delta(yr+23) + delta(yr+24);
);

display delta,pv;

fuelcoef(r,u,f)                 = 1;
fuelcoef(r,bio_cf,bio)          = 0.9;

PS_effic                        = 0.8;
PS_frac(mrnevn)
        = sum(num,  sum(daytime, hours(num,daytime)) - sum(night, hours(num,night))*(1+(1-PS_effic))  )
                / sum(num,  sum(mrnevns,hours(num,mrnevns))*(1+(1-PS_effic)) );
*PS_frac(seas) = (Hours("USA",seas,"day")+hours("USA",seas,"peak") - hours("usa",seas,"night")*(1+(1-PS_effic))) / (Hours("USA",seas,"mrnevn")*(1+(1-PS_effic)));

display PS_frac;

K_chrg(u)                       = 0.13;
K_chrg("igcc")                  = 0.13;
IDC                             = 0.2;


parameter
        CarbTax         carbon tax amount ($ per MTCO2)
        CarbSeq         carbon sequestration costs ($ per MTCO2);

carbtax("2015")                 = 0;
carbtax(t)$(t.val gt 2015)      = carbtax("2015")*(1.05)**(t.val-2015);

carbseq(t)                      = 15;


* use shares from demands in ELEC model to share out CGE demands
parameter
        dele_shr;

dele_shr(r_cge,r,t)$mapRegions(r_cge,r) = dele(r,t) / sum(mapRegions(r_cge,rr), dele(rr,t));

* index on costs (from CGE) - if in loop mode

$if set iter $goto loadCGE

pvom(r,t)                       = 1;
pfom(r,t)                       = 1;
pcap(r,t)                       = 1;

pelas(r,t)                      = - (0.15 + (1/5/10)**(ord(t)-1) );
display pelas;

$goto setdemands

$label loadCGE

$evalglobal piter %iter% - 1
$include load_CGE


$label setdemands

* P(ele)                        = pref - (1/pelas)*(pref/qref)*(qref-dele)

lnr_coef(r,t)                   = pele(r,t) * (1 - 1/pelas(r,t));

*qdr_coef(r,t)                  = 0.5 * (1/pelas(r,t))*(pele(r,t)/(dele(r,t)*dadj(r,t)));
qdr_coef(r,t)                   = 0.5 * (1/pelas(r,t))*(pele(r,t)/dele(r,t));

display lnr_coef,qdr_coef;

* adjust peak
peak(r,t)                       = peak(r,"2010") * dele(r,t) / dele(r,"2010");
display peak;



* misc *
ypp(t)$(t.val lt 2050)          = 5;
ypp(t)$(t.val ge 2050)          = 25;

emis_cap(p,t)                   = +inf;
cap_limit("nuc_N",t)            = +inf;

newv(v)$(ord(v) ne 1)           = yes;
vt("2010",t)                    = yes;
vt(v,t)$(ord(t) ge ord(v))      = yes;
tv(v,t)$sameas(v,t)             = yes;

feasible(r,u,"2010",t)$capacity(r,u,"2010")     = yes;
feasible(r,newu,newv,t)$vt(newv,t)              = yes;

* remove some new unit options in 2015
feasible(r,"col_N","2015",t)                    = no;
feasible(r,"col_Nccs","2015",t)                 = no;
feasible(r,"nuc_N","2015",t)                    = no;


$ontext
**------------- quick fixes -----------------**

* AEO 2011 approximations
capcost(r,"col_N",t)            = capcost(r,"col_N",t) * 1.23;
capcost(r,"col_Nccs",t)         = capcost(r,"col_Nccs",t) * 1.39;
capcost(r,"ngcc_N",t)           = capcost(r,"ngcc_N",t) * 1.01;
capcost(r,"ngcc_Nccs",t)        = capcost(r,"ngcc_Nccs",t) * 1.04;
capcost(r,"ngtb_N",t)           = capcost(r,"ngtb_N",t) * 1.00;
capcost(r,"nuc_N",t)            = capcost(r,"nuc_N",t) * 1.37;
capcost(r,"bio_N",t)            = capcost(r,"bio_N",t) * 0.98;
capcost(r,"lfg_N_hi",t)         = capcost(r,"lfg_N_hi",t) * 2.10;
capcost(r,"lfg_N_lo",t)         = capcost(r,"lfg_N_lo",t) * 2.10;
capcost(r,"lfg_N_vl",t)         = capcost(r,"lfg_N_vl",t) * 2.10;
capcost(r,geoN,t)               = capcost(r,geoN,t) * 1.32;
capcost(r,wndN,t)               = capcost(r,wndN,t) * 1.21;
capcost(r,slrN,t)               = capcost(r,slrN,t) * 0.9;

* AEO 2011 fuel prices
table pf_pct(f,t)
        2010    2015    2020    2025    2030    2035
col     1.112   1.044   1.070   1.108   1.120   1.126
gas     1.050   0.772   0.782   0.846   0.800   0.805
oil     1.157   0.951   0.962   0.980   0.967   0.911 ;

pf(r,cgo,t)$(t.val le 2035)
        = pf(r,cgo,t) * pf_pct(cgo,t);

pf(r,cgo,t)$(t.val gt 2035)
        = pf(r,cgo,t) * pf_pct(cgo,"2035");

* AEO 2011 demand levels
table NELchg(*,t)
        2010    2015    2020    2025    2030    2035
NEL     1.029   0.984   0.974   0.971   0.967   0.964 ;

NELchg("NEL",t)$(t.val gt 2035) = NELchg("NEL","2035");

display 'pre',dele;
*dele(r,t)      = dele(r,t) * NELchg("NEL",t);
display 'post',dele;
$offtext

* approx of transmission/distribution costs for retail price reporting
parameter
        distrib(t);

distrib("2010") = 38.5;
distrib("2015") = 38.8;
distrib("2020") = 36.1;
distrib("2025") = 33.8;
distrib("2030") = 32.4;
distrib("2035") = 31.9;
distrib("2040") = 31.9;
distrib("2045") = 31.9;
distrib("2050") = 31.9;
distrib("2075") = 31.9;


**------------- new stuff -----------------**

parameter
        wind_bin(rm_step) "wind variability bins, by fraction of load"
        wind_cv(r,rm_step) "wind capacity value by var. bin; fraction of CF"
        w_curtail(rm_step,l,t) "wind curtailment by var. bin and timeslice; fraction of output"
;

wind_bin("1")   = 0.10;
wind_bin("2")   = 0.05;
wind_bin("3")   = 0.05;
wind_bin("4")   = 0.10;
wind_bin("5")   = 0.05;
wind_bin("6")   = 0.65;

wind_cv(r,"1")  = 0.8;
wind_cv(r,"2")  = 0.6;
wind_cv(r,"3")  = 0.4;
wind_cv(r,"4")  = 0.2;
wind_cv(r,"5")  = 0;
wind_cv(r,"6")  = 0;

w_curtail("1",night,t)  = 0.02;
w_curtail("2",night,t)  = 0.10;
w_curtail("3",night,t)  = 0.25;
w_curtail("4",night,t)  = 0.70;
w_curtail("5",night,t)  = 0.75;
w_curtail("6",night,t)  = 0.80;

w_curtail("1",l,t)$(not night(l))       = 0.00;
w_curtail("2",l,t)$(not night(l))       = 0.01;
w_curtail("3",l,t)$(not night(l))       = 0.05;
w_curtail("4",l,t)$(not night(l))       = 0.30;
w_curtail("5",l,t)$(not night(l))       = 0.40;
w_curtail("6",l,t)$(not night(l))       = 0.50;

w_curtail("1","sum_day",t)              = 0.00;
w_curtail("2","sum_day",t)              = 0.01;
w_curtail("3","sum_day",t)              = 0.02;
w_curtail("4","sum_day",t)              = 0.15;
w_curtail("5","sum_day",t)              = 0.20;
w_curtail("6","sum_day",t)              = 0.30;



parameter
        slr_bin(rm_step) "wind variability bins, by fraction of load"
        slr_cv(r,rm_step) "wind capacity value by var. bin; fraction of CF"

;

slr_bin("1")   = 0.10;
slr_bin("2")   = 0.05;
slr_bin("3")   = 0.05;
slr_bin("4")   = 0.10;
slr_bin("5")   = 0.05;
slr_bin("6")   = 0.65;

slr_cv(r,"1")  = 0.1;
slr_cv(r,"2")  = 0.8;
slr_cv(r,"3")  = 0.4;
slr_cv(r,"4")  = 0.2;
slr_cv(r,"5")  = 0;
slr_cv(r,"6")  = 0;

table s_curtail(rm_step,l) "wind curtailment by var. bin and timeslice; fraction of output"
     sum_day   sum_mrnevn  sum_night   sprfl_day   sprfl_mrnevn   sprfl_night   wntr_day  wntr_mrnevn   wntr_night  peak
1     0.00       0.00        0          0.00          0.00          0            0.00      0.00            0        0.00
2     0.01       0.01        0          0.01          0.01          0            0.01      0.01            0        0.01
3     0.05       0.02        0          0.05          0.02          0            0.05      0.02            0        0.05
4     0.15       0.05        0          0.20          0.05          0            0.20      0.05            0        0.15
5     0.30       0.10        0          0.40          0.10          0            0.40      0.10            0        0.30
6     0.50       0.20        0          0.60          0.20          0            0.60      0.20            0        0.50
;


* reset things to remove NREL assumptions
*wind_cv(r,rm_step)                      = 1;
*w_curtail(rm_step,l,t)                  = 0;
*slr_cv(r,rm_step)                      = 1;
*s_curtail(rm_step,l)                  = 0;



**------------- checks -----------------**

parameter
        chk_existing
        chk_new;

chk_existing(r,u,"fom")$(capacity(r,u,"2010") and not fomcost(r,u))     = 1;
chk_existing(r,u,"vom")$(capacity(r,u,"2010") and not vomcost(r,u))     = 1;
chk_existing(r,u,"heatrate")$(capacity(r,u,"2010") and not heatrate(r,u,"2010"))        = 1;
chk_existing(r,u,l)$(capacity(r,u,"2010") and not maxCF(r,u,l)) = 1;
chk_existing(r,u,"rsrv_factor")$(capacity(r,u,"2010") and not rsrv_factor(r,u)) = 1;
chk_existing(r,u,p)$(capacity(r,u,"2010") and not emis_factor(r,u,p))   = 1;

chk_existing(r,u,p)$noCO2exist(u)                       = 0;
chk_existing(r,u,p)$(not CO2(p))                        = 0;
chk_existing(r,"ps_x","rsrv_factor")                    = 0;

chk_new(t,r,newu,"fom")$(not fomcost(r,newu))           = 1;
chk_new(t,r,newu,"vom")$(not vomcost(r,newu))           = 1;
chk_new(t,r,newu,"cap")$(not capcost(r,newu,t))         = 1;
chk_new(t,r,newu,"heatrate")$(not heatrate(r,newu,t))   = 1;
chk_new(t,r,newu,l)$(not maxCF(r,newu,l))               = 1;
chk_new(t,r,newu,"rsrv_factor")$(not rsrv_factor(r,newu) and not wndn(newu))= 1;
chk_new(t,r,newu,p)$(not emis_factor(r,newu,p))         = 1;

chk_new(t,r,geo,"cap")$(not geo_limit(r,geo))           = 0;
chk_new(t,r,wnd,"fom")$(not sum(cc,wind_limit(r,wnd,cc)))               = 0;
chk_new(t,r,geo,"fom")$(not geo_limit(r,geo))           = 0;
chk_new(t,r,lfg,"fom")$(not lfg_limit(r,lfg))           = 0;
chk_new(t,r,wnd,"vom")                                  = 0;
chk_new(t,r,slr,"vom")                                  = 0;
chk_new(t,r,geo,"vom")                                  = 0;
chk_new(t,r,lfg,"vom")$(not lfg_limit(r,lfg))           = 0;

chk_new(t,r,noCO2new,"CO2")                             = 0;
chk_new(t,r,u,p)$(not CO2(p))                           = 0;

chk_new(t,r,"slrPV_N",night)                            = 0;

chk_new(t,r,wnd,l)$(not sum(cc,wind_limit(r,wnd,cc)))           = 0;
chk_new(t,r,wnd,"rsrv_factor")$(not sum(cc,wind_limit(r,wnd,cc)))       = 0;

* don't check slr CSP for some regions
chk_new(t,r,"slrCSP_N",l)                               = 0;
chk_new(t,r,"slrCSP_N","rsrv_factor")                   = 0;

option chk_existing:2:2:1;
option chk_new:2:3:1;
display chk_existing,chk_new;

*pf(r,"gas",t)  = pf(r,"gas",t) * 1.1;

display pf;

display vt,tv,feasible,newv,newu;


**----------------- Add improvement in Wind/Solar costs to NREL data ------------------------------------**
parameter       wind_cost;

* AEO shows around 1% improvement for other capital costs (this includes all costs)
*wind_cost(r,u,cc,t)     = wind_cost_nrel(r,u,cc) * (1-0.01)**(t.val-2010);
wind_cost(r,u,cc,t)     = wind_cost_nrel(r,u,cc);
capcost(r,wnd,t)        = capcost(r,wnd,"2010") * (1-0.01)**(t.val-2010);
capcost(r,slr,t)        = capcost(r,slr,"2010") * (1-0.01)**(t.val-2010);
display wind_cost;



* add 3% to coal/gas for carbon
* http://www.eia.doe.gov/oiaf/aeo/electricity_generation.html

capcost(r,newff,v)      = capcost(r,newff,v) * 1.03;
display capcost

parameter lvlcost,lvlcost_new,lcoe;

lvlcost(r,existu,"fuel")$capacity(r,existu,"2010")
        = sum(f,pf(r,f,"2010") * fueltype(existu,f) * heatrate(r,existu,"2010") * 1e-3 );

lvlcost("USA",existu,"fuel")
        = sum(r, capacity(r,existu,"2010") * sum(l, hours(r,l) * maxCF(r,existu,l)) * sum(f,pf(r,f,"2010") * fueltype(existu,f) * heatrate(r,existu,"2010") * 1e-3 ) )
                / sum(r, capacity(r,existu,"2010") * sum(l, hours(r,l) * maxCF(r,existu,l)) );


lvlcost(r,existu,"fom")$sum(l, (capacity(r,existu,"2010") * hours(r,l) * maxCF(r,existu,l) * 1e-3) )
        = capacity(r,existu,"2010") * fomcost(r,existu)
                / sum(l, (capacity(r,existu,"2010") * hours(r,l) * maxCF(r,existu,l) * 1e-3) );

lvlcost("USA",existu,"fom")
        = sum(r, capacity(r,existu,"2010") * fomcost(r,existu))
                / sum((r,l), (capacity(r,existu,"2010") * hours(r,l) * maxCF(r,existu,l) * 1e-3) );

lvlcost(r,existu,"vom")$capacity(r,existu,"2010")
        = vomcost(r,existu);

lvlcost("USA",existu,"vom")
        = sum(r, capacity(r,existu,"2010") * sum(l, hours(r,l) * maxCF(r,existu,l)) * vomcost(r,existu))
                / sum(r, capacity(r,existu,"2010") * sum(l, hours(r,l) * maxCF(r,existu,l)) );

lvlcost(r,existu,"total")       = lvlcost(r,existu,"fuel") + lvlcost(r,existu,"fom") + lvlcost(r,existu,"vom");
lvlcost("USA",existu,"total")   = lvlcost("USA",existu,"fuel") + lvlcost("USA",existu,"fom") + lvlcost("USA",existu,"vom");



* new units
lvlcost_new(r,newu,"fuel",t) = sum(f, pf(r,f,t) * fueltype(newu,f) * heatrate(r,newu,t) * 1e-3 );

lvlcost_new(r,newu,"fom",t)$(sum(l, hours(r,l) * maxCF(r,newu,l) * 1e-3 ))   = fomcost(r,newu) / sum(l, hours(r,l) * maxCF(r,newu,l) * 1e-3 );

lvlcost_new(r,newu,"vom",t)$(sum(l, maxCF(r,newu,l)))        = vomcost(r,newu);

lvlcost_new(r,newu,"cap",t)$(sum(l, maxCF(r,newu,l) * hours(r,l)))
        = (capcost(r,newu,t) / (8.76 * sum(l, maxCF(r,newu,l) * hours(r,l)) / sum(l, hours(r,l)) ) ) * k_chrg(newu) * (1+IDC);

lvlcost_new(r,newu,"total",t) = lvlcost_new(r,newu,"fuel",t) + lvlcost_new(r,newu,"fom",t) + lvlcost_new(r,newu,"vom",t) + lvlcost_new(r,newu,"cap",t);
*lvlcost_new(r,wndn,"total",t) = lvlcost_new(r,wndn,"total",t)
*         + (wind_cost(r,wndn,"3",t) / (8.76 * sum(l, maxCF(r,wndn,l) * hours(r,l)) / sum(l, hours(r,l)) ) ) * k_chrg(wndn) * (1+IDC);

*lvlcost_new("USA",newu,"total")        = lvlcost_new("USA",newu,"fuel") + lvlcost_new("USA",newu,"fom") + lvlcost_new("USA",newu,"vom");

option lvlcost:1:2:1;
display lvlcost;

option lvlcost_new:1:3:1;
display lvlcost_new;

lcoe(existu,r)$capacity(r,existu,"2010")        = lvlcost(r,existu,"total");
lcoe(newu,r)$maxCF(r,newu,"sum_day")            = lvlcost_new(r,newu,"total","2020");

option lcoe:1;
display lcoe;
