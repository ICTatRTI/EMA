$title Findings for Electricity Market Analysis model (EMA)

parameter
        chk_gen
        chk_newgen
        chk_demand
        chk_transmit
        chk_newinv
        chk_cap
        chk_newcap
        chk_price
        chk_gen2010
        chk_totdemand
;

chk_gen(r,gentype,t)            = sum(mapGEN(gentype,u), sum((v,l), gen.l(r,u,v,l,t)));
chk_gen(r,"total",t)            = sum((v,l,u), gen.l(r,u,v,l,t));
chk_gen("USA",gentype,t)        = sum(mapGEN(gentype,u), sum((r,v,l), gen.l(r,u,v,l,t)));
chk_gen("USA","total",t)        = sum((r,v,l,u), gen.l(r,u,v,l,t));

chk_newgen(r,newu,v,t)          = sum(l, gen.l(r,newu,v,l,t));

chk_demand(r,t)                 = sum(l, DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t));
chk_demand("USA",t)             = sum((r,l), DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t));

chk_transmit(r,rr,t)            = sum(l, transmit.l(r,rr,l,t));

chk_cap(r,gentype,t)            = sum(mapGEN(gentype,u), sum(v, cap.l(r,u,v,t)));
chk_cap("USA",gentype,t)        = sum(mapGEN(gentype,u), sum((r,v), cap.l(r,u,v,t)));

chk_newinv(r,u,v,t)             = INV.l(r,u,v,t);

chk_newcap(r,newu,v,t)          = cap.l(r,newu,v,t);

chk_price(l,r,t)                = c_demandseg.m(r,l,t) / pv(t);
chk_price("avg",r,t)            = ( sum(l,(c_demandseg.m(r,l,t) * DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) / sum(l,(DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) ) / pv(t);
chk_price("avg","USA",t)        = ( sum((r,l),(c_demandseg.m(r,l,t) * DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) / sum((r,l),(DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) ) / pv(t);

*price(s,r,t) = DemandSeg.M(s,r,t) / (1e-3 * hours(s) * dfact(t));

execute_unload 'output\pivot.gdx', chk_price;

parameter chk_UScap;
chk_UScap(newgentype,t)         = chk_cap("USA",newgentype,t);

option chk_gen:1:2:1;
option chk_price:2;
option chk_UScap:2;
display chk_demand,chk_transmit,chk_cap,chk_newgen,chk_newcap,chk_newinv,chk_gen,chk_price,chk_UScap;


chk_gen2010(r,u,l)              = gen.l(r,u,"2010",l,"2010");
chk_gen2010(r,"total",l)        = sum((u), gen.l(r,u,"2010",l,"2010"));
chk_gen2010("USA",u,l)          = sum((r), gen.l(r,u,"2010",l,"2010"));
chk_gen2010("USA","total",l)    = sum((r,u), gen.l(r,u,"2010",l,"2010"));

option chk_gen2010:1:2:1;
display chk_gen2010;

* check CO2 emissions
parameter
        carbemis;

carbemis(t,"model")     = sum(r, emis.l(r,"co2",t));
carbemis("2010","AEO")  = 2218;
carbemis("2015","AEO")  = 2277;
carbemis("2020","AEO")  = 2343;
carbemis("2025","AEO")  = 2434;
carbemis("2030","AEO")  = 2533;
carbemis("2035","AEO")  = 2634;

carbemis(t,"pct")$carbemis(t,"aeo")
        = 100 * (carbemis(t,"model")/carbemis(t,"aeo")-1);

option carbemis:1;
display carbemis;

**--- Set up Results Tables ---**
parameter
        physical
        dollars
        USAphy%scn%
        USAdlr%scn%
        EMF_elec
;

USAphy%scn%(var,t)              = 0;
USAdlr%scn%(var,t)              = 0;

$onechov >output\report.gms

* Fuel use
physical("USA","%1","1",t)      = sum(r, fuel.l(r,"col",t)) / 1e+6      + eps;
physical("USA","%1","2",t)      = sum(r, fuel.l(r,"gas",t)) / 1e+6      + eps;
physical("USA","%1","3",t)      = sum(r, fuel.l(r,"oil",t)) / 1e+6      + eps;
physical("USA","%1","4",t)      = sum(r, fuel.l(r,"nuc",t)) / 1e+6      + eps;
physical("USA","%1","5",t)      = sum(r, fuel.l(r,"bio",t)) / 1e+6      + eps;

* emissions
physical("USA","%1","6",t)      = eps;
physical("USA","%1","7",t)      = eps;
physical("USA","%1","8",t)      = eps;
physical("USA","%1","9",t)      = sum(r, emis.l(r,"co2",t))     + eps;
physical("USA","%1","10",t)     = eps;

* generation
physical("USA","%1","11",t)     = sum((r,colu,v,l), gen.l(r,colu,v,l,t))        + eps;
physical("USA","%1","12",t)     = sum((r,igcc,v,l), gen.l(r,igcc,v,l,t))        + eps;
physical("USA","%1","13",t)     = sum((r,igccCCS,v,l), gen.l(r,igccCCS,v,l,t))  + eps;
physical("USA","%1","14",t)     = sum((r,ngcc,v,l), gen.l(r,ngcc,v,l,t))        + eps;
physical("USA","%1","15",t)     = sum((r,ngccCCS,v,l), gen.l(r,ngccCCS,v,l,t))  + eps;
physical("USA","%1","16",t)     = sum((r,ngtbu,v,l), gen.l(r,ngtbu,v,l,t))      + eps;
physical("USA","%1","17",t)     = sum((r,stog,v,l), gen.l(r,stog,v,l,t))        + eps;
physical("USA","%1","18",t)     = sum((r,nucu,v,l), gen.l(r,nucu,v,l,t))        + eps;
physical("USA","%1","19",t)     = sum((r,biou,v,l), gen.l(r,biou,v,l,t))        + eps;
physical("USA","%1","20",t)     = sum((r,hydu,v,l), gen.l(r,hydu,v,l,t))        + eps;
physical("USA","%1","21",t)     = sum((r,geou,v,l), gen.l(r,geou,v,l,t))        + eps;
physical("USA","%1","22",t)     = sum((r,lfgu,v,l), gen.l(r,lfgu,v,l,t))        + eps;
physical("USA","%1","23",t)     = sum((r,slru,v,l), gen.l(r,slru,v,l,t))        + eps;
physical("USA","%1","24",t)     = sum((r,wndu,v,l), gen.l(r,wndu,v,l,t))        + eps;
physical("USA","%1","25",t)     = sum((r,u,v,l), gen.l(r,u,v,l,t))              + eps;

* capacity
physical("USA","%1","26",t)     = sum((r,u,v)$(colu(u) or igcc(u) or igccCCS(u)), cap.l(r,u,v,t))       + eps;
physical("USA","%1","27",t)     = sum((r,u,v)$(ngcc(u) or ngccCCS(u)), cap.l(r,u,v,t))  + eps;
physical("USA","%1","28",t)     = sum((r,u,v)$(ngtbu(u)), cap.l(r,u,v,t))       + eps;
physical("USA","%1","29",t)     = sum((r,u,v)$(stog(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","30",t)     = sum((r,u,v)$(nucu(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","31",t)     = sum((r,u,v)$(biou(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","32",t)     = sum((r,u,v)$(hydu(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","33",t)     = sum((r,u,v)$(geou(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","34",t)     = sum((r,u,v)$(slru(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","35",t)     = sum((r,u,v)$(wndu(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","36",t)     = sum((r,u,v)$(not psu(u)), cap.l(r,u,v,t))     + eps;

* capacity utilization (in spreadsheet)
physical("USA","%1","37",t)     = eps;
physical("USA","%1","38",t)     = eps;
physical("USA","%1","39",t)     = eps;
physical("USA","%1","40",t)     = eps;
physical("USA","%1","41",t)     = eps;
physical("USA","%1","42",t)     = eps;
physical("USA","%1","43",t)     = eps;
physical("USA","%1","44",t)     = eps;
physical("USA","%1","45",t)     = eps;

* new capacity
physical("USA","%1","46",t)     = eps;
physical("USA","%1","47",t)     = sum((r,u,v)$(igcc(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","48",t)     = sum((r,u,v)$(igccCCS(u)), cap.l(r,u,v,t))     + eps;
physical("USA","%1","49",t)     = sum((r,u,v)$(ngccN(u)), cap.l(r,u,v,t))       + eps;
physical("USA","%1","50",t)     = sum((r,u,v)$(ngccCCS(u)), cap.l(r,u,v,t))     + eps;
physical("USA","%1","51",t)     = sum((r,u,v)$(ngtbN(u)), cap.l(r,u,v,t))       + eps;
physical("USA","%1","52",t)     = sum((r,u,v)$(nucN(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","53",t)     = sum((r,u,v)$(bioN(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","54",t)     = sum((r,u,v)$(geoN(u)), cap.l(r,u,v,t))        + eps;
physical("USA","%1","55",t)     = sum((r,u,v)$(wndN(u)), cap.l(r,u,v,t))        + eps;

* retrofits
physical("USA","%1","56",t)     = eps;
physical("USA","%1","57",t)     = eps;
physical("USA","%1","58",t)     = eps;

* retirements
physical("USA","%1","59",t)     = sum((r,u,v)$(colx(u)), retire.l(r,u,v,t))     + eps;
physical("USA","%1","60",t)     = sum((r,u,v)$(stog(u)), retire.l(r,u,v,t))     + eps;
physical("USA","%1","61",t)     = sum((r,u,v)$(ngtbX(u)), retire.l(r,u,v,t))    + eps;


* Fuel prices
dollars("USA","%1","1",t)$sum(r, fuel.l(r,"col",t))     = sum(r, pf(r,"col",t) * fuel.l(r,"col",t)) / sum(r, fuel.l(r,"col",t)) + eps;
dollars("USA","%1","2",t)$sum(r, fuel.l(r,"gas",t))     = sum(r, pf(r,"gas",t) * fuel.l(r,"gas",t)) / sum(r, fuel.l(r,"gas",t)) + eps;
dollars("USA","%1","3",t)$sum(r, fuel.l(r,"oil",t))     = sum(r, pf(r,"oil",t) * fuel.l(r,"oil",t)) / sum(r, fuel.l(r,"oil",t)) + eps;
dollars("USA","%1","4",t)$sum(r, fuel.l(r,"nuc",t))     = sum(r, pf(r,"nuc",t) * fuel.l(r,"nuc",t)) / sum(r, fuel.l(r,"nuc",t)) + eps;
dollars("USA","%1","5",t)$sum(r, fuel.l(r,"bio",t))     = sum(r, pf(r,"bio",t) * fuel.l(r,"bio",t)) / sum(r, fuel.l(r,"bio",t)) + eps;

* Allowance prices
dollars("USA","%1","6",t)       = eps;
dollars("USA","%1","7",t)       = eps;
dollars("USA","%1","8",t)       = eps;
dollars("USA","%1","9",t)       = eps;
dollars("USA","%1","10",t)      = eps;
dollars("USA","%1","11",t)      = carbtax(t) + eps;

dollars("USA","%1","12",t)      = ( sum((r,l),(c_demandseg.m(r,l,t) * DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) / sum((r,l),(DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) ) / pv(t);

dollars("USA","%1","13",t)      = dollars("USA","%1","12",t) + distrib(t);
dollars("USA","%1","14",t)      = eps;
dollars("USA","%1","15",t)      = eps;
dollars("USA","%1","16",t)      = eps;
dollars("USA","%1","17",t)      = eps;
dollars("USA","%1","18",t)      = eps;
dollars("USA","%1","19",t)      = eps;
dollars("USA","%1","20",t)      = eps;

dollars("USA","%1","21",t)      = eps;
dollars("USA","%1","22",t)      = eps;
dollars("USA","%1","23",t)      = eps;
dollars("USA","%1","24",t)      = eps;
dollars("USA","%1","25",t)      = eps;
dollars("USA","%1","26",t)      = eps;
dollars("USA","%1","27",t)      = eps;
dollars("USA","%1","28",t)      = eps;
dollars("USA","%1","29",t)      = eps;
dollars("USA","%1","30",t)      = eps;

dollars("USA","%1","31",t)      = eps;
dollars("USA","%1","32",t)      = eps;
dollars("USA","%1","33",t)      = eps;
dollars("USA","%1","34",t)      = eps;
dollars("USA","%1","35",t)      = eps;
dollars("USA","%1","36",t)      = eps;
dollars("USA","%1","37",t)      = eps;
dollars("USA","%1","38",t)      = eps;
dollars("USA","%1","39",t)      = eps;
dollars("USA","%1","40",t)      = eps;

dollars("USA","%1","41",t)      = eps;
dollars("USA","%1","42",t)      = eps;
dollars("USA","%1","43",t)      = eps;
dollars("USA","%1","44",t)      = eps;
dollars("USA","%1","45",t)      = eps;
dollars("USA","%1","46",t)      = eps;
dollars("USA","%1","47",t)      = eps;
dollars("USA","%1","48",t)      = eps;
dollars("USA","%1","49",t)      = eps;
dollars("USA","%1","50",t)      = eps;
dollars("USA","%1","51",t)      = eps;
dollars("USA","%1","52",t)      = eps;
dollars("USA","%1","53",t)      = eps;
dollars("USA","%1","54",t)      = eps;
dollars("USA","%1","55",t)      = eps;
dollars("USA","%1","56",t)      = eps;
dollars("USA","%1","57",t)      = eps;
dollars("USA","%1","58",t)      = eps;
dollars("USA","%1","59",t)      = eps;
dollars("USA","%1","60",t)      = eps;
dollars("USA","%1","59",t)      = eps;
dollars("USA","%1","60",t)      = eps;
dollars("USA","%1","61",t)      = eps;

physical(r,"%1",var,"2075")     = 0;
dollars(r,"%1",var,"2075")      = 0;

physical("USA","%1",var,"2075") = 0;
dollars("USA","%1",var,"2075")  = 0;

USAphy%scn%(var,t)              = physical("USA","%1",var,t);
USAdlr%scn%(var,t)              = dollars("USA","%1",var,t);


**--------------- EMF reporting ----------------------**

**--- Primary Energy ---**
* total use in primary direct equivalent
EMF_elec("%1","USA","4",t)      = 1.05506 *
        ( sum((r,cgo), fuel.l(r,cgo,t)/1e+6)
        + sum(r, fuel.l(r,"bio",t)/1e+6)
        + sum(r, fuel.l(r,"nuc",t)/1e+6)
        + 1.05506 * sum((r,u,v,l)$hydu(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6
        + 1.05506 * sum((r,u,v,l)$wndu(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6
        + 1.05506 * sum((r,u,v,l)$slru(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6
        + 1.05506 * sum((r,u,v,l)$geou(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6
        + 1.05506 * sum((r,u,v,l)$lfgu(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6
        );

* total use of fossil fuels
EMF_elec("%1","USA","5",t)      = 1.05506 * ( sum((r,cgo), fuel.l(r,cgo,t)/1e+6) ) + eps;;

* total use of fossil fuels with CCS
EMF_elec("%1","USA","6",t)
        = 1.05506 * sum((r,u,v,l)$igccCCS(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,"col"))/1e+6
        + 1.05506 * sum((r,u,v,l)$ngccCCS(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,"gas"))/1e+6 + eps;

* total use of fossil fuels without CCS
EMF_elec("%1","USA","7",t)
        = 1.05506 * sum((r,u,v,l)$(colu(u) or igcc(u)), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,"col"))/1e+6
        + 1.05506 * sum((r,u,v,l)$stog(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,"oil"))/1e+6
        + 1.05506 * sum((r,u,v,l)$ngcc(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,"gas"))/1e+6 + eps;

* total use of col
EMF_elec("%1","USA","8",t)      = 1.05506 * ( sum(r, fuel.l(r,"col",t)/1e+6) ) + eps;

* total use of col with CCS
EMF_elec("%1","USA","9",t)      = 1.05506 * sum((r,u,v,l)$igccCCS(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,"col"))/1e+6 + eps;

* total use of col without CCS
EMF_elec("%1","USA","10",t)     = 1.05506 * sum((r,u,v,l)$(colu(u) or igcc(u)), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,"col"))/1e+6 + eps;

* total use of oil
EMF_elec("%1","USA","11",t)     = 1.05506 * ( sum(r, fuel.l(r,"oil",t)/1e+6) );

* total use of oil with CCS
EMF_elec("%1","USA","12",t)     = eps;

* total use of oil without CCS
EMF_elec("%1","USA","13",t)     = 1.05506 * sum((r,u,v,l)$stog(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,"oil"))/1e+6 + eps;

* total use of gas
EMF_elec("%1","USA","14",t)     = 1.05506 * ( sum(r, fuel.l(r,"gas",t)/1e+6) );

* total use of gas with CCS
EMF_elec("%1","USA","15",t)     = 1.05506 * sum((r,u,v,l)$ngccCCS(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,"gas"))/1e+6 + eps;

* total use of gas without CCS
EMF_elec("%1","USA","16",t)     = 1.05506 * sum((r,u,v,l)$(ngcc(u) or ngtbu(u)), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * fueltype(u,"gas"))/1e+6 + eps;

* total use of biomass
EMF_elec("%1","USA","17",t)     = 1.05506 * ( sum(r, fuel.l(r,"bio",t)/1e+6) ) + eps;

* total use of biomass with CCS
EMF_elec("%1","USA","18",t)     = eps;

* total use of biomass without CCS
EMF_elec("%1","USA","19",t)     = 1.05506 * ( sum(r, fuel.l(r,"bio",t)/1e+6) ) + eps;

* total use of nuclear
EMF_elec("%1","USA","20",t)     = 1.05506 * ( sum(r, fuel.l(r,"nuc",t)/1e+6) ) + eps;

* total use of renewables
EMF_elec("%1","USA","21",t)
        = 1.05506 * sum((r,u,v,l)$hydu(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6
        + 1.05506 * sum((r,u,v,l)$wndu(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6
        + 1.05506 * sum((r,u,v,l)$slru(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6
        + 1.05506 * sum((r,u,v,l)$geou(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6
        + 1.05506 * sum((r,u,v,l)$lfgu(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6;

* total use of hydro
EMF_elec("%1","USA","22",t)     = 1.05506 * sum((r,u,v,l)$hydu(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6 + eps;

* total use of wind
EMF_elec("%1","USA","23",t)     = 1.05506 * sum((r,u,v,l)$wndu(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6 + eps;

* total use of solar
EMF_elec("%1","USA","24",t)     = 1.05506 * sum((r,u,v,l)$slru(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6 + eps;

* total use of geothermal
EMF_elec("%1","USA","25",t)     = 1.05506 * sum((r,u,v,l)$geou(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v))/1e+6 + eps;

* total use of ocean
EMF_elec("%1","USA","26",t)     = eps;

* trade in secondary carriers (electricity, hydrogen, etc)
EMF_elec("%1","USA","27",t)     = eps;

* other primary energy consumption
EMF_elec("%1","USA","28",t)     = eps;

**--- Secondary Energy (Electricity) ---**
* total net electricity production
EMF_elec("%1","USA","29",t)     = 1.05506 * sum((r,u,v,l), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from total COL
EMF_elec("%1","USA","30",t)     = 1.05506 * sum((r,u,v,l)$(colx(u) or coln(u)), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from COL with CCS
EMF_elec("%1","USA","31",t)     = 1.05506 * sum((r,u,v,l)$igccCCS(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from COL without CCS
EMF_elec("%1","USA","32",t)     = 1.05506 * sum((r,u,v,l)$(colx(u) or (coln(u) and not ccsu(u))), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from total OIL
EMF_elec("%1","USA","33",t)     = 1.05506 * sum((r,u,v,l)$stog(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from OIL with CCS
EMF_elec("%1","USA","34",t)     = eps;

* electricity from OIL without CCS
EMF_elec("%1","USA","35",t)     = 1.05506 * sum((r,u,v,l)$stog(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from total GAS
EMF_elec("%1","USA","36",t)     = 1.05506 * sum((r,u,v,l)$(gasu(u) or ngccCCS(u)), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from GAS with CCS
EMF_elec("%1","USA","37",t)     = 1.05506 * sum((r,u,v,l)$ngccCCS(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from GAS without CCS
EMF_elec("%1","USA","38",t)     = 1.05506 * sum((r,u,v,l)$gasu(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from total BIO
EMF_elec("%1","USA","39",t)     = 1.05506 * sum((r,u,v,l)$biou(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from BIO with CCS
EMF_elec("%1","USA","40",t)     = eps;

* electricity from BIO without CCS
EMF_elec("%1","USA","41",t)     = 1.05506 * sum((r,u,v,l)$biou(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from nuclear
EMF_elec("%1","USA","42",t)     = 1.05506 * sum((r,u,v,l)$nucu(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from renewables
EMF_elec("%1","USA","43",t)     = 1.05506 * sum((r,u,v,l)$resu(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from hydro
EMF_elec("%1","USA","44",t)     = 1.05506 * sum((r,u,v,l)$hydu(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from solar
EMF_elec("%1","USA","45",t)     = 1.05506 * sum((r,u,v,l)$slru(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from solar PV
EMF_elec("%1","USA","46",t)     = 1.05506 * sum((r,u,v,l)$slrpvu(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from solar CSP
EMF_elec("%1","USA","47",t)     = 1.05506 * sum((r,u,v,l)$slrcspu(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from total wind
EMF_elec("%1","USA","48",t)     = 1.05506 * sum((r,u,v,l)$wndu(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from wind - onshore
EMF_elec("%1","USA","49",t)     = 1.05506 * sum((r,u,v,l)$wndon(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from wind - offshore
EMF_elec("%1","USA","50",t)     = 1.05506 * sum((r,u,v,l)$wndoff(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from geothermal
EMF_elec("%1","USA","51",t)     = 1.05506 * sum((r,u,v,l)$geou(u), gen.l(r,u,v,l,t)) * 3.412/1000 + eps;

* electricity from ocean
EMF_elec("%1","USA","52",t)     = eps;

* electricity from other
EMF_elec("%1","USA","53",t)     = eps;

**--- Secondary Energy (Liquids and Gases) ---**


* total carbon dioxide emissions, including emissions from fossil fuel combustion, industrial processes and land-use change
EMF_elec("%1","USA","81",t)     = sum(r, emis.l(r,"co2",t)) + eps;

* carbon dioxide emissions from power and heat generation, other energy conversion (e.g. refineries, synfuel production), resource extration and energy transmission and distribution (e.g. gas pipelines)
EMF_elec("%1","USA","83",t)     = sum(r, emis.l(r,"co2",t)) + eps;

* carbon dioxide emissions from electric power generation
EMF_elec("%1","USA","84",t)     = sum(r, emis.l(r,"co2",t)) + eps;

* total carbon dioxide emissions captured and stored in geological deposits (e.g. in depleted oil and gas fields, unmined coal seams, saline aquifers) and the deep ocean, stored amounts should be reported as positive numbers
EMF_elec("%1","USA","89",t)     = sum((r,u,v,l)$ccsu(u), GEN.l(r,u,v,l,t) * heatrate(r,u,v) * emis_factor(r,u,"CO2") * 0.9 / 0.1) + eps;

* light fuel oil price at the secondary level, i.e. for large scale consumers (e.g. oil power plant). Prices should include the effect of carbon prices.
EMF_elec("%1","USA","102",t)$sum(r, fuel.l(r,"oil",t))
        = sum(r, pf(r,"oil",t) * fuel.l(r,"oil",t)) / sum(r, fuel.l(r,"oil",t)) * deflator + eps;

* natural gas price at the secondary level, i.e. for large scale consumers (e.g. gas power plant). Prices should include the effect of carbon prices.
EMF_elec("%1","USA","103",t)$sum(r, fuel.l(r,"gas",t))
        = sum(r, pf(r,"gas",t) * fuel.l(r,"gas",t)) / sum(r, fuel.l(r,"gas",t)) * deflator + eps;

* coal price at the secondary level, i.e. for large scale consumers (e.g. coal power plant). Prices should include the effect of carbon prices.
EMF_elec("%1","USA","104",t)$sum(r, fuel.l(r,"col",t))
        = sum(r, pf(r,"col",t) * fuel.l(r,"col",t)) / sum(r, fuel.l(r,"col",t)) * deflator + eps;

* biomass price at the secondary level, i.e. for large scale consumers (e.g. biomass power plant). Prices should include the effect of carbon prices.
EMF_elec("%1","USA","105",t)$sum(r, fuel.l(r,"bio",t))
        = sum(r, pf(r,"bio",t) * fuel.l(r,"bio",t)) / sum(r, fuel.l(r,"bio",t)) * deflator + eps;

* biomass producer price
EMF_elec("%1","USA","106",t)    = EMF_elec("%1","USA","105",t) / 1.3;


**--- LCOE ---**

* average levelized cost of electricity of coal-fired power with CCS  (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate. LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","109",t)    = ( sum(r, lvlcost_new(r,"col_Nccs","total",t) * dele(r,t)) / sum(r, dele(r,t)) ) * deflator + eps;

* average levelized cost of electricity of coal-fired power without CCS  (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate. LCOE should NOT include effect of carbon price + eps.
EMF_elec("%1","USA","110",t)    = ( sum(r, lvlcost_new(r,"col_N","total",t) * dele(r,t)) / sum(r, dele(r,t)) ) * deflator + eps;

* average levelized cost of electricity of gas-fired power with CCS  (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate. LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","111",t)    = ( sum(r, lvlcost_new(r,"ngcc_Nccs","total",t) * dele(r,t)) / sum(r, dele(r,t)) ) * deflator + eps;

* average levelized cost of electricity of gas-fired power without CCS  (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate. LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","112",t)    = ( sum(r, lvlcost_new(r,"ngcc_N","total",t) * dele(r,t)) / sum(r, dele(r,t)) ) * deflator + eps;

* average levelized cost of electricity of nuclear generated power (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate.  LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","113",t)    = ( sum(r, lvlcost_new(r,"nuc_N","total",t) * dele(r,t)) / sum(r, dele(r,t)) ) * deflator + eps;

* average levelized cost of electricity of non-biomass renewable generated power (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate.  LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","114",t)    = ( sum((r,resu), lvlcost_new(r,resu,"total",t) * sum((v,l), gen.l(r,resu,v,l,t))) / sum((r,resu), sum((v,l), gen.l(r,resu,v,l,t))) ) * deflator + eps;

* average levelized cost of electricity of solar power (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate.  LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","115",t)    = ( sum(r, lvlcost_new(r,"slrCSP_N","total",t) * capacity(r,"slr_x","2010")) / sum(r, capacity(r,"slr_x","2010")) ) * deflator + eps;

* marginal levelized cost of electricity of solar power (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate.  LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","116",t)    = lvlcost_new("wnc","slrCSP_N","total",t) * deflator + eps;

* average levelized cost of electricity of wind power (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate.  LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","117",t)    = ( sum((r,wndon,cc), wind_cost_nrel(r,wndon,cc) * wind_limit(r,wndon,cc) ) / sum((r,wndon,cc), wind_limit(r,wndon,cc)) ) * deflator + eps;

* marginal levelized cost of electricity of wind power (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate.  LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","118",t)    = ( sum((r,wndon), wind_cost_nrel(r,wndon,"10") * wind_limit(r,wndon,"10") ) / sum((r,wndon), wind_limit(r,wndon,"10")) ) * deflator + eps;

* average levelized cost of electricity of biomass generated power with CCS (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate.  LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","119",t)    = eps;

* average levelized cost of electricity of biomass generated power without CCS (weighted by the share of electricity of the various power plant types). LCOE should include the investment, O&M, fuel and decomissioning costs. Each model is free to chose its own discount rate.  LCOE should NOT include effect of carbon price.
EMF_elec("%1","USA","120",t)    = ( sum(r, lvlcost_new(r,"bio_N","total",t) * dele(r,t)) / sum(r, dele(r,t)) ) * deflator + eps;




**--- New Units ---**

* capital cost for a new IGCC facility
EMF_elec("%1","USA","133",t)    = (sum(r, capcost(r,"col_N",t)*dele(r,t)) / sum(r, dele(r,t)) ) * deflator + eps;

* efficiency of a new IGCC facility
EMF_elec("%1","USA","134",t)    = sum(num, heatrate(num,"col_N",t)) + eps;

* capital cost of a new gas CC facility
EMF_elec("%1","USA","135",t)    = (sum(r, capcost(r,"col_N",t)*dele(r,t)) / sum(r, dele(r,t)) ) * deflator + eps;

* efficiency of a new gas CC facility
EMF_elec("%1","USA","136",t)    = sum(num, heatrate(num,"ngcc_N",t)) + eps;

* capital cost of new solar PV cells
EMF_elec("%1","USA","137",t)    = (sum(r, capcost(r,"slrPV_N",t)*capacity(r,"slr_x","2010")) / sum(r, capacity(r,"slr_x","2010")) ) * deflator + eps;

* capital cost of a new solar CSP facility
EMF_elec("%1","USA","138",t)    = (sum(r, capcost(r,"slrCSP_N",t)*capacity(r,"slr_x","2010")) / sum(r, capacity(r,"slr_x","2010")) ) * deflator + eps;

* capital cost of a new nuclear power plant
EMF_elec("%1","USA","139",t)    = (sum(r, capcost(r,"nuc_N",t)*dele(r,t)) / sum(r, dele(r,t)) ) * deflator + eps;

display EMF_elec;

$offecho

