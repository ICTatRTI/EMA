$title New Units Costs and Performance data in AEO Assumptions 2010

set     t               time periods / 2010,2015,2020,2025,2030,2035,2040,2045,2050,2075 /;

* vintages
alias (t,v);

set     r               regions /
        NEG             New England
        NY              New York
        MID             Middle Atlantic
        ENC             East North Central
        WNC             West North Central
        FL              Florida
        SAC             South Atlantic
        ESC             East South Central
        TX              Texas
        WSC             West South Central
        MTN             Mountain
        CA              California
        PAC             Pacific /;

alias   (r,rr);


**--    Model Unit Types        --**
set     u               units   /
* existing units
        col_X1          existing coal class 1 (low heatrate)
        col_X2          existing coal class 2 (medium heatrate)
        col_X3          existing coal class 3 (high heatrate)

        col_X1bio       existing coal class 1 with biomass cofiring (low heatrate)
        col_X2bio       existing coal class 2 with biomass cofiring (medium heatrate)
        col_X3bio       existing coal class 3 with biomass cofiring (high heatrate)

        col_X1ccs       existing coal class 1 with CCS retrofit (low heatrate)
        col_X2ccs       existing coal class 2 with CCS retrofit (medium heatrate)
        col_X3ccs       existing coal class 3 with CCS retrofit (high heatrate)

        ngcc_X1         existing natural gas combined cycle class 1 (low heatrate)
        ngcc_X2         existing natural gas combined cycle class 2 (medium heatrate)
        ngcc_X3         existing natural gas combined cycle class 3 (high heatrate)

        ngtb_X1         existing natural gas turbine class 1 (low heatrate)
        ngtb_X2         existing natural gas turbine class 2 (medium heatrate)
        ngtb_X3         existing natural gas turbine class 3 (high heatrate)

        stog_X1         existing steam oil-gas class 1 (low heatrate)
        stog_X2         existing steam oil-gas class 2 (medium heatrate)
        stog_X3         existing steam oil-gas class 3 (high heatrate)

        nuc_X           existing nuclear
        hyd_X           existing hydro
        geo_X           existing geothermal
        bio_X           existing biomass
        lfg_X           existing landfill gas
        msw_X           existing municipal solid waste
        slr_X           existing solar
        wnd_X           existing wind
        oth_X           existing other
        ps_X            existing pump storage

* new units
        col_N           new coal (pulverized IGCC)
        col_Nccs        new coal with CCS
        ngcc_N          new natural gas combined cycle
        ngcc_Nccs       new natural gas combined cycle with CCS
        ngtb_N          new natural gas turbine
        nuc_N           new nuclear
        geo_N           new geothermal
        bio_N           new biomass
        lfg_N           new landfill gas
        fc_N            new fuel cell
        slrPV_N         new solar photovoltaic
        slrCSP_N        new concentrating solar power

        wnd_Nonsh_3     new wind - onshore - wind class 3
        wnd_Nonsh_4     new wind - onshore - wind class 4
        wnd_Nonsh_5     new wind - onshore - wind class 5
        wnd_Nonsh_6     new wind - onshore - wind class 6
        wnd_Nonsh_7     new wind - onshore - wind class 7

        wnd_Nshal_3     new wind - offshore shallow - wind class 3
        wnd_Nshal_4     new wind - offshore shallow - wind class 4
        wnd_Nshal_5     new wind - offshore shallow - wind class 5
        wnd_Nshal_6     new wind - offshore shallow - wind class 6
        wnd_Nshal_7     new wind - offshore shallow - wind class 7

        wnd_Ndeep_3     new wind - offshore deep - wind class 3
        wnd_Ndeep_4     new wind - offshore deep - wind class 4
        wnd_Ndeep_5     new wind - offshore deep - wind class 5
        wnd_Ndeep_6     new wind - offshore deep - wind class 6
        wnd_Ndeep_7     new wind - offshore deep - wind class 7
/;

set     wnd(u) /wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7/;


* include AEO data
set     aeo_u /
        "Scrubbed Coal New7"
        "Integrated Coal-Gasification Comb Cycle (IGCC)7"
        "IGCC with carbon sequestration"
        "Conv Gas/Oil Comb Cycle"
        "Adv Gas/Oil Comb Cycle (CC)"
        "Adv CC with carbon sequestration"
        "Conv Comb Turbine8"
        "Adv Comb Turbine"
        "Fuel Cells"
        "Adv Nuclear"
        "Distributed Generation - Base"
        "Distributed Generation - Peak"
        "Biomass"
        "Geothermal7,9"
        "MSW - Landfill Gas"
        "Conventional Hydropower9"
        "Wind"
        "Wind Offshore"
        "Solar Thermal7"
        "Photovoltaic7" /;

set     aeo_var /
        ONLINE
        MW
        LEADTIME
        OVERNIGHT
        PROJECT
        TECHNOLOGY
        COST
        VAR_OM
        FIXED_OM
        HEATRATE /;

set     mapAEO(u,aeo_u) /
*       ."Scrubbed Coal New7"
        col_N."Integrated Coal-Gasification Comb Cycle (IGCC)7"
        col_Nccs."IGCC with carbon sequestration"
*       ."Conv Gas/Oil Comb Cycle"
        ngcc_N."Adv Gas/Oil Comb Cycle (CC)"
        ngcc_Nccs."Adv CC with carbon sequestration"
*       ."Conv Comb Turbine8"
        ngtb_N."Adv Comb Turbine"
        fc_N."Fuel Cells"
        nuc_N."Adv Nuclear"
*       ."Distributed Generation - Base"
*       ."Distributed Generation - Peak"
        bio_N."Biomass"
        geo_N."Geothermal7,9"
        lfg_N."MSW - Landfill Gas"
*       ."Conventional Hydropower9"

        (wnd_Nonsh_3*wnd_Nonsh_7)."Wind"

        (wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7)."Wind Offshore"

        slrCSP_N."Solar Thermal7"
        slrPV_N."Photovoltaic7" /;

set     state /
        "Alabama"
        "Arizona"
        "Arkansas"
        "California"
        "Colorado"
        "Connecticut"
        "Delaware"
        "District of Columbia"
        "Florida"
        "Georgia"
        "Idaho"
        "Illinois"
        "Indiana"
        "Iowa"
        "Kansas"
        "Kentucky"
        "Louisiana"
        "Maine"
        "Maryland"
        "Massachusetts"
        "Michigan"
        "Minnesota"
        "Mississippi"
        "Missouri"
        "Montana"
        "Nebraska"
        "Nevada"
        "New Hampshire"
        "New Jersey"
        "New Mexico"
        "New York"
        "North Carolina"
        "North Dakota"
        "Ohio"
        "Oklahoma"
        "Oregon"
        "Pennsylvania"
        "Rhode Island"
        "South Carolina"
        "South Dakota"
        "Tennessee"
        "Texas"
        "Utah"
        "Vermont"
        "Virginia"
        "Washington"
        "West Virginia"
        "Wisconsin"
        "Wyoming" /;

set     st      State code (without US) /
        AK, AL, AR, AZ, CA, CO, CT, DC, DE, FL, GA, HI, IA, ID, IL, IN,
        KS, KY, LA, MA, MD, ME, MI, MN, MO, MS, MT, NE, NC, ND, NH, NJ, NM,
        NV, NY, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VA, VT, WA, WI, WV, WY/;


set     mapStates(st,state) /
        AL."Alabama"
        AZ."Arizona"
        AR."Arkansas"
        CA."California"
        CO."Colorado"
        CT."Connecticut"
        DE."Delaware"
        DC."District of Columbia"
        FL."Florida"
        GA."Georgia"
        ID."Idaho"
        IL."Illinois"
        IN."Indiana"
        IA."Iowa"
        KS."Kansas"
        KY."Kentucky"
        LA."Louisiana"
        ME."Maine"
        MD."Maryland"
        MA."Massachusetts"
        MI."Michigan"
        MN."Minnesota"
        MS."Mississippi"
        MO."Missouri"
        MT."Montana"
        NE."Nebraska"
        NV."Nevada"
        NH."New Hampshire"
        NJ."New Jersey"
        NM."New Mexico"
        NY."New York"
        NC."North Carolina"
        ND."North Dakota"
        OH."Ohio"
        OK."Oklahoma"
        OR."Oregon"
        PA."Pennsylvania"
        RI."Rhode Island"
        SC."South Carolina"
        SD."South Dakota"
        TN."Tennessee"
        TX."Texas"
        UT."Utah"
        VT."Vermont"
        VA."Virginia"
        WA."Washington"
        WV."West Virginia"
        WI."Wisconsin"
        WY."Wyoming" /;

set     mapRegions(r,st)        /
        NEG.(ct,me,ma,nh,ri,vt)
        NY.ny
        MID.(nj,pa)
        ENC.(il,in,mi,oh,wi)
        WNC.(ia,ks,mn,mo,ne,nd,sd)
        FL.fl
        SAC.(de,ga,md,nc,sc,va,wv)
        ESC.(al,ky,ms,tn)
        TX.tx
        WSC.(ar,la,ok)
        MTN.(az,co,id,mt,nv,nm,ut,wy)
        CA.ca
        PAC.(ak,hi,or,wa) /;

set     id /1*105/;

parameter
        aeo_data(aeo_u,aeo_var,*)
        regional_costs
        nuc_costs(id,state,*)
        vom_costs(*,*)
        vom_cgo_costs(*,*,*,*,*)
        fom_costs(*,*,*,*,*,*);

$call 'gdxxrw i=new_units\aeo2011-estimate.xlsx o=New_units\new_unit_costs.gdx log=New_units\new_unit_costs.log par=aeo_data rng=Sheet1!a11:u32 rdim=1 cdim=2'
$gdxin 'New_units\new_unit_costs.gdx'
$loaddc aeo_data

$gdxin 'IPM\regional_costs.gdx'
$load regional_costs

$if not exist IPM\nuc_costs.gdx $call 'gdxxrw i=IPM\Chapter4_nuclear_costs.xls o=IPM\nuc_costs.gdx log=IPM\nuc_costs.log par=nuc_costs rng=Sheet1!o3:s108 rdim=2 cdim=1'
$gdxin 'IPM\nuc_costs.gdx'
$loaddc nuc_costs

$if not exist IPM\vom_costs.gdx $call 'gdxxrw i=IPM\Chapter4_operating_costs.xls o=IPM\vom_costs.gdx log=IPM\vom_costs.log par=vom_costs rng=Sheet1!c8:d18 rdim=1 cdim=1'
$gdxin 'IPM\vom_costs.gdx'
$loaddc vom_costs

$if not exist IPM\fom_costs.gdx $call 'gdxxrw i=IPM\Chapter4_operating_costs.xls o=IPM\fom_costs.gdx log=IPM\fom_costs.log par=fom_costs rng=Sheet1!h8:m107 rdim=5 cdim=1'
$gdxin 'IPM\fom_costs.gdx'
$loaddc fom_costs

$if not exist IPM\vom_cgo_costs.gdx $call 'gdxxrw i=IPM\Chapter4_operating_costs.xls o=IPM\vom_cgo_costs.gdx log=IPM\vom_cgo_costs.log par=vom_cgo_costs rng=Sheet1!p13:u38 rdim=4 cdim=1'
$gdxin 'IPM\vom_cgo_costs.gdx'
$load vom_cgo_costs

option vom_cgo_costs:2:4:1;
display vom_costs,fom_costs,vom_cgo_costs;

parameter
        capcost(r,u,v)          capital costs ($ per kW)
        fomcost(r,u)            fixed O&M ($ per kW)
        vomcost(r,u)            variable O&M ($ per MWh)
        heatrate(r,u,v)         heat rate for new units
        deflator                deflate AEO data from $2008 to $2007 to match IPM
;

* convert IPM data from $2007 to $2010 (AEO data is already in $2010)
deflator        = (1.105783/1.085975) * (1.225/1.19819);

capcost(r,u,v)          = sum(mapAEO(u,aeo_u), aeo_data(aeo_u,"COST",v)) * regional_costs(r);
fomcost(r,u)            = sum(mapAEO(u,aeo_u), aeo_data(aeo_u,"fixed_OM","x"));
vomcost(r,u)            = sum(mapAEO(u,aeo_u), aeo_data(aeo_u,"var_OM","x"));

heatrate(r,u,v)         = sum(mapAEO(u,aeo_u), aeo_data(aeo_u,"heatrate",v));

* add IPM nuclear existing units cost data
fomcost(r,"nuc_X")$sum(mapRegions(r,st), sum(mapStates(st,state), sum(id, nuc_costs(id,state,"MW"))))
        = deflator * sum(mapRegions(r,st), sum(mapStates(st,state), sum(id, nuc_costs(id,state,"MW") * nuc_costs(id,state,"FOM"))))
                / sum(mapRegions(r,st), sum(mapStates(st,state), sum(id, nuc_costs(id,state,"MW"))));

vomcost(r,"nuc_X")$sum(mapRegions(r,st), sum(mapStates(st,state), sum(id, nuc_costs(id,state,"MW"))))
        = deflator * sum(mapRegions(r,st), sum(mapStates(st,state), sum(id, nuc_costs(id,state,"MW") * nuc_costs(id,state,"VOM"))))
                / sum(mapRegions(r,st), sum(mapStates(st,state), sum(id, nuc_costs(id,state,"MW"))));

* add IPM non-nuclear existing units cost data
vomcost(r,u)$vom_costs(u,"VOM") = deflator * vom_costs(u,"VOM");

set u_ipm(u) /
        hyd_X           existing hydro
        geo_X           existing geothermal
        bio_X           existing biomass
        lfg_X           existing landfill gas
        msw_X           existing municipal solid waste
        wnd_X           existing wind
        ps_X            existing pump storage
/;

fomcost(r,u_ipm)$fom_costs(u_ipm,"x","x","x","all","fom")       = deflator * fom_costs(u_ipm,"x","x","x","all","fom");


* fill in missing data
fomcost(r,"slr_X")      = deflator * fom_costs("slrCSP_X","x","x","x","all","fom");
fomcost(r,"oth_X")      = deflator * fomcost(r,"msw_X");

vomcost(r,"slr_X")      = deflator * vom_costs("slrCSP_X","VOM");
vomcost(r,"oth_X")      = deflator * vomcost(r,"msw_X");

display capcost,fomcost,vomcost,heatrate;


**--- add availability of new units (from IPM) ---**

* Tables 3.7 and 4.13
parameter       newavail(u) /
        col_N           85
        col_Nccs        85
        ngcc_N          87
        ngcc_Nccs       87
        ngtb_N          92
        nuc_N           90
        geo_N           87
        bio_N           83
        lfg_N           83
        fc_N            90 /;


execute_unload 'output\cost_data.gdx', capcost,fomcost,vomcost,fom_costs,vom_costs,vom_cgo_costs,heatrate=heatrate_n,newavail,regional_costs;
