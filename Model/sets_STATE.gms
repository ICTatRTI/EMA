$title Sets for Electricity Dispatch Partial Equilibrium Model
* Copyright RTI International 1/1/2011

set     var /1*1000/;

**--    Time periods and Vintages       --**
set     yr              years           / 2020 * 2100 /
*        t(yr)           time periods    / 2020,2025,2030,2035,2040,2045,2050,2075 /;          alias(t,v);
        t(yr)           time periods    / 2020 /;          alias(t,v);

**--    Regions         --**
* set     r               regions /
*         NEG             New England
*         NY              New York
*         MID             Middle Atlantic
*         ENC             East North Central
*         WNC             West North Central
*         FL              Florida
*         SAC             South Atlantic
*         ESC             East South Central
*         TX              Texas
*         WSC             West South Central
*         MTN             Mountain
*         CA              California
*         PAC             Pacific /;

**--    Regions are now states  --**
set r                   regions /
        AL              Alabama
        AR              Arkansas
        AZ              Arizona
        CA              California
        CO              Colorado
        CT              Connecticut
        DC              Washington DC
        DE              Delaware
        FL              Florida
        GA              Georgia
        IA              Iowa
        ID              Idaho
        IL              Illinois
        IN              Indiana
        KS              Kansas
        KY              Kentucky
        LA              Louisiana
        MA              Massachusetts
        MD              Maryland
        ME              Maine
        MI              Michigan
        MN              Minnesota
        MO              Missouri
        MS              Mississippi
        MT              Montana
        NC              North Carolina
        ND              North Dakota
        NE              Nebraska
        NH              New Hampshire
        NJ              New Jersey
        NM              New Mexico
        NV              Nevada
        NY              New York
        OH              Ohio
        OK              Oklahoma
        OR              Oregon
        PA              Pennsylvania
        RI              Rhode Island
        SC              South Carolina
        SD              South Dakota
        TN              Tennessee
        TX              Texas
        UT              Utah
        VA              Virginia
        VT              Vermont
        WA              Washington
        WI              Wisconsin
        WV              West Virginia
        WY              Wyoming /;

**--    Model Unit Types        --**
set     u               units   /
* existing units *
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

* new units *
        col_N           new coal (pulverized IGCC)
        col_Nccs        new coal with CCS
        ngcc_N          new natural gas combined cycle
        ngcc_Nccs       new natural gas combined cycle with CCS
        ngtb_N          new natural gas turbine
        nuc_N           new nuclear
        bio_N           new biomass
        lfg_N_hi        new landfill gas - high flow rate
        lfg_N_lo        new landfill gas - low flow rate
        lfg_N_vl        new landfill gas - very low flow rate
        geo_N_1         new geothermal - unit 1
        geo_N_2         new geothermal - unit 2
        geo_N_3         new geothermal - unit 3
        geo_N_4         new geothermal - unit 4
        geo_N_5         new geothermal - unit 5
        geo_N_6         new geothermal - unit 6
        geo_N_7         new geothermal - unit 7
        geo_N_8         new geothermal - unit 8
        geo_N_9         new geothermal - unit 9
        geo_N_10        new geothermal - unit 10
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

set
        ps(u)           pump storage            / ps_X /
;

**--    Load Segments by Season and Time of Day    --**
* Summer = May to September
* Fall = October to November
* Winter = December to March
* Spring =  April
set     l               load segments /
        sum_day         summer day (8-18)
        sum_mrnevn      summer morning evening (6-7 19-24)
        sum_night       summer night (1-5)

        sprfl_day       spring-fall day (9-16)
        sprfl_mrnevn    spring-fall morning evening (6-8 17-24)
        sprfl_night     spring night (1-5)

        wntr_day        winter day (9-16)
        wntr_mrnevn     winter morning evening (6-8 17-24)
        wntr_night      winter night (1-5)

        peak            peak (top 88 hours) /;

set     daytime(l)      / sum_day,sprfl_day,wntr_day,peak /
        night(l)        / sum_night,sprfl_night,wntr_night /
        mrnevn(l)       / sum_mrnevn,sprfl_mrnevn,wntr_mrnevn /;

alias (mrnevn,mrnevns);

**--    Fuels   --**
set     f               fuel types /
        col             coal
        gas             natural gas
        oil             refined petroleum
        nuc             nuclear (uranium)
        hyd             hydro
        geo             geothermal
        bio             biomass
        lfg             landfill gas
        wnd             wind
        slr             solar /;

set     bio(f)          / bio /
        cgo(f)          / col,gas,oil/;

set     biostep         /1*17/;

set     p               pollutants / co2,so2,nox,hg /
        co2(p)          / co2 /;

set     
        feasible(r,u,v,t);


alias   (r,rr);

