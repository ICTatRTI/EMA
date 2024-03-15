$title Sets for Electricity Market Analysis model (EMA)
* Copyright RTI International 1/1/2011

*--- next steps ---*
* add nuclear uprates and retirements
* minimum generation

set     var /1*1000/;


**--    Time periods and Vintages       --**
set     yr              years           / 2010 * 2100 /
        t(yr)           time periods    / 2010,2015,2020,2025,2030,2035,2040,2045,2050,2075 /;          alias(t,v);

**--    Regions         --**
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

set     num(r)          / CA /;

set     r_cge           CGE model regions /
        NRTH            Northeast
        SATL            South Atlantic
        SCNT            South Central
        NCNT            North Central (Midwest)
        MNTN            Mountain
        WEST            West Coast /;

set     mapRegions(r_cge,r) /
        NRTH.(NEG,NY,MID)
        SATL.(SAC,FL)
        SCNT.(ESC,TX,WSC)
        NCNT.(ENC,WNC)
        MNTN.(MTN)
        WEST.(PAC,CA)
/;


**--    Generation Types        --**
set     gentype /
        col_x
        ngcc_x
        ngtb_x
        stog_x
        nuc_X
        hyd_X
        geo_X
        bio_X
        lfg_X
        msw_X
        slr_X
        wnd_X
        oth_X
        ps_X

        col_N
        col_Nccs
        ngcc_N
        ngcc_Nccs
        ngtb_N
        nuc_N
        bio_N
        geo_N
        lfg_N
        fc_N
        slrPV_N
        slrCSP_N
        wnd_N /;

set     newgentype(gentype) /
        col_N
        col_Nccs
        ngcc_N
        ngcc_Nccs
        ngtb_N
        nuc_N
        bio_N
        geo_N
        lfg_N
        fc_N
        slrPV_N
        slrCSP_N
        wnd_N /;

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

set     bio_cf(u)       biomass cofiring units  / col_X1bio,col_X2bio,col_X3bio /
        wnd(u)          new wind units          / wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7/
        geo(u)          new geothermal          / geo_N_1,geo_N_2,geo_N_3,geo_N_4,geo_N_5,geo_N_6,geo_N_7,geo_N_8,geo_N_9,geo_N_10/
        lfg(u)          new landfill gas        / lfg_N_hi,lfg_N_lo,lfg_N_vl /
        slr(u)          new solar units         / slrPV_N,slrCSP_N /
        nuc_n(u)        new nuclear             / nuc_n /
        bio_n(u)        new biomass             / bio_n /
        fc_n(u)         new fuel cells          / fc_n /
        baseu(u)        existing base load      / nuc_X,hyd_X,geo_X,lfg_X,msw_X,oth_X /
        newu(u) new units                       / col_N,col_Nccs,ngcc_N,ngcc_Nccs,ngtb_N,nuc_N,bio_N,lfg_N_hi,lfg_N_lo,lfg_N_vl,fc_N,slrPV_N,slrCSP_N,
                                geo_N_1,geo_N_2,geo_N_3,geo_N_4,geo_N_5,geo_N_6,geo_N_7,geo_N_8,geo_N_9,geo_N_10,
                                wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7 /
        ps(u)           pump storage            / ps_X /
        existu(u) /
                col_X1,col_X2,col_X3,
                ngcc_X1,ngcc_X2,ngcc_X3,
                ngtb_X1,ngtb_X2,ngtb_X3,
                stog_X1,stog_X2,stog_X3,
                nuc_X,hyd_X,geo_X,bio_X,lfg_X,msw_X,slr_X,wnd_X,oth_X/

        colu(u)         / col_X1,col_X2,col_X3 /
        igcc(u)         / col_N /
        igccCCS(u)      / col_Nccs/
        ngcc(u)         / ngcc_X1,ngcc_X2,ngcc_X3,ngcc_N /
        ngccX(u)        / ngcc_X1,ngcc_X2,ngcc_X3 /
        ngccCCS(u)      / ngcc_Nccs /
        ngtbu(u)        / ngtb_X1,ngtb_X2,ngtb_X3,ngtb_N /
        stog(u)         / stog_X1,stog_X2,stog_X3 /
        nucu(u)         / nuc_X,nuc_N /
        biou(u)         / bio_X,bio_N /
        hydu(u)         / hyd_X /
        geou(u)         / geo_X,geo_N_1,geo_N_2,geo_N_3,geo_N_4,geo_N_5,geo_N_6,geo_N_7,geo_N_8,geo_N_9,geo_N_10 /
        lfgu(u)         / msw_X,lfg_X,lfg_N_hi,lfg_N_lo,lfg_N_vl /
        slru(u)         / slr_X,slrPV_N,slrCSP_N /
        slrpvu(u)       / slrPV_N /
        slrcspu(u)      / slr_X,slrCSP_N /
        wndu(u)         / wnd_X, wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7 /
        wndon(u)        / wnd_X, wnd_Nonsh_3*wnd_Nonsh_7 /
        wndoff(u)       / wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7 /
        psu(u)          / ps_X /
        othu(u)         / oth_X,fc_N /

        ngccN(u)        / ngcc_N /
        ngtbN(u)                                / ngtb_N /
        nucN(u)         / nuc_N /
        bioN(u)         / bio_N /
        geoN(u)         / geo_N_1,geo_N_2,geo_N_3,geo_N_4,geo_N_5,geo_N_6,geo_N_7,geo_N_8,geo_N_9,geo_N_10 /
        wndN(u)         / wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7 /

        colN(u)         / col_N,col_Nccs /
        colX(u)         / col_X1,col_X2,col_X3 /
        ngtbX(u)        / ngtb_X1,ngtb_X2,ngtb_X3 /
        slrN(u)         / slrPV_N,slrCSP_N /

        resu(u)         / hyd_X,geo_X,bio_X,lfg_X,msw_X,slr_X,wnd_X,
                          bio_N,lfg_N_hi,lfg_N_lo,lfg_N_vl,
                          geo_N_1,geo_N_2,geo_N_3,geo_N_4,geo_N_5,geo_N_6,geo_N_7,geo_N_8,geo_N_9,geo_N_10,
                          slrPV_N,slrCSP_N,
                          wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7 /

        cesu(u)         / ngcc_X1,ngcc_X2,ngcc_X3,
                          ngtb_X1,ngtb_X2,ngtb_X3,
                          nuc_X,hyd_X,geo_X,bio_X,lfg_X,msw_X,slr_X,wnd_X,
                          col_Nccs,ngcc_N,ngcc_Nccs,ngtb_N,
                          nuc_N,bio_N,lfg_N_hi,lfg_N_lo,lfg_N_vl,
                          geo_N_1,geo_N_2,geo_N_3,geo_N_4,geo_N_5,geo_N_6,geo_N_7,geo_N_8,geo_N_9,geo_N_10,
                          slrPV_N,slrCSP_N,
                          wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7 /

        ccsu(u)         / col_Nccs,ngcc_Nccs /

        gasu(u)         / ngcc_X1,ngcc_X2,ngcc_X3,
                          ngtb_X1,ngtb_X2,ngtb_X3,
                          ngcc_N,ngtb_N /;


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

set     biostep         /1*17/
        rm_step         /1*6/;

set     p               pollutants / co2,so2,nox,hg /
        co2(p)          / co2 /;

set     windloc         wind location / on,shl,dep /
        windclass       / 3*7 /
        costclass       / 1*4 /;

set     newv(v),
        vt(v,t),
        tv(v,t),
        feasible(r,u,v,t);

set     mapGEN(gentype,u) /
        col_x.col_X1
        col_x.col_X2
        col_x.col_X3

        col_x.col_X1bio
        col_x.col_X2bio
        col_x.col_X3bio

        col_x.col_X1ccs
        col_x.col_X2ccs
        col_x.col_X3ccs

        ngcc_x.ngcc_X1
        ngcc_x.ngcc_X2
        ngcc_x.ngcc_X3

        ngtb_x.ngtb_X1
        ngtb_x.ngtb_X2
        ngtb_x.ngtb_X3

        stog_x.stog_X1
        stog_x.stog_X2
        stog_x.stog_X3

        nuc_X.nuc_X
        hyd_X.hyd_X
        geo_X.geo_X
        bio_X.bio_X
        lfg_X.lfg_X
        msw_X.msw_X
        slr_X.slr_X
        wnd_X.wnd_X
        oth_X.oth_X
        ps_X.ps_X

        col_N.col_N
        col_Nccs.col_Nccs
        ngcc_N.ngcc_N
        ngcc_Nccs.ngcc_Nccs
        ngtb_N.ngtb_N
        nuc_N.nuc_N
        geo_N.geo_N_1
        geo_N.geo_N_2
        geo_N.geo_N_3
        geo_N.geo_N_4
        geo_N.geo_N_5
        geo_N.geo_N_6
        geo_N.geo_N_7
        geo_N.geo_N_8
        geo_N.geo_N_9
        geo_N.geo_N_10
        bio_N.bio_N
        lfg_N.lfg_N_hi
        lfg_N.lfg_N_lo
        lfg_N.lfg_N_vl
        fc_N.fc_N
        slrPV_N.slrPV_N
        slrCSP_N.slrCSP_N

        wnd_N.(wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7) /;


set     noCO2exist(u) /nuc_X,hyd_X,geo_X,bio_X,lfg_X,msw_X,slr_X,wnd_X,oth_X,ps_X/;

set     noCO2new(u);    noCO2new(u)     = wnd(u) + geo(u) + slr(u) + ps(u) + nuc_n(u) + bio_n(u) + fc_n(u) + lfg(u);

set     payback         /101*130/;

set     newff(u) /
        col_N           new coal (pulverized IGCC)
        ngcc_N          new natural gas combined cycle
        ngtb_N          new natural gas turbine /;

set     cc      wind cost class /1*10/;


alias   (r,rr);

