$title Utility Data

* NEEDS Utility Data
$include IPM\NEEDS\id.txt

set     t /2010,2015/; alias (t,v);

set     season /winter,summer/;

set type                Plant Type /
        "Biomass"
        "Coal Steam"
        "Combined Cycle"
        "Combustion Turbine"
        "Fossil Waste"
        "Fuel Cell"
        "Geothermal"
        "Hydro"
        "IGCC"
        "Landfill Gas"
        "Municipal Solid Waste"
        "Non-Fossil Waste"
        "Nuclear"
        "O/G Steam"
        "Pumped Storage"
        "Solar"
        "Tires"
        "Wind" /;


set     r_ipm   IPM regions /
        AZNM
        CA-N
        CA-S
        COMD
        DSNY
        ENTG
        ERCT
        FRCC
        GWAY
        LILC
        MACE
        MACS
        MACW
        MECS
        MRO
        NENG
        NWPE
        NYC
        PNW
        RFCO
        RFCP
        RMPA
        SNV
        SOU
        SPPN
        SPPS
        TVA
        TVAK
        UPNY
        VACA
        VAPW
        WUMS /;

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

set     fuels /
        "Biomass"
        "Bituminous"
        "Bituminous, Subbituminous"
        "Distillate Fuel Oil"
        "Distillate Fuel Oil, Residual Fuel Oil"
        "Fossil Waste"
        "Geothermal"
        "Hydro"
        "Landfill Gas"
        "Lignite"
        "Lignite, Subbituminous"
        "MSW"
        "Natural Gas"
        "Natural Gas, Distillate Fuel Oil"
        "Natural Gas, Distillate Fuel Oil, Residual Fuel Oil"
        "Natural Gas, Residual Fuel Oil"
        "Non-Fossil Waste"
        "Nuclear Fuel"
        "Petroleum Coke"
        "Pumped Storage"
        "Residual Fuel Oil"
        "Solar"
        "Subbituminous"
        "Tires"
        "Waste Coal"
        "Wind" /;

set     f /col,gas,oil,nuc,hyd,geo,bio,lfg,msw,sol,wnd,wst,oth,ps/;

set     mapFuels(f,fuels) /
        bio."Biomass"
        col."Bituminous"
        col."Bituminous, Subbituminous"
        oil."Distillate Fuel Oil"
        oil."Distillate Fuel Oil, Residual Fuel Oil"
        wst."Fossil Waste"
        geo."Geothermal"
        hyd."Hydro"
        lfg."Landfill Gas"
        col."Lignite"
        col."Lignite, Subbituminous"
        msw."MSW"
        gas."Natural Gas"
        gas."Natural Gas, Distillate Fuel Oil"
        gas."Natural Gas, Distillate Fuel Oil, Residual Fuel Oil"
        gas."Natural Gas, Residual Fuel Oil"
        wst."Non-Fossil Waste"
        nuc."Nuclear Fuel"
        oil."Petroleum Coke"
        ps."Pumped Storage"
        oil."Residual Fuel Oil"
        sol."Solar"
        col."Subbituminous"
        wst."Tires"
        col."Waste Coal"
        wnd."Wind" /;

set     so2 /
        "Dry Scrubber"
        "Reagent Injection"
        "Wet Scrubber"
        "" /;

set     nox /
        "SCR"
        "SNCR"
        "" /;

set     scr(nox) /scr/;

set     notscr(nox) /"SNCR",""/;

set     pm /
        "B"
        "B + C"
        "B + WS"
        "C"
        "C + B"
        "C + WS"
        "ESPC"
        "ESPC + B"
        "ESPC + C"
        "ESPC + C + WS"
        "ESPC + ESPH"
        "ESPC + WS"
        "ESPH"
        "ESPH + B"
        "ESPH + C"
        "WS"
        "" /;

set     hg /
        "ACI"
        "" /;

set     hg_emf /
        "Cold-side ESP"
        "Cold-side ESP + Cyclone"
        "Cold-side ESP + Cyclone + SCR + Dry Scrubber"
        "Cold-side ESP + Cyclone + SCR + Wet Scrubber"
        "Cold-side ESP + Cyclone + SNCR"
        "Cold-side ESP + Cyclone + SNCR + Wet Scrubber"
        "Cold-side ESP + Dry Scrubber"
        "Cold-side ESP + Fabric Filter"
        "Cold-side ESP + Fabric Filter + SCR"
        "Cold-side ESP + Fabric Filter + SCR + Dry Scrubber"
        "Cold-side ESP + Fabric Filter + SCR + Wet Scrubber"
        "Cold-side ESP + Fabric Filter + SNCR"
        "Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber"
        "Cold-side ESP + Fabric Filter + Wet Scrubber"
        "Cold-side ESP + Hot-side ESP + Dry Scrubber"
        "Cold-side ESP + Hot-side ESP + SNCR"
        "Cold-side ESP + PM Scrubber"
        "Cold-side ESP + PM Scrubber + SCR"
        "Cold-side ESP + SCR"
        "Cold-side ESP + SCR + Dry Scrubber"
        "Cold-side ESP + SCR + Wet Scrubber"
        "Cold-side ESP + SNCR"
        "Cold-side ESP + SNCR + Dry Scrubber"
        "Cold-side ESP + SNCR + Wet Scrubber"
        "Cold-side ESP + Wet Scrubber"
        "Cyclone"
        "Cyclone + Wet Scrubber"
        "Fabric Filter"
        "Fabric Filter + Cyclone"
        "Fabric Filter + Cyclone + Dry Scrubber"
        "Fabric Filter + Cyclone + SNCR"
        "Fabric Filter + Dry Scrubber"
        "Fabric Filter + SCR + Dry Scrubber"
        "Fabric Filter + SCR + Wet Scrubber"
        "Fabric Filter + SNCR"
        "Fabric Filter + SNCR + Dry Scrubber"
        "Fabric Filter + SNCR + Wet Scrubber"
        "Fabric Filter + Wet Scrubber"
        "Hot-side ESP"
        "Hot-side ESP + Cyclone"
        "Hot-side ESP + Cyclone + SNCR"
        "Hot-side ESP + Fabric Filter"
        "Hot-side ESP + Fabric Filter + SCR"
        "Hot-side ESP + Fabric Filter + SCR + Wet Scrubber"
        "Hot-side ESP + Fabric Filter + SNCR"
        "Hot-side ESP + Fabric Filter + Wet Scrubber"
        "Hot-side ESP + SCR"
        "Hot-side ESP + SCR + Dry Scrubber"
        "Hot-side ESP + SCR + Wet Scrubber"
        "Hot-side ESP + SNCR"
        "Hot-side ESP + Wet Scrubber"
        "PM Scrubber"
        "PM Scrubber + SNCR"
        "SCR + Wet Scrubber"
        "SNCR + Wet Scrubber"
        "Wet Scrubber"
        ""

* new retrofit possibilities
"Hot-side ESP + SNCR + Wet Scrubber"
"SNCR"
"SCR"
/;

parameter
        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,*)         IPM data
        avail(id,*)                                             IPM availability by plant
        hydroavail(r_ipm,*)                                     IPM hydro availability
        rsrvmrgn(*,*)                                           IPM reserve margin
        regcost(*,*)                                            IPM regional cost markups;

$if not exist IPM\NEEDS\needs_data.gdx $call 'gdxxrw i=IPM\NEEDS\needsv410.xls o=IPM\NEEDS\needs_data.gdx log=IPM\NEEDS\needs.log par=data rng=GAMS!a2:aa15023 rdim=8 cdim=1'
$gdxin 'IPM\NEEDS\needs_data.gdx'
$loaddc data

$if not exist IPM\avail_data.gdx $call 'gdxxrw i=IPM\Chapter3Appendix3_9.xls o=IPM\avail_data.gdx log=IPM\avail.log par=avail rng=Availability!d6:g15027 rdim=1 cdim=1'
$gdxin 'IPM\avail_data.gdx'
$loaddc avail

$if not exist IPM\hydroavail_data.gdx $call 'gdxxrw i=IPM\Chapter3_hydro_capfactor.xls o=IPM\hydroavail_data.gdx log=IPM\hydroavail.log par=hydroavail rng=Sheet1!a1:d31 rdim=1 cdim=1'
$gdxin 'IPM\hydroavail_data.gdx'
$loaddc hydroavail

$if not exist IPM\rsrvmrgn_data.gdx $call 'gdxxrw i=IPM\Chapter3_reserve_margin.xls o=IPM\rsrvmrgn_data.gdx log=IPM\rsrvmrgn.log par=rsrvmrgn rng=Sheet1!a1:d44 rdim=1 cdim=1'
$gdxin 'IPM\rsrvmrgn_data.gdx'
$load rsrvmrgn

$if not exist IPM\regcost_data.gdx $call 'gdxxrw i=IPM\Chapter4_regionalcosts.xls o=IPM\regcost_data.gdx log=IPM\regcost.log par=regcost rng=Sheet1!l1:m33 rdim=1 cdim=1'
$gdxin 'IPM\regcost_data.gdx'
$loaddc regcost

*option data:2:10:1;
option data:2:8:1;
*display data;
*display avail;
*display hydroavail;
*display rsrvmrgn;
*display regcost;
*$exit

**--- Unit Labels for Electricity Model ---**

set     u               units   /
        col_X01         existing coal class 1 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X02         existing coal class 2 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X03         existing coal class 3 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X04         existing coal class 4 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X05         existing coal class 5 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X06         existing coal class 6 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X07         existing coal class 7 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X08         existing coal class 8 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X09         existing coal class 9 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X10         existing coal class 10 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X11         existing coal class 11 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X12         existing coal class 12 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X13         existing coal class 13 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X14         existing coal class 14 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X15         existing coal class 15 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X16         existing coal class 16 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X17         existing coal class 17 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X18         existing coal class 18 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X19         existing coal class 19 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X20         existing coal class 20 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X21         existing coal class 21 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X22         existing coal class 22 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X23         existing coal class 23 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X24         existing coal class 24 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X25         existing coal class 25 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X26         existing coal class 26 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X27         existing coal class 27 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X28         existing coal class 28 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X29         existing coal class 29 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X30         existing coal class 30 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X31         existing coal class 31 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X32         existing coal class 32 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X33         existing coal class 33 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X34         existing coal class 34 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X35         existing coal class 35 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X36         existing coal class 36 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X37         existing coal class 37 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X38         existing coal class 38 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X39         existing coal class 39 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X40         existing coal class 40 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X41         existing coal class 41 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X42         existing coal class 42 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X43         existing coal class 43 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X44         existing coal class 44 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X45         existing coal class 45 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X46         existing coal class 46 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X47         existing coal class 47 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X48         existing coal class 48 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X49         existing coal class 49 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X50         existing coal class 50 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X51         existing coal class 51 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X52         existing coal class 52 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X53         existing coal class 53 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X54         existing coal class 54 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X55         existing coal class 55 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X56         existing coal class 56 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X57         existing coal class 57 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X58         existing coal class 58 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X59         existing coal class 59 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X60         existing coal class 60 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI



        col_X61         existing coal class 1 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X62         existing coal class 2 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X63         existing coal class 3 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X64         existing coal class 4 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X65         existing coal class 5 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X66         existing coal class 6 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X67         existing coal class 7 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X68         existing coal class 8 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X69         existing coal class 9 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X70         existing coal class 10 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X71         existing coal class 11 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X72         existing coal class 12 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X73         existing coal class 13 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X74         existing coal class 14 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X75         existing coal class 15 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X76         existing coal class 16 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X77         existing coal class 17 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X78         existing coal class 18 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X79         existing coal class 19 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X80         existing coal class 20 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X81         existing coal class 21 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X82         existing coal class 22 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X83         existing coal class 23 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X84         existing coal class 24 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X85         existing coal class 25 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X86         existing coal class 26 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X87         existing coal class 27 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X88         existing coal class 28 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X89         existing coal class 29 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X90         existing coal class 30 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X91         existing coal class 31 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X92         existing coal class 32 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X93         existing coal class 33 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X94         existing coal class 34 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X95         existing coal class 35 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X96         existing coal class 36 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X97         existing coal class 37 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X98         existing coal class 38 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X99         existing coal class 39 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X100        existing coal class 40 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X101        existing coal class 41 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X102        existing coal class 42 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X103        existing coal class 43 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X104        existing coal class 44 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X105        existing coal class 45 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X106        existing coal class 46 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X107        existing coal class 47 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X108        existing coal class 48 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X109        existing coal class 49 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X110        existing coal class 50 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X111        existing coal class 51 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X112        existing coal class 52 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X113        existing coal class 53 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X114        existing coal class 54 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X115        existing coal class 55 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X116        existing coal class 56 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X117        existing coal class 57 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X118        existing coal class 58 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X119        existing coal class 59 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X120        existing coal class 60 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI



        col_X121        existing coal class 1 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X122        existing coal class 2 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X123        existing coal class 3 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X124        existing coal class 4 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X125        existing coal class 5 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X126        existing coal class 6 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X127        existing coal class 7 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X128        existing coal class 8 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X129        existing coal class 9 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X130        existing coal class 10 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X131        existing coal class 11 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X132        existing coal class 12 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X133        existing coal class 13 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X134        existing coal class 14 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X135        existing coal class 15 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X136        existing coal class 16 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X137        existing coal class 17 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X138        existing coal class 18 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X139        existing coal class 19 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X140        existing coal class 20 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X141        existing coal class 21 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X142        existing coal class 22 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X143        existing coal class 23 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X144        existing coal class 24 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X145        existing coal class 25 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X146        existing coal class 26 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X147        existing coal class 27 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X148        existing coal class 28 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X149        existing coal class 29 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X150        existing coal class 30 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X151        existing coal class 31 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X152        existing coal class 32 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X153        existing coal class 33 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X154        existing coal class 34 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X155        existing coal class 35 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X156        existing coal class 36 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X157        existing coal class 37 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X158        existing coal class 38 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X159        existing coal class 39 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X160        existing coal class 40 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X161        existing coal class 41 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X162        existing coal class 42 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X163        existing coal class 43 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X164        existing coal class 44 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X165        existing coal class 45 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X166        existing coal class 46 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X167        existing coal class 47 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X168        existing coal class 48 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X169        existing coal class 49 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X170        existing coal class 50 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X171        existing coal class 51 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X172        existing coal class 52 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X173        existing coal class 53 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X174        existing coal class 54 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X175        existing coal class 55 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        col_X176        existing coal class 56 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X177        existing coal class 57 - low heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X178        existing coal class 58 - medium heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X179        existing coal class 59 - med-high heatrate - no PM - no FGD - no SCR-SNCR - no ACI
        col_X180        existing coal class 60 - high heatrate - no PM - no FGD - no SCR-SNCR - no ACI

        ngcc_X01                existing natural gas combined cycle class 1 - low heatrate - no SCR
        ngcc_X02                existing natural gas combined cycle class 2 - medium heatrate - no SCR
        ngcc_X03                existing natural gas combined cycle class 3 - high heatrate - no SCR
        ngcc_X04                existing natural gas combined cycle class 4 - low heatrate - with SCR
        ngcc_X05                existing natural gas combined cycle class 5 - medium heatrate - with SCR
        ngcc_X06                existing natural gas combined cycle class 6 - high heatrate - with SCR

        ngtb_X01                existing natural gas turbine class 1 - low heatrate - no SCR
        ngtb_X02                existing natural gas turbine class 2 - medium heatrate - no SCR
        ngtb_X03                existing natural gas turbine class 3 - high heatrate - no SCR
        ngtb_X04                existing natural gas turbine class 4 - low heatrate - with SCR
        ngtb_X05                existing natural gas turbine class 5 - medium heatrate - with SCR
        ngtb_X06                existing natural gas turbine class 6 - high heatrate - with SCR

        stog_X01                existing steam oil-gas class 1 - low heatrate - no SCR
        stog_X02                existing steam oil-gas class 2 - medium heatrate - no SCR
        stog_X03                existing steam oil-gas class 3 - high heatrate - no SCR
        stog_X04                existing steam oil-gas class 4 - low heatrate - with SCR
        stog_X05                existing steam oil-gas class 5 - medium heatrate - with SCR
        stog_X06                existing steam oil-gas class 6 - high heatrate) - with SCR

        nuc_X           existing nuclear
        hyd_X           existing hydro
        geo_X           existing geothermal
        bio_X           existing biomass
        lfg_X           existing landfill gas
        msw_X           existing municipal solid waste
        slr_X           existing solar
        wnd_X           existing wind
        oth_X           existing other
        ps_X            existing pump storage /;

set     special(u)       special units (non-aggregated) /
        col_X01*col_X180
        ngcc_X01*ngcc_X06
        ngtb_X01*ngtb_x06
        stog_x01*stog_x06 /;

set     coal(u)         /
        col_X01*col_X180 /;

set     gasoil(u)       /
        ngcc_X01*ngcc_X06
        ngtb_X01*ngtb_x06
        stog_x01*stog_x06 /;

set     slr_u(u) /
        slr_X           existing solar /;

set     mapSCR(u,nox)   /
        ngcc_X01.("SNCR","")
        ngcc_X02.("SNCR","")
        ngcc_X03.("SNCR","")
        ngcc_X04.("SCR")
        ngcc_X05.("SCR")
        ngcc_X06.("SCR")

        ngtb_X01.("SNCR","")
        ngtb_X02.("SNCR","")
        ngtb_X03.("SNCR","")
        ngtb_X04.("SCR")
        ngtb_X05.("SCR")
        ngtb_X06.("SCR")

        stog_X01.("SNCR","")
        stog_X02.("SNCR","")
        stog_X03.("SNCR","")
        stog_X04.("SCR")
        stog_X05.("SCR")
        stog_X06.("SCR") /;

set     mapTypes(u,type) /
        (col_X01*col_X180)."Coal Steam"

        ngcc_X01."Combined Cycle"
        ngcc_X02."Combined Cycle"
        ngcc_X03."Combined Cycle"
        ngcc_X04."Combined Cycle"
        ngcc_X05."Combined Cycle"
        ngcc_X06."Combined Cycle"

        ngtb_X01."Combustion Turbine"
        ngtb_X02."Combustion Turbine"
        ngtb_X03."Combustion Turbine"
        ngtb_X04."Combustion Turbine"
        ngtb_X05."Combustion Turbine"
        ngtb_X06."Combustion Turbine"

        stog_X01."O/G Steam"
        stog_X02."O/G Steam"
        stog_X03."O/G Steam"
        stog_X04."O/G Steam"
        stog_X05."O/G Steam"
        stog_X06."O/G Steam"

        nuc_X."Nuclear"
        hyd_X."Hydro"
        geo_X."Geothermal"
        bio_X."Biomass"
        lfg_X."Landfill Gas"
        msw_X."Municipal Solid Waste"
        slr_X."Solar"
        wnd_X."Wind"
        oth_X.("Tires","Fuel Cell","Non-Fossil Waste","Fossil Waste")
        ps_X."Pumped Storage" /;

set     col_low(u) /col_x01*col_x60/;
set     col_med(u) /col_x61*col_x120/;
set     col_high(u) /col_x121*col_x180/;


set     mapEquip(u,hg_emf,hg) /
        col_X01.("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("")
        col_X02.("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("")
        col_X03.("Cold-side ESP + SNCR + Dry Scrubber").("")
        col_X04.("Cold-side ESP + SCR").("")
        col_X05.("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("")
        col_X06.("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("")
        col_X07.("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("")
        col_X08.("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("")
        col_X09.("Cold-side ESP","Cold-side ESP + Cyclone").("")

        col_X10.("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("")
        col_X11.("Fabric Filter + SNCR + Wet Scrubber").("")
        col_X12.("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("")
        col_X13.("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("")
        col_X14.("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("")
        col_X15.("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("")
        col_X16.("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("")
        col_X17.("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("")
        col_X18.("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("")

        col_X19.("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("")
        col_X20.("Hot-side ESP + SNCR + Wet Scrubber").("")
        col_X21.("Hot-side ESP + SCR").("")
        col_X22.("Hot-side ESP + SCR + Wet Scrubber").("")
        col_X23.("Hot-side ESP + SCR + Dry Scrubber").("")
        col_X24.("Hot-side ESP + Wet Scrubber").("")
        col_X25.("Hot-side ESP","Hot-side ESP + Cyclone").("")

        col_X26.("SNCR","Cyclone","").("")
        col_X27.("SNCR + Wet Scrubber","PM Scrubber + SNCR").("")
        col_X28.("SCR").("")
        col_X29.("SCR + Wet Scrubber").("")
        col_X30.("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("")


        col_X31.("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("ACI")
        col_X32.("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("ACI")
        col_X33.("Cold-side ESP + SNCR + Dry Scrubber").("ACI")
        col_X34.("Cold-side ESP + SCR").("ACI")
        col_X35.("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("ACI")
        col_X36.("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("ACI")
        col_X37.("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("ACI")
        col_X38.("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("ACI")
        col_X39.("Cold-side ESP","Cold-side ESP + Cyclone").("ACI")

        col_X40.("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("ACI")
        col_X41.("Fabric Filter + SNCR + Wet Scrubber").("ACI")
        col_X42.("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("ACI")
        col_X43.("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("ACI")
        col_X44.("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("ACI")
        col_X45.("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("ACI")
        col_X46.("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("ACI")
        col_X47.("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("ACI")
        col_X48.("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("ACI")

        col_X49.("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("ACI")
        col_X50.("Hot-side ESP + SNCR + Wet Scrubber").("ACI")
        col_X51.("Hot-side ESP + SCR").("ACI")
        col_X52.("Hot-side ESP + SCR + Wet Scrubber").("ACI")
        col_X53.("Hot-side ESP + SCR + Dry Scrubber").("ACI")
        col_X54.("Hot-side ESP + Wet Scrubber").("ACI")
        col_X55.("Hot-side ESP","Hot-side ESP + Cyclone").("ACI")

        col_X56.("SNCR","Cyclone","").("ACI")
        col_X57.("SNCR + Wet Scrubber","PM Scrubber + SNCR").("ACI")
        col_X58.("SCR").("ACI")
        col_X59.("SCR + Wet Scrubber").("ACI")
        col_X60.("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("ACI")


        col_X61.("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("")
        col_X62.("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("")
        col_X63.("Cold-side ESP + SNCR + Dry Scrubber").("")
        col_X64.("Cold-side ESP + SCR").("")
        col_X65.("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("")
        col_X66.("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("")
        col_X67.("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("")
        col_X68.("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("")
        col_X69.("Cold-side ESP","Cold-side ESP + Cyclone").("")

        col_X70.("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("")
        col_X71.("Fabric Filter + SNCR + Wet Scrubber").("")
        col_X72.("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("")
        col_X73.("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("")
        col_X74.("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("")
        col_X75.("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("")
        col_X76.("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("")
        col_X77.("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("")
        col_X78.("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("")

        col_X79.("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("")
        col_X80.("Hot-side ESP + SNCR + Wet Scrubber").("")
        col_X81.("Hot-side ESP + SCR").("")
        col_X82.("Hot-side ESP + SCR + Wet Scrubber").("")
        col_X83.("Hot-side ESP + SCR + Dry Scrubber").("")
        col_X84.("Hot-side ESP + Wet Scrubber").("")
        col_X85.("Hot-side ESP","Hot-side ESP + Cyclone").("")

        col_X86.("SNCR","Cyclone","").("")
        col_X87.("SNCR + Wet Scrubber","PM Scrubber + SNCR").("")
        col_X88.("SCR").("")
        col_X89.("SCR + Wet Scrubber").("")
        col_X90.("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("")


        col_X91.("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("ACI")
        col_X92.("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("ACI")
        col_X93.("Cold-side ESP + SNCR + Dry Scrubber").("ACI")
        col_X94.("Cold-side ESP + SCR").("ACI")
        col_X95.("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("ACI")
        col_X96.("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("ACI")
        col_X97.("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("ACI")
        col_X98.("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("ACI")
        col_X99.("Cold-side ESP","Cold-side ESP + Cyclone").("ACI")

        col_X100.("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("ACI")
        col_X101.("Fabric Filter + SNCR + Wet Scrubber").("ACI")
        col_X102.("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("ACI")
        col_X103.("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("ACI")
        col_X104.("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("ACI")
        col_X105.("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("ACI")
        col_X106.("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("ACI")
        col_X107.("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("ACI")
        col_X108.("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("ACI")

        col_X109.("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("ACI")
        col_X110.("Hot-side ESP + SNCR + Wet Scrubber").("ACI")
        col_X111.("Hot-side ESP + SCR").("ACI")
        col_X112.("Hot-side ESP + SCR + Wet Scrubber").("ACI")
        col_X113.("Hot-side ESP + SCR + Dry Scrubber").("ACI")
        col_X114.("Hot-side ESP + Wet Scrubber").("ACI")
        col_X115.("Hot-side ESP","Hot-side ESP + Cyclone").("ACI")

        col_X116.("SNCR","Cyclone","").("ACI")
        col_X117.("SNCR + Wet Scrubber","PM Scrubber + SNCR").("ACI")
        col_X118.("SCR").("ACI")
        col_X119.("SCR + Wet Scrubber").("ACI")
        col_X120.("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("ACI")


        col_X121.("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("")
        col_X122.("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("")
        col_X123.("Cold-side ESP + SNCR + Dry Scrubber").("")
        col_X124.("Cold-side ESP + SCR").("")
        col_X125.("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("")
        col_X126.("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("")
        col_X127.("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("")
        col_X128.("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("")
        col_X129.("Cold-side ESP","Cold-side ESP + Cyclone").("")

        col_X130.("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("")
        col_X131.("Fabric Filter + SNCR + Wet Scrubber").("")
        col_X132.("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("")
        col_X133.("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("")
        col_X134.("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("")
        col_X135.("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("")
        col_X136.("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("")
        col_X137.("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("")
        col_X138.("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("")

        col_X139.("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("")
        col_X140.("Hot-side ESP + SNCR + Wet Scrubber").("")
        col_X141.("Hot-side ESP + SCR").("")
        col_X142.("Hot-side ESP + SCR + Wet Scrubber").("")
        col_X143.("Hot-side ESP + SCR + Dry Scrubber").("")
        col_X144.("Hot-side ESP + Wet Scrubber").("")
        col_X145.("Hot-side ESP","Hot-side ESP + Cyclone").("")

        col_X146.("SNCR","Cyclone","").("")
        col_X147.("SNCR + Wet Scrubber","PM Scrubber + SNCR").("")
        col_X148.("SCR").("")
        col_X149.("SCR + Wet Scrubber").("")
        col_X150.("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("")


        col_X151.("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("ACI")
        col_X152.("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("ACI")
        col_X153.("Cold-side ESP + SNCR + Dry Scrubber").("ACI")
        col_X154.("Cold-side ESP + SCR").("ACI")
        col_X155.("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("ACI")
        col_X156.("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("ACI")
        col_X157.("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("ACI")
        col_X158.("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("ACI")
        col_X159.("Cold-side ESP","Cold-side ESP + Cyclone").("ACI")

        col_X160.("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("ACI")
        col_X161.("Fabric Filter + SNCR + Wet Scrubber").("ACI")
        col_X162.("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("ACI")
        col_X163.("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("ACI")
        col_X164.("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("ACI")
        col_X165.("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("ACI")
        col_X166.("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("ACI")
        col_X167.("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("ACI")
        col_X168.("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("ACI")

        col_X169.("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("ACI")
        col_X170.("Hot-side ESP + SNCR + Wet Scrubber").("ACI")
        col_X171.("Hot-side ESP + SCR").("ACI")
        col_X172.("Hot-side ESP + SCR + Wet Scrubber").("ACI")
        col_X173.("Hot-side ESP + SCR + Dry Scrubber").("ACI")
        col_X174.("Hot-side ESP + Wet Scrubber").("ACI")
        col_X175.("Hot-side ESP","Hot-side ESP + Cyclone").("ACI")

        col_X176.("SNCR","Cyclone","").("ACI")
        col_X177.("SNCR + Wet Scrubber","PM Scrubber + SNCR").("ACI")
        col_X178.("SCR").("ACI")
        col_X179.("SCR + Wet Scrubber").("ACI")
        col_X180.("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("ACI")
/;

set     basegentype(type) /
        "Biomass"
        "Coal Steam"
        "Combined Cycle"
        "Combustion Turbine"
        "Fossil Waste"
        "Geothermal"
        "Hydro"
        "IGCC"
        "Landfill Gas"
        "Municipal Solid Waste"
        "Non-Fossil Waste"
        "Nuclear"
        "O/G Steam"
        "Pumped Storage"
        "Solar"
        "Tires"
        "Wind" /;

set     basecaptype(type) /
        "Biomass"
        "Coal Steam"
        "Combined Cycle"
        "Hydro"
        "Nuclear" /;


parameter
        capacity(*,*,*)         capacity (megawatts)
        htrate
        heatrate
        equip
        age
        size
        emis_rate
        so2_effic
        maxCF
        RsrvMargin              reserve margin
        regional_costs
        categories(u,*,*)
        basegen(*,*)            estimate of generation by IPM region and EMA region
        genshr                  estimate of regional generation shares
        basecap(r_ipm,r)        estimate of capacity by IPM region and EMA region
        statecap                capacity by state and type
        biocap                  biomass capacity
        availtype
        count
;

*-- develop shares based on estimated generation to convert IPM regions into model regions --*
availtype("Biomass")            = 0.83;
availtype("Coal Steam")         = 0.85;
availtype("Combined Cycle")     = 0.3;
availtype("Combustion Turbine") = 0.2;

availtype("Geothermal")         = 0.87;
availtype("O/G Steam")          = 0.05;
availtype("Hydro")              = 0.4;
availtype("Nuclear")            = 0.9;
availtype("Wind")               = 0.35;

availtype("Fuel Cell")          = 0.8;
availtype("Fossil Waste")       = 0.8;
availtype("IGCC")               = 0.85;
availtype("Landfill Gas")       = 0.8;
availtype("Municipal Solid Waste")= 0.8;
availtype("Non-Fossil Waste")   = 0.8;
availtype("Solar")              = 0.25;
availtype("Tires")              = 0.8;

basegen(r_ipm,r)
        = sum(mapRegions(r,st), sum(mapStates(st,state),
          sum(id, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                sum(basegentype,
                        data(id,basegentype,r_ipm,state,fuels,nox,hg,hg_emf,"MW") * availtype(basegentype) * 8760 )
          ))))))) / 1e6;

genshr(r_ipm,r)
        = basegen(r_ipm,r) / sum(rr, basegen(r_ipm,rr));

basecap(r_ipm,r)
        = sum(mapRegions(r,st), sum(mapStates(st,state),
          sum(id, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                sum(basecaptype,
                        data(id,basecaptype,r_ipm,state,fuels,nox,hg,hg_emf,"MW") )
          ))))))) / 1e6;

RsrvMargin(r)   = sum(r_ipm, basegen(r_ipm,r) * rsrvmrgn(r_ipm,"margin")) / sum(r_ipm, basegen(r_ipm,r));

regional_costs(r)= sum(r_ipm, basegen(r_ipm,r) * regcost(r_ipm,"reg_cost")) / sum(r_ipm, basegen(r_ipm,r));

statecap(st,u)
        = sum(mapTypes(u,type),
                sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010) )
          ))))))) / 1000;

biocap(st) = statecap(st,"bio_x");

display basegen,basecap,genshr;
display RsrvMargin,regional_costs;
option biocap:2:0:1;
display statecap,biocap;
*$exit

execute_unload 'IPM\regional_shares.gdx', basegen,basecap,genshr;

execute_unload 'IPM\regional_costs.gdx', regional_costs;

execute_unload 'IPM\state_capacity.gdx', statecap, biocap;


**--- save NEEDS data ---**
categories(col_low,"heatrate","lower")          = 0;
categories(col_low,"heatrate","upper")          = 9500;

categories(col_med,"heatrate","lower")          = 9500;
categories(col_med,"heatrate","upper")          = 10500;

categories(col_high,"heatrate","lower")         = 10500;
categories(col_high,"heatrate","upper")         = +inf;


categories("ngcc_X01","heatrate","lower")       = 0;
categories("ngcc_X01","heatrate","upper")       = 7500;
categories("ngcc_X02","heatrate","upper")       = 9500;

categories("ngcc_X02","heatrate","lower")       = categories("ngcc_X01","heatrate","upper");
categories("ngcc_X03","heatrate","lower")       = categories("ngcc_X02","heatrate","upper");
categories("ngcc_X03","heatrate","upper")       = +inf;

categories("ngcc_X04","heatrate","lower")       = 0;
categories("ngcc_X04","heatrate","upper")       = 7500;
categories("ngcc_X05","heatrate","upper")       = 9500;

categories("ngcc_X05","heatrate","lower")       = categories("ngcc_X04","heatrate","upper");
categories("ngcc_X06","heatrate","lower")       = categories("ngcc_X05","heatrate","upper");
categories("ngcc_X06","heatrate","upper")       = +inf;

categories("ngtb_X01","heatrate","lower")       = 0;
categories("ngtb_X01","heatrate","upper")       = 11500;
categories("ngtb_X02","heatrate","upper")       = 13000;

categories("ngtb_X02","heatrate","lower")       = categories("ngtb_X01","heatrate","upper");
categories("ngtb_X03","heatrate","lower")       = categories("ngtb_X02","heatrate","upper");
categories("ngtb_X03","heatrate","lower")       = +inf;

categories("ngtb_X04","heatrate","lower")       = 0;
categories("ngtb_X04","heatrate","upper")       = 11500;
categories("ngtb_X05","heatrate","upper")       = 13000;

categories("ngtb_X05","heatrate","lower")       = categories("ngtb_X04","heatrate","upper");
categories("ngtb_X06","heatrate","lower")       = categories("ngtb_X05","heatrate","upper");
categories("ngtb_X06","heatrate","upper")       = +inf;

categories("stog_X01","heatrate","lower")       = 0;
categories("stog_X01","heatrate","upper")       = 10500;
categories("stog_X02","heatrate","upper")       = 12000;

categories("stog_X02","heatrate","lower")       = categories("stog_X01","heatrate","upper");
categories("stog_X03","heatrate","lower")       = categories("stog_X02","heatrate","upper");
categories("stog_X03","heatrate","upper")       = +inf;

categories("stog_X04","heatrate","lower")       = 0;
categories("stog_X04","heatrate","upper")       = 10500;
categories("stog_X05","heatrate","upper")       = 12000;

categories("stog_X05","heatrate","lower")       = categories("stog_X04","heatrate","upper");
categories("stog_X06","heatrate","lower")       = categories("stog_X05","heatrate","upper");
categories("stog_X06","heatrate","upper")       = +inf;


*- aggregated non-fossil units -*
capacity(r,"2010",u)$(not special(u))
        = sum(mapTypes(u,type)$(not special(u)),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010) )
          )))))))) / 1000;

capacity(r,"2015",u)$(not special(u))
        = sum(mapTypes(u,type)$(not special(u)),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") gt 2010) )
          )))))))) / 1000;

capacity("USA",v,u)     = sum(r, capacity(r,v,u));

htrate(r,"2010",u)$(capacity(r,"2010",u) and not special(u))
        = sum(mapTypes(u,type)$(not special(u)),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate")*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010) )
          ))))))))
                / (capacity(r,"2010",u)*1000);

htrate(r,"2015",u)$(capacity(r,"2015",u) and not special(u))
        = sum(mapTypes(u,type)$(not special(u)),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate")*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") gt 2010) )
          ))))))))
                / (capacity(r,"2015",u)*1000);

htrate("USA",v,u)$sum(r, capacity(r,v,u))
        = sum(r, capacity(r,v,u)*htrate(r,v,u)) / sum(r, capacity(r,v,u));


maxCF(r,"2010",u,season)$(capacity(r,"2010",u) and not special(u))
        = sum(mapTypes(u,type)$(not special(u)),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        avail(id,season)*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010) )
          ))))))))
                / (capacity(r,"2010",u)*1000);

maxCF(r,"2015",u,season)$(capacity(r,"2015",u) and not special(u))
        = sum(mapTypes(u,type)$(not special(u)),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        avail(id,season)*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") gt 2010) )
          ))))))))
                / (capacity(r,"2015",u)*1000);

data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"age")$data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online")      = 2011 - data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online");

count(r,u)$(not special(u))
        = sum(mapTypes(u,type),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum((id,r_ipm,fuels,nox,hg,hg_emf)$data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"count"),
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"count")$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010) )
          )));

count(r,u)$coal(u)
        = sum(mapTypes(u,type)$coal(u),
          sum(mapEquip(u,hg_emf,hg)$coal(u),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"count")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          )))))));

count(r,u)$gasoil(u)
        = sum(mapTypes(u,type)$gasoil(u),
                sum(mapRegions(r,st), sum(mapStates(st,state), sum(mapSCR(u,nox),
                sum(id, sum(r_ipm, sum(fuels, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"count")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          ))))))));

display count;

age(r,"2010",u)$(capacity(r,"2010",u) and not special(u))
        = sum(mapTypes(u,type)$(not special(u)),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"age")*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW"))$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010) )
          ))))))))
                / (capacity(r,"2010",u)*1000);

age("USA",v,u)$sum(r, capacity(r,v,u))
        = sum(r, capacity(r,v,u)*age(r,v,u)) / sum(r, capacity(r,v,u));

size(r,"2010",u)$(capacity(r,"2010",u) and not special(u))
        = sum(mapTypes(u,type)$(not special(u)),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010) )
          ))))))))
                / count(r,u);

size("USA",v,u)$sum(r, capacity(r,v,u))
        = sum(r, capacity(r,v,u)*size(r,v,u)) / sum(r, capacity(r,v,u));


*-- fossil units with additional disaggregation based on heatrates etc --*

capacity(r,"2010",u)$coal(u)
        = sum(mapTypes(u,type)$coal(u),
          sum(mapEquip(u,hg_emf,hg)$coal(u),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          ))))))) / 1000;


capacity(r,"2015",u)$coal(u)
        = sum(mapTypes(u,type)$coal(u),
          sum(mapEquip(u,hg_emf,hg)$coal(u),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") gt 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          ))))))) / 1000;


capacity(r,"2010",u)$gasoil(u)
        = sum(mapTypes(u,type)$gasoil(u),
                sum(mapRegions(r,st), sum(mapStates(st,state), sum(mapSCR(u,nox),
                sum(id, sum(r_ipm, sum(fuels, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          )))))))) / 1000;

capacity(r,"2015",u)$gasoil(u)
        = sum(mapTypes(u,type)$gasoil(u),
                sum(mapRegions(r,st), sum(mapStates(st,state), sum(mapSCR(u,nox),
                sum(id, sum(r_ipm, sum(fuels, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") gt 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          )))))))) / 1000;

capacity("USA",v,u)     = sum(r, capacity(r,v,u));
capacity(r,v,"total")   = sum(u, capacity(r,v,u));
capacity("USA",v,"total")=sum((r,u), capacity(r,v,u));


htrate(r,"2010",u)$(capacity(r,"2010",u) and coal(u))
        = sum(mapTypes(u,type)$coal(u),
          sum(mapEquip(u,hg_emf,hg)$coal(u),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate")*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          )))))))
                / (capacity(r,"2010",u)*1000);

htrate(r,"2015",u)$(capacity(r,"2015",u) and coal(u))
        = sum(mapTypes(u,type)$coal(u),
          sum(mapEquip(u,hg_emf,hg)$coal(u),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate")*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") gt 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          )))))))
                / (capacity(r,"2015",u)*1000);

htrate(r,"2010",u)$(capacity(r,"2010",u) and gasoil(u))
        = sum(mapTypes(u,type)$gasoil(u),
                sum(mapRegions(r,st), sum(mapStates(st,state), sum(mapSCR(u,nox),
                sum(id, sum(r_ipm, sum(fuels, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate")*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          ))))))))
                / (capacity(r,"2010",u)*1000);

htrate(r,"2015",u)$(capacity(r,"2015",u) and gasoil(u))
        = sum(mapTypes(u,type)$gasoil(u),
                sum(mapRegions(r,st), sum(mapStates(st,state), sum(mapSCR(u,nox),
                sum(id, sum(r_ipm, sum(fuels, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate")*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") gt 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          ))))))))
                / (capacity(r,"2015",u)*1000);

htrate("USA",v,u)$sum(r, capacity(r,v,u))
        = sum(r, capacity(r,v,u)*htrate(r,v,u)) / sum(r, capacity(r,v,u));


maxCF(r,"2010",u,season)$(capacity(r,"2010",u) and coal(u))
        = sum(mapTypes(u,type)$coal(u),
          sum(mapEquip(u,hg_emf,hg)$coal(u),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox,
                        avail(id,season)*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          ))))))) / (capacity(r,"2010",u)*1000);


maxCF(r,"2015",u,season)$(capacity(r,"2015",u) and coal(u))
        = sum(mapTypes(u,type)$coal(u),
          sum(mapEquip(u,hg_emf,hg)$coal(u),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox,
                        avail(id,season)*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") gt 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          ))))))) / (capacity(r,"2015",u)*1000);


maxCF(r,"2010",u,season)$(capacity(r,"2010",u) and gasoil(u))
        = sum(mapTypes(u,type)$gasoil(u),
                sum(mapRegions(r,st), sum(mapStates(st,state), sum(mapSCR(u,nox),
                sum(id, sum(r_ipm, sum(fuels, sum(hg, sum(hg_emf,
                        avail(id,season)*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          )))))))) / (capacity(r,"2010",u)*1000);

maxCF(r,"2015",u,season)$(capacity(r,"2015",u) and gasoil(u))
        = sum(mapTypes(u,type)$gasoil(u),
                sum(mapRegions(r,st), sum(mapStates(st,state), sum(mapSCR(u,nox),
                sum(id, sum(r_ipm, sum(fuels, sum(hg, sum(hg_emf,
                        avail(id,season)*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") gt 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          )))))))) / (capacity(r,"2015",u)*1000);



set hyd(u) /hyd_X/;

maxCF(r,"2010",u,season)$(capacity(r,"2010",u) and hyd(u))
        = sum(mapTypes(u,type),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        hydroavail(r_ipm,season)*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010) )
          ))))))))
                / (capacity(r,"2010",u)*1000);



option maxCF:2:3:1;
display maxCF;

age(r,"2010",u)$(capacity(r,"2010",u) and coal(u))
        = sum(mapTypes(u,type)$coal(u),
          sum(mapEquip(u,hg_emf,hg)$coal(u),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox,
                        (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"age")*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW"))$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          )))))))
                / (capacity(r,"2010",u)*1000);

age(r,"2010",u)$(capacity(r,"2010",u) and gasoil(u))
        = sum(mapTypes(u,type)$gasoil(u),
                sum(mapRegions(r,st), sum(mapStates(st,state), sum(mapSCR(u,nox),
                sum(id, sum(r_ipm, sum(fuels, sum(hg, sum(hg_emf,
                        (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"age")*data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW"))$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          ))))))))
                / (capacity(r,"2010",u)*1000);

size(r,"2010",u)$(capacity(r,"2010",u) and coal(u))
        = sum(mapTypes(u,type)$coal(u),
          sum(mapEquip(u,hg_emf,hg)$coal(u),
                sum(mapRegions(r,st), sum(mapStates(st,state),
                sum(id, sum(r_ipm, sum(fuels, sum(nox,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          )))))))
                / count(r,u);

size(r,"2010",u)$(capacity(r,"2010",u) and gasoil(u))
        = sum(mapTypes(u,type)$gasoil(u),
                sum(mapRegions(r,st), sum(mapStates(st,state), sum(mapSCR(u,nox),
                sum(id, sum(r_ipm, sum(fuels, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW")$(
                                (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"online") le 2010)
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") ge categories(u,"heatrate","lower"))
                                and (data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"heatrate") lt categories(u,"heatrate","upper"))
                        ))
          ))))))))
                / count(r,u);

display age,size;

option capacity:2:2:1;
option htrate:0:2:1;
display capacity,htrate;

parameter chk_cap;

chk_cap(r,"coal") = sum(t, sum(u$coal(u), capacity(r,t,u)));
chk_cap("USA","coal") = sum(r, chk_cap(r,"coal"));

display chk_cap;

* save solar capacity for weights
parameter slr_capacity;

slr_capacity(r_ipm)
        = sum(u, sum(mapTypes(u,type)$slr_u(u),
                sum(id, sum(state, sum(fuels, sum(nox, sum(hg, sum(hg_emf,
                        data(id,type,r_ipm,state,fuels,nox,hg,hg_emf,"MW") )
          )))))));

slr_capacity(r_ipm)$(slr_capacity(r_ipm) eq 0) = 0.1;

option slr_capacity:1:0:1;
display slr_capacity;

execute_unload 'output\needs_data.gdx', capacity, htrate, age, size, count, maxCF, RsrvMargin, slr_capacity;

