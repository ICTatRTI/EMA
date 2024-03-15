$title Data aggregation and labeling

$oneolcom
$eolcom !

set     t               time periods    / 2010,2015,2020,2025,2030,2035,2040,2045,2050,2075 /;

set     postAEO(t)      after 2035      / 2040,2045,2050,2075/
        post2030(t)                     / 2035,2040,2045,2050,2075/ ;

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

set     mapIPM(r_ipm,r) /
        AZNM.MTN
        CA-N.CA
        CA-S.CA
        COMD.ENC
        DSNY.NY
        ENTG.WSC
        ERCT.TX
        FRCC.FL
        GWAY.ENC
        LILC.NY
        MACE.MID
        MACS.SAC
        MACW.MID
        MECS.ENC
        MRO.WNC
        NENG.NEG
        NWPE.MTN
        NYC.NY
        PNW.PAC
        RFCO.ENC
        RFCP.SAC
        RMPA.MTN
        SNV.MTN
        SOU.SAC
        SPPN.WNC
        SPPS.WSC
        TVA.ESC
        TVAK.ESC
        UPNY.NY
        VACA.SAC
        VAPW.SAC
        WUMS.ENC
/;

set     st      State code (without US) /
        AK, AL, AR, AZ, CA, CO, CT, DC, DE, FL, GA, HI, IA, ID, IL, IN,
        KS, KY, LA, MA, MD, ME, MI, MN, MO, MS, MT, NE, NC, ND, NH, NJ, NM,
        NV, NY, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VA, VT, WA, WI, WV, WY/;


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

        ngcc_X1         existing natural gas combined cycle class 1 - low heatrate
        ngcc_X2         existing natural gas combined cycle class 2 - medium heatrate
        ngcc_X3         existing natural gas combined cycle class 3 - high heatrate

        ngtb_X1         existing natural gas turbine class 1 - low heatrate
        ngtb_X2         existing natural gas turbine class 2 - medium heatrate
        ngtb_X3         existing natural gas turbine class 3 - high heatrate

        stog_X1         existing steam oil-gas class 1 - low heatrate
        stog_X2         existing steam oil-gas class 2 - medium heatrate
        stog_X3         existing steam oil-gas class 3 - high heatrate

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
        bio_N           new biomass
        lfg_N_hi        new landfill gas - high flow rate
        lfg_N_lo        new landfill gas - low flow rate
        lfg_N_vl        new landfill gas - very low flow rate
        fc_N            new fuel cell
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
        wnd_Ndeep_7     new wind - offshore deep - wind class 7 /;


set     u_lbl           units   /
        col_X01*col_X180
        ngcc_X01*ngcc_X06
        ngtb_X01*ngtb_x06
        stog_x01*stog_x06

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
        bio_N           new biomass
        lfg_N_hi        new landfill gas - high flow rate
        lfg_N_lo        new landfill gas - low flow rate
        lfg_N_vl        new landfill gas - very low flow rate
        fc_N            new fuel cell
        slrPV_N         new solar photovoltaic
        slrCSP_N        new concentrating solar power

        geo_N_1,geo_N_2,geo_N_3,geo_N_4,geo_N_5,geo_N_6,geo_N_7,geo_N_8,geo_N_9,geo_N_10,

        wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7/;

set     ccsu(u)         /col_Nccs,ngcc_Nccs/;

set     special_u(u_lbl) /
        col_X01*col_X180
        ngcc_X01*ngcc_X06
        ngtb_X01*ngtb_x06
        stog_x01*stog_x06
/;

set     col_u(u_lbl)    coal units      / col_X01*col_X180 /
        ngcc_u(u_lbl)                   / ngcc_X01*ngcc_X06 /
        ngcc_u13(u_lbl)                 / ngcc_X01*ngcc_X03 /
        ngcc_u46(u_lbl)                 / ngcc_X04*ngcc_X06 /
        ngtb_u(u_lbl)                   / ngtb_X01*ngtb_X06 /
        ngtb_u13(u_lbl)                 / ngtb_X01*ngtb_X03 /
        ngtb_u46(u_lbl)                 / ngtb_X04*ngtb_X06 /
        stog_u(u_lbl)                   / stog_X01*stog_X06 /
        stog_u13(stog_u)                / stog_X01*stog_X03 /
        stog_u46(stog_u)                / stog_X04*stog_X06 /;

set     fgd             / wet,dry,none /
        nox             / scr,sncr,none /
        hg              / aci,none /;

set     mapEquip(col_u,fgd,nox,hg) /
        col_X01.none.sncr.none                  ! ("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("")
        col_X02.wet.sncr.none                   ! ("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("")
        col_X03.dry.sncr.none                   ! ("Cold-side ESP + SNCR + Dry Scrubber").("")
        col_X04.none.scr.none                   ! ("Cold-side ESP + SCR").("")
        col_X05.wet.scr.none                    ! ("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("")
        col_X06.dry.scr.none                    ! ("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("")
        col_X07.wet.none.none                   ! ("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("")
        col_X08.dry.none.none                   ! ("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("")
        col_X09.none.none.none                  ! ("Cold-side ESP","Cold-side ESP + Cyclone").("")

        col_X10.none.sncr.none                  ! ("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("")
        col_X11.wet.sncr.none                   ! ("Fabric Filter + SNCR + Wet Scrubber").("")
        col_X12.dry.sncr.none                   ! ("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("")
        col_X13.none.scr.none                   ! ("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("")
        col_X14.wet.scr.none                    ! ("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("")
        col_X15.dry.scr.none                    ! ("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("")
        col_X16.wet.none.none                   ! ("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("")
        col_X17.dry.none.none                   ! ("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("")
        col_X18.none.none.none                  ! ("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("")

        col_X19.none.sncr.none                  ! ("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("")
        col_X20.wet.sncr.none                   ! ("Hot-side ESP + SNCR + Wet Scrubber").("")
        col_X21.none.scr.none                   ! ("Hot-side ESP + SCR").("")
        col_X22.wet.scr.none                    ! ("Hot-side ESP + SCR + Wet Scrubber").("")
        col_X23.dry.scr.none                    ! ("Hot-side ESP + SCR + Dry Scrubber").("")
        col_X24.wet.none.none                   ! ("Hot-side ESP + Wet Scrubber").("")
        col_X25.none.none.none                  ! ("Hot-side ESP","Hot-side ESP + Cyclone").("")

        col_X26.none.sncr.none                  ! ("SNCR","Cyclone","").("")
        col_X27.wet.sncr.none                   ! ("SNCR + Wet Scrubber","PM Scrubber + SNCR").("")
        col_X28.none.scr.none                   ! ("SCR").("")
        col_X29.wet.scr.none                    ! ("SCR + Wet Scrubber").("")
        col_X30.wet.none.none                   ! ("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("")


        col_X31.none.sncr.aci                   ! ("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("ACI")
        col_X32.wet.sncr.aci                    ! ("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("ACI")
        col_X33.dry.sncr.aci                    ! ("Cold-side ESP + SNCR + Dry Scrubber").("ACI")
        col_X34.none.scr.aci                    ! ("Cold-side ESP + SCR").("ACI")
        col_X35.wet.scr.aci                     ! ("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("ACI")
        col_X36.dry.scr.aci                     ! ("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("ACI")
        col_X37.wet.none.aci                    ! ("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("ACI")
        col_X38.dry.none.aci                    ! ("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("ACI")
        col_X39.none.none.aci                   ! ("Cold-side ESP","Cold-side ESP + Cyclone").("ACI")

        col_X40.none.sncr.aci                   ! ("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("ACI")
        col_X41.wet.sncr.aci                    ! ("Fabric Filter + SNCR + Wet Scrubber").("ACI")
        col_X42.dry.sncr.aci                    ! ("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("ACI")
        col_X43.none.scr.aci                    ! ("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("ACI")
        col_X44.wet.scr.aci                     ! ("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("ACI")
        col_X45.dry.scr.aci                     ! ("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("ACI")
        col_X46.wet.none.aci                    ! ("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("ACI")
        col_X47.dry.none.aci                    ! ("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("ACI")
        col_X48.none.none.aci                   ! ("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("ACI")

        col_X49.none.sncr.aci                   ! ("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("ACI")
        col_X50.wet.sncr.aci                    ! ("Hot-side ESP + SNCR + Wet Scrubber").("ACI")
        col_X51.none.scr.aci                    ! ("Hot-side ESP + SCR").("ACI")
        col_X52.wet.scr.aci                     ! ("Hot-side ESP + SCR + Wet Scrubber").("ACI")
        col_X53.dry.scr.aci                     ! ("Hot-side ESP + SCR + Dry Scrubber").("ACI")
        col_X54.wet.none.aci                    ! ("Hot-side ESP + Wet Scrubber").("ACI")
        col_X55.none.none.aci                   ! ("Hot-side ESP","Hot-side ESP + Cyclone").("ACI")

        col_X56.none.sncr.aci                   ! ("SNCR","Cyclone","").("ACI")
        col_X57.wet.sncr.aci                    ! ("SNCR + Wet Scrubber","PM Scrubber + SNCR").("ACI")
        col_X58.none.scr.aci                    ! ("SCR").("ACI")
        col_X59.wet.scr.aci                     ! ("SCR + Wet Scrubber").("ACI")
        col_X60.wet.none.aci                    ! ("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("ACI")


        col_X61.none.sncr.none                  ! ("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("")
        col_X62.wet.sncr.none                   ! ("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("")
        col_X63.dry.sncr.none                   ! ("Cold-side ESP + SNCR + Dry Scrubber").("")
        col_X64.none.scr.none                   ! ("Cold-side ESP + SCR").("")
        col_X65.wet.scr.none                    ! ("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("")
        col_X66.dry.scr.none                    ! ("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("")
        col_X67.wet.none.none                   ! ("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("")
        col_X68.dry.none.none                   ! ("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("")
        col_X69.none.none.none                  ! ("Cold-side ESP","Cold-side ESP + Cyclone").("")

        col_X70.none.sncr.none                  ! ("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("")
        col_X71.wet.sncr.none                   ! ("Fabric Filter + SNCR + Wet Scrubber").("")
        col_X72.dry.sncr.none                   ! ("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("")
        col_X73.none.scr.none                   ! ("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("")
        col_X74.wet.scr.none                    ! ("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("")
        col_X75.dry.scr.none                    ! ("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("")
        col_X76.wet.none.none                   ! ("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("")
        col_X77.dry.none.none                   ! ("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("")
        col_X78.none.none.none                  ! ("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("")

        col_X79.none.sncr.none                  ! ("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("")
        col_X80.wet.sncr.none                   ! ("Hot-side ESP + SNCR + Wet Scrubber").("")
        col_X81.none.scr.none                   ! ("Hot-side ESP + SCR").("")
        col_X82.wet.scr.none                    ! ("Hot-side ESP + SCR + Wet Scrubber").("")
        col_X83.dry.scr.none                    ! ("Hot-side ESP + SCR + Dry Scrubber").("")
        col_X84.wet.none.none                   ! ("Hot-side ESP + Wet Scrubber").("")
        col_X85.none.none.none                  ! ("Hot-side ESP","Hot-side ESP + Cyclone").("")

        col_X86.none.sncr.none                  ! ("SNCR","Cyclone","").("")
        col_X87.wet.sncr.none                   ! ("SNCR + Wet Scrubber","PM Scrubber + SNCR").("")
        col_X88.none.scr.none                   ! ("SCR").("")
        col_X89.wet.scr.none                    ! ("SCR + Wet Scrubber").("")
        col_X90.wet.none.none                   ! ("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("")


        col_X91.none.sncr.aci                   ! ("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("ACI")
        col_X92.wet.sncr.aci                   ! ("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("ACI")
        col_X93.dry.sncr.aci                   ! ("Cold-side ESP + SNCR + Dry Scrubber").("ACI")
        col_X94.none.scr.aci                   ! ("Cold-side ESP + SCR").("ACI")
        col_X95.wet.scr.aci                     ! ("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("ACI")
        col_X96.dry.scr.aci                     ! ("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("ACI")
        col_X97.wet.none.aci                   ! ("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("ACI")
        col_X98.dry.none.aci                   ! ("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("ACI")
        col_X99.none.none.aci                   ! ("Cold-side ESP","Cold-side ESP + Cyclone").("ACI")

        col_X100.none.sncr.aci                  ! ("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("ACI")
        col_X101.wet.sncr.aci                   ! ("Fabric Filter + SNCR + Wet Scrubber").("ACI")
        col_X102.dry.sncr.aci                   ! ("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("ACI")
        col_X103.none.scr.aci                   ! ("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("ACI")
        col_X104.wet.scr.aci                    ! ("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("ACI")
        col_X105.dry.scr.aci                    ! ("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("ACI")
        col_X106.wet.none.aci                   ! ("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("ACI")
        col_X107.dry.none.aci                   ! ("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("ACI")
        col_X108.none.none.aci                  ! ("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("ACI")

        col_X109.none.sncr.aci                  ! ("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("ACI")
        col_X110.wet.sncr.aci                   ! ("Hot-side ESP + SNCR + Wet Scrubber").("ACI")
        col_X111.none.scr.aci                   ! ("Hot-side ESP + SCR").("ACI")
        col_X112.wet.scr.aci                    ! ("Hot-side ESP + SCR + Wet Scrubber").("ACI")
        col_X113.dry.scr.aci                    ! ("Hot-side ESP + SCR + Dry Scrubber").("ACI")
        col_X114.wet.none.aci                   ! ("Hot-side ESP + Wet Scrubber").("ACI")
        col_X115.none.none.aci                  ! ("Hot-side ESP","Hot-side ESP + Cyclone").("ACI")

        col_X116.none.sncr.aci                  ! ("SNCR","Cyclone","").("ACI")
        col_X117.wet.sncr.aci                   ! ("SNCR + Wet Scrubber","PM Scrubber + SNCR").("ACI")
        col_X118.none.scr.aci                   ! ("SCR").("ACI")
        col_X119.wet.scr.aci                    ! ("SCR + Wet Scrubber").("ACI")
        col_X120.wet.none.aci                   ! ("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("ACI")


        col_X121.none.sncr.none                 ! ("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("")
        col_X122.wet.sncr.none                  ! ("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("")
        col_X123.dry.sncr.none                  ! ("Cold-side ESP + SNCR + Dry Scrubber").("")
        col_X124.none.scr.none                  ! ("Cold-side ESP + SCR").("")
        col_X125.wet.scr.none                   ! ("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("")
        col_X126.dry.scr.none                   ! ("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("")
        col_X127.wet.none.none                  ! ("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("")
        col_X128.dry.none.none                  ! ("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("")
        col_X129.none.none.none                 ! ("Cold-side ESP","Cold-side ESP + Cyclone").("")

        col_X130.none.sncr.none                 ! ("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("")
        col_X131.wet.sncr.none                  ! ("Fabric Filter + SNCR + Wet Scrubber").("")
        col_X132.dry.sncr.none                  ! ("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("")
        col_X133.none.scr.none                  ! ("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("")
        col_X134.wet.scr.none                   ! ("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("")
        col_X135.dry.scr.none                   ! ("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("")
        col_X136.wet.none.none                  ! ("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("")
        col_X137.dry.none.none                  ! ("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("")
        col_X138.none.none.none                 ! ("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("")

        col_X139.none.sncr.none                 ! ("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("")
        col_X140.wet.sncr.none                  ! ("Hot-side ESP + SNCR + Wet Scrubber").("")
        col_X141.none.scr.none                  ! ("Hot-side ESP + SCR").("")
        col_X142.wet.scr.none                   ! ("Hot-side ESP + SCR + Wet Scrubber").("")
        col_X143.dry.scr.none                   ! ("Hot-side ESP + SCR + Dry Scrubber").("")
        col_X144.wet.none.none                  ! ("Hot-side ESP + Wet Scrubber").("")
        col_X145.none.none.none                 ! ("Hot-side ESP","Hot-side ESP + Cyclone").("")

        col_X146.none.sncr.none                 ! ("SNCR","Cyclone","").("")
        col_X147.wet.sncr.none                  ! ("SNCR + Wet Scrubber","PM Scrubber + SNCR").("")
        col_X148.none.scr.none                  ! ("SCR").("")
        col_X149.wet.scr.none                   ! ("SCR + Wet Scrubber").("")
        col_X150.wet.none.none                  ! ("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").(,,)  ! ("")


        col_X151.none.sncr.aci                  ! ("Cold-side ESP + Cyclone + SNCR","Cold-side ESP + SNCR","Cold-side ESP + Hot-side ESP + SNCR").("ACI")
        col_X152.wet.sncr.aci                   ! ("Cold-side ESP + SNCR + Wet Scrubber","Cold-side ESP + Cyclone + SNCR + Wet Scrubber").("ACI")
        col_X153.dry.sncr.aci                   ! ("Cold-side ESP + SNCR + Dry Scrubber").("ACI")
        col_X154.none.scr.aci                   ! ("Cold-side ESP + SCR").("ACI")
        col_X155.wet.scr.aci                    ! ("Cold-side ESP + SCR + Wet Scrubber","Cold-side ESP + Cyclone + SCR + Wet Scrubber","Cold-side ESP + PM Scrubber + SCR").("ACI")
        col_X156.dry.scr.aci                    ! ("Cold-side ESP + SCR + Dry Scrubber","Cold-side ESP + Cyclone + SCR + Dry Scrubber").("ACI")
        col_X157.wet.none.aci                   ! ("Cold-side ESP + Wet Scrubber","Cold-side ESP + PM Scrubber").("ACI")
        col_X158.dry.none.aci                   ! ("Cold-side ESP + Dry Scrubber","Cold-side ESP + Hot-side ESP + Dry Scrubber").("ACI")
        col_X159.none.none.aci                  ! ("Cold-side ESP","Cold-side ESP + Cyclone").("ACI")

        col_X160.none.sncr.aci                  ! ("Cold-side ESP + Fabric Filter + SNCR","Fabric Filter + SNCR","Hot-side ESP + Fabric Filter + SNCR","Fabric Filter + Cyclone + SNCR").("ACI")
        col_X161.wet.sncr.aci                   ! ("Fabric Filter + SNCR + Wet Scrubber").("ACI")
        col_X162.dry.sncr.aci                   ! ("Cold-side ESP + Fabric Filter + SNCR + Dry Scrubber","Fabric Filter + SNCR + Dry Scrubber").("ACI")
        col_X163.none.scr.aci                   ! ("Cold-side ESP + Fabric Filter + SCR","Hot-side ESP + Fabric Filter + SCR").("ACI")
        col_X164.wet.scr.aci                    ! ("Cold-side ESP + Fabric Filter + SCR + Wet Scrubber","Fabric Filter + SCR + Wet Scrubber","Hot-side ESP + Fabric Filter + SCR + Wet Scrubber").("ACI")
        col_X165.dry.scr.aci                    ! ("Cold-side ESP + Fabric Filter + SCR + Dry Scrubber","Fabric Filter + SCR + Dry Scrubber").("ACI")
        col_X166.wet.none.aci                   ! ("Cold-side ESP + Fabric Filter + Wet Scrubber","Fabric Filter + Wet Scrubber","Hot-side ESP + Fabric Filter + Wet Scrubber").("ACI")
        col_X167.dry.none.aci                   ! ("Fabric Filter + Cyclone + Dry Scrubber","Fabric Filter + Dry Scrubber").("ACI")
        col_X168.none.none.aci                  ! ("Cold-side ESP + Fabric Filter","Fabric Filter","Fabric Filter + Cyclone","Hot-side ESP + Fabric Filter").("ACI")

        col_X169.none.sncr.aci                  ! ("Hot-side ESP + SNCR","Hot-side ESP + Cyclone + SNCR").("ACI")
        col_X170.wet.sncr.aci                   ! ("Hot-side ESP + SNCR + Wet Scrubber").("ACI")
        col_X171.none.scr.aci                   ! ("Hot-side ESP + SCR").("ACI")
        col_X172.wet.scr.aci                    ! ("Hot-side ESP + SCR + Wet Scrubber").("ACI")
        col_X173.dry.scr.aci                    ! ("Hot-side ESP + SCR + Dry Scrubber").("ACI")
        col_X174.wet.none.aci                   ! ("Hot-side ESP + Wet Scrubber").("ACI")
        col_X175.none.none.aci                  ! ("Hot-side ESP","Hot-side ESP + Cyclone").("ACI")

        col_X176.none.sncr.aci                  ! ("SNCR","Cyclone","").("ACI")
        col_X177.wet.sncr.aci                   ! ("SNCR + Wet Scrubber","PM Scrubber + SNCR").("ACI")
        col_X178.none.scr.aci                   ! ("SCR").("ACI")
        col_X179.wet.scr.aci                    ! ("SCR + Wet Scrubber").("ACI")
        col_X180.wet.none.aci                   ! ("PM Scrubber","Wet Scrubber","Cyclone + Wet Scrubber").("ACI")
/;

set     mapUnits(u_lbl,u) /
        col_X01.col_X1
        col_X02.col_X1
        col_X03.col_X1
        col_X04.col_X1
        col_X05.col_X1
        col_X06.col_X1
        col_X07.col_X1
        col_X08.col_X1
        col_X09.col_X1

        col_X10 .col_X1
        col_X11 .col_X1
        col_X12 .col_X1
        col_X13 .col_X1
        col_X14 .col_X1
        col_X15 .col_X1
        col_X16 .col_X1
        col_X17 .col_X1
        col_X18 .col_X1

        col_X19 .col_X1
        col_X20 .col_X1
        col_X21 .col_X1
        col_X22 .col_X1
        col_X23 .col_X1
        col_X24 .col_X1
        col_X25 .col_X1

        col_X26 .col_X1
        col_X27 .col_X1
        col_X28 .col_X1
        col_X29 .col_X1
        col_X30 .col_X1

        col_X31 .col_X1
        col_X32 .col_X1
        col_X33 .col_X1
        col_X34 .col_X1
        col_X35 .col_X1
        col_X36 .col_X1
        col_X37 .col_X1
        col_X38 .col_X1
        col_X39 .col_X1

        col_X40 .col_X1
        col_X41 .col_X1
        col_X42 .col_X1
        col_X43 .col_X1
        col_X44 .col_X1
        col_X45 .col_X1
        col_X46 .col_X1
        col_X47 .col_X1
        col_X48 .col_X1

        col_X49 .col_X1
        col_X50 .col_X1
        col_X51 .col_X1
        col_X52 .col_X1
        col_X53 .col_X1
        col_X54 .col_X1
        col_X55 .col_X1

        col_X56 .col_X1
        col_X57 .col_X1
        col_X58 .col_X1
        col_X59 .col_X1
        col_X60 .col_X1



        col_X61.col_X2
        col_X62.col_X2
        col_X63.col_X2
        col_X64.col_X2
        col_X65.col_X2
        col_X66.col_X2
        col_X67.col_X2
        col_X68.col_X2
        col_X69.col_X2

        col_X70 .col_X2
        col_X71 .col_X2
        col_X72 .col_X2
        col_X73 .col_X2
        col_X74 .col_X2
        col_X75 .col_X2
        col_X76 .col_X2
        col_X77 .col_X2
        col_X78 .col_X2

        col_X79 .col_X2
        col_X80 .col_X2
        col_X81 .col_X2
        col_X82 .col_X2
        col_X83 .col_X2
        col_X84 .col_X2
        col_X85 .col_X2

        col_X86 .col_X2
        col_X87 .col_X2
        col_X88 .col_X2
        col_X89 .col_X2
        col_X90 .col_X2

        col_X91 .col_X2
        col_X92 .col_X2
        col_X93 .col_X2
        col_X94 .col_X2
        col_X95 .col_X2
        col_X96 .col_X2
        col_X97 .col_X2
        col_X98 .col_X2
        col_X99 .col_X2

        col_X100 .col_X2
        col_X101 .col_X2
        col_X102 .col_X2
        col_X103 .col_X2
        col_X104 .col_X2
        col_X105 .col_X2
        col_X106 .col_X2
        col_X107 .col_X2
        col_X108 .col_X2

        col_X109 .col_X2
        col_X110 .col_X2
        col_X111 .col_X2
        col_X112 .col_X2
        col_X113 .col_X2
        col_X114 .col_X2
        col_X115 .col_X2

        col_X116 .col_X2
        col_X117 .col_X2
        col_X118 .col_X2
        col_X119 .col_X2
        col_X120 .col_X2



        col_X121.col_X3
        col_X122.col_X3
        col_X123.col_X3
        col_X124.col_X3
        col_X125.col_X3
        col_X126.col_X3
        col_X127.col_X3
        col_X128.col_X3
        col_X129.col_X3

        col_X130 .col_X3
        col_X131 .col_X3
        col_X132 .col_X3
        col_X133 .col_X3
        col_X134 .col_X3
        col_X135 .col_X3
        col_X136 .col_X3
        col_X137 .col_X3
        col_X138 .col_X3

        col_X139 .col_X3
        col_X140 .col_X3
        col_X141 .col_X3
        col_X142 .col_X3
        col_X143 .col_X3
        col_X144 .col_X3
        col_X145 .col_X3

        col_X146 .col_X3
        col_X147 .col_X3
        col_X148 .col_X3
        col_X149 .col_X3
        col_X150 .col_X3

        col_X151 .col_X3
        col_X152 .col_X3
        col_X153 .col_X3
        col_X154 .col_X3
        col_X155 .col_X3
        col_X156 .col_X3
        col_X157 .col_X3
        col_X158 .col_X3
        col_X159 .col_X3

        col_X160 .col_X3
        col_X161 .col_X3
        col_X162 .col_X3
        col_X163 .col_X3
        col_X164 .col_X3
        col_X165 .col_X3
        col_X166 .col_X3
        col_X167 .col_X3
        col_X168 .col_X3

        col_X169 .col_X3
        col_X170 .col_X3
        col_X171 .col_X3
        col_X172 .col_X3
        col_X173 .col_X3
        col_X174 .col_X3
        col_X175 .col_X3

        col_X176 .col_X3
        col_X177 .col_X3
        col_X178 .col_X3
        col_X179 .col_X3
        col_X180 .col_X3

        ngcc_X01.ngcc_X1
        ngcc_X02.ngcc_X2
        ngcc_X03.ngcc_X3
        ngcc_X04.ngcc_X1
        ngcc_X05.ngcc_X2
        ngcc_X06.ngcc_X3

        ngtb_X01.ngtb_X1
        ngtb_X02.ngtb_X2
        ngtb_X03.ngtb_X3
        ngtb_X04.ngtb_X1
        ngtb_X05.ngtb_X2
        ngtb_X06.ngtb_X3

        stog_X01.stog_X1
        stog_X02.stog_X2
        stog_X03.stog_X3
        stog_X04.stog_X1
        stog_X05.stog_X2
        stog_X06.stog_X3
/;



* NEEDS data and capacity factors
parameter
        capacity                        NEEDS capacity (GW)
        htrate
        age
        size
        count
        maxCF
        RsrvMargin
        slr_capacity
;

$gdxin 'output\needs_data.gdx'
$load capacity htrate age size count maxCF RsrvMargin slr_capacity

* Operating cost data
parameter
        capcost
        fomcost
        vomcost
        heatrate_n
        fom_costs
        vom_costs
        vom_cgo_costs
        newavail
        regional_costs;

$gdxin 'output\cost_data.gdx'
$load capcost fomcost vomcost fom_costs vom_costs vom_cgo_costs heatrate_n newavail regional_costs

* Load curves
parameter
        load
        load_pct
        hours
        peak
        transmit_lmt;

$gdxin 'output\load_curve_data.gdx'
$load load load_pct hours peak transmit_lmt


* add FOM and VOM to equip/age specific categories (coal and steam oil/gas)
set     ages /
        20      "0 to 20 years"
        30      "20 to 30 years"
        40      "30 to 40 years"
        100     "greater than 40 years" /;

parameter
        col_ages(r,col_u,ages)
        ngtb_ages(r,ngtb_u,ages)
        stog_ages(r,stog_u,ages)
        chk_fom;

col_ages(r,col_u,"20")$(age(r,"2010",col_u) le 20)                                      = yes;
col_ages(r,col_u,"30")$((age(r,"2010",col_u) gt 20) and (age(r,"2010",col_u) le 30))    = yes;
col_ages(r,col_u,"40")$((age(r,"2010",col_u) gt 30) and (age(r,"2010",col_u) le 40))    = yes;
col_ages(r,col_u,"100")$(age(r,"2010",col_u) gt 40)                                     = yes;

ngtb_ages(r,ngtb_u,"20")$(age(r,"2010",ngtb_u) le 20)                                   = yes;
ngtb_ages(r,ngtb_u,"30")$((age(r,"2010",ngtb_u) gt 20) and (age(r,"2010",ngtb_u) le 30))= yes;
ngtb_ages(r,ngtb_u,"40")$((age(r,"2010",ngtb_u) gt 30) and (age(r,"2010",ngtb_u) le 40))= yes;
ngtb_ages(r,ngtb_u,"100")$(age(r,"2010",ngtb_u) gt 40)                                  = yes;

stog_ages(r,stog_u,"20")$(age(r,"2010",stog_u) le 20)                                   = yes;
stog_ages(r,stog_u,"30")$((age(r,"2010",stog_u) gt 20) and (age(r,"2010",stog_u) le 30))= yes;
stog_ages(r,stog_u,"40")$((age(r,"2010",stog_u) gt 30) and (age(r,"2010",stog_u) le 40))= yes;
stog_ages(r,stog_u,"100")$(age(r,"2010",stog_u) gt 40)                                  = yes;


fomcost(r,col_u)$capacity(r,"2010",col_u)       = sum(mapEquip(col_u,fgd,nox,hg), sum(ages, col_ages(r,col_u,ages) * fom_costs("col_x",fgd,hg,nox,ages,"fom")));
fomcost(r,ngtb_u)$capacity(r,"2010",ngtb_u)     = sum(ages, ngtb_ages(r,ngtb_u,ages) * fom_costs("ngtb_x","x","x","x",ages,"fom"));
fomcost(r,ngcc_u)$capacity(r,"2010",ngcc_u)     = fom_costs("ngcc_x","x","x","x","all","fom");


fomcost(r,stog_u13)$capacity(r,"2010",stog_u13) = sum(ages, stog_ages(r,stog_u13,ages) * fom_costs("stog_x","none","none","none",ages,"fom"));
fomcost(r,stog_u46)$capacity(r,"2010",stog_u46) = sum(ages, stog_ages(r,stog_u46,ages) * fom_costs("stog_x","none","none","SCR",ages,"fom"));

loop((r,col_u),
abort$((capacity(r,"2010",col_u) gt 0) and (fomcost(r,col_u) eq 0)) "missing FOM costs";
);

display age,fomcost;

* add coal, gas, oil VOM
vom_cgo_costs("col_X",fgd,hg,nox,"wgtavg")      = (vom_cgo_costs("col_X",fgd,hg,nox,"vom_low") + vom_cgo_costs("col_X",fgd,hg,nox,"vom_high")) / 2;
vom_cgo_costs("ngcc_X",fgd,hg,nox,"wgtavg")     = (3*vom_cgo_costs("ngcc_X",fgd,hg,nox,"vom_low") + vom_cgo_costs("ngcc_X",fgd,hg,nox,"vom_high")) / 4;
vom_cgo_costs("ngtb_X",fgd,hg,nox,"wgtavg")     = (3*vom_cgo_costs("ngtb_X",fgd,hg,nox,"vom_low") + vom_cgo_costs("ngtb_X",fgd,hg,nox,"vom_high")) / 4;
vom_cgo_costs("stog_X",fgd,hg,nox,"wgtavg")     = (3*vom_cgo_costs("stog_X",fgd,hg,nox,"vom_low") + vom_cgo_costs("stog_X",fgd,hg,nox,"vom_high")) / 4;


vomcost(r,col_u)$capacity(r,"2010",col_u)       = sum(mapEquip(col_u,fgd,nox,hg), vom_cgo_costs("col_x",fgd,hg,nox,"wgtavg"));
vomcost(r,ngcc_u13)$capacity(r,"2010",ngcc_u13) = vom_cgo_costs("ngcc_x","none","none","none","wgtavg");
vomcost(r,ngcc_u46)$capacity(r,"2010",ngcc_u46) = vom_cgo_costs("ngcc_x","none","none","SCR","wgtavg");
vomcost(r,ngtb_u13)$capacity(r,"2010",ngtb_u13) = vom_cgo_costs("ngtb_x","none","none","none","wgtavg");
vomcost(r,ngtb_u46)$capacity(r,"2010",ngtb_u46) = vom_cgo_costs("ngtb_x","none","none","SCR","wgtavg");
vomcost(r,stog_u13)$capacity(r,"2010",stog_u13) = vom_cgo_costs("stog_x","none","none","none","wgtavg");
vomcost(r,stog_u46)$capacity(r,"2010",stog_u46) = vom_cgo_costs("stog_x","none","none","SCR","wgtavg");


**-- aggregate units --**

set     agg_u(u) /
        col_X1          existing coal class 1 (low heatrate)
        col_X2          existing coal class 2 (medium heatrate)
        col_X3          existing coal class 3 (high heatrate)

        ngcc_X1         existing natural gas combined cycle class 1 - low heatrate
        ngcc_X2         existing natural gas combined cycle class 2 - medium heatrate
        ngcc_X3         existing natural gas combined cycle class 3 - high heatrate

        ngtb_X1         existing natural gas turbine class 1 - low heatrate
        ngtb_X2         existing natural gas turbine class 2 - medium heatrate
        ngtb_X3         existing natural gas turbine class 3 - high heatrate

        stog_X1         existing steam oil-gas class 1 - low heatrate
        stog_X2         existing steam oil-gas class 2 - medium heatrate
        stog_X3         existing steam oil-gas class 3 - high heatrate
/;


vomcost(r,agg_u)$sum(mapUnits(u_lbl,agg_u), capacity(r,"2010",u_lbl))
        = sum(mapUnits(u_lbl,agg_u), vomcost(r,u_lbl) * capacity(r,"2010",u_lbl))
                / sum(mapUnits(u_lbl,agg_u), capacity(r,"2010",u_lbl));

fomcost(r,agg_u)$sum(mapUnits(u_lbl,agg_u), capacity(r,"2010",u_lbl))
        = sum(mapUnits(u_lbl,agg_u), fomcost(r,u_lbl) * capacity(r,"2010",u_lbl))
                / sum(mapUnits(u_lbl,agg_u), capacity(r,"2010",u_lbl));

vomcost(r,special_u)    = 0;
fomcost(r,special_u)    = 0;

* new parameter labels
set     original(u) /
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
/;

set     new(u) /
* new units
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
        fc_N            new fuel cell
        slrPV_N         new solar photovoltaic
        slrCSP_N        new concentrating solar power

        geo_N_1,geo_N_2,geo_N_3,geo_N_4,geo_N_5,geo_N_6,geo_N_7,geo_N_8,geo_N_9,geo_N_10,

        wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7/;

set     season /winter,summer/;

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

        peak            peak (top 40 hours) /;

set     mapLoad(season,l) /
        winter.(sprfl_day,sprfl_mrnevn,sprfl_night,wntr_day,wntr_mrnevn,wntr_night)
        summer.(sum_day,sum_mrnevn,sum_night,peak) /;

parameter
        heatrate
        cap
        sizes
        counts
        ACF
        ACF_season;

heatrate(r,agg_u,v)$sum(mapUnits(u_lbl,agg_u), capacity(r,v,u_lbl))
        = sum(mapUnits(u_lbl,agg_u), htrate(r,v,u_lbl) * capacity(r,v,u_lbl))
                / sum(mapUnits(u_lbl,agg_u), capacity(r,v,u_lbl));

heatrate(r,u,v)$original(u)     = htrate(r,v,u);
heatrate(r,u,v)$new(u)          = heatrate_n(r,u,v);

heatrate(r,"hyd_x","2010")      = 9884;
heatrate(r,"wnd_x","2010")      = 9884;
heatrate(r,"slr_x","2010")      = 9884;
heatrate(r,"ps_x","2010")       = 9884;


cap(r,agg_u,v)                  = sum(mapUnits(u_lbl,agg_u), capacity(r,v,u_lbl));

cap(r,u,v)$original(u)          = capacity(r,v,u);

sizes(r,agg_u,v)$sum(mapUnits(u_lbl,agg_u), capacity(r,v,u_lbl))
        = sum(mapUnits(u_lbl,agg_u), size(r,v,u_lbl) * capacity(r,v,u_lbl))
                / sum(mapUnits(u_lbl,agg_u), capacity(r,v,u_lbl));

sizes(r,u,v)$original(u)        = size(r,v,u);

counts(r,agg_u)                 = sum(mapUnits(u_lbl,agg_u), count(r,u_lbl));

counts(r,u)$original(u)         = count(r,u);

ACF_season(r,agg_u,season)$sum(mapUnits(u_lbl,agg_u), capacity(r,"2010",u_lbl))
        = sum(mapUnits(u_lbl,agg_u), maxCF(r,"2010",u_lbl,season) * capacity(r,"2010",u_lbl))
                / sum(mapUnits(u_lbl,agg_u), capacity(r,"2010",u_lbl)) / 100;

ACF_season(r,u,season)$original(u)      = maxCF(r,"2010",u,season) / 100;

ACF_season(r,u,season)$new(u)           = newavail(u) / 100;

ACF(r,u,l)                              = sum(mapLoad(season,l), ACF_season(r,u,season));

**-- read AEO forecasts --**
* EIA NERC regions
set     NERC            NERC regions in AEO /
        ECAR            East Central Area Reliability Coordination Agreement
        ERCT            Electric Reliability Council of Texas
        MAAC            Mid-Atlantic Area Council
        MAIN            Mid-America Interconnected Network
        MAPP            Mid-Continent Area Power Pool
        NY              Northeast Power Coordinating Council - New York
        NE              Northeast Power Coordinating Council - New England
        FRCC            Florida Reliability Coordinating Council
        SERC            Southeastern Electric Reliability Council
        SPP             Southwest Power Pool
        NPPA            Western Systems Coordinating Council - Northwest Power Pool Area
        RMPA            Western Systems Coordinating Council - Rocky Mountain Power Area Arizona New Mexico Southern Nevada
        CA              Western Systems Coordinating Council - California
        USA             United States /;

set mapNERC(NERC,st)  /
        ECAR.(in,mi,oh,ky,wv)
        ERCT.(tx)
        MAAC.(pa,nj,md,de,dc)
        MAIN.(ia,il,mo,wi)
        MAPP.(mn,ne,nd,sd)
        NY.(ny)
        NE.(me,ct,ma,nh,ri,vt)
        FRCC.(fl)
        SERC.(nc,sc,va,al,ga,la,ms,ar,tn)
        SPP.(ks,ok)
        NPPA.(or,wa,id,mt,ut,nv,wy)
        RMPA.(az,nm,co)
        CA.(ca)
        USA.(ak,hi) /;

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

set     Census /
        NEG             New England
        MID             Middle Atlantic
        ENC             East North Central
        WNC             West North Central
        SAC             South Atlantic
        ESC             East South Central
        WSC             West South Central
        MTN             Mountain
        PAC             Pacific /;

set     mapCensus(r,census)     /
        NEG.NEG
        (NY,MID).MID
        ENC.ENC
        WNC.WNC
        (FL,SAC).SAC
        ESC.ESC
        (TX,WSC).WSC
        MTN.MTN
        (CA,PAC).PAC /;


parameter
        demand_AEO(*,*)
        elegen_AEO(*,*,*)
        eleprc_AEO(*,*)
        eleGW_AEO(*,*,*)
        fuelprc_AEO(*,*,*)
        fueluse_AEO(*,*,*)
        nel_AEO(*,*,*)
        eledemand(*);

$gdxin 'output\AEO_electricity_data.gdx'
$load demand_AEO elegen_AEO eleprc_AEO fueluse_AEO fuelprc_AEO eleGW_AEO nel_AEO

$gdxin 'output\state_electricity_demands.gdx'
$load eledemand


parameter
        adjustment(nerc,t)              adjustment to turn demand into NEL (including CHP and trade)
        adjust(r,t)                     adjustment to turn demand into NEL (including CHP and trade)
        demand                          electricity demand
        save_demand
        scale_ele
        elestate(st,t)
        demand_growth
        price_growth
        peak_growth
        ;


adjustment(nerc,t)$nel_AEO(nerc,"demand",t)
        = (nel_AEO(nerc,"gen",t) + nel_AEO(nerc,"chp",t) + nel_AEO(nerc,"fimp",t)*(1-0.02) + nel_AEO(nerc,"nimp",t)*(1-0.02) - nel_AEO(nerc,"fexp",t) - nel_AEO(nerc,"nexp",t))
                / nel_AEO(nerc,"demand",t);

* state electricity demand growth
elestate(st,t)
        = eledemand(st) * sum(mapNERC(nerc,st), nel_AEO(nerc,"demand",t)) / sum(mapNERC(nerc,st), nel_AEO(nerc,"demand","2007"));

* regional adjustments
adjust(r,t)$sum(mapRegions(r,st), elestate(st,t) )
        = sum(mapRegions(r,st), elestate(st,t) * sum(mapNERC(NERC,st), adjustment(nerc,t)) )
                / sum(mapRegions(r,st), elestate(st,t) );

save_demand(r,t)
        = sum(mapRegions(r,st), elestate(st,t));

scale_ele(r,census,t)$(mapCensus(r,census) and save_demand(r,t))
        = save_demand(r,t) / sum(rr$mapCensus(rr,census), save_demand(rr,t));

* regional demands scaled to AEO Census forecasts
demand(r,t)
        = sum(mapCensus(r,census), demand_AEO(census,t) * scale_ele(r,census,t));



**--- Wind and Solar data ---**
set     wnd(u) / wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7 /;

set     windloc         /on,shl,dep/;
set     costclass       /1*4/;
set     windclass       /3*7/;
set     hour            /1*24/;

set     mapHour(l,hour) /
        sum_day.(8*18)
        sum_mrnevn.(6,7,19*24)
        sum_night.(1*5)

        sprfl_day.(9*16)
        sprfl_mrnevn.(6*8,17*24)
        sprfl_night.(1*5)

        wntr_day.(9*16)
        wntr_mrnevn.(6*8,17*24)
        wntr_night.(1*5) /;

set     mapLoadHour(season,l,hour) /
        summer.sum_day.(8*18)
        summer.sum_mrnevn.(6,7,19*24)
        summer.sum_night.(1*5)

        winter.sprfl_day.(9*16)
        winter.sprfl_mrnevn.(6*8,17*24)
        winter.sprfl_night.(1*5)

        winter.wntr_day.(9*16)
        winter.wntr_mrnevn.(6*8,17*24)
        winter.wntr_night.(1*5) /;

set     mapWindClass(u,windloc,windclass) /
        wnd_Nonsh_3.on.3
        wnd_Nonsh_4.on.4
        wnd_Nonsh_5.on.5
        wnd_Nonsh_6.on.6
        wnd_Nonsh_7.on.7

        wnd_Nshal_3.shl.3
        wnd_Nshal_4.shl.4
        wnd_Nshal_5.shl.5
        wnd_Nshal_6.shl.6
        wnd_Nshal_7.shl.7

        wnd_Ndeep_3.dep.3
        wnd_Ndeep_4.dep.4
        wnd_Ndeep_5.dep.5
        wnd_Ndeep_6.dep.6
        wnd_Ndeep_7.dep.7
/;

parameter
        wnd_lmt(r_ipm,windclass,costclass)      IPM wind data - onshore
        wndshl_lmt(r_ipm,windclass,costclass)   IPM wind data - offshore shallow
        wnddep_lmt(r_ipm,windclass,costclass)   IPM wind data - offshore deep
        windlimit(windloc,r,windclass,costclass)
        chk_limit
        genshr(r_ipm,r)
        wnd_rsrv
        wnd_rsrv_on
        wnd_rsrv_shl
        wnd_rsrv_dep
        wnd_ldc_sum(hour,windclass)
        wnd_ldc_wntr(hour,windclass)
        wnd_ldc(season,hour,windclass)
        wnd_acf
        avg_ldc
        wndCF
        count_hours
        slr_ldc(hour,*,*)
        slr_data(r_ipm,*,*)
        avg_slr_ldc
        slr_rsrv
        slr_acf
        rsrv_factor                             reserve margin contribution factor for wind-solar
;

$if not exist IPM\wind_ldc_sum_data.gdx $call 'gdxxrw i=IPM\Chapter4_wind-solar_CF.xls o=IPM\wind_ldc_sum_data.gdx log=IPM\wind_ldc_sum_data.log par=wnd_ldc_sum rng=wind!ad2:ai26 rdim=1 cdim=1'
$gdxin 'IPM\wind_ldc_sum_data.gdx'
$loaddc wnd_ldc_sum

$if not exist IPM\wind_ldc_wntr_data.gdx $call 'gdxxrw i=IPM\Chapter4_wind-solar_CF.xls o=IPM\wind_ldc_wntr_data.gdx log=IPM\wind_ldc_wntr_data.log par=wnd_ldc_wntr rng=wind!w2:ab26 rdim=1 cdim=1'
$gdxin 'IPM\wind_ldc_wntr_data.gdx'
$loaddc wnd_ldc_wntr

$if not exist IPM\wind_rsrv_on_data.gdx $call 'gdxxrw i=IPM\Chapter4_wind-solar_CF.xls o=IPM\wind_rsrv_on_data.gdx log=IPM\wind_rsrv_on_data.log par=wnd_rsrv_on rng=wind!a2:f32 rdim=1 cdim=1'
$gdxin 'IPM\wind_rsrv_on_data.gdx'
$loaddc wnd_rsrv_on

$if not exist IPM\wind_rsrv_shl_data.gdx $call 'gdxxrw i=IPM\Chapter4_wind-solar_CF.xls o=IPM\wind_rsrv_shl_data.gdx log=IPM\wind_rsrv_shl_data.log par=wnd_rsrv_shl rng=wind!i2:n24 rdim=1 cdim=1'
$gdxin 'IPM\wind_rsrv_shl_data.gdx'
$loaddc wnd_rsrv_shl

$if not exist IPM\wind_rsrv_dep_data.gdx $call 'gdxxrw i=IPM\Chapter4_wind-solar_CF.xls o=IPM\wind_rsrv_dep_data.gdx log=IPM\wind_rsrv_dep_data.log par=wnd_rsrv_dep rng=wind!p2:u23 rdim=1 cdim=1'
$gdxin 'IPM\wind_rsrv_dep_data.gdx'
$loaddc wnd_rsrv_dep

$if not exist IPM\wind_capacity_data.gdx $call 'gdxxrw i=IPM\Chapter4_wind_potential.xls o=IPM\wind_capacity_data.gdx log=IPM\wind_capacity.log par=wnd_lmt rng=onshore!a4:f120 rdim=2 cdim=1'
$gdxin 'IPM\wind_capacity_data.gdx'
$loaddc wnd_lmt

$if not exist IPM\wind_shl_data.gdx $call 'gdxxrw i=IPM\Chapter4_wind_potential.xls o=IPM\wind_shl_data.gdx log=IPM\wind_shl.log par=wndshl_lmt rng=off_shallow!a4:e82 rdim=2 cdim=1'
$gdxin 'IPM\wind_shl_data.gdx'
$loaddc wndshl_lmt

$if not exist IPM\wind_dep_data.gdx $call 'gdxxrw i=IPM\Chapter4_wind_potential.xls o=IPM\wind_dep_data.gdx log=IPM\wind_dep.log par=wnddep_lmt rng=off_deep!a4:e74 rdim=2 cdim=1'
$gdxin 'IPM\wind_dep_data.gdx'
$loaddc wnddep_lmt

$gdxin 'IPM\regional_shares.gdx'
$loaddc genshr


$if not exist IPM\solar_data.gdx $call 'gdxxrw i=IPM\Chapter4_wind-solar_CF.xls o=IPM\solar_data.gdx log=IPM\solar_data.log par=slr_data rng=solar!a1:g34 rdim=1 cdim=2'
$gdxin 'IPM\solar_data.gdx'
$loaddc slr_data

$if not exist IPM\solar_ldc_data.gdx $call 'gdxxrw i=IPM\Chapter4_wind-solar_CF.xls o=IPM\solar_ldc_data.gdx log=IPM\solar_ldc_data.log par=slr_ldc rng=solar!j2:n27 rdim=1 cdim=2'
$gdxin 'IPM\solar_ldc_data.gdx'
$loaddc slr_ldc


**-- Wind Data --**

set     cc      wind cost class /1*10/;

* NREL data on resources and costs
parameter
        wind_limit(r,u,cc)      Limits on wind resource (GW)
        wind_cost(r,u,cc)       Cost of wind transmission connection ($ per kW);

$gdxin 'nrel\NREL_data.gdx'
$load wind_limit wind_cost


wnd_ldc_sum(hour,windclass)             = wnd_ldc_sum(hour,windclass) / 1000;
wnd_ldc_wntr(hour,windclass)            = wnd_ldc_wntr(hour,windclass) / 1000;

wnd_ldc("winter",hour,windclass)        = wnd_ldc_wntr(hour,windclass);
wnd_ldc("summer",hour,windclass)        = wnd_ldc_sum(hour,windclass);

wnd_acf("all","avg",windclass)          = sum(hour, ((wnd_ldc_wntr(hour,windclass) * 7 + wnd_ldc_sum(hour,windclass) * 5) / 12)) / 24;

count_hours(l)                          = sum(mapHour(l,hour), 1);

avg_ldc(l,windclass)$count_hours(l)
        = sum(mapLoadHour(season,l,hour), wnd_ldc(season,hour,windclass)) / count_hours(l);
avg_ldc("peak",windclass)               = avg_ldc("sum_day",windclass);

wnd_rsrv("on","avg",windclass)
        = sum(r, sum(r_ipm, genshr(r_ipm,r) * sum(costclass,wnd_lmt(r_ipm,windclass,costclass)) * wnd_rsrv_on(r_ipm,windclass)) )
                / sum(r, sum(r_ipm, genshr(r_ipm,r) * sum(costclass,wnd_lmt(r_ipm,windclass,costclass))) );

wnd_rsrv("on",r,windclass)$sum(r_ipm, genshr(r_ipm,r) * sum(costclass,wnd_lmt(r_ipm,windclass,costclass)))
        = sum(r_ipm, genshr(r_ipm,r) * sum(costclass,wnd_lmt(r_ipm,windclass,costclass)) * wnd_rsrv_on(r_ipm,windclass))
                / sum(r_ipm, genshr(r_ipm,r) * sum(costclass,wnd_lmt(r_ipm,windclass,costclass))) ;

wnd_rsrv("shl","avg",windclass)$sum(r, sum(mapIPM(r_ipm,r), sum(costclass, wndshl_lmt(r_ipm,windclass,costclass))) )
        = sum(r, sum(mapIPM(r_ipm,r), sum(costclass, wndshl_lmt(r_ipm,windclass,costclass)) * wnd_rsrv_shl(r_ipm,windclass)) )
                / sum(r, sum(mapIPM(r_ipm,r), sum(costclass, wndshl_lmt(r_ipm,windclass,costclass))) );

wnd_rsrv("shl",r,windclass)$sum(mapIPM(r_ipm,r), sum(costclass, wndshl_lmt(r_ipm,windclass,costclass)))
        = sum(mapIPM(r_ipm,r), sum(costclass, wndshl_lmt(r_ipm,windclass,costclass)) * wnd_rsrv_shl(r_ipm,windclass))
                / sum(mapIPM(r_ipm,r), sum(costclass, wndshl_lmt(r_ipm,windclass,costclass)) );

wnd_rsrv("dep","avg",windclass)$sum(r, sum(mapIPM(r_ipm,r), sum(costclass, wnddep_lmt(r_ipm,windclass,costclass))) )
        = sum(r, sum(mapIPM(r_ipm,r), sum(costclass, wnddep_lmt(r_ipm,windclass,costclass)) * wnd_rsrv_dep(r_ipm,windclass)) )
                / sum(r, sum(mapIPM(r_ipm,r), sum(costclass, wnddep_lmt(r_ipm,windclass,costclass))) );

wnd_rsrv("dep",r,windclass)$sum(mapIPM(r_ipm,r), sum(costclass, wnddep_lmt(r_ipm,windclass,costclass)))
        = sum(mapIPM(r_ipm,r), sum(costclass, wnddep_lmt(r_ipm,windclass,costclass)) * wnd_rsrv_dep(r_ipm,windclass))
                / sum(mapIPM(r_ipm,r), sum(costclass, wnddep_lmt(r_ipm,windclass,costclass)) );


wnd_acf(windloc,r,windclass)
        = wnd_acf("all","avg",windclass) * wnd_rsrv(windloc,r,windclass) / wnd_rsrv(windloc,"avg",windclass);

wndCF(windloc,r,windclass,l)$count_hours(l)
        = (wnd_acf(windloc,r,windclass)/wnd_acf("all","avg",windclass)) * avg_ldc(l,windclass);


ACF(r,"wnd_X",l)        = wndCF("on",r,"5",l);
ACF(r,"wnd_X","peak")   = wndCF("on",r,"5","sum_day");

ACF(r,wnd,l)            = sum(mapWindClass(wnd,windloc,windclass), wndCF(windloc,r,windclass,l));
ACF(r,wnd,"peak")       = ACF(r,wnd,"sum_day");

rsrv_factor(r,u)                = 1;
rsrv_factor(r,"ps_X")           = 0;

rsrv_factor(r,"wnd_X")  = wnd_rsrv("on",r,"5");

display wnd_acf,wnd_rsrv,wnd_acf,wndCF,ACF,rsrv_factor;


**-- Solar Data --**
set     slr_lbl  / slrPV,slrCSP /;

set     slr_u(u) / slr_X,slrPV_N,slrCSP_N /;

set     mapSLR(u,slr_lbl) /
        (slrPV_N).slrPV
        (slr_X,slrCSP_N).slrCSP /;

set     mapSeason(season,l) /
        summer.sum_day
        summer.sum_mrnevn
        summer.sum_night

        winter.sprfl_day
        winter.sprfl_mrnevn
        winter.sprfl_night

        winter.wntr_day
        winter.wntr_mrnevn
        winter.wntr_night       /;


parameter
        slr_capacity;

avg_slr_ldc(slr_lbl,l)$count_hours(l)
        = sum(mapLoadHour(season,l,hour), slr_ldc(hour,slr_lbl,season)) / count_hours(l);
avg_slr_ldc(slr_lbl,"peak")     = avg_slr_ldc(slr_lbl,"sum_day");

slr_acf(r,slr_u,l)$count_hours(l)
        = sum(mapSLR(slr_u,slr_lbl), avg_slr_ldc(slr_lbl,l))
        * ( (sum(mapIPM(r_ipm,r), slr_capacity(r_ipm) * sum(mapSLR(slr_u,slr_lbl), sum(mapSeason(season,l), slr_data(r_ipm,slr_lbl,season))))) / (sum(mapIPM(r_ipm,r), slr_capacity(r_ipm))) )
                / sum(mapSLR(slr_u,slr_lbl), sum(mapSeason(season,l), slr_data("AZNM",slr_lbl,season)));

slr_acf(r,slr_u,l)      = slr_acf(r,slr_u,l) / 1000;
slr_acf(r,slr_u,"peak") = slr_acf(r,slr_u,"sum_day");

slr_acf(r,"slr_X",l)    = slr_acf(r,"slrCSP_X",l);

slr_rsrv(r,slr_u)
        = sum(mapIPM(r_ipm,r), slr_capacity(r_ipm) * sum(mapSLR(slr_u,slr_lbl), slr_data(r_ipm,slr_lbl,"resmargin")))
                / sum(mapIPM(r_ipm,r), slr_capacity(r_ipm));

rsrv_factor(r,slr_u)    = slr_rsrv(r,slr_u);

ACF(r,slr_u,l)          = slr_acf(r,slr_u,l);

ACF(r,"slr_X",l)        = acf(r,"slrCSP_N",l);

display avg_slr_ldc,slr_rsrv,slr_acf,acf;


**-- Geothermal and Landfill Gas Data --**

set     geo(u) /
        geo_N_1         new geothermal - unit 1
        geo_N_2         new geothermal - unit 2
        geo_N_3         new geothermal - unit 3
        geo_N_4         new geothermal - unit 4
        geo_N_5         new geothermal - unit 5
        geo_N_6         new geothermal - unit 6
        geo_N_7         new geothermal - unit 7
        geo_N_8         new geothermal - unit 8
        geo_N_9         new geothermal - unit 9
        geo_N_10        new geothermal - unit 10 /;

set     geo_units /1*10/;

set     mapGEO(geo,geo_units) /
        geo_N_1.1
        geo_N_2.2
        geo_N_3.3
        geo_N_4.4
        geo_N_5.5
        geo_N_6.6
        geo_N_7.7
        geo_N_8.8
        geo_N_9.9
        geo_N_10.10 /;

* read Excel data
parameter
        geo_data(*,geo_units,*)
        lfg_data
        lfg_shr
        chk_geo
        chk_lfg
        geo_limit
        lfg_limit
;

$if not exist IPM\geo_data.gdx $call 'gdxxrw i=IPM\Chapter4_geo_lfg.xls o=IPM\geo_data.gdx log=IPM\geo_data.log par=geo_data rng=geo!a2:e28 rdim=2 cdim=1'
$gdxin 'IPM\geo_data.gdx'
$loaddc geo_data

display geo_data;

$if not exist IPM\lfg_data.gdx $call 'gdxxrw i=IPM\Chapter4_geo_lfg.xls o=IPM\lfg_data.gdx log=IPM\lfg_data.log par=lfg_data rng=lfg!f2:i16 rdim=1 cdim=1'
$gdxin 'IPM\lfg_data.gdx'
$loaddc lfg_data

geo_limit(r,geo)        = sum(r_ipm, genshr(r_ipm,r) * sum(mapGEO(geo,geo_units), geo_data(r_ipm,geo_units,"MW"))) / 1000;

capcost(r,geo,t)$sum(r_ipm, genshr(r_ipm,r) * sum(mapGEO(geo,geo_units), geo_data(r_ipm,geo_units,"MW")))
        = sum(r_ipm, genshr(r_ipm,r) * sum(mapGEO(geo,geo_units), geo_data(r_ipm,geo_units,"capcost")*geo_data(r_ipm,geo_units,"MW")))
                / sum(r_ipm, genshr(r_ipm,r) * sum(mapGEO(geo,geo_units), geo_data(r_ipm,geo_units,"MW")));

fomcost(r,geo)$sum(r_ipm, genshr(r_ipm,r) * sum(mapGEO(geo,geo_units), geo_data(r_ipm,geo_units,"MW")))
        = sum(r_ipm, genshr(r_ipm,r) * sum(mapGEO(geo,geo_units), geo_data(r_ipm,geo_units,"fomcost")*geo_data(r_ipm,geo_units,"MW")))
                / sum(r_ipm, genshr(r_ipm,r) * sum(mapGEO(geo,geo_units), geo_data(r_ipm,geo_units,"MW")));

capcost(r,"geo_N",t)    = 0;
fomcost(r,"geo_N")      = 0;

vomcost(r,geo)          = vomcost(r,"geo_N");

vomcost(r,"geo_N")      = 0;

ACF(r,geo,l)            = 0.87;

heatrate(r,geo,v)       = 32969;

chk_geo = sum((r,geo), geo_limit(r,geo));

display geo_limit,chk_geo,capcost,fomcost;

set     lfg_units /lghi,lglo,lglvo/;

set     lfg_n(u) /lfg_N_hi,lfg_N_lo,lfg_N_vl /;

set     lfg_xn /lfg_X,lfg_N /;

set     mapLFGunits(lfg_n,lfg_units) /
        lfg_N_hi.lghi
        lfg_N_lo.lglo
        lfg_N_vl.lglvo /;

set     lfg_r /
        ECAR
        ERCOT
        MAAC
        MAIN
        MAPP
        NY
        NE
        FL
        STV
        SPP
        NWP
        RA
        CNV
        US /;

set mapLFG(lfg_r,r) /
        ECAR.(ENC,SAC,ESC)
        ERCOT.TX
        MAAC.(MID,SAC)
        MAIN.ENC
        MAPP.WNC
        NY.NY
        NE.NEG
        FL.FL
        STV.(SAC,ESC,WSC)
        SPP.(WNC,WSC)
        NWP.(PAC,MTN)
        RA.MTN
        CNV.CA /;


chk_lfg(r,lfg_xn)       = cap(r,lfg_xn,"2010");

lfg_shr(lfg_r,r)$sum(rr$mapLFG(lfg_r,rr), cap(rr,"lfg_X","2010"))
        = sum(rr$mapLFG(lfg_r,rr), cap(rr,"lfg_X","2010"));

lfg_shr(lfg_r,r)$(not mapLFG(lfg_r,r))  = 0;

lfg_shr(lfg_r,r)$lfg_shr(lfg_r,r)
        = cap(r,"lfg_X","2010") / lfg_shr(lfg_r,r);

lfg_limit(r,lfg_n)      = sum(mapLFG(lfg_r,r), lfg_shr(lfg_r,r) * sum(mapLFGunits(lfg_n,lfg_units), lfg_data(lfg_r,lfg_units))) / 1000;

chk_lfg(r,lfg_n)        = lfg_limit(r,lfg_n);
chk_lfg("USA",lfg_n)    = sum(r,lfg_limit(r,lfg_n));

display lfg_data,chk_lfg,lfg_shr,lfg_limit;

ACF(r,lfg_n,l)                  = 0.9;
heatrate(r,lfg_n,v)             = 18000;

* capital costs from IPM (vintage #1 for "hi" is similar to AEO in 2009)
capcost(r,"lfg_N_hi","2010")    = 2596;
capcost(r,"lfg_N_lo","2010")    = 3270;
capcost(r,"lfg_N_vl","2010")    = 5035;

capcost(r,"lfg_N_hi","2020")    = 2505;
capcost(r,"lfg_N_lo","2020")    = 3156;
capcost(r,"lfg_N_vl","2020")    = 4859;

capcost(r,"lfg_N_hi","2030")    = 2019;
capcost(r,"lfg_N_lo","2030")    = 2544;
capcost(r,"lfg_N_vl","2030")    = 3916;

capcost(r,"lfg_N_hi","2015")    = (capcost(r,"lfg_N_hi","2020") + capcost(r,"lfg_N_hi","2010")) / 2;
capcost(r,"lfg_N_lo","2015")    = (capcost(r,"lfg_N_lo","2020") + capcost(r,"lfg_N_lo","2010")) / 2;
capcost(r,"lfg_N_vl","2015")    = (capcost(r,"lfg_N_vl","2020") + capcost(r,"lfg_N_vl","2010")) / 2;

capcost(r,"lfg_N_hi","2025")    = (capcost(r,"lfg_N_hi","2030") + capcost(r,"lfg_N_hi","2020")) / 2;
capcost(r,"lfg_N_lo","2025")    = (capcost(r,"lfg_N_lo","2030") + capcost(r,"lfg_N_lo","2020")) / 2;
capcost(r,"lfg_N_vl","2025")    = (capcost(r,"lfg_N_vl","2030") + capcost(r,"lfg_N_vl","2020")) / 2;

capcost(r,"lfg_N_hi","2035")    = capcost(r,"lfg_N_hi","2030");
capcost(r,"lfg_N_lo","2035")    = capcost(r,"lfg_N_lo","2030");
capcost(r,"lfg_N_vl","2035")    = capcost(r,"lfg_N_vl","2030");

capcost(r,lfg_n,t)              = capcost(r,lfg_n,t) * regional_costs(r);

fomcost(r,lfg_n)                = fomcost(r,"lfg_N");
vomcost(r,lfg_n)                = vomcost(r,"lfg_N");

capcost(r,"lfg_N",t)            = 0;
fomcost(r,"lfg_N")              = 0;
vomcost(r,"lfg_N")              = 0;



**-- Fuel Price Data --**
set     f               fuel types /
        col             coal
        gas             natural gas
        oil             refined petroleum
        nuc             nuclear (uranium)
        lfg             landfill gas
        bio             biomass                 /;

set     col(u) /
        col_X1          existing coal class 1 (low heatrate)
        col_X2          existing coal class 2 (medium heatrate)
        col_X3          existing coal class 3 (high heatrate)

        col_X1bio       existing coal class 1 with biomass cofiring (low heatrate)
        col_X2bio       existing coal class 2 with biomass cofiring (medium heatrate)
        col_X3bio       existing coal class 3 with biomass cofiring (high heatrate)

        col_X1ccs       existing coal class 1 with CCS retrofit (low heatrate)
        col_X2ccs       existing coal class 2 with CCS retrofit (medium heatrate)
        col_X3ccs       existing coal class 3 with CCS retrofit (high heatrate)

        col_N           new coal (pulverized IGCC)
        col_Nccs        new coal with CCS /;

set     gas(u) /
        ngcc_X1         existing natural gas combined cycle class 1 - low heatrate
        ngcc_X2         existing natural gas combined cycle class 2 - medium heatrate
        ngcc_X3         existing natural gas combined cycle class 3 - high heatrate

        ngtb_X1         existing natural gas turbine class 1 - low heatrate
        ngtb_X2         existing natural gas turbine class 2 - medium heatrate
        ngtb_X3         existing natural gas turbine class 3 - high heatrate
        ngcc_N          new natural gas combined cycle
        ngcc_Nccs       new natural gas combined cycle with CCS
        ngtb_N          new natural gas turbine /;

set     oil(u) /
        stog_X1         existing steam oil-gas class 1 - low heatrate
        stog_X2         existing steam oil-gas class 2 - medium heatrate
        stog_X3         existing steam oil-gas class 3 - high heatrate /;

set     nuc(u)          / nuc_X, nuc_N /;

set     bio(u)          / bio_X, bio_N /;

set     lfg(u)          / lfg_X, lfg_N_hi,lfg_N_lo,lfg_N_vl /;

parameter
        pf(r,f,t)               fuel prices from AEO
        eleprc(r,t)
        fueltype(u,f)           type of fuel used
        emis_factor(r,u,*);

pf(r,f,t)       = sum(mapCensus(r,census), fuelprc_AEO(census,f,t));

eleprc(r,t)     = sum(mapCensus(r,census), eleprc_AEO(census,t));

* add IPM nuclear and biomass fuel costs (2012, 2015, 2020, 2030)
pf(r,"nuc","2010")      = 0.71;
pf(r,"nuc","2015")      = 0.75;
pf(r,"nuc","2020")      = 0.76;
pf(r,"nuc","2025")      = 0.80;
pf(r,"nuc","2030")      = 0.84;
pf(r,"nuc","2035")      = 0.88;
pf(r,"nuc","2040")      = 0.92;
pf(r,"nuc","2045")      = 0.96;
pf(r,"nuc","2050")      = 1.00;

pf(r,"bio","2010")      = 2.25;
pf(r,"bio","2015")      = 2.42;
pf(r,"bio","2020")      = 2.65;
pf(r,"bio","2025")      = 2.57;
pf(r,"bio","2030")      = 2.50;
pf(r,"bio","2035")      = 2.50;
pf(r,"bio","2040")      = 2.50;
pf(r,"bio","2045")      = 2.50;
pf(r,"bio","2050")      = 2.50;

* add biofuel supply curves from IPM
set     r_bio /
        "AK, CA, HI, OR, WA"
        "AL, MS"
        "AR, LA, OK, TX"
        "AZ, NM"
        "CO, NV, UT"
        "CT, MA, ME, NH, RI, VT"
        "DC, DE, MD, NC, SC, WV"
        "FL, GA"
        "IA, KS, MN, MO, ND, NE, SD"
        "ID, MT, WY"
        "IL, IN, MI, WI"
        "KY, TN"
        "OH"
        "PA, NJ, NY" /;

set     IPM_biostep /BM02*BM09,BM010*BM018/;
set     biostep /1*17/;

set     mapStep(IPM_biostep,biostep) /
        BM02.1
        BM03.2
        BM04.3
        BM05.4
        BM06.5
        BM07.6
        BM08.7
        BM09.8
        BM010.9
        BM011.10
        BM012.11
        BM013.12
        BM014.13
        BM015.14
        BM016.15
        BM017.16
        BM018.17 /;

parameter
        bio_supply_data(*,r_bio,*,*)
        pbio
        biocap
        bioshr
        bioreg
        biostate
        biosupply
        chk_bio;

$if not exist IPM\bio_supply_data.gdx $call 'gdxxrw i=IPM\Appendix11_1.xls o=IPM\bio_supply_data.gdx log=IPM\bio_supply_data.log par=bio_supply_data rng=biomass_supply_curve!b4:f2692 rdim=3 cdim=1'
$gdxin 'IPM\bio_supply_data.gdx'
$load bio_supply_data

display bio_supply_data;

$gdxin 'IPM\state_capacity.gdx'
$load biocap

display biocap;

set     mapBIOreg(r_bio,st) /
        "AK, CA, HI, OR, WA".(ak,ca,hi,or,wa)
        "AL, MS".(al,ms)
        "AR, LA, OK, TX".(ar,la,ok,tx)
        "AZ, NM".(az,nm)
        "CO, NV, UT".(co,nv,ut)
        "CT, MA, ME, NH, RI, VT".(ct,ma,me,nh,ri,vt)
        "DC, DE, MD, NC, SC, WV".(de,md,nc,sc,wv)
        "FL, GA".(fl,ga)
        "IA, KS, MN, MO, ND, NE, SD".(ia,ks,mn,mo,nd,ne,sd)
        "ID, MT, WY".(id,mt,wy)
        "IL, IN, MI, WI".(il,in,mi,wi)
        "KY, TN".(ky,tn)
        "OH".oh
        "PA, NJ, NY".(pa,nj,ny)
/;

alias (st,stt);

bioreg(r_bio)   = sum(mapBIOreg(r_bio,st), biocap(st));

bioshr(st,r_bio)$sum(mapBIOreg(r_bio,st), bioreg(r_bio))
        = biocap(st) / sum(mapBIOreg(r_bio,st), bioreg(r_bio));

bioshr("chk",r_bio)     = sum(st, bioshr(st,r_bio));

biostate(st,biostep,"btu",t)
        = sum(mapBIOreg(r_bio,st),
                sum(mapStep(IPM_biostep,biostep),
                        bioshr(st,r_bio) * bio_supply_data(t,r_bio,IPM_biostep,"Tbtu")));

biostate(st,biostep,"btu","2010")
        = sum(mapBIOreg(r_bio,st),
                sum(mapStep(IPM_biostep,biostep),
                        bioshr(st,r_bio) * bio_supply_data("2012",r_bio,IPM_biostep,"Tbtu")));


biostate("USA",biostep,"btu",t)
        = sum(st, biostate(st,biostep,"btu",t));

biostate(st,biostep,"pbio",t)
        = sum(mapBIOreg(r_bio,st),
                sum(mapStep(IPM_biostep,biostep),
                        bio_supply_data(t,r_bio,IPM_biostep,"$perMMBtu")));

biostate(st,biostep,"pbio","2010")
        = sum(mapBIOreg(r_bio,st),
                sum(mapStep(IPM_biostep,biostep),
                        bio_supply_data("2012",r_bio,IPM_biostep,"$perMMBtu")));

biostate(st,biostep,"pbio","2010")      = biostate(st,biostep,"pbio","2012");
biostate(st,biostep,"pbio","2012")      = 0;

biostate(st,biostep,"value",t)          = biostate(st,biostep,"btu",t) * biostate(st,biostep,"pbio",t);

pbio(r,biostep,t)$sum(mapRegions(r,st), biostate(st,biostep,"btu",t))
        = sum(mapRegions(r,st), biostate(st,biostep,"value",t)) / sum(mapRegions(r,st), biostate(st,biostep,"btu",t));

biosupply(r,biostep,t)
        = sum(mapRegions(r,st), biostate(st,biostep,"btu",t));

biosupply(r,biostep,"2025")     = (biosupply(r,biostep,"2030")+biosupply(r,biostep,"2020"))/2;
pbio(r,biostep,"2025")          = (pbio(r,biostep,"2030")+pbio(r,biostep,"2020"))/2;

display bioshr,biostate,pbio,biosupply;



fueltype(u,"col")$col(u)        = 1;
fueltype(u,"gas")$gas(u)        = 1;
fueltype(u,"oil")$oil(u)        = 1;
fueltype(u,"nuc")$nuc(u)        = 1;
fueltype(u,"bio")$bio(u)        = 1;
fueltype(u,"lfg")$lfg(u)        = 1;

emis_factor(r,col,"CO2")        = 25.2 * 44/12;
emis_factor(r,gas,"CO2")        = 14.47 * 44/12;
emis_factor(r,oil,"CO2")        = 20.97 * 44/12;

* adjust emissions for CCS units
emis_factor(r,"col_Nccs","CO2")         = 0.1 * 25.2 * 44/12;
emis_factor(r,"ngcc_Nccs","CO2")        = 0.1 * 14.47 * 44/12;


** extend AEO forecasts **
set     r_adage /
        NRTH    Northeast
        SATL    South Atlantic
        SCNT    South Central
        NCNT    North Central (Midwest)
        MNTN    Mountain
        WEST    West Coast /;

set     mapADAGEr(r,r_adage) /
        NEG.NRTH
        NY.NRTH
        MID.SATL
        ENC.NCNT
        WNC.NCNT
        FL.SATL
        SAC.SATL
        ESC.SCNT
        TX.SCNT
        WSC.SCNT
        MTN.MNTN
        CA.WEST
        PAC.WEST
/;

parameter
        pf_growth(*,f,t)
        demand_growth(*,t)
        eleprc_growth(*,t)
        biosupply_growth(*,t);

$gdxin 'output\EMA_forecasts.gdx'
$load pf_growth demand_growth eleprc_growth biosupply_growth

biosupply(r,biostep,t)$(t.val ge 2035)
        = biosupply(r,biostep,"2030") * sum(mapADAGEr(r,r_adage), biosupply_growth(r_adage,t));

pbio(r,biostep,"2035")  = pbio(r,biostep,"2030") * pf(r,"col","2035") / pf(r,"col","2030");


chk_bio(t) = sum((r,biostep), biosupply(r,biostep,t));

pf_growth(r_adage,"lfg",t)      = pf_growth(r_adage,"gas",t);
pf_growth(r_adage,"nuc",t)      = pf_growth(r_adage,"col",t);

heatrate(r,u,postAEO)$new(u)    = heatrate_n(r,u,"2035");
heatrate(r,geo,postAEO)         = 32969;
heatrate(r,lfg_n,postAEO)       = 18000;


capcost(r,u,postAEO)            = capcost(r,u,"2035");
adjust(r,postAEO)               = adjust(r,"2035");

demand(r,postAEO)               = demand(r,"2035") * sum(mapADAGEr(r,r_adage), demand_growth(r_adage,postAEO));
pf(r,f,postAEO)                 = pf(r,f,"2035") * sum(mapADAGEr(r,r_adage), pf_growth(r_adage,f,postAEO));
eleprc(r,postAEO)               = eleprc(r,"2035") * sum(mapADAGEr(r,r_adage), eleprc_growth(r_adage,postAEO));

display demand,demand_growth;

demand(r,"2075")                = demand(r,"2050") * ((sum(mapADAGEr(r,r_adage), demand_growth(r_adage,"2050")) / sum(mapADAGEr(r,r_adage), demand_growth(r_adage,"2045")))**(1/5))**25;
pf(r,f,"2075")                  = pf(r,f,"2050") * ((sum(mapADAGEr(r,r_adage), pf_growth(r_adage,f,"2050")) / sum(mapADAGEr(r,r_adage), pf_growth(r_adage,f,"2045")))**(1/5))**25;
eleprc(r,"2075")                = eleprc(r,"2050") * ((sum(mapADAGEr(r,r_adage), eleprc_growth(r_adage,"2050")) / sum(mapADAGEr(r,r_adage), eleprc_growth(r_adage,"2045")))**(1/5))**25;

pbio(r,biostep,postAEO)         = pbio(r,biostep,"2035") * pf(r,"col",postAEO) / pf(r,"col","2035");

option chk_bio:2:0:1;
display chk_bio,pbio;


peak_growth(r,t)                = demand(r,t) / demand(r,"2010");
peak(r,t)                       = peak(r,"2010") * peak_growth(r,t);

Display demand,pf,peak,eleprc,vomcost,fomcost,heatrate,cap,sizes,counts,acf;

**--- add deflator for IPM data from 2007 to 2010 (AEO already in $2010) ---**

parameter
        deflator;

deflator        = (1.105783/1.085975) * (1.225/1.19819);
display deflator;

pbio(r,biostep,t)       = pbio(r,biostep,t) * deflator;

* ( cap & O&M costs done in cost_data.gms )

execute_unload 'output\model_data.gdx',
        cap = capacity,
        heatrate,
        sizes = size,
        counts = count,
        acf = maxCF,
        fueltype
        hours,
        peak,
        transmit_lmt = transmit_limit,
        RsrvMargin
        rsrv_factor
        load_pct = loadpct
        geo_limit
        lfg_limit
        emis_factor
        pbio
        biosupply

        wind_limit
        wind_cost

        capcost
        fomcost
        vomcost
        pf

        eleprc = pele
        demand  = dele
        adjust = dadj

;
