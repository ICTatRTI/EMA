$title Load Duration Curve data and Wind-Solar generation

set     t               time periods / 2010,2015,2020,2025,2030,2035,2040,2045,2050,2075 /;

set     month /1*12/;

set     day /1*31/;

set     hour /1*24/;

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

set     st      State code (without US) /
        AK, AL, AR, AZ, CA, CO, CT, DC, DE, FL, GA, HI, IA, ID, IL, IN,
        KS, KY, LA, MA, MD, ME, MI, MN, MO, MS, MT, NE, NC, ND, NH, NJ, NM,
        NV, NY, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VA, VT, WA, WI, WV, WY/;

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

alias (r,rr), (r_IPM,rr_IPM);

parameter
        ldc(r_ipm,month,day,*)          IPM load data
        ;

$if not exist IPM\load_data.gdx $call 'gdxxrw i=IPM\Chapter2Appendix2_1Data.xls o=IPM\load_data.gdx log=IPM\load.log par=ldc rng=LDC!b4:ab11684 rdim=3 cdim=1'
$gdxin 'IPM\load_data.gdx'
$loaddc ldc

option ldc:0:3:1;
*display ldc;


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

alias (l,ll);

set     mapLoad(l,month,day,hour) /
        sum_day.(5*9).(1*31).(8*18)
        sum_mrnevn.(5*9).(1*31).(6,7,19*24)
        sum_night.(5*9).(1*31).(1*5)

        sprfl_day.(4,10,11).(1*31).(9*16)
        sprfl_mrnevn.(4,10,11).(1*31).(6*8,17*24)
        sprfl_night.(4,10,11).(1*31).(1*5)

        wntr_day.(12,1*3).(1*31).(9*16)
        wntr_mrnevn.(12,1*3).(1*31).(6*8,17*24)
        wntr_night.(12,1*3).(1*31).(1*5) /;


parameter
        genshr(r_ipm,r) shares to convert from IPM region to model region
        load_data       load by model regions
        load_nopeak     load without peak hours
        load_seg        load by segment
        load_pct        percentage of load by segment
        load_US         total US load
        maxhour
        savemax
        count_hours
        hours
        peak(*,t)               peak demand by region
;

$gdxin 'IPM\regional_shares.gdx'
$loaddc genshr

load_data(r,month,day,hour)     = sum(r_ipm, genshr(r_ipm,r) * ldc(r_ipm,month,day,hour));

* absolute peak for reserve margins
peak(r,t)                       = 0;
loop((month,day,hour),
peak(r,"2010")                  = max(peak(r,"2010"),load_data(r,month,day,hour));
);

* separate out peak hours
set peakhours /1*88/;

maxhour(r,peakhours)            = 0;

loop(peakhours,

loop((r,month,day,hour),
maxhour(r,peakhours)            = max(load_data(r,month,day,hour), maxhour(r,peakhours));
);

loop((r,month,day,hour)$(load_data(r,month,day,hour) eq maxhour(r,peakhours)),
savemax(r,month,day,hour)$(load_data(r,month,day,hour) eq maxhour(r,peakhours)) = load_data(r,month,day,hour);
load_data(r,month,day,hour)$(load_data(r,month,day,hour) eq maxhour(r,peakhours)) = 0;
);
);

count_hours(r,l)                = sum(mapLoad(l,month,day,hour)$load_data(r,month,day,hour), 1);
count_hours(r,"peak")           = card(peakhours);
count_hours(r,"total")          = sum(l, count_hours(r,l));
display count_hours;

hours(r,l)                      = count_hours(r,l);

load_seg(r,l)                   = sum(mapLoad(l,month,day,hour), load_data(r,month,day,hour));
load_seg(r,"peak")              = sum(mapLoad(l,month,day,hour), savemax(r,month,day,hour));

load_pct(r,l)                   = load_seg(r,l) / sum(ll, load_seg(r,ll));

display load_pct;

loop(r,
abort$(round(sum(l, load_pct(r,l)),6) ne 1) "load percentages off";
);

* convert to billion kWh (TWh)
load_seg(r,l)                   = load_seg(r,l) / 1e6;

* get peak in GW
peak(r,"2010")                  = peak(r,"2010") / 1e3;

load_US("pre") = sum((r_ipm,month,day,hour), ldc(r_ipm,month,day,hour)) / 1e6;
load_US("post") = sum((r,month,day,hour), load_data(r,month,day,hour)+savemax(r,month,day,hour)) / 1e6;
load_US("load") = sum((r,l), load_seg(r,l));

option load_data:1:3:1;
option load_seg:0;
display load_seg,peak,load_US,load_pct;

parameter
        transmit(*,*,*)         IPM transmission limits (energy)
        transmits
        translimit              transmission limit
;

$if not exist IPM\transmit_data.gdx $call 'gdxxrw i=IPM\Chapter3_transmission.xls o=IPM\transmit_data.gdx log=IPM\trans.log par=transmit rng=Sheet1!i3:m144 rdim=2 cdim=1'
$gdxin 'IPM\transmit_data.gdx'
$load transmit

transmits(r,rr_IPM)     = sum(r_ipm, genshr(r_IPM,r) * transmit(rr_IPM,r_IPM,"energy"));

translimit(r,rr)        = sum(rr_ipm, genshr(rr_IPM,rr) * transmits(r,rr_IPM));

translimit(r,r)                                 = 0;
translimit(r,rr)$(translimit(r,rr) lt 100)      = 0;

translimit(r,rr)                                = translimit(r,rr) / 1000;

option transmit:0:2:1;
option translimit:3;
display transmit,transmits,genshr,translimit;

execute_unload 'output\load_curve_data.gdx', load_seg=load, load_pct, hours, peak, translimit=transmit_lmt;

* save transmission for ADAGE electricity

set     census          regions /
        NEG             New England
        MID             Middle Atlantic
        ENC             East North Central
        WNC             West North Central
        SAC             South Atlantic
        ESC             East South Central
        WSC             West South Central
        MTN             Mountain
        PAC             Pacific /;

set     mapCensus(r,census) /
        NEG.NEG
        NY.NEG
        MID.MID
        ENC.ENC
        WNC.WNC
        FL.SAC
        SAC.SAC
        ESC.ESC
        TX.WSC
        WSC.WSC
        MTN.MTN
        CA.PAC
        PAC.PAC /;

set     mapCensuss(r,census); mapCensuss(r,census) = mapcensus(r,census);

alias (census,censuss);

parameter
        trans_mit       Annual Transmission Capabilities (GW);

trans_mit(census,censuss)
        = sum(mapCensus(r,census), sum(mapCensuss(rr,censuss), translimit(r,rr)));

trans_mit(census,census) = 0;

display trans_mit;


execute_unload 'output\IPM_transmission_data.gdx', trans_mit=transmission;
