$title Wind Resource Data

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

set     st      State code (without US) /
        AK, AL, AR, AZ, CA, CO, CT, DC, DE, FL, GA, HI, IA, ID, IL, IN,
        KS, KY, LA, MA, MD, ME, MI, MN, MO, MS, MT, NE, NC, ND, NH, NJ, NM,
        NV, NY, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VA, VT, WA, WI, WV, WY/;

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

set     r_NREL /1*356/;

set     wl /onsh,shal,deep/;

set     wc /3*7/
        cc_nrel /1*200 /;

set     cc /1*10/;

set     mapCC(cc,cc_nrel) /
        1.(1*19)
        2.(20*39)
        3.(40*59)
        4.(60*79)
        5.(80*99)
        6.(100*119)
        7.(120*139)
        8.(140*159)
        9.(160*179)
        10.(180*200)
/;

$ontext
set     mapNREL(r_nrel,st);
$if not exist NRELmap.gdx       $call 'gdxxrw i=ReEDS_regionstate.xlsx o=NRELmap.gdx log=NRELmap.log par=mapNREL rng=sheet1!a2:c357 cdim=1'
$gdxin 'NRELmap.gdx'
$load mapNREL
$offtext

set     mapNREL(r_nrel,st) /
1.WA
2.WA
3.WA
4.WA
5.WA
6.WA
7.WA
8.WA
9.WA
10.OR
11.OR
12.OR
13.OR
14.OR
15.OR
16.OR
17.OR
18.OR
19.OR
20.CA
21.CA
22.CA
23.CA
24.CA
25.CA
26.CA
27.CA
28.CA
29.CA
30.CA
31.CA
32.CA
33.CA
34.NV
35.NV
36.NV
37.NV
38.NV
39.NV
40.NV
41.ID
42.ID
43.ID
44.ID
45.ID
46.ID
47.ID
48.UT
49.UT
50.UT
51.UT
52.UT
53.AZ
54.AZ
55.AZ
56.AZ
57.AZ
58.AZ
59.AZ
60.MT
61.MT
62.MT
63.MT
64.MT
65.MT
66.MT
67.MT
68.MT
69.MT
70.WY
71.WY
72.WY
73.WY
74.WY
75.CO
76.CO
77.CO
78.CO
79.CO
80.CO
81.CO
82.CO
83.CO
84.CO
85.CO
86.CO
87.CO
88.NM
89.NM
90.NM
91.NM
92.NM
93.NM
94.TX
95.SD
96.MT
97.ND
98.ND
99.ND
100.ND
101.ND
102.ND
103.ND
104.SD
105.SD
106.SD
107.SD
108.SD
109.SD
110.SD
111.SD
112.SD
113.SD
114.NE
115.NE
116.NE
117.NE
118.NE
119.NE
120.MN
121.MN
122.MN
123.MN
124.MN
125.MN
126.MN
127.MN
128.MN
129.WI
130.WI
131.WI
132.IA
133.IA
134.IA
135.KS
136.KS
137.KS
138.KS
139.KS
140.KS
141.KS
142.KS
143.KS
144.MO
145.MO
146.MO
147.MO
148.NM
149.TX
150.TX
151.TX
152.OK
153.OK
154.OK
155.OK
156.OK
157.OK
158.OK
159.AR
160.AR
161.LA
162.LA
163.TX
164.TX
165.TX
166.TX
167.TX
168.TX
169.TX
170.TX
171.TX
172.TX
173.TX
174.TX
175.TX
176.MN
177.IA
178.IA
179.IA
180.IA
181.IA
182.IA
183.IA
184.MO
185.MO
186.MO
187.MO
188.MO
189.MO
190.MO
191.MI
192.WI
193.WI
194.WI
195.WI
196.WI
197.WI
198.WI
199.IL
200.IL
201.IL
202.IL
203.IL
204.IL
205.IL
206.IL
207.IL
208.IL
209.IL
210.IL
211.MI
212.MI
213.MI
214.MI
215.MI
216.MI
217.MI
218.MI
219.IN
220.IN
221.IN
222.IN
223.IN
224.IN
225.IN
226.IN
227.OH
228.OH
229.OH
230.OH
231.OH
232.OH
233.OH
234.OH
235.OH
236.KY
237.KY
238.KY
239.KY
240.KY
241.KY
242.KY
243.VA
244.VA
245.VA
246.WV
247.WV
248.WV
249.WV
250.WV
251.PA
252.PA
253.MD
254.PA
255.PA
256.TX
257.AR
258.AR
259.AR
260.LA
261.LA
262.MS
263.MS
264.MS
265.AL
266.AL
267.AL
268.AL
269.FL
270.FL
271.FL
272.GA
273.GA
274.GA
275.GA
276.TN
277.TN
278.TN
279.TN
280.TN
281.TN
282.TN
283.TN
284.TN
285.TN
286.KY
287.KY
288.NC
289.NC
290.NC
291.NC
292.NC
293.NC
294.NC
295.SC
296.SC
297.SC
298.SC
299.SC
300.VA
301.VA
302.VA
303.VA
304.VA
305.VA
306.VA
307.VA
308.WV
309.FL
310.FL
311.FL
312.FL
313.FL
314.VA
315.MD
316.MD
317.MD
318.DE
319.DE
320.PA
321.PA
322.PA
323.PA
324.PA
325.PA
326.PA
327.PA
328.NJ
329.NJ
330.NJ
331.NJ
332.NJ
333.NY
334.NY
335.NY
336.NY
337.NY
338.NY
339.NY
340.NY
341.NY
342.NY
343.CT
344.CT
345.RI
346.RI
347.MA
348.MA
349.MA
350.VT
351.VT
352.VT
353.NH
354.NH
355.ME
356.ME
/;


parameter
        windon(st,wc,cc_nrel) "MW of onshore wind resource by state, class, and transmission bin"
        windon_tc(cc_nrel) "transmission cost associated with onshore wind bin ($/kW)"
        windoffshl(st,wc,cc_nrel) "MW of offshore shallow wind resource by state, class, and transmission bin"
        windoffshl_tc(cc_nrel) "transmission cost associated with onshore wind bin ($/kW)"
        windoffdep(st,wc,cc_nrel) "MW of offshore deep wind resource by state, class, and transmission bin"
        windoffdep_tc(cc_nrel) "transmission cost associated with onshore wind bin ($/kW)"
        winddata(wl,r,wc,cc) "MW of wind by placement, region, class, and transmission bin"
        windcost(wl,r,wc,cc) "transmission cost ($/MW) of wind by placement, region, class, and transmission bin"
;


$if not exist NREL_wind_onshore.gdx             $call 'gdxxrw i=WindbyState.xlsx o=NREL_wind_onshore.gdx par=windon rng="onshore ($MW)"!j5:hc245 rdim=2 cdim=1  par=windon_tc rng="onshore ($MW)"!k1:hc2 rdim=0 cdim=1'
$gdxin 'NREL_wind_onshore.gdx'
$load windon, windon_tc

$if not exist NREL_wind_offshore_shallow.gdx    $call 'gdxxrw i=WindbyState.xlsx o=NREL_wind_offshore_shallow.gdx par=windoffshl rng="offshore_shallow ($MW)"!j5:hc245 rdim=2 cdim=1  par=windoffshl_tc rng="offshore_shallow ($MW)"!k1:hc2 rdim=0 cdim=1'
$gdxin 'NREL_wind_offshore_shallow.gdx'
$load windoffshl, windoffshl_tc

$if not exist NREL_wind_offshore_deep.gdx       $call 'gdxxrw i=WindbyState.xlsx o=NREL_wind_offshore_deep.gdx par=windoffdep rng="offshore_deep ($MW)"!j5:hc245 rdim=2 cdim=1  par=windoffdep_tc rng="offshore_deep ($MW)"!k1:hc2 rdim=0 cdim=1'
$gdxin 'NREL_wind_offshore_deep.gdx'
$load windoffdep, windoffdep_tc

*display windon,windoffshl,windoffdep;

winddata("onsh",r,wc,cc)        = sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windon(st,wc,cc_nrel)));
winddata("shal",r,wc,cc)        = sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windoffshl(st,wc,cc_nrel)));
winddata("deep",r,wc,cc)        = sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windoffdep(st,wc,cc_nrel)));

parameter
        IPM_lcoe;

IPM_lcoe("onsh","3")            = 131.98;
IPM_lcoe("onsh","4")            = 114.33;
IPM_lcoe("onsh","5")            = 94.81;
IPM_lcoe("onsh","6")            = 80.98;
IPM_lcoe("onsh","7")            = 73.80;

IPM_lcoe("shal","3")            = 316.56;
IPM_lcoe("shal","4")            = 264.25;
IPM_lcoe("shal","5")            = 219.13;
IPM_lcoe("shal","6")            = 187.18;
IPM_lcoe("shal","7")            = 177.02;

IPM_lcoe("deep","3")            = 316.56;
IPM_lcoe("deep","4")            = 264.25;
IPM_lcoe("deep","5")            = 219.13;
IPM_lcoe("deep","6")            = 187.18;
IPM_lcoe("deep","7")            = 177.02;

* PTS remove IPM LCOE; reinstate AEO costs.
IPM_lcoe(wl,wc) = 0 ;

* PTS weighted-average of transmission cost to reduce from cc_nrel to cc bins: 200 bins down to 10.
windcost("onsh",r,wc,cc)$sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windon(st,wc,cc_nrel)))
        = sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windon(st,wc,cc_nrel)*(IPM_lcoe("onsh",wc)+windon_tc(cc_nrel)))) / sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windon(st,wc,cc_nrel)));

windcost("shal",r,wc,cc)$sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windoffshl(st,wc,cc_nrel)))
        = sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windoffshl(st,wc,cc_nrel)*(IPM_lcoe("shal",wc)+windoffshl_tc(cc_nrel)))) / sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windoffshl(st,wc,cc_nrel)));

windcost("deep",r,wc,cc)$sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windoffdep(st,wc,cc_nrel)))
        = sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windoffdep(st,wc,cc_nrel)*(IPM_lcoe("deep",wc)+windoffdep_tc(cc_nrel)))) / sum(mapRegions(r,st), sum(mapCC(cc,cc_nrel), windoffdep(st,wc,cc_nrel)));

display winddata,windcost;
*$exit

execute_unload 'NREL_wind_data.gdx', winddata;


* convert to EMA notation

set     u       units   / wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7 /
        wnd(u)          / wnd_Nonsh_3*wnd_Nonsh_7,wnd_Nshal_3*wnd_Nshal_7,wnd_Ndeep_3*wnd_Ndeep_7 /
        wc_onsh(u)      / wnd_Nonsh_3*wnd_Nonsh_7 /
        wc_shal(u)      / wnd_Nshal_3*wnd_Nshal_7 /
        wc_deep(u)      / wnd_Ndeep_3*wnd_Ndeep_7 /
;

set     mapWL(wl,u) /
        onsh.(wnd_Nonsh_3*wnd_Nonsh_7)
        shal.(wnd_Nshal_3*wnd_Nshal_7)
        deep.(wnd_Ndeep_3*wnd_Ndeep_7) /;

set     mapWC(wc,u) /
        3.(wnd_Nonsh_3,wnd_Nshal_3,wnd_Ndeep_3)
        4.(wnd_Nonsh_4,wnd_Nshal_4,wnd_Ndeep_4)
        5.(wnd_Nonsh_5,wnd_Nshal_5,wnd_Ndeep_5)
        6.(wnd_Nonsh_6,wnd_Nshal_6,wnd_Ndeep_6)
        7.(wnd_Nonsh_7,wnd_Nshal_7,wnd_Ndeep_7) /;


parameter
        wind_limit(r,u,cc)      Limits on wind resource (GW)
        wind_cost(r,u,cc)       Cost of wind transmission connection ($ per kW)
        chk_wind;

*wind_limit(r,u)                = sum(mapWL(wl,u), sum(mapWC(wc,u), sum(cc, winddata(wl,r,wc,cc)))) / 1000;
wind_limit(r,u,cc)      = sum(mapWL(wl,u), sum(mapWC(wc,u), winddata(wl,r,wc,cc))) / 1000;

*!!! assumes NREL data is in $2010 (probably is not)
wind_cost(r,u,cc)       = sum(mapWL(wl,u), sum(mapWC(wc,u), windcost(wl,r,wc,cc)));


chk_wind(u)             = sum((r,cc), wind_limit(r,u,cc));

option chk_wind:3:0:1;
display wind_limit,chk_wind,wind_cost;

execute_unload 'NREL_data.gdx', wind_limit,wind_cost;
