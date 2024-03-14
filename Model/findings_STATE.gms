$title Findings for Electricity Dispatch Partial Equilibrium Model

parameter
        chk_price
;

* Wholesale price from the marginal on demand
chk_price(l,r,t)                = c_demandseg.m(r,l,t) / 1;
chk_price("avg",r,t)            = ( sum(l,(c_demandseg.m(r,l,t) * DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) / sum(l,(DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) );
chk_price("avg","USA",t)        = ( sum((r,l),(c_demandseg.m(r,l,t) * DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) / sum((r,l),(DemandSeg.l(r,t) * dele(r,t) * loadpct(r,l) * dadj(r,t))) );

execute_unload '..\Model\Output\pivot_STATE.gdx', chk_price;

option chk_price:2;

* OUTPUTS: TRICK GAMS into outputting data in csv format
File generation_dat /..\Model\Results\PE_output_generation_STATE.csv/,
*        demand_seg_dat /..\Model\Results\PE_output_demandseg.csv/,
*        demand_reg_dat /..\Model\Results\PE_output_demandreg.csv/,
        capacity_dat /..\Model\Results\PE_output_capacity_STATE.csv/,
*        transmission_dat /..\Model\Results\PE_output_transmission.csv/,
*        emissions_dat /..\Model\Results\PE_output_emissions.csv/,
*        retirement_dat /..\Model\Results\PE_output_retirement.csv/;
        fomcost_dat /..\Model\Results\PE_output_fomcost_STATE.csv/,
        vomcost_dat /..\Model\Results\PE_output_vomcost_STATE.csv/;

put generation_dat;
put 'region,unit,load_segment,vintage,time,value'/ ;
* loop through region, unit, "vintage", load_segment, year
loop((r,u,v,l,t), put r.tl:0,',':0,u.tl:0,',':0,l.tl:0,',':0,v.tl:0,',':0,t.tl:0,',':0,GEN.l(r,u,v,l,t):0:2 /);

* put demand_seg_dat;
* put 'region,time,value'/;
* loop((r,t), put r.tl:0,',':0,t.tl:0,',':0,DemandSeg.l(r,t):0:2 /);

put capacity_dat;
put 'region,unit,vintage,time,value'/;
loop((r,u,v,t), put r.tl:0,',':0,u.tl:0,',':0,v.tl:0,',':0,t.tl:0,',',CAP.l(r,u,v,t):0:2 /);

*put transmission_dat;
*put 'region,destination,load_segment,time'/;
*loop((r,rr,l,t), put r.tl:0,',':0,rr.tl:0,',':0,l.tl:0,',':0,t.tl:0,',':0,Transmit.l(r,rr,l,t):0:2 /);

*put emissions_dat;
*put 'region,pollutant,time,value'/;
*loop((r,p,t), put r.tl:0,',':0,p.tl:0,',':0,t.tl:0,',':0,Emis.l(r,p,t):0:2 /);

*put demand_reg_dat;
*put 'region,time,value'/;
*loop((r,t), put r.tl:0,',':0,t.tl:0,',':0,Demand.l(r,t):0:2 /);

put fomcost_dat;
put 'region,unit,value'/;
loop((r,u), put r.tl:0,',':0,u.tl:0,',':0,fomcost(r,u):0:2 /);

put vomcost_dat;
put 'region,unit,value'/;
loop((r,u), put r.tl:0,',':0,u.tl:0,',':0,vomcost(r,u):0:2 /);

*put retirement_dat;
*put 'region,unit,vintage,time,value'/;
*loop((r,u,v,t), put r.tl:0,',':0,u.tl:0,',':0,v.tl:0,',':0,t.tl:0,',',Retire.l(r,u,v,t):0:2 /);

$onechov >..\Model\Output\report_STATE.gms

$offecho

