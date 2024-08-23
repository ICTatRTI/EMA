$title Data for Electricity Dispatch Partial Equilibrium Model


$onInLine

* data
parameters
        capacity(r,u,v)         capacity of existing /* MEEDE */ /*XXX*/
        heatrate(r,u,v)         heat rate of existing units /* MEEDE */ /*XXX*/
        emis_factor(r,u,p)      pollutant emissions factors (CO2 SO2 NOx Hg) /* constant by fuel type--unmodified */

        fomcost(r,u)            fixed O&M ($ per kW) /* MEEDE */ /*XXX*/
        vomcost(r,u)            variable O&M ($ per MWh) /* MEEDE */ /*XXX*/
        pf(r,f,t)               fuel prices /* MEEDE for oil, gas, coal; unmodified for bio, nuc */ /*XXX*/
        fueltype(u,f)           type of fuel used by a unit /* unmodified */ /*XXX*/

        dele(r,t)               annual demand (TWh) /* NEEDE (= netgen) */ /*XXX*/
        loadpct(r,l)            percentage of demand by load segment /* unmodified */
        dadj(r,t)               demand adjustment to turn demand into NEL (including CHP and trade) /* unmodified */ /* XXX */
        hours(r,l)              hours per load segment /* constant */
        maxCF(r,u,l)            maximum capacity factor /* unmodified */
        /*peak(r,t)*/               peak demand /* unmodified */
        /*rsrv_factor(r,u)*/        reserve margin contribution factor for wind-solar /* unmodified */
        transmit_limit(r,rr)    transmission limits /* unmodified */

        pbio(r,biostep,t)       price of biomass by step function /* unmodified */ /* XXX */
        biosupply(r,biostep,t)  supply of biomass by step function /* unmodified */
;

* Features with region dimensions
** From MEEDE - capacity, dele, fomcost, heatrate, pf, vomcost
** Not from MEEDE - biosupply (tricky), 
* dadj (easy-just assume constant w/i region), 
* emis_factor (easy-just assume constant w/i region), 
* hours (easy-just assume constant w/i region), 
* loadpct, (easy-just assume constant w/i region)
* maxCF, (easy-just assume constant w/i region)
* pbio, (easy I think...-just assume constant w/i region) 
* peak, (think i should have gotten rid of this)
* transmit_limit, (tricky...assume no max transmission between states within regions?)

* Features without region dimensions
** From MEEDE
** Not from MEEDE - fueltype

* Load in New data from MEEDE -------------------
$gdxin '..\Data\output\MEEDE_capacity.gdx'
$loaddc capacity

$gdxin '..\Data\output\MEEDE_fomcost.gdx'
$loaddc fomcost

$gdxin '..\Data\output\MEEDE_vomcost.gdx'
$loaddc vomcost

$gdxin '..\Data\output\MEEDE_heatrate.gdx'
$loaddc heatrate

$gdxin '..\Data\output\MEEDE_dele.gdx'
$loaddc dele

$gdxin '..\Data\output\MEEDE_pf.gdx'
$loaddc pf

* Load in remaining data unmodified
$gdxin '..\Data\output\biosupply_2020.gdx'
$loaddc biosupply

$gdxin '..\Data\output\dadj_2020.gdx'
$loaddc dadj

$gdxin '..\Data\output\pbio_2020.gdx'
$loaddc pbio

$gdxin '..\Data\output\maxCF_2020.gdx'
$loaddc maxCF

$gdxin '..\Data\output\hours_2020.gdx'
$loaddc hours

$gdxin '..\Data\output\transmit_limit_2020.gdx'
$loaddc transmit_limit

$gdxin '..\Data\output\loadpct_2020.gdx'
$loaddc loadpct

$gdxin '..\Data\output\emis_factor_2020.gdx'
$loaddc emis_factor

/*$gdxin '..\Data\output\peak_2020.gdx'
$loaddc peak */

$gdxin '..\Data\output\model_data.gdx'
$loaddc /*maxCF*/ /*hours*/ /*transmit_limit*/ /*rsrv_factor*/ /*loadpct*/ fueltype /*emis_factor*/

biosupply(r,biostep,t)  = biosupply(r,biostep,t) * 1e3; /* scaling constant */

* misc *
feasible(r,u,"2020",t)$capacity(r,u,"2020")     = yes;

