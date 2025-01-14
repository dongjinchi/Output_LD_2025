clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\output_ld
insheet using GDPpc1.csv ,clear

*****************************************
*      panel regression for GDPpc       *
*****************************************

*Variable generation
rename area_ID gdlcode1
encode country, gen(country1)
xtset gdlcode1 year
gen year2 = year*year

gen lngdp = log(gdppc)
gen dgdp = d.lngdp

gen dT = d.tem
gen dP = d.pre

keep if year <=2014
 reghdfe dgdp c.l(0/1).dT c.l(0/1).dT#c.tem  c.tem##c.tem c.l(0/1).dP c.l(0/1).dP#c.pre c.pre##c.pre [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
estimates store r1

preserve
local deltaT = 10
gen N_usd = gdppc

sum year
gen y_max = 2014

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')


collapse(mean) gdp tem pre ///
   (first) country1 weight_r ///
   (count) N_usd  ///
   , by(gdlcode1 period)

keep if N_usd>= floor(`deltaT'/4)

sum period
gen T_max = r(max)

gen year = period
xtset gdlcode1 year  

gen dT=tem-l.tem
gen dP=pre-l.pre
gen lngdp = log(gdppc)
gen dgdp= lngdp-l.lngdp

reghdfe dgdp c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre c.pre##c.pre [aweight=weight_r] ,absorb(i.gdlcode1 i.year) vce( cluster country1) 
estimates store r3
restore

keep if kalkuhl ==1
 reghdfe dgdp c.l(0/1).dT c.l(0/1).dT#c.tem  c.tem##c.tem c.l(0/1).dP c.l(0/1).dP#c.pre c.pre##c.pre [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
estimates store r2

local deltaT = 10
gen N_usd = gdppc

sum year
gen y_max = 2014

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')


collapse(mean) gdp tem pre ///
   (first) country1 weight_r ///
   (count) N_usd  ///
   , by(gdlcode1 period)

*keep if N_usd>= floor(`deltaT'/4)

sum period
gen T_max = r(max)

gen year = period
xtset gdlcode1 year  

gen dT=tem-l.tem
gen dP=pre-l.pre
gen lngdp = log(gdppc)
gen dgdp= lngdp-l.lngdp

reghdfe dgdp c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre c.pre##c.pre [aweight=weight_r] ,absorb(i.country1) vce( cluster country1) 
estimates store r4

use 01data\kalkuhl_gdppc.dta,clear
egen province_id = group(ID)
egen num_max = max(province_id), by(wrld1id_0)
egen num_min = min(province_id), by(wrld1id_0)
replace num_max =. if (num_max ==1544 & wrld1id_0 !=250)
replace num_min =. if (num_min ==1 & wrld1id_0 !=4)
gen num_prov = num_max-num_min +1
gen weight_r = 1/num_prov
drop province_id num_max num_min num_prov
drop if year > 2014

gen dlgdp_pc_usd = d.lgdp_pc_usd
gen year_sqr = year*year
gen dT = d.temp
gen dP = d.prec

rename temp tem
rename prec pre

keep if year >= 1991

reghdfe dlgdp_pc_usd  c.l(0/1).dT c.l(0/1).dT#c.tem c.tem##c.tem c.l(0/1).dP c.l(0/1).dP#c.pre c.pre##c.pre   [aweight=weight_r],absorb(i.StructChange i.year ID##c.year) vce(cluster wrld1id_0)
estimates store r5
 local deltaT = 10
gen N_usd = lgdp_pc_usd

sum year
gen y_max = 2014

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')


collapse(mean) lgdp_pc_usd tem pre ///
   (first) wrld1id_0 weight_r ///
   (count) N_usd  ///
   , by(ID period)

keep if N_usd>= floor(`deltaT'/4)

sum period
gen T_max = r(max)

gen year = period
xtset ID year  

gen dlgdp_pc_usd=d.lgdp_pc_usd
gen dT=d.tem
gen dP=d.pre

reghdfe dlgdp_pc_usd c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre c.pre##c.pre [aweight=weight_r],absorb(i.wrld1id_0) vce( cluster wrld1id_0) 
estimates store r6
esttab r1 r2 r3 r4 r5 r6 using "02table\Table5.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)

