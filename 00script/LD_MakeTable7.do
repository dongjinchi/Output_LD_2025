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

replace pre_era=365*pre_era

gen lngdp = log(gdppc)
gen dgdp = d.lngdp

gen dT = d.tem
gen dP = d.pre

drop if missing(gdppc, tem, pre)

reghdfe dgdp c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre  c.pre##c.pre [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
estimates store r1

replace dT = d.tem_era
replace dP = d.pre_era

reghdfe dgdp c.dT c.dT#c.tem_era  c.tem_era##c.tem_era c.dP c.dP#c.pre_era  c.pre_era##c.pre_era [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
estimates store r2



local deltaT = 10
gen N_usd = gdp

sum year
gen y_max = 2015

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')


collapse(mean) gdp tem pre tem_era pre_era ///
   (first) country1 weight_r ///
   (count) N_usd  ///
   , by(gdlcode1 period)

keep if N_usd>= floor(`deltaT'/2)

sum period
gen T_max = r(max)

gen year = period
xtset gdlcode1 year 

gen dT=tem-l.tem
gen dP=pre-l.pre
gen lngdp = log(gdp)
gen dgdp= lngdp-l.lngdp

reghdfe dgdp dT c.dT#c.tem  c.tem##c.tem   dP c.dP#c.pre c.dP#c.pre   c.pre##c.pre [aweight=weight_r],absorb(i.gdlcode1 i.year) vce( cluster country1)
estimates store r3

replace dT=d.tem_era
replace dP=d.pre_era

reghdfe dgdp dT c.dT#c.tem_era  c.tem_era##c.tem_era   dP c.dP#c.pre_era c.dP#c.pre_era   c.pre_era##c.pre_era [aweight=weight_r],absorb(i.gdlcode1 i.year) vce( cluster country1)
estimates store r4

esttab r1 r2 r3 r4 using "02table\Table6.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)
