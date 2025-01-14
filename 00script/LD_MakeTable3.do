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

drop if missing(gdppc, tem, pre)

local weights weight_r pop
local gaps 5 10
foreach weight of local weights{
foreach gap of local gaps{
if `gap' == 5  {
	local absorb = "i.year gdlcode1##c.year"
} 
else if `gap' == 10 {
	local absorb = "i.year i.gdlcode1"
}
preserve
local deltaT = `gap'
gen N_usd = gdp

sum year
gen y_max = 2015

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')


collapse(mean) gdp tem pre pop urb edu ///
   (first) country1 weight_r ///
   (count) N_usd  ///
   , by(gdlcode1 period)

keep if N_usd>= 2

sum period
gen T_max = r(max)

gen year = period
xtset gdlcode1 year 

gen dT=tem-l.tem
gen dP=pre-l.pre
gen lngdp = log(gdp)
gen dgdp= lngdp-l.lngdp

qui reghdfe dgdp dT c.dT#c.tem  c.tem##c.tem   dP c.dP#c.pre c.dP#c.pre   c.pre##c.pre [aweight=`weight'],absorb("`absorb'") vce( cluster country1)
estimates store r1_`weight'_`gap'

qui reghdfe dgdp dT c.dT#c.tem  c.tem##c.tem   dP c.dP#c.pre c.dP#c.pre   c.pre##c.pre [aweight=`weight'],absorb("`absorb'") vce( cluster country1)
estimates store r2_`weight'_`gap'

reghdfe dgdp dT c.dT#c.tem c.tem##c.tem   dP c.dP#c.pre c.dP#c.pre c.pre##c.pre edu urb pop [aweight=`weight'],absorb("`absorb'") vce( cluster country1)
estimates store r3_`weight'_`gap'_cont

restore
}
}
esttab r1_weight_r_5 r2_weight_r_5 r2_weight_r_10 r3_weight_r_10_cont r2_pop_10 r3_pop_10_cont using "02table\Table2.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)