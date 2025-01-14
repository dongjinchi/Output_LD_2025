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

qui{
reghdfe dgdp  c.tem##c.tem c.pre##c.pre [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
estimates store r1

reghdfe dgdp c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre  c.pre##c.pre [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
estimates store r2

reghdfe dgdp dT c.dT#c.tem  c.tem##c.tem dP c.dP#c.pre c.pre##c.pre  edu urb pop [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
estimates store r3

preserve
drop if missing(edu, urb, pop)
reghdfe dgdp dT c.dT#c.tem c.tem##c.tem dP c.dP#c.pre c.pre##c.pre [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
estimates store r4
restore

reghdfe dgdp c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre  c.pre##c.pre [aweight=pop],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
estimates store r5

reghdfe dgdp dT c.dT#c.tem  c.tem##c.tem dP c.dP#c.pre c.pre##c.pre  edu urb pop [aweight=pop],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
estimates store r6
}
esttab r1 r2 r3 r4 r5 r6 using "02table\Table1.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)
