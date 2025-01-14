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


egen total_area = total(area), by(country year)
egen weighted_tem = total(tem * area), by(country year)
egen weighted_pre = total(pre * area), by(country year)
gen country_tem = weighted_tem / total_area
gen country_pre = weighted_pre / total_area

egen total_pop = total(pop), by(country year)
egen weighted_gdppc = total(gdppc * pop), by(country year)
gen country_gdppc = weighted_gdppc / total_pop

collapse (mean) country_tem country_pre country_gdppc ///
(first) country  iso_code, by(country1 year)

rename country_tem tem 
rename country_pre pre
rename country_gdppc gdppc

replace iso_code = "CHN_tw" if country =="Taiwan"
   
merge 1:1 iso_code year  using "01data\country_GDPpc.dta",
drop if _merge==2
xtset country1 year
gen lngdp = log(gdppc)
gen dgdp = d.lngdp
gen dT = d.tem
gen dP = d.pre

reghdfe dgdp dT c.dT#c.tem c.tem##c.tem dP c.dP#c.pre c.pre##c.pre ,absorb(i.year country1##c.year ) vce( cluster country1) 
estimates store r1

preserve
drop if missing(gdppc_wb, gdppc)
reghdfe dgdp dT c.dT#c.tem c.tem##c.tem dP c.dP#c.pre c.pre##c.pre ,absorb(i.year country1##c.year ) vce( cluster country1) 
estimates store r2

replace lngdp = log(gdppc_wb)
replace dgdp = d.lngdp
reghdfe dgdp dT c.dT#c.tem c.tem##c.tem dP c.dP#c.pre c.pre##c.pre ,absorb(i.year country1##c.year ) vce( cluster country1) 
estimates store r3
restore

local deltaT = 10
gen N_usd = gdppc

sum year
gen y_max = 2015

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')


collapse(mean) gdppc gdppc_wb tem pre ///
   (first) country  iso_code ///
   (count) N_usd  ///
   , by(country1 period)

keep if N_usd>= floor(`deltaT'/2)

sum period
gen T_max = r(max)

gen year = period
xtset country1 year 

gen dT=tem-l.tem
gen dP=pre-l.pre
gen lngdp = log(gdppc)
gen dgdp= lngdp-l.lngdp

reghdfe dgdp dT c.dT#c.tem c.tem##c.tem dP c.dP#c.pre c.pre##c.pre ,absorb(i.year i.country1 ) vce( cluster country1) 
estimates store r4
preserve
drop if missing(gdppc_wb, gdppc)
reghdfe dgdp dT c.dT#c.tem c.tem##c.tem dP c.dP#c.pre c.pre##c.pre ,absorb(i.year i.country1 ) vce( cluster country1) 
estimates store r5

replace lngdp = log(gdppc_wb)
replace dgdp= lngdp-l.lngdp
reghdfe dgdp dT c.dT#c.tem c.tem##c.tem dP c.dP#c.pre c.pre##c.pre ,absorb(i.year i.country1 ) vce( cluster country1) 
estimates store r6
restore

esttab r1 r2 r3 r4 r5 r6 using "02table\Table4.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)