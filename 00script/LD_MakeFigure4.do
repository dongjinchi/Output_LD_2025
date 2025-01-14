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

gen bin  = floor(year/10)
egen gdppc_mean = mean(gdppc), by(gdlcode1 bin)
pctile pct = gdppc_mean [aweight=weight_r], nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50]


local vars tem
matrix results = J(62, 4, .) 
foreach var of local vars{
local index =1	
preserve
reghdfe dgdp dT c.dT#c.tem  dP c.dP#c.pre poor#c.(c.tem##c.tem c.pre##c.pre)  [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
forvalues i = 0/30 {
	forvalues j=0/1{
		if "`var'" == "dT"{
			local title = "Panel A: Marginal Level Effects-Panel Estimates"
			local marginal = "`j'.poor#c.dT+`i'*(`j'.poor#c.dT#c.tem)"
}
else if "`var'" == "tem"{
			local title = "Panel A: Marginal Growth Effects-Panel Estimates"
			local marginal = "`j'.poor#c.tem+2*`i'*(`j'.poor#c.tem#c.tem)"
}
		qui lincom "`marginal'"
		local T_`index' = r(estimate)
		local T_`index'_se= r(se)
		local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
		local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
		* 将结果存储到矩阵
		matrix results[`index', 1] = `i'
		matrix results[`index', 2] = `T_`index''
		matrix results[`index', 3] = `ci_lower_`index''
		matrix results[`index', 4] = `ci_upper_`index''
		local index = `index'+1
	}
}
svmat results, names(col)
egen  categ = seq(), from(0) to(1)
twoway ///
(line c2 c1 if categ ==0 , lcolor("252 141 98") ) ///
(line c2 c1 if categ ==1 , lcolor("60 180 160") ) ///
(rarea c3 c4 c1 if categ ==0 , fcolor("252 141 98%30") lcolor(%0) msize(small )) ///
(rarea c3 c4 c1 if categ ==1 , fcolor("60 180 160%30") lcolor(%0) msize(small )) ///
 , ///
ylabel(-0.06(0.02)0.06, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(order(3 "Poor regions" 4 "Rich regions") pos(4) ring(0) col(3) bmargin(medium) region(lc(gs15)) lw(none)) title(`title',color(black)) xtitle("Temperature(℃)",color(black) size(medium)) graphregion(color(white))
graph save Graph "03figure\Figure4_panel_`var'.gph",replace
restore
}

local deltaT = 10
gen N_usd = gdppc

sum year
gen y_max = 2015

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')


collapse(mean) gdppc tem pre pop urb edu ///
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

pctile pct = gdppc[aweight=weight_r], nq(100)
gen poor = 0 if gdppc <= pct[50]
replace poor = 1 if gdppc >pct[50]

local vars tem
matrix results = J(62, 4, .) 
foreach var of local vars{
local index =1	
preserve
 reghdfe dgdp dT c.dT#c.tem  dP c.dP#c.pre poor#c.(c.tem##c.tem c.pre##c.pre)  [aweight=weight_r],absorb(i.year i.gdlcode1) vce( cluster country1) 

forvalues i = 0/30 {
	forvalues j=0/1{
		if "`var'" == "dT"{
			local title = "Panel B: Marginal Level Effects-Long difference Estimates"
			local marginal = "`j'.poor#c.dT+`i'*(`j'.poor#c.dT#c.tem)"
}
		else if "`var'" == "tem"{
			local title = "Panel B: Marginal Growth Effects-Long difference Estimates"
			local marginal = "`j'.poor#c.tem+2*`i'*(`j'.poor#c.tem#c.tem)"
}
		qui lincom "`marginal'"
		local T_`index' = r(estimate)
		local T_`index'_se= r(se)
		local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
		local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
		* 将结果存储到矩阵
		matrix results[`index', 1] = `i'
		matrix results[`index', 2] = `T_`index''
		matrix results[`index', 3] = `ci_lower_`index''
		matrix results[`index', 4] = `ci_upper_`index''
		local index = `index'+1
	}
}
svmat results, names(col)
egen  categ = seq(), from(0) to(1)
twoway ///
(line c2 c1 if categ ==0 , lcolor("252 141 98") ) ///
(line c2 c1 if categ ==1 , lcolor("60 180 160") ) ///
(rarea c3 c4 c1 if categ ==0 , fcolor("252 141 98%30") lcolor(%0) msize(small )) ///
(rarea c3 c4 c1 if categ ==1 , fcolor("60 180 160%30") lcolor(%0) msize(small )) ///
 , ///
ylabel(-0.8(0.2)0.8, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
legend(order(3 "Poor regions" 4 "Rich regions") pos(4) ring(0) col(3) bmargin(medium) region(lc(gs15)) lw(none)) title(`title',color(black)) xtitle("Temperature(℃)",color(black) size(medium)) graphregion(color(white))
graph save Graph "03figure\Figure4_ld_`var'.gph",replace
restore
}

graph combine 03figure\Figure4_panel_tem.gph 03figure\Figure4_ld_tem.gph, cols(2) imargin(vsmall) graphregion(color(white))
*graph save Graph "03figure\Figure4.png", replace
