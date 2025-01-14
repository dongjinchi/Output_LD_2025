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

drop if missing(gdppc, tem, pre)

local deltaT = 10
gen N_usd = gdp

sum year
gen y_max = 2015

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')


collapse(mean) gdp tem pre pop urb edu ///
   (first) country country1 weight_r ///
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

local vars dT tem dP pre
local weights weight_r pop
matrix results = J(124, 5, .) 
foreach var of local vars{
local index =1	
preserve
foreach weight of local weights{
qui	reghdfe dgdp dT c.dT#c.tem c.tem##c.tem dP c.dP#c.pre c.pre##c.pre  [aweight=`weight'],absorb(i.year i.gdlcode1) vce( cluster country1) 

forvalues i = 0/30 {
	if "`var'" == "dT"{
		local i = `i'
		local marg = "c.dT+`i'*c.dT#c.tem"
		local title = "Panel A: Marginal Level Effects of Temperature"
		local xtitle = "Temperature(℃)"
	}
	if "`var'" == "tem"{
		local i = `i'
		local marg = "c.tem+2*`i'*c.tem#c.tem"
		local title = "Panel B: Marginal Growth Effects of Temperature"
		local xtitle = "Temperature(℃)"
	}
	else if "`var'" == "dP"{
		local i = `i'/10
		local marg = "c.dP+`i'*c.dP#c.pre"
		local title = "Panel C: Marginal Level Effects of Precipitation"
		local xtitle = "Precipitation(m)"
	}
	if "`var'" == "pre"{
		local i = `i'/10
		local marg = "c.pre+2*`i'*c.pre#c.pre"
		local title = "Panel D: Marginal Growth Effects of Precipitation"
		local xtitle = "Precipitation(m)"
	}
	
	qui lincom `marg'
	local T_`index' = r(estimate)
	local T_`index'_se= r(se)
	local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
    local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
    * 将结果存储到矩阵
    matrix results[`index', 1] = `i'
    matrix results[`index', 2] = `T_`index''
    matrix results[`index', 3] = `ci_lower_`index''
    matrix results[`index', 4] = `ci_upper_`index''
	if "`weight'" == "weight_r"{
		local code = 1
	}
	else if "`weight'" == "pop"{
		local code = 2
	}
	matrix results[`index', 5] = `code'
	local index = `index'+1
}
}
* 将矩阵保存到一个新数据集
svmat results, names(col)
twoway ///
(rarea c3 c4 c1 if c5 == 1, fcolor( "252 141 98%30") lcolor(%0) ) ///
(line c2 c1 if c5 == 1, lcolor("252 141 98") )  ///
(rarea c3 c4 c1 if c5 == 2, fcolor( "60 180 160%30") lcolor(%0) ) ///
(line c2 c1  if c5 == 2,lcolor("60 180 160") )  ///
 , ///
ylabel(-1.2(0.4)1.2, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off) title(`title',color(black)) xtitle(`xtitle',color(black) size(medium)) graphregion(color(white))
graph save Graph "03figure\Figure3_`var'.gph",replace
restore
}


graph combine 03figure\Figure3_dT.gph 03figure\Figure3_tem.gph 03figure\Figure3_dP.gph 03figure\Figure3_pre.gph, cols(2)  imargin(vsmall) graphregion(color(white))
graph save Graph "03figure\Figure3.png", replace


histogram tem, yscale(off) ylabel(,nogrid) xtitle("")  graphregion(color(white)) fcolor("40 120 180") lcolor("40 120 180")
graph save Graph "03figure\Figure3_hist_tem.eps", replace

histogram pre, yscale(off) ylabel(,nogrid) xtitle("") xlabel(0(1)3) graphregion(color(white)) fcolor("40 120 180") lcolor("40 120 180")
graph save Graph "03figure\Figure3_hist_pre.eps", replace

