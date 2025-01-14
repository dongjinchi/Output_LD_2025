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

gen tem_sd1 = tem_sd+1
gen bin_tem_sd = floor(tem_sd1/2)
replace bin_tem_sd = -2 if bin_tem_sd <-2
replace bin_tem_sd = 2 if bin_tem_sd >2
replace bin_tem_sd = bin_tem_sd+2

gen pre_sd1 = pre_sd+1
gen bin_pre_sd = floor(pre_sd1/2)
replace bin_pre_sd = -2 if bin_pre_sd <-2
replace bin_pre_sd = 2 if bin_pre_sd >2
replace bin_pre_sd = bin_pre_sd+2

reghdfe dgdp c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre  c.pre##c.pre ib2.bin_tem_sd ib2.bin_pre_sd [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 

local vars tem pre
foreach var of local vars{
preserve
gen id =.
gen coef = .
gen lb = .
gen ub = .
	if "`var'" == "tem" {
		local title = "Panel A: Panel Estimates of AST Effect"
	}
	else if "`var'" == "pre" {
		local title = "Panel B: Panel Estimates of ASP Effect"
	}

	forval i = 0/4 {
    * 提取系数
    scalar coef_val = _b[`i'.bin_`var'_sd]
    
    * 提取标准误
    scalar se_val = _se[`i'.bin_`var'_sd]
    
    * 计算置信区间
    scalar lb_val = coef_val - 1.645 * se_val
    scalar ub_val = coef_val + 1.645 * se_val
    
    * 将结果赋值给新变量
	local index = `i'+1
	local id = `i'-2
	replace id = `id' in `index'
    replace coef = coef_val in `index'
    replace lb = lb_val in `index'
    replace ub = ub_val in `index'
}

twoway (rarea ub lb id , lw(0) color(gs12%50)) ///
	   (line coef id , lp(solid) lw(0.4) lcolor(red)), ///
	   yline(0, lp(longdash) lc(gs8)) xtitle("Fahrenheit") ytitle("GDP per capita Growth", m(small))  xlabel(-2(1)2)  xtitle("")  ///
	   title(`title', color(black) ) ///
	   legend(off) ///
	   graphregion(c(white)) ylabel(-0.04(0.02)0.04, nogrid)
graph save Graph "03figure\Figure9_panel_`var'.gph", replace   	   
restore  
}

local deltaT = 10
gen N_usd = gdp

sum year
gen y_max = 2015

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')


collapse(mean) gdp tem pre tem_sd pre_sd ///
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
gen lngdp = log(gdp)
gen dgdp= lngdp-l.lngdp

drop if missing(dgdp, tem, pre)

replace tem_sd = tem_sd*10+1
gen bin_tem_sd = floor(tem_sd/2)
replace bin_tem_sd = 3 if bin_tem_sd >3
replace bin_tem_sd = -3 if bin_tem_sd <-3
replace bin_tem_sd = bin_tem_sd+3

replace pre_sd = pre_sd*10+1
gen bin_pre_sd = floor(pre_sd/2)
replace bin_pre_sd = 3 if bin_pre_sd >3
replace bin_pre_sd = -3 if bin_pre_sd <-3
replace bin_pre_sd = bin_pre_sd+3

reghdfe dgdp c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre  c.pre##c.pre  ib3.bin_tem_sd ib3.bin_pre_sd [aweight=weight_r],absorb(i.year i.gdlcode1) vce( cluster country1) 

local vars tem pre
foreach var of local vars{
preserve
gen id =.
gen coef = .
gen lb = .
gen ub = .
	if "`var'" == "tem" {
		local title = "Panel C: Long-difference Estimates of AST Effect"
	}
	else if "`var'" == "pre" {
		local title = "Panel D: Long-difference Estimates of ASP Effect"
	}

	forval i = 0/6 {
    * 提取系数
    scalar coef_val = _b[`i'.bin_`var'_sd]
    
    * 提取标准误
    scalar se_val = _se[`i'.bin_`var'_sd]
    
    * 计算置信区间
    scalar lb_val = coef_val - invttail(e(df_r), 0.1) * se_val
    scalar ub_val = coef_val + invttail(e(df_r), 0.1) * se_val
    
    * 将结果赋值给新变量
	local index = `i'+1
	local id = `i'-3
	replace id = `id' in `index'
    replace coef = coef_val in `index'
    replace lb = lb_val in `index'
    replace ub = ub_val in `index'
}

twoway (rarea ub lb id , lw(0) color(gs12%50)) ///
	   (line coef id , lp(solid) lw(0.4) lcolor(red)), ///
	   yline(0, lp(longdash) lc(gs8)) xtitle("Fahrenheit") ytitle("GDP per capita Growth", m(small))  xlabel(-3(1)3)  xtitle("")  ///
	   title(`title', color(black) ) ///
	   legend(off) ///
	   graphregion(c(white)) ylabel(, nogrid)
graph save Graph "03figure\Figure9_ld_`var'.gph", replace   	   
restore  
}
graph combine 03figure\Figure9_panel_tem.gph 03figure\Figure9_panel_pre.gph 03figure\Figure9_ld_tem.gph 03figure\Figure9_ld_pre.gph,  imargin(vsmall) graphregion(color(white))
	 