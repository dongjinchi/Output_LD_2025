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
gen bin_dT  = floor(dT/0.4)
replace bin_dT = bin_dT+9
drop if missing(gdppc, tem, pre)


gen bin_tem  = floor(tem/3)
replace bin_tem = -1 if bin_tem < 0
replace bin_tem = bin_tem+1
gen bin_pre  = floor(pre/0.2)
replace bin_pre = 11 if bin_pre >10 

reghdfe dgdp ib5.bin_tem ib6.bin_pre [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 

gen id =.
gen coef = .
gen lb = .
gen ub = .

forval i = 0/10 {
    * 提取系数
    scalar coef_val = _b[`i'.bin_tem]
    
    * 提取标准误
    scalar se_val = _se[`i'.bin_tem]
    
    * 计算置信区间
    scalar lb_val = coef_val - invttail(e(df_r), 0.05) * se_val
    scalar ub_val = coef_val + invttail(e(df_r), 0.05) * se_val
    
    * 将结果赋值给新变量
	local index = `i'+1
	replace id = `i' in `index'
    replace coef = coef_val in `index'
    replace lb = lb_val in `index'
    replace ub = ub_val in `index'
}

twoway (rarea ub lb id , lw(0) color(gs12%50)) ///
	   (line coef id , lp(solid) lw(0.4) lcolor(red)), ///
	   yline(0, lp(longdash) lc(gs8)) xtitle("Fahrenheit") ytitle("GDP per capita Growth", m(small))  xlabel(0(1)10)  xtitle("")  ///
	   title("Panel A: Temperature Effect(℃)", color(black) ) ///
	   legend(order(3 "Point Estimates" 1 "95% CI") pos(4) ring(0) col(3) bmargin(medium) region(lc(gs15)) lw(none)) ///
	   graphregion(c(white)) ylabel(-0.15(0.03)0.03, nogrid) yscale(range(-0.15 0.03))  ///
	   xlabel(0 "[-20,0)" 1 "[0,3)" 2 "[3,6)" 3 "[6,9)" 4 "[9,12)" 5 "[12,15)" 6 "[15,18)" 7 "[18,21)" 8 "[21,24)" 9 "[24,27)" 10 "[27,30]", angle(45))	
graph save Graph "03figure\Figure6_1.gph", replace      

forval i = 0/11 {
    * 提取系数
    scalar coef_val = _b[`i'.bin_pre]
    
    * 提取标准误
    scalar se_val = _se[`i'.bin_pre]
    
    * 计算置信区间
    scalar lb_val = coef_val - invttail(e(df_r), 0.05) * se_val
    scalar ub_val = coef_val + invttail(e(df_r), 0.05) * se_val
    
    * 将结果赋值给新变量
	local index = `i'+1
	replace id = `i' in `index'
    replace coef = coef_val in `index'
    replace lb = lb_val in `index'
    replace ub = ub_val in `index'
}

twoway (rarea ub lb id , lw(0) color(gs12%50)) ///
	   (line coef id , lp(solid) lw(0.4) lcolor(red)), ///
	   yline(0, lp(longdash) lc(gs8)) xtitle("Fahrenheit") ytitle("GDP per capita Growth", m(small))  xlabel(0(1)10)  xtitle("")  ///
	   title("Panel B: Precipitation Effect(m)", color(black) ) ///
	   legend(order(3 "Point Estimates" 1 "95% CI") pos(4) ring(0) col(3) bmargin(medium) region(lc(gs15)) lw(none)) ///
	   graphregion(c(white)) ylabel(-0.03(0.01)0.01, nogrid) yscale(range(-0.03 0.01))  ///
	   xlabel(0 "[0,0.2)" 1 "[0.2,0.4)" 2 "[0.4,0.6)" 3 "[0.6,0.8)" 4 "[0.8,1.0)" 5 "[1.0,1.2)" 6 "[1.2,1.4)" 7 "[1.4,1.6)" 8 "[1.6,1.8)" 9 "[1.8,2.0)" 10 "[2.0,2.2)" 11 "[2.2,6.4]", angle(45))	
graph save Graph "03figure\Figure6_2.gph", replace      
   
graph combine 03figure\Figure6_1.gph 03figure\Figure6_2.gph, cols(2) ysize(2.5) imargin(vsmall) graphregion(color(white))
	   
	   
	   
	   
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

keep if N_usd>= floor(`deltaT'/4)

sum period
gen T_max = r(max)

gen year = period
xtset gdlcode1 year 

gen dT=tem-l.tem
gen dP=pre-l.pre
gen lngdp = log(gdp)
gen dgdp= lngdp-l.lngdp


gen bin_tem  = floor(tem/3)
replace bin_tem = -1 if bin_tem < 0
replace bin_tem = bin_tem+1
gen bin_pre  = floor(pre/0.2)
replace bin_pre = 11 if bin_pre >10 

reghdfe dgdp ib5.bin_tem ib6.bin_pre [aweight=weight_r],absorb(i.year i.gdlcode1) vce( cluster country1) 

gen id =.
gen coef = .
gen lb = .
gen ub = .

forval i = 0/10 {
    * 提取系数
    scalar coef_val = _b[`i'.bin_tem]
    
    * 提取标准误
    scalar se_val = _se[`i'.bin_tem]
    
    * 计算置信区间
    scalar lb_val = coef_val - invttail(e(df_r), 0.1) * se_val
    scalar ub_val = coef_val + invttail(e(df_r), 0.1) * se_val
    
    * 将结果赋值给新变量
	local index = `i'+1
	replace id = `i' in `index'
    replace coef = coef_val in `index'
    replace lb = lb_val in `index'
    replace ub = ub_val in `index'
}

twoway (rarea ub lb id , lw(0) color(gs12%50)) ///
	   (line coef id , lp(solid) lw(0.4) lcolor(red)), ///
	   yline(0, lp(longdash) lc(gs8)) xtitle("Fahrenheit") ytitle("GDP per capita Growth", m(small))  xlabel(0(1)10)  xtitle("Temperature(℃)")  ///
	   legend(order(3 "Point Estimates" 1 "90% CI") pos(4) ring(0) col(3) bmargin(medium) region(lc(gs15)) lw(none)) ///
	   graphregion(c(white)) ylabel(-1.0(0.2)0.4, nogrid) yscale(range(-1.0 0.4))  ///
	   xlabel(0 "[-20,0)" 1 "[0,3)" 2 "[3,6)" 3 "[6,9)" 4 "[9,12)" 5 "[12,15)" 6 "[15,18)" 7 "[18,21)" 8 "[21,24)" 9 "[24,27)" 10 "[27,30]", angle(45))	

forval i = 0/11 {
    * 提取系数
    scalar coef_val = _b[`i'.bin_pre]
    
    * 提取标准误
    scalar se_val = _se[`i'.bin_pre]
    
    * 计算置信区间
    scalar lb_val = coef_val - invttail(e(df_r), 0.05) * se_val
    scalar ub_val = coef_val + invttail(e(df_r), 0.05) * se_val
    
    * 将结果赋值给新变量
	local index = `i'+1
	replace id = `i' in `index'
    replace coef = coef_val in `index'
    replace lb = lb_val in `index'
    replace ub = ub_val in `index'
}

twoway (rarea ub lb id , lw(0) color(gs12%50)) ///
	   (line coef id , lp(solid) lw(0.4) lcolor(red)), ///
	   yline(0, lp(longdash) lc(gs8)) xtitle("Fahrenheit") ytitle("GDP per capita Growth", m(small))  xlabel(0(1)10)  xtitle("Precipitation(m)")  ///
	   legend(order(3 "Point Estimates" 1 "95% CI") pos(4) ring(0) col(3) bmargin(medium) region(lc(gs15)) lw(none)) ///
	   graphregion(c(white)) ylabel(-0.6(0.2)0.6, nogrid) yscale(range(-0.6 0.6))  ///
	   xlabel(0 "[0,0.2)" 1 "[0.2,0.4)" 2 "[0.4,0.6)" 3 "[0.6,0.8)" 4 "[0.8,1.0)" 5 "[1.0,1.2)" 6 "[1.2,1.4)" 7 "[1.4,1.6)" 8 "[1.6,1.8)" 9 "[1.8,2.0)" 10 "[2.0,2.2)" 11 "[2.2,6.4]", angle(45))	