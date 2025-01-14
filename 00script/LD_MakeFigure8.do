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

* 创建一个postfile，用于保存回归系数
postfile MakeFigure8 T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 model using "01data\MakeFigure8.dta", replace


forvalues i = 1/1000 {
    * 随机抽取m-1个地区
use GDPpc1.dta,replace    
bsample, cluster(country1)
sort gdlcode1 year
bysort gdlcode1 year: gen seq = _n
gsort gdlcode1 seq year
egen  region = seq(), from(1) block(26)
qui {
xtset region year 
reghdfe dgdp c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre  c.pre##c.pre [aweight=weight_r],absorb(i.year gdlcode1##c.year) vce( cluster country1) 
	
		forvalues j = 0/30 {
		    lincom (c.tem#c.tem)*2*`j' + c.tem
		    scalar T_`j' = r(estimate)
		}
		
		scalar model = 1

		post MakeFigure8 (T_0) (T_1) (T_2) (T_3) (T_4) (T_5) (T_6) (T_7) (T_8) (T_9) (T_10) (T_11) (T_12) (T_13) (T_14) (T_15) (T_16) (T_17) (T_18) (T_19) (T_20) (T_21) (T_22) (T_23) (T_24) (T_25) (T_26) (T_27) (T_28) (T_29) (T_30) (model)

local deltaT = 10
gen N_usd = gdppc

sum year
gen y_max = 2015

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')

collapse(mean) gdppc tem pre ///
   (first) country1 weight_r ///
   (count) N_usd  ///
   , by(region period)
   
keep if N_usd>= floor(`deltaT'/4)
sum period
gen T_max = r(max)

gen year = period
xtset region year 

gen dT=tem-l.tem
gen dP=pre-l.pre
gen lngdp = log(gdp)
gen dgdp= lngdp-l.lngdp

egen gdppc_mean = mean(gdppc), by(region)
summ gdppc_mean [aweight=weight_r],detail
gen poor = 1 if gdppc_mean <= r(p50) 
replace poor = 0 if gdppc_mean > r(p50) 

reghdfe dgdp c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre  c.pre##c.pre [aweight=weight_r],absorb(i.year i.region) vce( cluster country1) 
	
forvalues j = 0/30 {
		    lincom (c.tem#c.tem)*2*`j' + c.tem
		    scalar T_`j' = r(estimate)
		}
		
		scalar model = 2
		
		post MakeFigure8 (T_0) (T_1) (T_2) (T_3) (T_4) (T_5) (T_6) (T_7) (T_8) (T_9) (T_10) (T_11) (T_12) (T_13) (T_14) (T_15) (T_16) (T_17) (T_18) (T_19) (T_20) (T_21) (T_22) (T_23) (T_24) (T_25) (T_26) (T_27) (T_28) (T_29) (T_30) (model)
}
dis `i'
}

postclose MakeFigure8

use 01data\MakeFigure8.dta,clear 
preserve
keep if model ==1
tempfile stats
postfile stats mean lb ub using `stats'
forval i = 0/30 {
    qui {
        local varname T`i'
        summarize `varname',detail
        local median = r(p50)
        local se = r(sd) 

        local lb = `median' - 1.645*`se'
        local ub = `median' + 1.645*`se'

        post stats  (`median') (`lb') (`ub') 
	}
}
postclose stats

* 读取临时文件
use `stats', clear

gen tem = _n-1

twoway /// 
(scatter mean tem, msymbol(o) msize(small) mcolor(black)  ) ///  
(rcap lb ub tem, lcolor(black)),  /// 
xtitle("Temperature (°C)") /// 
ylabel(-0.04(0.02)0.04, ) ///
title("Panel A: Panel Estimates", color(black) ) ///
legend(off) graphregion(color(white))
graph save Graph "03figure\Figure8_1.gph", replace      
restore

clear all
use 01data\MakeFigure8.dta,clear 
preserve
keep if model ==2
tempfile stats
postfile stats mean lb ub using `stats'
forval i = 0/30 {
    qui {
        local varname T`i'
        summarize `varname',detail
        local median = r(p50)
        local se = r(sd) 

        local lb = `median' - 1.645*`se'
        local ub = `median' + 1.645*`se'

        post stats  (`median') (`lb') (`ub') 
	}
}
postclose stats

* 读取临时文件
use `stats', clear

gen tem = _n-1

twoway /// 
(scatter mean tem, msymbol(o) msize(small) mcolor(black)  ) ///  
(rcap lb ub tem, lcolor(black)),  /// 
xtitle("Temperature (°C)") /// 
ylabel(-1.2(0.4)1.2, ) ///
title("Panel B: Long-difference Estimates", color(black) ) ///
legend(off) graphregion(color(white))
graph save Graph "03figure\Figure8_2.gph", replace      
restore
graph combine 03figure\Figure8_1.gph 03figure\Figure8_2.gph, cols(2)ysize(2)  imargin(vsmall) graphregion(color(white))