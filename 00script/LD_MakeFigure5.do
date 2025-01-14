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

postfile MakeFigure5 perc0 perc1 perc2 perc3 perc4 perc5 perc6 perc7 perc8 perc9 perc10 perc11 perc12 perc13 perc14 perc15 perc16 perc17 perc18 perc19 perc20 perc21 perc22 perc23 perc24 perc25 perc26 perc27 perc28 perc29 perc30  using "01data\MakeFigure5.dta", replace

forvalues i = 1/1000 {
    * 随机抽取m-1个地区
use GDPpc1.dta,replace    
bsample, cluster(country1)
sort gdlcode1 year
bysort gdlcode1 year: gen seq = _n
gsort gdlcode1 seq year
egen  region = seq(), from(1) block(26)

qui{
gen bin  = floor(year/10)
egen gdppc_mean = mean(gdppc), by(region bin)
pctile pct = gdppc_mean [aweight=weight_r], nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50]  

xtset region year 
 reghdfe dgdp dT c.dT#c.tem  dP c.dP#c.pre poor#c.(c.tem##c.tem c.pre##c.pre)  [aweight=weight_r],absorb(i.year region##c.year) vce( cluster country1) 
	
		forvalues j = 0/30 {
		    lincom (0.poor#c.tem#c.tem)*2*`j' + 0.poor#c.tem
		    scalar T_panel_`j' = r(estimate)
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
   , by(region period)

keep if N_usd>= 2

sum period
gen T_max = r(max)

gen year = period
xtset region year 

gen dT=tem-l.tem
gen dP=pre-l.pre
gen lngdp = log(gdp)
gen dgdp= lngdp-l.lngdp   

pctile pct = gdppc[aweight=weight_r], nq(100)
gen poor = 0 if gdppc <= pct[50]
replace poor = 1 if gdppc >pct[50]
 reghdfe dgdp dT c.dT#c.tem  dP c.dP#c.pre poor#c.(c.tem##c.tem c.pre##c.pre)  [aweight=weight_r],absorb(i.year i.region) vce( cluster country1) 
	
		forvalues j = 0/30 {
		    lincom 0.poor#c.tem+2*`j'*(0.poor#c.tem#c.tem)
		    scalar T_LD_`j' = ((1+r(estimate))^(1/10)-1)
			*scalar T_LD_`j' = r(estimate)/10
		
		}

forvalues j = 0/30 {
		    scalar perc_`j' = 1- (T_LD_`j'/T_panel_`j')
		}
		
}
dis `i'
		
		* 将临时变量的值存储到 MakeFigure4 中的新行
		post MakeFigure5 (perc_0) (perc_1) (perc_2) (perc_3) (perc_4) (perc_5) (perc_6) (perc_7) (perc_8) (perc_9) (perc_10) (perc_11) (perc_12) (perc_13) (perc_14) (perc_15) (perc_16) (perc_17) (perc_18) (perc_19) (perc_20) (perc_21) (perc_22) (perc_23) (perc_24) (perc_25) (perc_26) (perc_27) (perc_28) (perc_29) (perc_30)
	
}

* 关闭postfile
postclose MakeFigure5

use 01data\MakeFigure5.dta,clear

tempfile stats
postfile stats mean lb ub using `stats'

forval i = 0/30 {
    qui {
        local varname perc`i'
        
        summarize `varname',detail
        local mean = r(p50)
        scalar lb = r(p10)
        scalar ub = r(p90)

        post stats  (`mean') (lb) (ub)
    }
}

postclose stats

* 读取临时文件
use `stats', clear

gen tem = _n-1

gen y=0
*replace mean =. if tem >10 & tem <20
*replace lb =. if tem >10 & tem <20
*replace ub =. if tem >10 & tem <20


twoway /// 
(scatter mean tem, msymbol(o) msize(small) mcolor(black)  ) ///  
(rcap lb ub tem, lcolor(black))  /// 
(line y tem, lp(dash) color(black) ), ///  
xtitle("Temperature (°C)") /// 
ylabel(-10(5)10 ,nogrid) ///
title("Panel A: Poor Regions", color(black) ) ///
legend(off) graphregion(color(white))
graph save Graph "03figure\Figure5_0.gph",replace

graph combine 03figure\Figure5_0.gph  03figure\Figure5_1.gph, cols(2)ysize(2) imargin(vsmall) graphregion(color(white))
*graph save Graph "03figure\Figure5.png", replace


