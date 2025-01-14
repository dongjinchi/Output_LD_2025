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

postfile Bootrstrap T2_panel T_panel T2_ld T_ld using "01data\Bootrstrap.dta", replace

forvalues i = 1/1000 {
    * 随机抽取m-1个地区
use GDPpc1.dta,replace    
bsample, cluster(country1)
sort gdlcode1 year
bysort gdlcode1 year: gen seq = _n
gsort gdlcode1 seq year
egen  region = seq(), from(1) block(26)

qui{ 
xtset region year 
    reghdfe dgdp dT c.dT#c.tem c.tem##c.tem dP c.dP#c.pre c.pre##c.pre  [aweight=weight_r],absorb(i.year region##c.year) vce( cluster country1) 
	
scalar T2_panel = _b[c.tem#c.tem]
scalar T_panel = _b[c.tem]
		
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

keep if N_usd>=2

sum period
gen T_max = r(max)

gen year = period
xtset region year 

gen dT=tem-l.tem
gen dP=pre-l.pre
gen lngdp = log(gdp)
gen dgdp= lngdp-l.lngdp   

reghdfe dgdp dT c.dT#c.tem c.tem##c.tem dP c.dP#c.pre c.pre##c.pre  [aweight=weight_r],absorb(i.year i.region) vce( cluster country1) 
 
scalar T2_ld = _b[c.tem#c.tem]
scalar T_ld = _b[c.tem]	
}
* 将临时变量的值存储到 MakeFigure4 中的新行
post Bootrstrap (T2_panel) (T_panel) (T2_ld) (T_ld)

dis `i'
}

* 关闭postfile
postclose Bootrstrap

use 01data\Bootrstrap.dta,clear
export delimited using "01data\Bootstrap.csv", replace
