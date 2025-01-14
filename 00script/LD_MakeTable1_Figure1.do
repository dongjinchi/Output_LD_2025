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

*********Make Table 1*************
preserve
summ gdppc tem pre pop urb edu[aweight=weight_r]
summ gdppc tem pre pop urb edu[aweight=pop]
drop if missing(gdppc, tem, pre)
duplicates drop gdlcode1, force
summ gdlcode1
duplicates drop country1, force
summ country1
restore

local vars "tem pre gdppc dgdp"
local weights "weight_r pop"

* 循环处理每个变量
foreach var of local vars {
    * 创建一个临时数据集来存储所有权重的结果
    tempfile combined
    save `combined', emptyok
    * 循环处理每个权重
    foreach weight of local weights {
        if "`var'" == "tem" {
            local title = "Panel A: Temperature"
            local l2title = "Temperature (℃)"
            local color = "252 141 98"
        }
        else if "`var'" == "pre" {
            local title = "Panel B: Precipitation"
            local l2title = "Precipitation (m)"
            local color = "60 180 160"
        }
        else if "`var'" == "gdppc" {
            local title = "Panel C: GDP per capita"
            local l2title = "GDP per capita"
            local color = "140 160 200"
        }
        else if "`var'" == "dgdp" {
            local title = "Panel D: GDP per capita Growth"
            local l2title = "GDP per capita growth"
            local color = "180 120 180"
        }

        if "`weight'" == "weight_r" {
            local lpat = "solid"
        }
        else if "`weight'" == "pop" {
            local lpat = "dash"
        }

        preserve
        
        * 计算加权均值和分位点
        collapse (mean) mean_temp=`var' (p5) lower_bound_temp=`var' (p95) upper_bound_temp=`var' [aweight=`weight'], by(year)
        
        * 确保权重变量的一致性
        gen weight = "`weight'"
        *destring weight, replace force
        
        * 追加结果到临时数据集中
        append using `combined',force
        save `combined', replace
        
        restore
    }
    preserve
    * 读取合并后的数据集
	use `combined', clear

    * 绘制合并后的图形
    twoway ///
        (rarea lower_bound_temp upper_bound_temp year if weight == "weight_r", fcolor("`color'%10") lcolor("0%0") lwidth(0)) ///
        (rarea lower_bound_temp upper_bound_temp year if weight == "pop", fcolor("`color'%50") lcolor("0%0") lwidth(0)) ///
        (scatter mean_temp year if weight == "weight_r", connect(direct) lpattern(solid) msymbol(none) mcolor(navy) lcolor("`color'") sort) ///
        (scatter mean_temp year if weight == "pop", connect(direct) lpattern(dash) msymbol(none) mcolor(navy) lcolor("`color'") sort) ///
        , title(`title',color(black)) l2title(`l2title') xtitle("") legend(off) graphregion(color(white))
    
    * 保存图形
    graph save "03figure\Figure1_`var'.gph", replace
	restore
}

graph combine 03figure\Figure1_tem.gph 03figure\Figure1_pre.gph 03figure\Figure1_gdppc.gph 03figure\Figure1_dgdp.gph ,  graphregion(color(white)) 
graph save Graph "Figure1.png", replace

local vars tem pre gdppc dgdp
local weights weight_r pop
foreach weight of local weights{
	foreach var of local vars{
	qui sum `var'  [aweight=weight] if year>2010
	scalar mean_v1 = r(mean)
	qui sum `var'  [aweight=weight] if year<1995
	scalar mean_v2 = r(mean)
	scalar diff = mean_v1-mean_v2
	dis "`var'" ":" diff
}
}

local vars dT dP tem pre dgdp
foreach var of loc vars  {
	xtunitroot ht `var', demean  trend
}
