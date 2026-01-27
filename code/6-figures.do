/*--------------------------------------------------------------------------
Last modification: 01/26/2026
Uses:              perceptions_data.dta, merged_data.dta, gini.dta,
                   fairness_groups.dta
Produces:          All figures (Oaxaca decomposition, scatterplots, time series)
--------------------------------------------------------------------------*/

#delimit                                                                     ;
clear all                                                                    ;
glo user = lower("`=c(username)'")                                           ;
glo root "C:/Users/${user}/Dropbox/Research/published-papers/fairness"       ;
glo raw "${root}/rawdata"                                                    ;
glo dat "${root}/data"                                                       ;
glo cod "${root}/code"                                                       ;
glo res "${root}/results"                                                    ;


*==========================================================================*;
*   FIGURE 1: OAXACA-BLINDER DECOMPOSITION                                 *;
*==========================================================================*;

use "${dat}/perceptions_data.dta", replace                                   ;
merge m:1 pais ano using "${dat}/merged_data.dta", nogen                     ;
merge m:1 pais ano using "${dat}/gini.dta"                                   ;

tempname temporal                                                            ;
tempfile temporal_file                                                       ;
postfile `temporal' str40(period bar value comp) using `temporal_file', replace;

levelsof ano , local(years)                                                  ;
foreach yr of local years {                                                  ;
    gen yr`yr' = (ano == `yr')                                               ;
}                                                                            ;
drop yr1997                                                                  ;

loc i = 0                                                                    ;
levelsof pais, local(paises)                                                 ;
foreach p of local paises {                                                  ;
    loc ++i                                                                  ;
    gen ct`i' = (pais == "`p'")                                              ;
}                                                                            ;
drop ct1                                                                     ;

tab maxedu, gen(educ)                                                        ;

gen     assets = 1 if cloaca == 1 & computadora  == 1 & lavarropas == 1 &
                      telefono_fijo == 1 & auto == 1                         ;
replace assets = 0 if cloaca == 0 | computadora  == 0 | lavarropas == 0 |
                      telefono_fijo == 0 | auto == 0                         ;

gen miss_assets   = (missing(cloaca) | missing(computadora) |
                     missing(lavarropas) | missing(telefono_fijo) | missing(auto));
qui sum assets, d                                                            ;
replace assets = r(mean) if miss_assets == 1                                 ;
gen miss_catolico = (missing(catolico))                                      ;
qui sum catolico, d                                                          ;
replace catolico = r(mean) if miss_catolico == 1                             ;
gen miss_ideology = (missing(ideology))                                      ;
qui sum ideology, d                                                          ;
replace ideology = r(mean) if miss_ideology == 1                             ;

glo yr1 = 2002                                                               ;
glo yr2 = 2013                                                               ;

oaxaca unfair_all gini_lel edad edad2 hombre casado educ3 educ4 pea desocupa
       assets miss_asse ideology miss_ideo catolico miss_cato [iw=pondera]
       if inlist(ano,${yr1},${yr2}), by(ano) relax                           ;

matrix yr${yr1}${yr2} = r(table)                                             ;

glo unfair_ini   =  yr${yr1}${yr2}[1,1]*100                                  ;
glo unfair_fin   =  yr${yr1}${yr2}[1,2]*100                                  ;
glo change       =  yr${yr1}${yr2}[1,3]*100                                  ;
glo unexplained  = (yr${yr1}${yr2}[1,5] + yr${yr1}${yr2}[1,6]) * 100         ;
glo explained    =  yr${yr1}${yr2}[1,4]*100                                  ;
glo gini         =  yr${yr1}${yr2}[1,7]*100                                  ;
glo demographics =  $explained - $gini                                       ;

glo dunfair_ini   : display %16.2fc $unfair_ini                              ;
glo dunfair_fin   : display %16.2fc $unfair_fin                              ;
glo dunexplained  : display %16.2fc $unexplained                             ;
glo dexplained    : display %16.2fc $explained                               ;
glo dgini         : display %16.2fc $gini                                    ;
glo ddemographics : display %16.2fc $demographics                            ;


post `temporal' ("2002-2013") ("1") ("$unfair_ini") ("ini")                   ;
post `temporal' ("2002-2013") ("2") ("$unfair_fin") ("fin")                   ;
post `temporal' ("2002-2013") ("2") ("$change") ("z1")                        ;
post `temporal' ("2002-2013") ("3") ("$unfair_fin") ("a1")                    ;
post `temporal' ("2002-2013") ("3") ("$explained") ("explained")              ;
post `temporal' ("2002-2013") ("3") ("$unexplained") ("unexplained")          ;
post `temporal' ("2002-2013") ("4") ("$unfair_ini") ("a1")                    ;

postclose `temporal'                                                         ;
use `temporal_file', clear                                                   ;
destring, replace                                                            ;
compress                                                                     ;

replace value = value - 50 if value > 50                                     ;

graph bar (sum) value, over(comp) over(bar, relabel(1 "2002" 2 "2013"
          3 "Change 2002-2013" 4  " "))  asyvars stack
           bar(1, color(none)   lcolor(none))
           bar(2, color(green)  lcolor(black))
           bar(3, color(maroon) lcolor(black))
           bar(4, color(navy)   lcolor(black))
           bar(5, color(orange) lcolor(black))
           bar(6, color(none)   lcolor(gs12) lpattern(dash))
           ylabel(0 "50" 10 "60" 20 "70" 30 "80" 40 "90", angle(0) nogrid)
           ytitle("Percent unfair or very unfair")
           text(34 7  "$dunfair_ini"                           , color(white) size(small))
           text(20 33 "$dunfair_fin"                             , color(white) size(small))
           text(36 59 "$dunexplained"                             , color(white) size(small))
           text(30 59 "$dexplained"                             , color(white) size(small))
           text(31 37 "Change in" "fairness" "views" "2002-2013", color(black) size(small))
           text(34 62.7 "Unexplained"                           , color(white) size(small))
           text(28 62.6 "Explained"                             , color(white) size(small))
           text(30 84 "$dgini Gini coefficient"                   , color(black) size(small))
           text(26 86 "$ddemographics Composition effect"                 , color(black) size(small))
           text(31 50 "`=ustrunescape("\u23A7")'" "`=ustrunescape("\u23A8")'" "`=ustrunescape("\u23A9")'", size(10) width(vsmall) color(black) )
           text(29 75 "`=ustrunescape("\u23A7")'" "`=ustrunescape("\u23A8")'" "`=ustrunescape("\u23A9")'", size(7)  width(vsmall) color(black) )
           legend(off) graphregion(color(white)) bgcolor(white)              ;
           graph export "${res}/fig-oaxaca-0213.pdf", replace                ;
           graph export "${res}/fig-oaxaca-0213.png", as(png) width(3000) replace;


*==========================================================================*;
*   FIGURE 2: INTENSITY OF FAIRNESS PERCEPTIONS OVER TIME                  *;
*==========================================================================*;

use "${dat}/merged_data", replace                                            ;
collapse (mean) very_fair unfair very_unfair fair, by(ano)                   ;
keep if ano <= 2015                                                          ;

twoway  connect unfair      ano, lcolor(navy)         mcolor(navy)         msymbol(o) lpattern(solid)
     || connect very_unfair ano, lcolor(maroon)       mcolor(maroon)       msymbol(d) lpattern(longdash)
     || connect fair        ano, lcolor(orange)       mcolor(orange)       msymbol(t) lpattern(shortdash)
     || connect very_fair   ano, lcolor(forest_green) mcolor(forest_green) msymbol(s) lpattern(dash)
        xtitle(" ") xlabel(1997(3)2015)
        ylabel(0(10)60  , format(%4.0fc) angle(0) axis(1) gmax gmin)
        ytitle("Percent of individuals",)
        text(55 2004 "Unfair"     , color(navy))
        text(35 2004 "Very unfair", color(maroon))
        text(17 2004 "Fair"       , color(orange))
        text(7  2004 "Very fair"  , color(forest_green))
        legend(off) graphregion(color(white)) bgcolor(white)                 ;
        graph export "${res}/fig-intensity-fairness.pdf", replace            ;


*==========================================================================*;
*   FIGURE 3: CHANGE IN UNFAIRNESS 2002 VS 2013                            *;
*==========================================================================*;

use "${dat}/merged_data.dta", replace                                        ;
keep pais ano unfair_all                                                     ;
rename unfair_all unfair_                                                    ;
reshape wide unfair_, i(pais) j(ano)                                         ;

replace unfair_2002 = unfair_2007 if pais == "dom"                           ;

preserve                                                                     ;
collapse (mean) unfair*                                                      ;
gen pais = "lac"                                                             ;
tempfile lac                                                                 ;
save `lac', replace                                                          ;
restore                                                                      ;
append using `lac'                                                           ;

gen     pais_lab = pais                                                      ;
replace pais_lab = "Argentina"     if pais == "arg"                          ;
replace pais_lab = "Bolivia"       if pais == "bol"                          ;
replace pais_lab = "Brazil"        if pais == "bra"                          ;
replace pais_lab = "Chile"         if pais == "chl"                          ;
replace pais_lab = "Colombia"      if pais == "col"                          ;
replace pais_lab = "Costa Rica"    if pais == "cri"                          ;
replace pais_lab = "Dom. R."       if pais == "dom"                          ;
replace pais_lab = "Ecuador"       if pais == "ecu"                          ;
replace pais_lab = "Guatemala"     if pais == "gtm"                          ;
replace pais_lab = "Honduras"      if pais == "hnd"                          ;
replace pais_lab = "Mexico"        if pais == "mex"                          ;
replace pais_lab = "Nicaragua"     if pais == "nic"                          ;
replace pais_lab = "Panama"        if pais == "pan"                          ;
replace pais_lab = "Peru"          if pais == "per"                          ;
replace pais_lab = "Paraguay"      if pais == "pry"                          ;
replace pais_lab = "El Salvador"   if pais == "slv"                          ;
replace pais_lab = "Uruguay"       if pais == "ury"                          ;
replace pais_lab = "Venezuela"     if pais == "ven"                          ;
replace pais_lab = "Latin America" if pais == "lac"                          ;

gen     pos = .                                                              ;
replace pos = 8  if pais == "arg"                                            ;
replace pos = 8  if pais == "bol"                                            ;
replace pos = 7  if pais == "bra"                                            ;
replace pos = 4  if pais == "chl"                                            ;
replace pos = 3  if pais == "col"                                            ;
replace pos = 9  if pais == "cri"                                            ;
replace pos = 6  if pais == "dom"                                            ;
replace pos = 6  if pais == "ecu"                                            ;
replace pos = 12 if pais == "hnd"                                            ;
replace pos = 9  if pais == "gtm"                                            ;
replace pos = 6  if pais == "mex"                                            ;
replace pos = 6  if pais == "nic"                                            ;
replace pos = 6  if pais == "pan"                                            ;
replace pos = 3  if pais == "per"                                            ;
replace pos = 2  if pais == "pry"                                            ;
replace pos = 6  if pais == "slv"                                            ;
replace pos = 3  if pais == "ury"                                            ;
replace pos = 9  if pais == "ven"                                            ;
replace pos = 5  if pais == "lac"                                            ;

twoway scatter unfair_2013 unfair_2002 if pais != "lac",  mlabel(pais_lab) mlabcolor(black) msym(O) mlabv(pos)
    || scatter unfair_2013 unfair_2002 if pais == "lac",  mlabel(pais_lab) mlabcolor(red)   msym(D) mlabv(pos)
    || lfit    unfair_2013 unfair_2013, range(30 100) lcolor(gs12) lpattern(dash)
       xtitle("Percent unfair or very unfair in 2002") xlabel(30(10)100, format(%4.0f)  grid) xscale(titlegap(2))
       ytitle("Percent unfair or very unfair in 2013") ylabel(30(10)100, format(%4.0f)  gmax gmin angle(0))
       xline(0, lcolor(gs12) lwidth(vthin))
       yline(0, lcolor(gs12) lwidth(vthin))
       legend(off) graphregion(color(white)) bgcolor(white)                  ;
       graph export "${res}/fig-chg-fairness-0213.pdf", replace              ;


*==========================================================================*;
*   FIGURE 4: CHANGE IN GINI VS CHANGE IN UNFAIRNESS                       *;
*==========================================================================*;

use "${raw}/inequality_indicators_original.dta", clear                        ;
drop if pais == "arg" & ano < 2004                                           ;
drop if pais == "col" & inlist(ano,1999,2000,2001,2006,2007)                 ;
drop if pais == "cri" & ano < 2010                                           ;
drop if pais == "dom" & ano == 2010                                          ;
drop if pais == "ecu" & inlist(ano,1998,1999,2000)                           ;
drop if pais == "gtm" & inlist(ano,2002,2003,2004)                           ;
drop if pais == "pan" & ano < 2008                                           ;
drop if pais == "per" & ano < 2004                                           ;

keep if indicator == "Gini coefficient"                                      ;
drop indicator                                                               ;
rename value gini_                                                           ;
keep if ano >= 2000                                                          ;
reshape wide gini, i(pais) j(ano)                                            ;
rename gini* ginilel*                                                        ;
tempfile gini                                                                ;
save `gini', replace                                                         ;

use pais ano unfair_all using "${dat}/merged_data.dta", replace              ;
rename unfair_all unfair_                                                    ;
reshape wide unfair_, i(pais) j(ano)                                         ;

merge 1:1 pais using `gini', nogen                                           ;

gen gini_ini   = .                                                           ;
gen gini_fin   = .                                                           ;
gen unfair_ini = .                                                           ;
gen unfair_fin = .                                                           ;
gen chg_gini   = .                                                           ;
gen chg_unfair = .                                                           ;

replace gini_ini = ginilel_2002                                              ;
replace gini_ini = ginilel_2004  if pais == "arg"                            ;
replace gini_ini = ginilel_2003  if pais == "chl"                            ;
replace gini_ini = ginilel_2010  if pais == "cri"                            ;
replace gini_ini = ginilel_2003  if pais == "ecu"                            ;
replace gini_ini = ginilel_2006  if pais == "gtm"                            ;
replace gini_ini = ginilel_2001  if pais == "nic"                            ;
replace gini_ini = ginilel_2001  if pais == "nic"                            ;
replace gini_ini = ginilel_2008  if pais == "pan"                            ;
replace gini_ini = ginilel_2004  if pais == "per"                            ;
replace gini_fin = ginilel_2013                                              ;
replace gini_fin = ginilel_2014  if pais == "gtm"                            ;
replace gini_fin = ginilel_2014  if pais == "mex"                            ;
replace gini_fin = ginilel_2014  if pais == "nic"                            ;
replace gini_fin = ginilel_2012  if pais == "ven"                            ;

replace unfair_ini = unfair_2002                                             ;
replace unfair_ini = unfair_2007 if pais == "ecu"                            ;
replace unfair_ini = unfair_2007 if pais == "dom"                            ;
replace unfair_fin = unfair_2013                                             ;

replace chg_gini   = gini_fin    - gini_ini                                  ;
replace chg_unfair = unfair_fin  - unfair_ini                                ;

preserve                                                                     ;
collapse (mean) chg*                                                         ;
gen pais = "lac"                                                             ;
tempfile lac                                                                 ;
save `lac', replace                                                          ;
restore                                                                      ;
append using `lac'                                                           ;

gen     pais_lab = pais                                                      ;
replace pais_lab = "Argentina"     if pais == "arg"                          ;
replace pais_lab = "Bolivia"       if pais == "bol"                          ;
replace pais_lab = "Brazil"        if pais == "bra"                          ;
replace pais_lab = "Chile"         if pais == "chl"                          ;
replace pais_lab = "Colombia"      if pais == "col"                          ;
replace pais_lab = "Costa Rica"    if pais == "cri"                          ;
replace pais_lab = "Dom. R."       if pais == "dom"                          ;
replace pais_lab = "Ecuador"       if pais == "ecu"                          ;
replace pais_lab = "Guatemala"     if pais == "gtm"                          ;
replace pais_lab = "Honduras"      if pais == "hnd"                          ;
replace pais_lab = "Mexico"        if pais == "mex"                          ;
replace pais_lab = "Nicaragua"     if pais == "nic"                          ;
replace pais_lab = "Panama"        if pais == "pan"                          ;
replace pais_lab = "Peru"          if pais == "per"                          ;
replace pais_lab = "Paraguay"      if pais == "pry"                          ;
replace pais_lab = "El Salvador"   if pais == "slv"                          ;
replace pais_lab = "Uruguay"       if pais == "ury"                          ;
replace pais_lab = "Venezuela"     if pais == "ven"                          ;
replace pais_lab = "Latin America" if pais == "lac"                          ;

gen     pos = .                                                              ;
replace pos = 7  if pais == "arg"                                            ;
replace pos = 8  if pais == "bol"                                            ;
replace pos = 6  if pais == "bra"                                            ;
replace pos = 9  if pais == "chl"                                            ;
replace pos = 5  if pais == "col"                                            ;
replace pos = 3  if pais == "cri"                                            ;
replace pos = 5  if pais == "dom"                                            ;
replace pos = 3  if pais == "ecu"                                            ;
replace pos = 2  if pais == "gtm"                                            ;
replace pos = 6  if pais == "hnd"                                            ;
replace pos = 6  if pais == "mex"                                            ;
replace pos = 3  if pais == "nic"                                            ;
replace pos = 7  if pais == "pan"                                            ;
replace pos = 9  if pais == "per"                                            ;
replace pos = 9  if pais == "pry"                                            ;
replace pos = 9  if pais == "slv"                                            ;
replace pos = 7  if pais == "ury"                                            ;
replace pos = 9  if pais == "ven"                                            ;
replace pos = 6  if pais == "lac"                                            ;

twoway scatter chg_unfair chg_gini if pais != "lac", mlabel(pais_lab) mlabcolor(black) msym(O) mlabv(pos) mlabgap(*1.1)
    || scatter chg_unfair chg_gini if pais == "lac", mlabel(pais_lab) mlabcolor(red) msym(d) mlabv(pos) mlabgap(*1.1)
       xtitle("Change in Gini coefficient (2002-2013)") xlabel(-.15(.05).15, format(%4.2f)  grid) xscale(titlegap(2))
       ytitle("Change in share unfair (2002-2013)")     ylabel(-40(10)10   , format(%4.0f)  gmax gmin angle(0))
       xline(0, lcolor(gs12) lwidth(vthin))
       yline(0, lcolor(gs12) lwidth(vthin))
       legend(off) graphregion(color(white)) bgcolor(white)                  ;
       graph export "${res}/fig-chg-gini-unfair-0213.pdf", replace           ;


*==========================================================================*;
*   FIGURE 5: SCATTER AND BINSCATTER GINI VS UNFAIRNESS                    *;
*==========================================================================*;

use "${dat}/merged_data", replace                                            ;
keep pais ano gini* gini_lel_original gini_abs_original unfair_all unfair very_unfair;

foreach v in gini_lel unfair_all {                                           ;
    qui sum `v' if ano <= 2015,                                              ;
    gen `v'_std = (`v'- r(mean))/r(sd)                                       ;
}                                                                            ;

reg      gini_lel_std unfair_all_std if ano <= 2015                          ;
spearman gini_lel_std unfair_all_std if ano <= 2015                          ;
corr gini gini_lel       if ano <= 2014                                      ;

twoway scatter unfair_all gini_lel, mcolor(gs12) msym(O) lpattern(solid)
    || lfit    unfair_all gini_lel, lcolor(maroon) lpattern(dash)
       ytitle("% Unfair or very unfair") ylabel(, angle(0) format(%4.0f))
       xtitle("Gini coefficient")        xlabel(, format(%4.2f))
       legend(off) graphregion(color(white)) bgcolor(white)                  ;
       graph export "${res}/fig-scatter-gini-unfair.pdf", replace            ;


gen bin = floor(gini_lel*50)/50                                              ;
gen n = 1                                                                    ;
collapse (mean) unfair* very* (sum) n, by(bin)                               ;

keep if n >= 3                                                               ;

corr unfair_all bin [w=n]                                                    ;
reg unfair_all bin  [w=n]                                                    ;
twoway scatter unfair_all bin [fw=n], mcolor(gs12) msym(O) lpattern(solid)
    || lfit    unfair_all bin [fw=n], lcolor(maroon) lpattern(dash)
       ytitle("Percent unfair or very unfair") ylabel(50(10)100, angle(0) format(%4.0f) gmin gmax)
       xtitle("Gini coefficient")        xlabel(, format(%4.2f))
       legend(off) graphregion(color(white)) bgcolor(white)                  ;
       graph export "${res}/fig-binscatter-gini-unfair.pdf", replace         ;


qui corr unfair      bin [fw=n]                                              ;
glo unfair      : display %4.2f r(rho)                                       ;
qui corr very_unfair bin [fw=n]                                              ;
glo very_unfair : display %4.2f r(rho)                                       ;
qui corr unfair_all  bin [fw=n]                                              ;
glo unfair_all  : display %4.2f r(rho)                                       ;

twoway scatter unfair      bin, mcolor(navy)         msym(O) lpattern(solid)
    || scatter very_unfair bin, mcolor(maroon)       msym(T) lpattern(solid)
    || scatter unfair_all  bin, mcolor(forest_green) msym(D) lpattern(solid)
    || lfit    unfair      bin, lcolor(navy)         lpattern(solid)
    || lfit    very_unfair bin, lcolor(maroon)       lpattern(solid)
    || lfit    unfair_all  bin, lcolor(forest_green) lpattern(solid)
       ytitle("Percent of individuals") ylabel(0(20)100, angle(0) format(%4.0f) gmin gmax)
       xtitle("Gini coefficient")        xlabel(, format(%4.2f))
       legend(order(1 "Unfair (Correlation = ${unfair})" 2 "Very unfair (Correlation = ${very_unfair})" 3 "Unfair or very unfair (Correlation = ${unfair_all})") col(1) pos(5) ring(0))
       graphregion(color(white)) bgcolor(white)                              ;
       graph export "${res}/fig-binscatter-gini-unfair-all.pdf", replace     ;


*==========================================================================*;
*   FIGURE 6: TIME SERIES UNFAIRNESS AND GINI                              *;
*==========================================================================*;

use "${dat}/merged_data", replace                                            ;

collapse (mean) gini gini_lel gini_abs_original unfair_all unfair very_unfair, by(ano);
drop if ano > 2015                                                           ;

foreach v in gini_lel unfair_all {                                           ;
    qui sum `v' if ano <= 2015,                                              ;
    gen `v'_std = (`v'- r(mean))/r(sd)                                       ;
}                                                                            ;

reg gini_lel_std unfair_all_std if ano <= 2015                               ;

twoway  connect unfair_all ano, lcolor(navy)   mcolor(navy)   msymbol(o)
     || connect gini_lel   ano, lcolor(maroon) mcolor(maroon) msymbol(d) lpattern(longdash) yaxis(2)
        xtitle(" ") xlabel(1997(3)2015)
        ylabel(60(10)100  , format(%4.0fc) angle(0) axis(1) gmax gmin) ytitle("",)
        ylabel(0.4(0.05).6, format(%4.2fc) angle(0) axis(2) gmax gmin) ytitle("", axis(2))
        text(75 2004 "% Unfair or very unfair (LHS)", color(navy))
        text(85 2008.3 "Gini coefficient (RHS)", color(maroon))
        legend(off) graphregion(color(white)) bgcolor(white)                 ;
        graph export "${res}/fig-timeseries-gini-unfair.pdf", replace        ;


*==========================================================================*;
*   FIGURE 7: SCATTER ABSOLUTE GINI VS UNFAIRNESS                          *;
*==========================================================================*;

use "${dat}/merged_data", replace                                            ;
keep pais ano gini_lel gini_lel_original gini_abs_original unfair_all unfair very_unfair;

twoway scatter unfair_all gini_abs, mcolor(gs12) msym(O) lpattern(solid)
    || lfit    unfair_all gini_abs, lcolor(maroon) lpattern(dash)
       ytitle("% Unfair or very unfair") ylabel(, angle(0) format(%4.0f))
       xtitle("Absolute Gini (index)")   xlabel(, format(%4.0f))
       legend(off) graphregion(color(white)) bgcolor(white)                  ;
       graph export "${res}/fig-chg-absgini-unfair.pdf", replace             ;


*==========================================================================*;
*   FIGURE 8: FAIRNESS BY DEMOGRAPHIC GROUPS OVER TIME                     *;
*==========================================================================*;

use "${dat}/fairness_groups", replace                                        ;
rename percent unfair                                                        ;
keep if inlist(class,"Unfair","Very unfair")                                 ;
drop if year > 2015                                                          ;
collapse (sum) unfair, by(category level year)                               ;

* By age group                                                               ;
twoway  connect unfair year if level == "15-24", lcolor(navy)         mcolor(navy)         msymbol(o) lpattern(solid)
     || connect unfair year if level == "25-40", lcolor(maroon)       mcolor(maroon)       msymbol(d) lpattern(longdash)
     || connect unfair year if level == "41-64", lcolor(orange)       mcolor(orange)       msymbol(t) lpattern(shortdash)
     || connect unfair year if level == "65+"  , lcolor(forest_green) mcolor(forest_green) msymbol(s) lpattern(dash)
        xtitle(" ") xlabel(1997(3)2015)
        ylabel(60(10)100, format(%4.0fc) angle(0) axis(1) gmax gmin) ytitle("",)
        legend(order(1 "15-24" 2 "25-40" 3 "41-64" 4 "65+") row(1) size(small))
        graphregion(color(white)) bgcolor(white)                             ;
        graph export "${res}/fig-fairness-age.pdf", replace                  ;

* By gender                                                                  ;
twoway  connect unfair year if level == "Female", lcolor(maroon)       mcolor(maroon)       msymbol(d) lpattern(longdash)
     || connect unfair year if level == "Male", lcolor(navy)         mcolor(navy)         msymbol(o) lpattern(solid)
        xtitle(" ") xlabel(1997(3)2015)
        ylabel(60(10)100, format(%4.0fc) angle(0) axis(1) gmax gmin) ytitle("",)
        legend(order(1 "Female" 2 "Male") row(1) size(small))
        graphregion(color(white)) bgcolor(white)                             ;
        graph export "${res}/fig-fairness-sex.pdf", replace                  ;

* By education level                                                         ;
twoway  connect unfair year if level == "Less than Primary", lcolor(navy)         mcolor(navy)         msymbol(o) lpattern(solid)
     || connect unfair year if level == "Complete Primary", lcolor(maroon)       mcolor(maroon)       msymbol(d) lpattern(longdash)
     || connect unfair year if level == "Complete Secondary", lcolor(orange)       mcolor(orange)       msymbol(t) lpattern(shortdash)
     || connect unfair year if level == "Complete Tertiary"  , lcolor(forest_green) mcolor(forest_green) msymbol(s) lpattern(dash)
        xtitle(" ") xlabel(1997(3)2015)
        ylabel(60(10)100, format(%4.0fc) angle(0) axis(1) gmax gmin) ytitle("",)
        legend(order(1 "Less than primary" 2 "Primary" 3 "Secondary" 4 "Tertiary") row(1) size(small))
        graphregion(color(white)) bgcolor(white)                             ;
        graph export "${res}/fig-fairness-educ.pdf", replace                 ;

* By employment type                                                         ;
twoway  connect unfair year if level == "Employee", lcolor(navy)         mcolor(navy)         msymbol(o) lpattern(solid)
     || connect unfair year if level == "Employer", lcolor(maroon)       mcolor(maroon)       msymbol(d) lpattern(longdash)
     || connect unfair year if level == "Self-employed", lcolor(orange)       mcolor(orange)       msymbol(t) lpattern(shortdash)
     || connect unfair year if level == "Unemployed"  , lcolor(forest_green) mcolor(forest_green) msymbol(s) lpattern(dash)
        xtitle(" ") xlabel(1997(3)2015)
        ylabel(60(10)100, format(%4.0fc) angle(0) axis(1) gmax gmin) ytitle("",)
        legend(order(1 "Employee" 2 "Employer" 3 "Self-employed" 4 "Unemployed") row(1) size(small) span)
        graphregion(color(white)) bgcolor(white)                             ;
        graph export "${res}/fig-fairness-employ.pdf", replace               ;


exit                                                                         ;
