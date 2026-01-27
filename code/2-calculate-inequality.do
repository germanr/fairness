/*--------------------------------------------------------------------------
Last modification: 01/26/2026
Uses:              SEDLAC household surveys (via datalib)
Produces:          inequality_indicators.dta
Note:              Requires World Bank internal access to SEDLAC data
--------------------------------------------------------------------------*/

#delimit                                                                     ;
clear all                                                                    ;
set more off                                                                 ;
set matsize 5000                                                             ;
set seed 5362                                                                ;

glo user = lower("`=c(username)'")                                           ;
glo root "C:/Users/${user}/Dropbox/Research/published-papers/fairness"       ;
glo raw "${root}/rawdata"                                                    ;
glo dat "${root}/data"                                                       ;
glo cod "${root}/code"                                                       ;
glo res "${root}/results"                                                    ;

glo countrylist "arg bol bra chl col cri dom ecu slv gtm hnd mex nic pan pry per ury ven";


*******************************************                                  ;
*** Calculate inequality indicators     ***                                  ;
*******************************************                                  ;

cap mat drop a                                                               ;

local n_country = 0                                                          ;
foreach country in $countrylist {                                            ;
    local ++n_country                                                        ;
    forvalues year = 1997(1)2015 {                                           ;

        di in yellow "`country' | `year'"                                    ;

        cap datalib, country(`country') years(`year') clear                  ;

        * Exceptions for Venezuela                                           ;
        if "`country'" == "ven" {                                            ;
            cap datalib, country(`country') years(`year') clear              ;

            if `year' == 2011 {                                              ;
                replace ipc_sedlac = 1050.2                                  ;
                replace ipcf_ppp = (ipcf * ipc05_sedlac/ipc_sedlac)/ ppp05 * 1000;
            }                                                                ;

            if `year' == 2012 {                                              ;
                replace ipc_sedlac = 1261                                    ;
                replace ipcf_ppp = (ipcf * ipc05_sedlac/ipc_sedlac)/ ppp05 * 1000;
            }                                                                ;
        }                                                                    ;

        if _rc continue                                                      ;

        di in red "`country' | `year' actually used"                         ;

        * Only montevideo + urban > 5000 for Uruguay                         ;
        if ("`country'"=="ury") & (`year'>=2006) {                           ;
            preserve                                                         ;
            cap datalib, country(`country') years(`year') type(base) clear   ;

            sort correlat                                                    ;
            egen id=group(correlat)                                          ;
            if `year'==2006 gen byte region_ech = 1 if region<20             ;
            cap rename region region_ech                                     ;
            cap rename region_3 region_ech                                   ;
            cap rename regi region_ech                                       ;

            keep id region_ech                                               ;
            tempfile temp                                                    ;
            save `temp' , replace                                            ;
            restore                                                          ;
            merge m:m id using `temp', nogen                                 ;
            keep if (region_ech==1 | region_ech==2)                          ;

        }                                                                    ;

        * Keep only consistent observations                                  ;
        keep if cohh==1 & ipcf!=.                                            ;


        ** Gini, Rate pX/p(100-X) include zeros income                       ;
        qui ineqdec0 ipcf_ppp [w=pondera]                                    ;

        mat a = nullmat(a) \ (`n_country', `year', `r(gini)'  , 0)
                           \ (`n_country', `year', `r(p90p10)', 1)
                           \ (`n_country', `year', `r(p75p25)', 2)           ;

        ** Gini no zero, GE, Akinson                                         ;
        qui ineqdeco ipcf_ppp [w=pondera]                                    ;

        mat a = nullmat(a) \ (`n_country', `year', `r(gem1)'  , 3)
                           \ (`n_country', `year', `r(ge0)', 4)
                           \ (`n_country', `year', `r(ge1)', 5)
                           \ (`n_country', `year', `r(ge2)', 6)
                           \ (`n_country', `year', `r(ahalf)', 7)
                           \ (`n_country', `year', `r(a1)', 8)
                           \ (`n_country', `year', `r(a2)', 9)
                           \ (`n_country', `year', `r(gini)', 10)            ;

        loc gini = r(gini)                                                   ;

        * Drop extreme outliers (to avoid noise in absolute indicators)      ;
        cap drop lipcf                                                       ;
        gen lipcf = ln(ipcf_ppp)                                             ;
        qui sum lipcf [w=pondera]                                            ;
        drop if lipcf > r(mean) + 5*r(sd) & lipcf !=.                        ;

        * Absolute indicators                                                ;
        qui sum ipcf_ppp [w=pondera]                                         ;
        loc mean = r(mean)                                                   ;
        loc cv = r(sd)/ `mean'                                               ;
        loc abs_gini = `gini' * `mean'                                       ;
        loc variance = r(Var)                                                ;

        * Kolm index                                                         ;
        gen double aux_kolm =  `mean' - ipcf_ppp                             ;

        forvalues j = 1(1)3 {                                                ;
            gen double aux_`j' = `j' * aux_kolm                              ;
            gen double e_`j' = exp(aux_`j')                                  ;
            gen double suma_`j' = sum(e_`j'*pondera)                         ;
            sum pondera                                                      ;
            loc N = r(sum)                                                   ;
            loc aux2_`j' =  suma_`j'[_N] / `N'                               ;
            loc aux3_`j' =  ln(`aux2_`j'')                                   ;
            loc kolm_`j' = `aux3_`j'' / `j'                                  ;
        }                                                                    ;

        mat a = nullmat(a) \ (`n_country', `year', `abs_gini', 11)
                           \ (`n_country', `year', `cv'  , 12)
                           \ (`n_country', `year', `variance' , 13)
                           \ (`n_country', `year', `kolm_1' , 14)
                           \ (`n_country', `year', `kolm_2' , 15)
                           \ (`n_country', `year', `kolm_3' , 16)            ;
    }                                                                        ;
}                                                                            ;

drop _all                                                                    ;

mat colnames a =  pais ano value indicator                                   ;
svmat double a, n(col)                                                       ;


*******************************************                                  ;
*** Label indicators                    ***                                  ;
*******************************************                                  ;

local a = wordcount("$countrylist")                                          ;
forvalues i=1(1)`a' {                                                        ;
    local w: word `i' of $countrylist                                        ;
    label define pais `i' "`w'", add                                         ;
}                                                                            ;
label values pais pais                                                       ;

label define indicator
0 "Gini coefficient"
1 "Ratio 90/10"
2 "Ratio 75/25"
3 "Generalized entropy, GE(-1)"
4 "Mean log deviation, GE(0)"
5 "Theil index, GE(1)"
6 "Generalized entropy, GE(2)"
7 "Atkinson, A(0.5)"
8 "Atkinson, A(1)"
9 "Atkinson, A(2)"
10 "Gini coefficient (no zero income)"
11 "Absolute Gini"
12 "Coefficient Variation"
13 "Variance"
14 "Kolm, K(1)"
15 "Kolm, K(2)"
16 "Kolm, K(3)"                                                              ;

label values indicator indicator                                             ;

decode indicator, gen(aux)                                                   ;
drop indicator                                                               ;
rename aux indicator                                                         ;

decode pais, gen(aux)                                                        ;
drop pais                                                                    ;
rename aux pais                                                              ;

save "${dat}/raw/inequality_indicators_original.dta", replace                ;


*******************************************                                  ;
*** Apply comparability adjustments     ***                                  ;
*******************************************                                  ;

use "${dat}/raw/inequality_indicators_original.dta", clear                   ;

* Non-comparable surveys                                                     ;
drop if pais == "arg" & ano < 2004                                           ;
drop if pais == "col" & inlist(ano,1999,2000,2001,2006,2007)                 ;
drop if pais == "cri" & ano < 2010                                           ;
drop if pais == "dom" & ano == 2010                                          ;
drop if pais == "ecu" & inlist(ano,1998,1999,2000)                           ;
drop if pais == "gtm" & inlist(ano,2002,2003,2004)                           ;
drop if pais == "pan" & ano < 2008                                           ;
drop if pais == "per" & ano < 2004                                           ;


* Circas (Only adjacent years)                                               ;

* Argentina                                                                  ;
replace ano = 2015 if pais == "arg" & ano == 2014                            ;

* Chile                                                                      ;
replace ano = 1997 if pais == "chl" & ano == 1998                            ;
replace ano = 2001 if pais == "chl" & ano == 2000                            ;
replace ano = 2002 if pais == "chl" & ano == 2003                            ;
replace ano = 2007 if pais == "chl" & ano == 2006                            ;

* Colombia                                                                   ;
replace ano = 2007 if pais == "col" & ano == 2008                            ;

* Ecuador                                                                    ;
replace ano = 2002 if pais == "ecu" & ano == 2003                            ;

* Guatemala                                                                  ;
replace ano = 2001 if pais == "gtm" & ano == 2000                            ;
replace ano = 2015 if pais == "gtm" & ano == 2014                            ;

* Mexico                                                                     ;
replace ano = 1997 if pais == "mex" & ano == 1998                            ;
replace ano = 2001 if pais == "mex" & ano == 2000                            ;
replace ano = 2007 if pais == "mex" & ano == 2006                            ;
replace ano = 2009 if pais == "mex" & ano == 2008                            ;
replace ano = 2011 if pais == "mex" & ano == 2012                            ;
replace ano = 2015 if pais == "mex" & ano == 2014                            ;

* Nicaragua                                                                  ;
replace ano = 1997 if pais == "nic" & ano == 1998                            ;
replace ano = 2007 if pais == "nic" & ano == 2005                            ;
replace ano = 2015 if pais == "nic" & ano == 2014                            ;

* El Salvador                                                                ;
replace ano = 1997 if pais == "slv" & ano == 1998                            ;

* Venezuela                                                                  ;
replace ano = 2013 if pais == "ven" & ano == 2012                            ;


save "${dat}/inequality_indicators.dta", replace                             ;


exit                                                                         ;
