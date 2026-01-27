/*--------------------------------------------------------------------------
Last modification: 01/26/2026
Uses:              perceptions_data.dta, merged_data.dta, gini.dta
Produces:          All tables (descriptive stats, regressions, correlations)
--------------------------------------------------------------------------*/

#delimit                                                                     ;
clear all                                                                    ;
glo user = lower("`=c(username)'")                                           ;
glo root "C:/Users/${user}/Dropbox/Research/published-papers/fairness"       ;
glo raw "${root}/raw_data"                                                   ;
glo dat "${root}/data"                                                       ;
glo cod "${root}/code"                                                       ;
glo res "${root}/results"                                                    ;


*==========================================================================*;
*   SECTION 1: DESCRIPTIVE STATISTICS (Table A1, A2)                       *;
*==========================================================================*;

*******************************************                                  ;
*** Variables availability table        ***                                  ;
*******************************************                                  ;

use "${dat}/perceptions_data.dta", replace                                   ;
drop if ano > 2015                                                           ;

order ciudad region                                                          ;
collapse (count) agua_potable - telefono_fijo , by(ano)                      ;

foreach var of varlist agua_potable - telefono_fijo {                        ;
    rename `var' yr`var'                                                     ;
}                                                                            ;

reshape long yr, i(ano) j(variable) string                                   ;
reshape wide yr, i(variable) j(ano)                                          ;
compress                                                                     ;
export excel using "${res}/Data analysis.xlsx", sheet("Variables Availability")
       sheetreplace firstrow(variables)                                      ;


*******************************************                                  ;
*** Descriptive statistics of sample    ***                                  ;
*******************************************                                  ;

use "${dat}/perceptions_data.dta", replace                                   ;
drop if ano > 2015                                                           ;

gen     assets = 1 if cloaca == 1 & computadora  == 1 & lavarropas == 1 &
                      telefono_fijo == 1 & auto == 1                         ;
replace assets = 0 if cloaca == 0 | computadora  == 0 | lavarropas == 0 |
                      telefono_fijo == 0 | auto == 0                         ;

tempfile db                                                                  ;
save `db', replace                                                           ;

loc vars "edad hombre casado catolico alfabeto secondary secondary_padre desocupa pea agua_potable cloaca auto computadora heladera propieta telefono_fijo celular lavarropas ideology assets";

foreach stat in mean sd count {                                              ;
    use `vars' pondera using `db', replace                                   ;
    order `vars'                                                             ;
    tempfile tmp_`stat'                                                      ;

    if "`stat'" != "count" replace desocupa = . if pea == 0                  ;

    collapse (`stat') `vars' [w=pondera]                                     ;

    foreach var of local vars {                                              ;
        rename `var' `stat'`var'                                             ;
    }                                                                        ;

    gen i = 1                                                                ;
    reshape long `stat', i(i) j(variable) string                             ;
    drop i                                                                   ;

    save `tmp_`stat'', replace                                               ;
}                                                                            ;

merge 1:1 variable using `tmp_mean', nogen                                   ;
merge 1:1 variable using `tmp_sd', nogen                                     ;

replace mean = mean * 100 if mean < 1                                        ;

gen Category = ""                                                            ;
replace Category = "Socio-demographic"
    if inlist(variable,"edad","hombre","casado","catolico","ideology")       ;
replace Category = "Education and Labor market"
    if inlist(variable,"alfabeto","secondary","secondary_padre","desocupa","pea");
replace Category = "Access to services"
    if inlist(variable,"agua_potable","cloaca")                              ;

replace variable = "Access to running water (%)"          if variable == "agua_potable"  ;
replace variable = "Literate (%)"                         if variable == "alfabeto"      ;
replace variable = "Car (%)"                              if variable == "auto"          ;
replace variable = "Married or civil union (%)"           if variable == "casado"        ;
replace variable = "Catholic religion (%)"                if variable == "catolico"      ;
replace variable = "Mobile (%)"                           if variable == "celular"       ;
replace variable = "Access to a sewage (%)"               if variable == "cloaca"        ;
replace variable = "Computer (%)"                         if variable == "computadora"   ;
replace variable = "Unemployed (% Labor Force)"           if variable == "desocupa"      ;
replace variable = "Age"                                  if variable == "edad"          ;
replace variable = "Fridge (%)"                           if variable == "heladera"      ;
replace variable = "Male (%)"                             if variable == "hombre"        ;
replace variable = "Washing machine (%)"                  if variable == "lavarropas"    ;
replace variable = "Economically active (%)"              if variable == "pea"           ;
replace variable = "Homeowner (%)"                        if variable == "propieta"      ;
replace variable = "Secondary education or more (%)"      if variable == "secondary"     ;
replace variable = "Parents with secondary education (%)" if variable == "secondary_padre";
replace variable = "Landline (%)"                         if variable == "telefono_fijo" ;
replace variable = "Assets index"                         if variable == "assets"        ;
replace variable = "Ideology (10 = right-wing)"           if variable == "ideology"      ;

rename (variable mean sd count) (Variable Mean Std_Dev Obs)                  ;

order Category                                                               ;
sort Category Variable                                                       ;
export excel using "${res}/Data analysis.xlsx", sheet("Statistics of the sample")
       sheetreplace firstrow(variables)                                      ;


*******************************************                                  ;
*** Unfairness by groups table          ***                                  ;
*******************************************                                  ;

use "${dat}/perceptions_data", replace                                       ;
tempfile tab_file                                                            ;
tempname tab                                                                 ;
postfile `tab' str70(Year Category Level Class Percent) using `tab_file', replace;

loc categories "all maxedu gedad hombre casado catolico relab id_pais "      ;

foreach year in 1997 2001 2002 2007 2009 2010 2011 2013 2015 all {           ;
    if ("`year'" == "all") local ifyear ""                                   ;
    else local ifyear `"& ano == `year'"'                                    ;

    forvalues class = 1(1)4 {                                                ;
        foreach category of local categories {                               ;
        levels `category', local(options)                                    ;
            foreach option of local options {                                ;

                loc option_lab: label `category' `option'                    ;
                loc class_lab: label distribucion_justa `class'              ;
                if "`category'" == "all"     loc category_lab "All"          ;
                if "`category'" == "maxedu"  loc category_lab "Education level";
                if "`category'" == "gedad"   loc category_lab "Age group"    ;
                if "`category'" == "hombre"  loc category_lab "Gender"       ;
                if "`category'" == "casado"  loc category_lab "Civil Status" ;
                if "`category'" == "catolico" loc category_lab "Religion"    ;
                if "`category'" == "relab"   loc category_lab "Type of employment";
                if "`category'" == "derecha" loc category_lab "Ideology"     ;
                if "`category'" == "id_pais" loc category_lab "Country"      ;
                if "`category'" == "size"    loc category_lab "City size"    ;

                sum all if `category' == `option' `ifyear' [w=pondera]       ;
                loc tot = r(sum_w)                                           ;

                sum all if `category' == `option' & distribucion_justa == `class'  `ifyear' [w=pondera];
                loc y = r(sum_w)/`tot'*100                                   ;

                post `tab' ("`year'") ("`category_lab'") ("`option_lab'") ("`class_lab'") ("`y'");
            }                                                                ;
        }                                                                    ;
    }                                                                        ;
}                                                                            ;


postclose `tab'                                                              ;
use `tab_file', clear                                                        ;
destring, replace                                                            ;

replace Level = "Argentina"     if Level == "arg"                            ;
replace Level = "Bolivia"       if Level == "bol"                            ;
replace Level = "Brazil"        if Level == "bra"                            ;
replace Level = "Chile"         if Level == "chl"                            ;
replace Level = "Colombia"      if Level == "col"                            ;
replace Level = "Costa Rica"    if Level == "cri"                            ;
replace Level = "Dominican Rep." if Level == "dom"                           ;
replace Level = "Ecuador"       if Level == "ecu"                            ;
replace Level = "Guatemala"     if Level == "gtm"                            ;
replace Level = "Honduras"      if Level == "hnd"                            ;
replace Level = "Mexico"        if Level == "mex"                            ;
replace Level = "Nicaragua"     if Level == "nic"                            ;
replace Level = "Panama"        if Level == "pan"                            ;
replace Level = "Peru"          if Level == "per"                            ;
replace Level = "Paraguay"      if Level == "pry"                            ;
replace Level = "El Salvador"   if Level == "slv"                            ;
replace Level = "Uruguay"       if Level == "ury"                            ;
replace Level = "Venezuela"     if Level == "ven"                            ;

export excel using "${res}/Data analysis.xlsx", sheet("Fairness by group")
       sheetreplace firstrow(variables)                                      ;

compress                                                                     ;
rename _all, lower                                                           ;
drop if year == "all"                                                        ;
destring _all, replace                                                       ;
save "${dat}/fairness_groups.dta", replace                                   ;


*==========================================================================*;
*   SECTION 2: CORRELATION ANALYSIS (Absolute vs. Relative)                *;
*==========================================================================*;

use "${dat}/merged_data.dta", clear                                          ;

tempname temporal                                                            ;
tempfile temporal_file                                                       ;
postfile `temporal' str40(Country Perception Objective Correlation P_Value
         Standard_Error_Reg Standard_Error_Boot Interpolation)
         using `temporal_file', replace                                      ;

local fairness "very_unfair unfair fair very_fair unfair_all distribucion_justa";
local inequality "gini_lel atk_05_original atk_1_original cv_original gen_entrophy_original gini_abs_original gini_lel_original gini_nozero_original mean_logdev_original ratio75_25_original ratio90_10_original theil_original variance_original kolm_original";

levelsof pais, local(countries)                                              ;
local countries "`countries' pool lac"                                       ;

foreach country of local countries {                                         ;

    use "${dat}/merged_data.dta", replace                                    ;

    if ("`country'" == "lac") collapse (mean) `fairness' `inequality', by(ano);
    if ("`country'" != "lac" & "`country'" != "pool" ) keep if pais == "`country'";

    foreach fair of local fairness {                                         ;
        foreach ineq of local inequality {                                   ;

            local ineq_lab   : variable label `ineq'                         ;
            local unfair_lab : variable label `fair'                         ;

            qui sum `fair', d                                                ;
            gen `fair'_std = (`fair'-r(mean))/r(sd)                          ;
            qui sum `ineq', d                                                ;
            gen `ineq'_std = (`ineq'-r(mean))/r(sd)                          ;

            reg `fair'_std `ineq'_std, robust                                ;
            loc pvalue = (2 * ttail(e(df_r), abs(_b[`ineq']/_se[`ineq'])))   ;
            loc se_reg = _se[`ineq']                                         ;

            corr `fair' `ineq'                                               ;
            loc corr = r(rho)                                                ;

            bootstrap r(rho), reps(500) seed(1): correlate `fair' `ineq'     ;
            mat aux = r(table)                                               ;
            loc se_boot = aux[2,1]                                           ;
            post `temporal' ("`country'") ("`unfair_lab'") ("`ineq_lab'")
                 ("`corr'") ("`pvalue'") ("`se_reg'") ("`se_boot'") ("Not Interpolating");

            matrix drop aux                                                  ;
            drop *_std                                                       ;

        }                                                                    ;
    }                                                                        ;
}                                                                            ;

postclose `temporal'                                                         ;
use `temporal_file', clear                                                   ;
destring, replace                                                            ;
compress                                                                     ;

replace Objective = subinstr(Objective,"(mean) ","",.)                       ;
replace Perception = "% Only Unfair"         if Perception == "(mean) unfair";
replace Perception = "% Very Unfair"         if Perception == "(mean) very_unfair";
replace Perception = "% Unfair or Very Unfair" if Perception == "(mean) unfair_all";
drop if inlist(Perception,"(mean) distribucion_justa","(mean) fair","(mean) very_fair", "Mean index", "% Fair", "% Very Fair");

foreach syntax in " value" "_original" {                                     ;
    replace Objective = "Atkinson, A(0.5)"        if Objective == "atk_05`syntax'"      ;
    replace Objective = "Atkinson, A(1)"          if Objective == "atk_1`syntax'"       ;
    replace Objective = "Atkinson, A(2)"          if Objective == "atk_2`syntax'"       ;
    replace Objective = "Coefficient Variation"   if Objective == "cv`syntax'"          ;
    replace Objective = "Generalized entropy, GE(2)" if Objective == "gen_entrophy`syntax'";
    replace Objective = "Absolute Gini"           if Objective == "gini_abs`syntax'"    ;
    replace Objective = "Gini coefficient"        if Objective == "gini_lel`syntax'"    ;
    replace Objective = "Gini coefficient (no zero income)" if Objective == "gini_nozero`syntax'";
    replace Objective = "Mean log deviation, GE(0)" if Objective == "mean_logdev`syntax'";
    replace Objective = "Ratio 75/25"             if Objective == "ratio75_25`syntax'"  ;
    replace Objective = "Ratio 90/10"             if Objective == "ratio90_10`syntax'"  ;
    replace Objective = "Theil index, GE(1)"      if Objective == "theil`syntax'"       ;
    replace Objective = "Variance"                if Objective == "variance`syntax'"    ;
    replace Objective = "Kolm, K(1)"              if Objective == "kolm`syntax'"        ;
}                                                                            ;

export excel using "${res}/Data Analysis.xlsx", sheet("Relative vs. Absolute")
       sheetreplace firstrow(variables)                                      ;


*==========================================================================*;
*   SECTION 3: REGRESSION TABLES                                           *;
*==========================================================================*;

use "${dat}/perceptions_data.dta", replace                                   ;
merge m:1 pais ano using "${dat}/merged_data.dta", nogen                     ;
merge m:1 pais ano using "${dat}/gini.dta"                                   ;
drop if _merge == 2                                                          ;
drop _merge                                                                  ;

drop unfair                                                                  ;
rename unfair_all unfair                                                     ;
recode distribucion_justa (4=1) (3=2) (2=3) (1=4), gen(index)                ;
tab maxedu, gen(educ)                                                        ;

gen     assets = 1 if cloaca == 1 & computadora  == 1 & lavarropas == 1 &
                      telefono_fijo == 1 & auto == 1                         ;
replace assets = 0 if cloaca == 0 | computadora  == 0 | lavarropas == 0 |
                      telefono_fijo == 0 | auto == 0                         ;

gen miss_assets   = (missing(cloaca) | missing(computadora) | missing(lavarropas) | missing(telefono_fijo) | missing(auto));
qui sum assets, d                                                            ;
replace assets = r(mean) if miss_assets == 1                                 ;
gen miss_catolico = (missing(catolico))                                      ;
qui sum catolico, d                                                          ;
replace catolico = r(mean) if miss_catolico == 1                             ;
gen miss_ideology = (missing(ideology))                                      ;
qui sum ideology, d                                                          ;
replace ideology = r(mean) if miss_ideology == 1                             ;

drop pol_boicot pol_piquete                                                  ;
foreach var of varlist pol_* {                                               ;
 recode `var' (1 2 = 0) (3   = 1), gen(d`var')                               ;
 recode `var' (1   = 0) (2 3 = 1), gen(q`var')                               ;
}                                                                            ;

gen dpol_any = dpol_evadir + dpol_media + dpol_peticion + dpol_protestaileg  ;
replace dpol_any = 1 if dpol_any > 0 & dpol_any != .                         ;
gen qpol_any = qpol_evadir + qpol_media + qpol_peticion + qpol_protestaileg  ;
replace qpol_any = 1 if qpol_any > 0 & qpol_any != .                         ;

levelsof ano, local(years)                                                   ;
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

egen cluster = group(id_pais ano)                                            ;
gen wt = pondera                                                             ;

glo lel "_lel"                                                               ;
keep if ano <= 2015                                                          ;


*******************************************                                  ;
*** Main regression tables (LPM)        ***                                  ;
*******************************************                                  ;

foreach dv in "unfair" "very_unfair" "index" {                               ;
    glo dv `dv'                                                              ;
    di "Depvar: `dv'"                                                        ;

    eststo clear                                                             ;
    eststo: reghdfe ${dv} gini$lel                                                                                                          [pw=wt], absorb(ano id_pais) cluster(cluster);
    eststo: reghdfe ${dv} gini$lel edad edad2 hombre                                                                                        [pw=wt], absorb(ano id_pais) cluster(cluster);
    eststo: reghdfe ${dv} gini$lel edad edad2 hombre casado educ3 educ4                                                                     [pw=wt], absorb(ano id_pais) cluster(cluster);
    eststo: reghdfe ${dv} gini$lel edad edad2 hombre casado educ3 educ4 pea desocupa                                                        [pw=wt], absorb(ano id_pais) cluster(cluster);
    eststo: reghdfe ${dv} gini$lel edad edad2 hombre casado educ3 educ4 pea desocupa assets miss_asse                                       [pw=wt], absorb(ano id_pais) cluster(cluster);
    eststo: reghdfe ${dv} gini$lel edad edad2 hombre casado educ3 educ4 pea desocupa assets miss_asse ideology miss_ideo catolico miss_cato [pw=wt], absorb(ano id_pais) cluster(cluster);

    esttab using "${res}/${dv}_lpm.tex", drop(_cons miss*)
    coeflabels(
     gini "Gini"
     gini$lel "Gini"
     edad   "Age"
     edad2 "Age~squared"
     hombre "Male"
     casado "Married"
     educ3 "Finished~HS"
     educ4 "Finished~coll."
     pea "In~labor~force"
     desocupa "Unemployed"
     cloaca "Sewage "
     computadora "Computer"
     lavarropas "Washing~machine"
     telefono_fijo "Landline"
     auto "Has~access~to~a~car"
     assets "Assets~index"
     ideology "Conservative"
     catolico "Catholic"
    )
    replace nonotes nonumbers nomtitles se nolines nogaps obslast b(3) star(* 0.10 ** 0.05 *** 0.01) booktabs fragment stats(N `mean', fmt(%20.0fc %4.2f)) substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&");
}                                                                            ;


*******************************************                                  ;
*** Logit regression tables             ***                                  ;
*******************************************                                  ;

foreach dv in "unfair" "very_unfair" {                                       ;
    glo dv `dv'                                                              ;
    di "Depvar: `dv'"                                                        ;

    eststo clear                                                             ;
    logit ${dv} gini$lel yr* ct*                                                                                                          [pw=wt], robust cluster(cluster); eststo: margins, dydx(*) atmeans post;
    logit ${dv} gini$lel yr* ct* edad edad2 hombre casado                                                                                 [pw=wt], robust cluster(cluster); eststo: margins, dydx(*) atmeans post;
    logit ${dv} gini$lel yr* ct* edad edad2 hombre casado educ3 educ4                                                                     [pw=wt], robust cluster(cluster); eststo: margins, dydx(*) atmeans post;
    logit ${dv} gini$lel yr* ct* edad edad2 hombre casado educ3 educ4 pea desocupa                                                        [pw=wt], robust cluster(cluster); eststo: margins, dydx(*) atmeans post;
    logit ${dv} gini$lel yr* ct* edad edad2 hombre casado educ3 educ4 pea desocupa assets miss_asse                                       [pw=wt], robust cluster(cluster); eststo: margins, dydx(*) atmeans post;
    logit ${dv} gini$lel yr* ct* edad edad2 hombre casado educ3 educ4 pea desocupa assets miss_asse ideology miss_ideo catolico miss_cato [pw=wt], robust cluster(cluster); eststo: margins, dydx(*) atmeans post;

    esttab using "${res}/${dv}_logit.tex", drop(yr* ct* miss*)
    coeflabels(
     gini "Gini"
     gini$lel "Gini"
     edad   "Age"
     edad2 "Age~squared"
     hombre "Male"
     casado "Married"
     educ2 "Primary~education"
     educ3 "Finished~HS"
     educ4 "Finished~coll."
     pea "In~labor~force"
     desocupa "Unemployed"
     cloaca "Sewage"
     computadora "Computer"
     lavarropas "Washing~machine"
     telefono_fijo "Landline"
     auto "Has~access~to~a~car"
     assets "Assets~index"
     ideology "Conservative"
     catolico "Catholic"
    )
    replace nonotes nonumbers nomtitles se nolines nogaps obslast b(3) star(* 0.10 ** 0.05 *** 0.01) booktabs fragment stats(N, fmt(%20.0fc)) substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&");


    * Different inequality indicators instead of Gini - 5 different indicators one per column;
    eststo clear                                                             ;
    logit ${dv} gini_nozero  edad edad2 hombre casado educ3 educ4 pea desocupa assets ideology catolico miss* yr* ct* [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post;
    logit ${dv} atk_1        edad edad2 hombre casado educ3 educ4 pea desocupa assets ideology catolico miss* yr* ct* [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post;
    logit ${dv} theil        edad edad2 hombre casado educ3 educ4 pea desocupa assets ideology catolico miss* yr* ct* [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post;
    logit ${dv} gen_entrophy edad edad2 hombre casado educ3 educ4 pea desocupa assets ideology catolico miss* yr* ct* [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post;
    logit ${dv} gini_abs     edad edad2 hombre casado educ3 educ4 pea desocupa assets ideology catolico miss* yr* ct* [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post;
    esttab using "${res}/${dv}_ineq_logit.tex", drop(yr* ct* miss*)
    coeflabels(
     gini_nozero "Gini~OPAREN no~zero~income CPAREN"
     atk_1 "Atkinson,~AOPAREN1CPAREN"
     theil "Theil~index"
     gen_entrophy "Generalized~entropy"
     gini_abs "Absolute~Gini"
     edad   "Age"
     edad2 "Age~squared"
     hombre "Male"
     casado "Married"
     educ2 "Primary~education"
     educ3 "Finished~HS"
     educ4 "Finished~coll."
     pea "In~labor~force"
     desocupa "Unemployed"
     cloaca "Sewage "
     computadora "Computer"
     lavarropas "Washing~machine"
     telefono_fijo "Landline"
     auto "Has~access~to~a~car"
     assets "Assets~index"
     ideology "Conservative"
     catolico "Catholic"
    )
    replace nonotes nonumbers nomtitles se nolines nogaps obslast b(3) star(* 0.10 ** 0.05 *** 0.01) booktabs fragment stats(N, fmt(%20.0fc)) order(gini_nozero atk_1 theil gen_entrophy gini_abs) substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&");

}                                                                            ;


*******************************************                                  ;
*** Social unrest tables                ***                                  ;
*******************************************                                  ;

foreach dv in "unfair" "very_unfair" "index" {                               ;
    glo dv `dv'                                                              ;
    di "Depvar: `dv'"                                                        ;

    loc controls "edad edad2 hombre casado educ2 educ3 educ4 pea desocupa assets ideology catolico miss*";
    if "${dv}" == "unfair"      loc lab "Unfair"                             ;
    if "${dv}" == "very_unfair" loc lab "Very~unfair"                        ;
    if "${dv}" == "index"       loc lab "Raw~unfairness~index"               ;

    eststo clear                                                             ;
    logit dpol_redessoc     ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum dpol_redessoc     if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit dpol_media        ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum dpol_media        if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit dpol_peticion     ${dv} gini$lel `controls' yr* ct* [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum dpol_peticion     if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit dpol_protestaleg  ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum dpol_protestaleg  if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit dpol_protestaileg ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum dpol_protestaileg if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit dpol_evadir       ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum dpol_evadir       if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit dpol_disturbios   ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum dpol_disturbios   if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit dpol_ocupa        ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum dpol_ocupa        if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit dpol_any          ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum dpol_any          if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';


    esttab using "${res}/${dv}_unrest_past.tex", keep(${dv} gini*) coeflabels(${dv} "`lab'" gini_lel "Gini") replace nonotes nonumbers nomtitles se nolines nogaps noobs b(3) star(* 0.10 ** 0.05 *** 0.01) booktabs fragment stats(mean, fmt(%4.3f) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var.")) substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&");

    esttab using "${res}/${dv}_unrest_past_N.tex", keep(${dv}) drop(${dv}) replace booktabs nonotes nonumbers nomtitles se b(3) noobs nolines nogaps nostar fragment stats(N, fmt(%20.0fc) layout("\multicolumn{1}{c}{@}")) substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&");


    eststo clear                                                             ;
    logit qpol_redessoc     ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum qpol_redessoc     if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit qpol_media        ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum qpol_media        if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit qpol_peticion     ${dv} gini$lel `controls' yr* ct* [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum qpol_peticion     if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit qpol_protestaleg  ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum qpol_protestaleg  if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit qpol_protestaileg ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum qpol_protestaileg if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit qpol_evadir       ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum qpol_evadir       if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit qpol_disturbios   ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum qpol_disturbios   if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit qpol_ocupa        ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum qpol_ocupa        if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    logit qpol_any          ${dv} gini$lel `controls'         [pw=wt], robust noomitted cluster(cluster); eststo: margins, dydx(*) atmeans post; qui sum qpol_any          if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';

    esttab using "${res}/${dv}_unrest_would.tex", keep(${dv} gini*) coeflabels(${dv} "`lab'" gini_lel "Gini") replace nonotes nonumbers nomtitles se nolines nogaps noobs b(3) star(* 0.10 ** 0.05 *** 0.01) booktabs fragment stats(mean, fmt(%4.3f) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var.")) substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&");

    esttab using "${res}/${dv}_unrest_would_N.tex", keep(${dv}) drop(${dv}) replace booktabs nonotes nonumbers nomtitles se b(3) noobs nolines nogaps nostar fragment stats(N, fmt(%20.0fc) layout("\multicolumn{1}{c}{@}")) substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&");

    eststo clear                                                             ;
    eststo: reghdfe protesta_trabajo    ${dv} gini$lel `controls' [pw=wt], absorb(ano id_pais) cluster(cluster); qui sum protesta_trabajo    if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    eststo: reghdfe protesta_democracia ${dv} gini$lel `controls' [pw=wt], absorb(ano id_pais) cluster(cluster); qui sum protesta_democracia if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    eststo: reghdfe protesta_educacion  ${dv} gini$lel `controls' [pw=wt], absorb(ano id_pais) cluster(cluster); qui sum protesta_educacion  if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    eststo: reghdfe protesta_naturaleza ${dv} gini$lel `controls' [pw=wt], absorb(ano id_pais) cluster(cluster); qui sum protesta_naturaleza if e(sample), mean; local mean : di %4.3f r(mean); estadd local mean `mean';
    esttab using "${res}/${dv}_unrest_will.tex", keep(${dv} gini*) coeflabels(${dv} "`lab'" gini_lel "Gini") replace nonotes nonumbers nomtitles se nolines nogaps obslast b(3) star(* 0.10 ** 0.05 *** 0.01) booktabs fragment stats(mean N, fmt(%4.3f %20.0fc) layout("\multicolumn{1}{c}{@}") labels("Mean~Dep.~Var." "N")) substitute(\_ _ " " "" "&" "&&" "~" " " "-" "$-$" "\\" "&\\" "\multicolumn{1}{c}{" "" "}&" "&" "\sym{***&" "&\sym{***}" "\sym{**&" "&\sym{**}" "\sym{*&" "&\sym{*}" ")&" "&)" "\&)" "\)&" "OPAREN" "(" "CPAREN" ")" "(.&)" "&");

}                                                                            ;


exit                                                                         ;
