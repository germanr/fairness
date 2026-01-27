/*--------------------------------------------------------------------------
Last modification: 01/26/2026
Uses:              perceptions_data.dta, inequality_indicators.dta, gini.dta
Produces:          merged_data.dta
--------------------------------------------------------------------------*/

#delimit                                                                     ;
clear all                                                                    ;
glo user = lower("`=c(username)'")                                           ;
glo root "C:/Users/${user}/Dropbox/Research/published-papers/fairness"       ;
glo raw "${root}/rawdata"                                                    ;
glo dat "${root}/data"                                                       ;
glo cod "${root}/code"                                                       ;
glo res "${root}/results"                                                    ;


*******************************************                                  ;
*** Reshape inequality indicators       ***                                  ;
*******************************************                                  ;

use "${dat}/inequality_indicators", replace                                  ;

replace indicator = "gini_lel"    if indicator == "Gini coefficient"         ;
replace indicator = "ratio90_10"  if indicator == "Ratio 90/10"              ;
replace indicator = "ratio75_25"  if indicator == "Ratio 75/25"              ;
replace indicator = "gen_entrophy" if indicator == "Generalized entropy, GE(2)";
replace indicator = "atk_05"      if indicator == "Atkinson, A(0.5)"         ;
replace indicator = "atk_1"       if indicator == "Atkinson, A(1)"           ;
replace indicator = "atk_2"       if indicator == "Atkinson, A(2)"           ;
replace indicator = "gini_nozero" if indicator == "Gini coefficient (no zero income)";
replace indicator = "mean_logdev" if indicator == "Mean log deviation, GE(0)";
replace indicator = "theil"       if indicator == "Theil index, GE(1)"       ;
replace indicator = "cv"          if indicator == "Coefficient Variation"    ;
replace indicator = "variance"    if indicator == "Variance"                 ;
replace indicator = "gini_abs"    if indicator == "Absolute Gini"            ;
replace indicator = "kolm"        if indicator == "Kolm, K(1)"               ;

drop if inlist(indicator,"Kolm, K(2)","Kolm, K(3)","Generalized entropy, GE(-1)");

reshape wide value, i(ano pais) j(indicator) string                          ;

foreach var of varlist _all {                                                ;
    loc newname = subinstr("`var'","value","",.)                             ;
    rename `var' `newname'                                                   ;
}                                                                            ;

tempfile inequality                                                          ;
save `inequality', replace                                                   ;


*******************************************                                  ;
*** Collapse fairness indicators        ***                                  ;
*******************************************                                  ;

use "${dat}/perceptions_data.dta", replace                                   ;
collapse (mean) unfair fair unfair_all very_fair very_unfair distribucion_justa
         [w=pondera], by(ano pais)                                           ;

foreach var in fair unfair_all very_fair very_unfair unfair {                ;
    replace `var' = `var' * 100                                              ;
}                                                                            ;


*******************************************                                  ;
*** Merge all data                      ***                                  ;
*******************************************                                  ;

merge 1:1 pais ano using "${dat}/gini.dta"                                   ;
drop if _merge == 2                                                          ;
drop _merge                                                                  ;

merge 1:1 pais ano using `inequality', gen(data_availability)                ;
drop if ano < 1997 | pais == "hti"                                           ;
drop if data_availability == 2                                               ;
drop data_availability                                                       ;


*******************************************                                  ;
*** Standardize absolute indicators     ***                                  ;
*******************************************                                  ;

foreach var in "gini_abs" "variance" "kolm" {                                ;
    bys pais: egen mean_`var' = mean(`var')                                  ;
    replace `var' = (`var'/mean_`var')*100                                   ;
    drop mean_`var'                                                          ;
}                                                                            ;


*******************************************                                  ;
*** Interpolation                       ***                                  ;
*******************************************                                  ;

local indicators "atk_05 atk_1 atk_2 cv gen_entrophy gini_abs gini_lel gini_nozero mean_logdev ratio75_25 ratio90_10 theil variance kolm";

foreach indicator of local indicators {                                      ;
    rename `indicator' `indicator'_original                                  ;
    bys pais: ipolate `indicator' ano, gen(`indicator') epolate              ;
    replace `indicator' = . if ano > 2015                                    ;
}                                                                            ;


*******************************************                                  ;
*** Labels                              ***                                  ;
*******************************************                                  ;

label var unfair            "% Only Unfair"                                  ;
label var fair              "% Fair"                                         ;
label var unfair_all        "% Unfair or Very Unfair"                        ;
label var very_fair         "% Very Fair"                                    ;
label var very_unfair       "% Very Unfair"                                  ;
label var distribucion_justa "Mean index"                                    ;
label var gini_lel          "Gini coefficient LEL"                           ;
label var ratio90_10        "Ratio 90/10"                                    ;
label var ratio75_25        "Ratio 75/25"                                    ;
label var gen_entrophy      "Generalized entropy, GE(2)"                     ;
label var atk_05            "Atkinson, A(0.5)"                               ;
label var atk_1             "Atkinson, A(1)"                                 ;
label var atk_2             "Atkinson, A(2)"                                 ;
label var gini_nozero       "Gini coefficient (no zero income)"              ;
label var mean_logdev       "Mean log deviation, GE(0)"                      ;
label var theil             "Theil index, GE(1)"                             ;
label var cv                "Coefficient Variation"                          ;
label var variance          "Variance"                                       ;
label var gini_abs          "Absolute Gini"                                  ;
label var kolm              "Kolm, K(1)"                                     ;


*******************************************                                  ;
*** Save                                ***                                  ;
*******************************************                                  ;

sort pais ano                                                                ;
order pais ano                                                               ;
save "${dat}/merged_data.dta", replace                                       ;


exit                                                                         ;
