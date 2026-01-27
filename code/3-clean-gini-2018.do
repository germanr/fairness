/*--------------------------------------------------------------------------
Last modification: 01/26/2026
Uses:              Gini_paises_AL.xls
Produces:          gini.dta
--------------------------------------------------------------------------*/

#delimit                                                                     ;
clear all                                                                    ;
glo user = lower("`=c(username)'")                                           ;
glo root "C:/Users/${user}/Dropbox/Research/published-papers/fairness"       ;
glo raw "${root}/rawdata"                                                    ;
glo dat "${root}/data"                                                       ;
glo cod "${root}/code"                                                       ;
glo res "${root}/results"                                                    ;


*******************************                                              ;
*** Import and clean Gini   ***                                              ;
*******************************                                              ;

import excel using "${dat}/Gini_paises_AL.xls", clear first 
       cellrange(A4:Q31) case(lower)                                         ;

ren * gini*                                                                  ;
ren giniano ano                                                              ;
reshape long gini, i(ano) j(pais) string                                     ;

keep if inrange(ano,1997,2018)                                               ;

replace pais = "chl" if pais == "chi"                                        ;
replace pais = "pry" if pais == "par"                                        ;
replace pais = "ury" if pais == "uru"                                        ;

replace gini = gini/100                                                      ;

save "${dat}/gini.dta", replace                                              ;


exit                                                                         ;
