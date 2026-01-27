/*--------------------------------------------------------------------------
Last modification: 01/26/2026
Uses:              latinobarometro.dta
Produces:          perceptions_data.dta
--------------------------------------------------------------------------*/

#delimit                                                                     ;
clear all                                                                    ;
glo user = lower("`=c(username)'")                                           ;
glo root "C:/Users/${user}/Dropbox/Research/published-papers/fairness"       ;
glo raw "${root}/raw_data"                                                   ;
glo dat "${root}/data"                                                       ;
glo cod "${root}/code"                                                       ;
glo res "${root}/results"                                                    ;


*******************************                                              ;
*** Create working sample   ***                                              ;
*******************************                                              ;

use "${raw}/latinobarometro.dta", replace                                    ;

keep if !missing(distribucion_justa) & pais != "esp"                         ;

loc general     "ano pais pondera region ciudad distrito comida"             ;
loc demographics "edad gedad hombre casado catolico alfabeto nivel*
                  skill_level ideology raza miembros"                        ;
loc labor       "ocupado desocupa pea relab"                                 ;
loc assets      "propieta heladera computadora lavarropas telefono_fijo
                 cloaca agua_potable auto celular telefono"                  ;
loc perceptions "class personal* pais* noticias peor_problema progreso_pais" ;
loc preferences "protesta* distribucion_justa riqueza_justa pol*"            ;

keep `general' `demographics' `labor' `assets' `perceptions' `preferences'   ;

encode pais, gen(id_pais)                                                    ;
encode region, gen(id_region)                                                ;
recode ano (nonmissing = 1 "All"), gen(all)                                  ;


*******************************                                              ;
*** Generate variables      ***                                              ;
*******************************                                              ;

* Age squared                                                                ;
gen edad2 = edad^2                                                           ;

* Education                                                                  ;
recode nivel (0/1 = 0 "Less than Primary")
             (2/3 = 1 "Complete Primary")
             (4/5 = 2 "Complete Secondary")
             (6   = 3 "Complete Tertiary"), gen(maxedu)                      ;
recode nivel_padre (0/1 = 0 "Less than Primary")
                   (2/3 = 1 "Complete Primary")
                   (4/5 = 2 "Complete Secondary")
                   (6   = 3 "Complete Tertiary"), gen(maxedu_padre)          ;

* Ideology                                                                   ;
recode ideology (0/3 = 0 "Left wing")
                (8/10 = 1 "Right wing")
                (nonmissing = . ), gen(derecha)                              ;

* Mobility                                                                   ;
gen mobility_edu = maxedu - maxedu_padre                                     ;
replace mobility_edu = . if missing(maxedu) | missing(maxedu_padre)          ;
recode personal_pasado (1   = 1 "Present is better")
                       (2/3 = 0 "Present is not better"),
                       gen(mobility_personal)                                ;

* Outlook                                                                    ;
recode pais_futuro (3   = 1 "Future will be better")
                   (1/2 = 0 "Future will not be better"),
                   gen(positive_outlook)                                     ;
recode pais_futuro (1   = 1 "Future will be worse")
                   (2/3 = 0 "Future will not be worse"),
                   gen(negative_outlook)                                     ;

* Fairness variables                                                         ;
tab distribucion_justa, gen(just)                                            ;
rename (just1 just2 just3 just4) (very_unfair unfair fair very_fair)         ;
gen unfair_all = very_unfair + unfair                                        ;

* For descriptive statistics table                                           ;
recode nivel (0/3 = 0 "Less than secondary complete")
             (4/6 = 1 "Secondary or more"), gen(secondary)                   ;
recode nivel_padre (0/3 = 0 "Less than secondary complete")
                   (4/6 = 1 "Secondary or more"), gen(secondary_padre)       ;


*******************************                                              ;
*** Labels                  ***                                              ;
*******************************                                              ;

lab var all              "All"                                               ;
lab var derecha          "Conservative dummy"                                ;
lab var edad2            "Age squared"                                       ;
lab var maxedu           "Highest level of education attainment"             ;
lab var maxedu_padre     "Highest level of parent's education attainment"    ;
lab var mobility_edu     "Social Mobility (education)"                       ;
lab var mobility_personal "Social Mobility (personal)"                       ;
lab var positive_outlook "Positive Outlook"                                  ;
lab var negative_outlook "Negative Outlook"                                  ;
lab var ideology         "Self-reported Ideology"                            ;
lab var secondary        "Secondary complete"                                ;
lab var secondary_padre  "Secondary complete parents"                        ;
lab var very_unfair      "Very Unfair"                                       ;
lab var unfair           "Unfair"                                            ;
lab var fair             "Fair"                                              ;
lab var very_fair        "Very Fair"                                         ;
lab var unfair_all       "Unfair (all)"                                      ;

drop skill_level nivel nivel_padres                                          ;


*******************************                                              ;
*** Save                    ***                                              ;
*******************************                                              ;

aorder                                                                       ;
sort ano pais                                                                ;
order ano pais id_pais                                                       ;
compress                                                                     ;
save "${dat}/perceptions_data.dta", replace                                  ;


exit                                                                         ;
