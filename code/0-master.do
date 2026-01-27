/*--------------------------------------------------------------------------
Last modification: 01/26/2026
Uses:              All do-files in this folder
Produces:          Reproduces all results for the paper

Paper:             "Are Fairness Perceptions Shaped by Income Inequality?
                    Evidence from Latin America"
                   Crespo & Reyes (Journal of Economic Behavior and Organization)

Notes:
- Run this file to reproduce all results
- Do-file 2 requires access to SEDLAC data (World Bank internal)
- If SEDLAC unavailable, data/inequality_indicators.dta must be provided
--------------------------------------------------------------------------*/

#delimit                                                                     ;
clear all                                                                    ;
set more off                                                                 ;
glo user = lower("`=c(username)'")                                           ;
glo root "C:/Users/${user}/Dropbox/Research/published-papers/fairness"       ;
glo cod "${root}/code"                                                       ;


*==========================================================================*;
*   RUN ALL DO-FILES                                                       *;
*==========================================================================*;

* 1. Create individual-level perception data from Latinobarometro            ;
do "${cod}/1-create-individual-data.do"                                      ;

* 2. Calculate inequality indicators from SEDLAC                             ;
* Note: Requires World Bank internal access. Skip if data is provided.       ;
* do "${cod}/2-calculate-inequality.do"                                      ;

* 3. Clean Gini data from Excel source                                       ;
do "${cod}/3-clean-gini-2018.do"                                             ;

* 4. Merge inequality with perception data                                   ;
do "${cod}/4-merge-data.do"                                                  ;

* 5. Generate all tables (descriptive stats, regressions, correlations)      ;
do "${cod}/5-tables.do"                                                      ;

* 6. Generate all figures                                                    ;
do "${cod}/6-figures.do"                                                     ;


exit                                                                         ;
