* Encoding: UTF-8.
*Filter used in the code below:  
AKM.urban.suburban.rural.defined.by.question.options >= 1 
& WVOdiscrete >= 1 
& RC_Bobyote.Affect < 999 
& Bobyote.Bi.flexible  >= 0  
& duration > 5  
& (Coyote.Time.Scenarios  >  21.5953 | Bobcat.Time.Scenarios > 23.224)

DATASET ACTIVATE DataSet2.

* code for data filter (all acceptable respondents)

USE ALL.
COMPUTE filter_$=(AKM.urban.suburban.rural.defined.by.question.options >= 1 & WVOdiscrete >= 1 & 
    RC_Bobyote.Affect < 999 & Bobyote.Bi.flexible  >= 0  & duration > 5  &  (Coyote.Time.Scenarios  >  
    21.5953 | Bobcat.Time.Scenarios > 23.224)).
VARIABLE LABELS filter_$ 'AKM.urban.suburban.rural.defined.by.question.options >= 1 & '+
    'WVOdiscrete >= 1 & RC_Bobyote.Affect < 999 & Bobyote.Bi.flexible  >= 0  & duration > 5  &  '+
    '(Coyote.Time.Scenarios  >  21.5953 | Bobcat.Time.Scenarios > 23.224) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


* PLAY- FILTER (all respondents) (URBAN SUBURBAN only)

*USE ALL.
*COMPUTE filter_$=(AKM.urban.suburban.rural.defined.by.question.options <= 2 & WVOdiscrete >= 1 & 
    RC_Bobyote.Affect < 999 & Bobyote.Bi.flexible  >= 0  & duration > 5  &  (Coyote.Time.Scenarios  >  
    21.5953 | Bobcat.Time.Scenarios > 23.224)).
*VARIABLE LABELS filter_$ 'AKM.urban.suburban.rural.defined.by.question.options >= 1 & '+
    'WVOdiscrete >= 1 & RC_Bobyote.Affect < 999 & Bobyote.Bi.flexible  >= 0  & duration > 5  &  '+
    '(Coyote.Time.Scenarios  >  21.5953 | Bobcat.Time.Scenarios > 23.224) (FILTER)'.
*VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
*FORMATS filter_$ (f1.0).
*FILTER BY filter_$.
*EXECUTE.



*MLR (All Respondents) (cognitive and demographic) (scenario qustions) (excluding affect)
NOMREG bobyoteCollapse (BASE='II' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options both.scenarios.given.in.order WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
   /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.

****MLR- no order in answers and scenarios------ this option is tempting because it takes care of the hessian issue 
***but the pearson goodness of fit (p=0.174; should be close to 1) suffers here compared to the MLR without the scenarios (pearson fit p=0.639) when including all respondnets
NOMREG Bobyote.no.order (BASE='N.L' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options both.scenarios.no.order WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
   /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.

****MLR- no order in scenarios- ***quasi complete separation with all respondents combined
NOMREG bobyoteCollapse (BASE='IL' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options both.scenarios.no.order WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
   /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.


*MLR (All Respondents) (cognitive and demographic)(excluding Affect)

NOMREG bobyoteCollapse (BASE='IL' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.

********************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************
* code for data filter (Contextually Sensitive Respondents)

USE ALL.
COMPUTE filter_$=(AKM.urban.suburban.rural.defined.by.question.options >= 1 & WVOdiscrete >= 1 & 
    RC_Bobyote.Affect < 999 & Bobyote.Bi.flexible = 1  & duration > 5  &  (Coyote.Time.Scenarios  >  
    21.5953 | Bobcat.Time.Scenarios > 23.224)).
VARIABLE LABELS filter_$ 'AKM.urban.suburban.rural.defined.by.question.options >= 1 & '+
    'WVOdiscrete >= 1 & RC_Bobyote.Affect < 999 & Bobyote.Bi.flexible  >= 0  & duration > 5  &  '+
    '(Coyote.Time.Scenarios  >  21.5953 | Bobcat.Time.Scenarios > 23.224) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*MLR (cognitive and demographic)(excluding Affect) (excluding bobcat coyote variable)

NOMREG bobyoteCollapse (BASE='IL' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options  WITH MutualismMean DominationMean PPGENDER 
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.


*LI.LM collapsed only (best reference category here is ML)
NOMREG Bobyote.Collapsed.LI.LM (BASE='ML' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.

****MLR- no order in answers and scenarios (this model severily hurts the deviance goodness of fit decreasing it down to p=0.045 (should be clode to 1))
NOMREG Bobyote.no.order (BASE='N.L' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options both.scenarios.no.order WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
   /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.

*Singularities in the Hessian Matric with every possible reference category
NOMREG bobyote (BASE='IL' ORDER=ASCENDING) BY 
    RC_AKM.urban.suburban.rural WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.

*MLR (Contextually Sensitive Respondents)

NOMREG bobyoteCollapse (BASE='IL' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents RC_Bobyote.Affect PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CLASSTABLE PARAMETER SUMMARY LRT CPS STEP MFI IC.



*MLR (Contextually Sensitive Respondents)(using suburban as categorical reference)

*NOMREG bobyoteCollapse (BASE='IL' ORDER=ASCENDING) BY AKM.urban1.suburban3.rural2 WITH 
    MutualismMean DominationMean Bobyote.scenario.respondents RC_Bobyote.Affect PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CLASSTABLE PARAMETER SUMMARY LRT CPS STEP MFI IC.

*MLR (contextually sensitive)(not including WVOs)(including severity and location)

NOMREG bobyoteCollapse (BASE='IL' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options both.scenarios.given.in.order WITH 
    PPGENDER RC_Bobyote.Affect
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.


********************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************
********************************************************************************************************************************************************************************************
* code for data filter (Contextually Insensitive Respondents)

USE ALL.
COMPUTE filter_$=(AKM.urban.suburban.rural.defined.by.question.options >= 1 & WVOdiscrete >= 1 & 
    RC_Bobyote.Affect < 999 & Bobyote.Bi.flexible = 0  & duration > 5  &  (Coyote.Time.Scenarios  >  
    21.5953 | Bobcat.Time.Scenarios > 23.224)).
VARIABLE LABELS filter_$ 'AKM.urban.suburban.rural.defined.by.question.options >= 1 & '+
    'WVOdiscrete >= 1 & RC_Bobyote.Affect < 999 & Bobyote.Bi.flexible  >= 0  & duration > 5  &  '+
    '(Coyote.Time.Scenarios  >  21.5953 | Bobcat.Time.Scenarios > 23.224) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


*MLR (Cognitive and demographic) (excluding Affect)

NOMREG bobyoteCollapse (BASE='II' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
 /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.

****MLR- no order in answers and scenarios (this model severily hurts the pearson goodness of fit decreasing it down to p=0.057 (should be clode to 1), 
NOMREG Bobyote.no.order (BASE='II' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options both.scenarios.no.order WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
   /PRINT=CLASSTABLE FIT PARAMETER SUMMARY LRT CPS MFI IC.


*MLR (Contextually Insensitive Respondents)- severity and location included
NOMREG bobyoteCollapse (BASE='II' ORDER=ASCENDING) BY 
    AKM.urban.suburban.rural.defined.by.question.options Bobyote.location Bobyote.severity WITH MutualismMean DominationMean 
    Bobyote.scenario.respondents RC_Bobyote.Affect PPGENDER
  /CRITERIA CIN(95) DELTA(0) MXITER(100) MXSTEP(5) CHKSEP(20) LCONVERGE(0) PCONVERGE(0.000001) 
    SINGULAR(0.00000001)
  /MODEL
  /STEPWISE=PIN(.05) POUT(0.1) MINEFFECT(0) RULE(SINGLE) ENTRYMETHOD(LR) REMOVALMETHOD(LR)
  /INTERCEPT=INCLUDE
  /PRINT=CLASSTABLE PARAMETER SUMMARY LRT CPS STEP MFI IC.




********************************************************************************************************************************************************************************************

*create new variable that collapses the scenario questions assigned groups

RECODE both.scenarios.given.in.order ('Ro/Rk'='Ro/Rk.and.Uo/Rk') ('Uo/Rk'='Ro/Rk.and.Uo/Rk') 
    (ELSE=Copy) INTO Collapse.both.scenarios.given.in.order.
VARIABLE LABELS  Collapse.both.scenarios.given.in.order 'both scenarios given in order with some '+
    'groups collapsed'.
EXECUTE.

RECODE both.scenarios.given.in.order ('Ro/Rk'='Ro/Rk.and.Uo/Rk') ('Uo/Rk'='Ro/Rk.and.Uo/Rk') 
    ('Rk/Ro'='Rk/Ro.and.Rk/Uo') ('Rk/Uo'='Rk/Ro.and.Rk/Uo') (ELSE=Copy) INTO 
    Collapse.both.scenarios.given.in.order.
VARIABLE LABELS  Collapse.both.scenarios.given.in.order 'both scenarios given in order with some '+
    'groups collapsed'.
EXECUTE.

*FREQUENCIES- predator control preferences-collapsed

FREQUENCIES VARIABLES=Bobyote.scenario.respondents PPGENDER 
    AKM.urban.suburban.rural.defined.by.question.options Bobyote bobyoteCollapse MutualismMean 
    DominationMean
  /STATISTICS=STDDEV RANGE MINIMUM MAXIMUM MEAN MEDIAN MODE
  /ORDER=ANALYSIS.

*******TESTING assumptions 
* for the tables (cognitive and demographic)(mutualism and domination mean)

CROSSTABS
  /TABLES=MutualismMean DominationMean Bobyote.scenario.respondents RC_Bobyote.Affect PPGENDER 
    AKM.urban.suburban.rural.defined.by.question.options BY bobyoteCollapse
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=ROW 
  /COUNT ROUND CELL.

* for the tables  (cognitive and demographic)(categorical WVOs)

CROSSTABS
  /TABLES=WVOdiscrete Bobyote.scenario.respondents RC_Bobyote.Affect PPGENDER 
    AKM.urban.suburban.rural.defined.by.question.options BY bobyoteCollapse
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=ROW 
  /COUNT ROUND CELL.

* for the tables (both.scenarios.in.order)

CROSSTABS
  /TABLES=both.scenarios.given.in.order BY bobyoteCollapse
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=ROW 
  /COUNT ROUND CELL.

* for the tables (severity)

CROSSTABS
  /TABLES=Bobyote.severity BY bobyoteCollapse
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=ROW 
  /COUNT ROUND CELL.

* for the tables (location)

CROSSTABS
  /TABLES=Bobyote.location BY bobyoteCollapse
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=ROW 
  /COUNT ROUND CELL.


*******TESTING assumptions (with affect)
CROSSTABS
  /TABLES=Bobyote.scenario.respondents BY PPGENDER 
    AKM.urban.suburban.rural.defined.by.question.options Bobyote.Affect_positive.to.negative BY 
    Bobyote.Bi.flexible
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=COUNT EXPECTED COLUMN 
  /COUNT ROUND CELL.

*******TESTING assumptions (affect X Bobyote.scenario.respondents)

CROSSTABS
  /TABLES=Bobyote.scenario.respondents BY  RC_Bobyote.Affect BY 
    Bobyote.Bi.flexible
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=COUNT EXPECTED COLUMN 
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=Bobyote.scenario.respondents BY  RC_Bobyote.Affect 
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=Xwild BY  RC_Bobyote.Affect 
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=COUNT EXPECTED COLUMN 
  /COUNT ROUND CELL.

*******TESTING assumptions
CROSSTABS
  /TABLES=Bobyote.scenario.respondents BY  PPGENDER 
    AKM.urban.suburban.rural.defined.by.question.options BY Bobyote.Bi.flexible
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=COUNT EXPECTED COLUMN 
  /COUNT ROUND CELL.

**TESTING warning of empty cells in MLR to see if it is a problem- it looks good to me

CROSSTABS
  /TABLES=bobyoteCollapse BY  WVOdiscrete PPGENDER Bobyote.scenario.respondents
    AKM.urban.suburban.rural.defined.by.question.options BY Bobyote.Bi.flexible
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=COUNT EXPECTED COLUMN 
  /COUNT ROUND CELL.


****Testing for independence between the scenario responces and if the respondents were from the united states smaple or the ohio sample 

CROSSTABS
  /TABLES=Bobyote.scenario.respondents BY bobyoteCollapse BY Bobyote.Bi.flexible
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ CC PHI 
  /CELLS=COUNT EXPECTED COLUMN PROP 
  /COUNT ROUND CELL.
