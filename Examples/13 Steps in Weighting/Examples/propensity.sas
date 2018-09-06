*********************************************************************************;
* SAS code from D'Agostino, R.B, Jr. (1998). Tutorial in Biostatistics:           ;
* Propensity Score Methods for Bias Reduction in the Comparison of a             ;
* Treatment to a Non-randomized control Group. Statistics in Medicine, 17, 2281. ;
*********************************************************************************;

* Perform a series of ttests to determine what the initial differences between the             ; 
* treated and control groups are. Here the variable epidural is the treatment indicator in the ; 
* model.                                                                                        ; 

proc ttest data = matchset;
	class epidural;
	var amladmit cm l arom gestage birthwt gender rate 
	    chyper phyper height weigbt momage insprvt momw momb;
run;

* Perform a stepwise logistic regression to estimate propensity scores for each subject.;
* The variable pr is the propensity score.  The variable epidural is;
* the treatment indicator in this model.	;

proc logistic data = matchset nosimple;
	model epidural = amladmit cm l arom gestage birthwt gender rate chyper 
		         phyper height weigbt momage insprvt momw momb
		/selection = stepwise;
output out = preds pred = pr;
run;

* Take the propensity score and create quint1les based on the estimated propensity score;

proc rank groups = 5 out = r;
	ranks rnks;
	var pr;
run;

data a; 
	set r; 
	quintile = rnks + 1;
run;

* Show the breakdown of subjects by treatment (here epidural) and propensity score quint1le;

proc freq; 
	tables quintile * epidural;
run;

* Perform 2-way anovas to determine whether the propensity score quintiles removed the init1al bias ;
* found by the t-tests above.;

proc glm;
	class quint1le;
	model amladmit cm l arom gestage birthwt gender rate chyper phyper
		height weight mom&ge insprvt momw momb = 
              quintile epidural quint1ie * epidural;
run;
