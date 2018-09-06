*********************************************************************************;
* Adapted from SAS code from D'Agostino, R.B, Jr. (1998). Tutorial in           *;
* Biostatistics: Propensity Score Methods for Bias Reduction in the Comparison  *;
* of a Treatment to a Non-randomized control Group.                             *;
* Statistics in Medicine, 17, 2281.                                             *;
*********************************************************************************;

* Perform a logistic regression to estimate propensity scores for each subject.;

*libname z 'C:\JPSM\SURV 699 Practical Tools\Lecture Notes\6 Steps in Weighting\Examples';
*options nofmterr;

proc import out= work.nhis 
            DATAFILE= "C:\Projects\Practical Tools Book\Data\nhis.csv" 
            dbms=csv replace;
     getnames=yes;
     datarow=2; 
run;

proc contents data=nhis;
run;

  
  			* Model probability of response ;
proc logistic data = nhis;
   class hisp (ref = '1') 
   	 race (ref = '1')
   	 parents_r  (ref = '1')
   	 educ_r     (ref = '1')
   	/ param=ref;
   model RESP (event = '1') = 
              age 
              hisp 
	      	  race 
	      	  parents_r
	      	  educ_r;	      
   output out = preds pred = pr;
run;

			* Take the propensity score and create quint1les 
			  based on the estimated propensity score;
proc rank groups = 5 out = r;
   ranks rnks;
   var pr;
run;

data a; 
   set r;
   pclass = rnks + 1;
run;

* Show the breakdown of units by propensity class and response ;

proc freq data = a; 
   tables pclass * RESP;
run;

* Perform 2-way anova and logistic regressions to determine whether ;
* difference in covariate means was removed by creating classes.    ;

proc glm data = a;
   class pclass;
   model age = pclass resp pclass * resp;
run;

proc logistic data = a;
   class pclass (ref = '1')
         resp   (ref = '0');
   model hisp (event = '2') = pclass resp pclass * resp;
run;

proc logistic data = a;
   class pclass (ref = '1')
         resp   (ref = '1');
   model race (event = '1') = pclass resp pclass * resp;
run;

			* There is a problem with this model. "Quasi-complete separation
			  of data points detected. MLE may not exist.";
proc logistic data = a;
   class pclass (ref = '1')
         resp   (ref = '1');
   model parents_r (event = '1') = pclass resp pclass * resp;
run;

proc logistic data = a;
   class pclass (ref = '1')
         resp   (ref = '1');
   model educ_r  = pclass resp pclass * resp;	   
run;
