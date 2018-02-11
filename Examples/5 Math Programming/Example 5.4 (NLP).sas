/*****************************************************************************/
/* FILE:    Example 5.4 (NLP).sas                                            */
/* PROJECT: Practical Tools for Designing and Weighting Survey Samples       */
/* PURPOSE: Compare results from Solver for course example.                  */
/* DATE:    04/13/2010                                                       */
/* AUTHOR:  J.Dever, R.Valliant                                              */
/* REVISED: 10/17/2010                                                       */
/*	    Added data step to initialize stratum sample sizes.              */
/*****************************************************************************/
options nocenter;

		* Initialize stratum sample sizes;
data start100 (type = est);
	input _type_ $ n1 n2 n3 n4 n5;
	datalines;
		parms 100 100 100 100 100
	;
run;

data start500 (type = est);
	input _type_ $ n1 n2 n3 n4 n5;
	datalines;
		parms 500 500 500 500 500
	;
run;

******************************************************************************;
** Optimization - Nelder-Mead Method.                                       **;
******************************************************************************;
*	PROC NLP TECH=nmsimp OUT=aa;
PROC NLP INEST=start500 TECH=nmsimp 
	OUT=aa;

 *_____ LOAD PARAMETERS _____*;

                                                       ** Population counts **;
  ARRAY Nh[5] 6221 11738 4333 22809 5467;
                                                     ** Stratum cost values **;
  ARRAY cost[5] 120 80 80 90 150;
                                                   ** Means and proportions **;
  ARRAY p[4,5]  85    11    23    17   126
               511    21    70    32   157
                 0.8   0.2   0.5   0.3   0.9
                 0.06  0.03  0.03  0.21  0.77;

                                          ** Population Standard deviations **;
  ARRAY sd[4,5] 170   8.8  23 25.5 315
                255.5 5.25 35 32   471;
                                               ** Calculate for proportions **;
  DO J=3 TO 4;
    DO I=1 TO 5;
      sd[j,i] = sqrt(p[j,i] * (1 - p[j,i]) * Nh[i] / (Nh[i] - 1));
    END;      
  END;


  *_____ DECISION VARIABLES _____*;

                        ** Optimized Values = Stratum-specific Sample Sizes **;
  ARRAY n[5] n1-n5;
  DECVAR n1-n5;


  *_____ CONSTRAINTS _____*;
                                 ** Bounds on Stratum-specific Sample Sizes **;
  BOUNDS n1-n5 >= 100, 
         n1 <= 6221, n2 <= 11738, n3 <= 4333, n4 <= 22809, n5 <= 5467;

                  ** Linear Constraint = Overall Cost Constraint in Example **;
  LINCON 120*n1 + 80*n2 + 80*n3 + 90*n4 + 150*n5
         <= 300000;          

                           ** Calculate Stratum Components of Overall total **;
  ARRAY m[4,5] m1-m20;
  DO J=1 TO 4;
    DO I=1 TO 5;
      m[j,i] = p[j,i] * Nh[i];
    END;      
  END;
                                          ** Variable-specific relvariances **;
  ARRAY v[4,5]    v1 - v20;
  ARRAY var[4]    var1 - var4;
  ARRAY tot[4]    tot1 - tot4;
  ARRAY relvar[4] relvar1 - relvar4;

  DO J=1 TO 4;
    DO I=1 TO 5;
      v[j,i] = ((Nh[i]**2/n[i]) - Nh[i]) * (sd[j,i]**2);
    END;

    var[j] = v[j,1] + v[j,2] + v[j,3] + v[j,4] + v[j,5];
    tot[j] = m[j,1] + m[j,2] + m[j,3] + m[j,4] + m[j,5];
    relvar[j] = var[j] / tot[j]**2;
  END;

                               ** Non-Linear Constraints = Max Value for CV **;
  NLINCON relvar2 <= 0.0025, relvar3 <= 0.0009, relvar4 <= 0.0009;
  

  *_____ OBJECTIVE FUNCTION _____*;
                                                      ** Importance weights **;
  ARRAY impwts[4] 1 0 0 0;

                                                ** Function to be Minimized **;
  ARRAY f[4] f1-f4;
  MIN f1-f4;

  DO J=1 TO 4;
    f[j] = impwts[j] * relvar[j];
  END;
    
RUN;

/*****************************************************************************/
