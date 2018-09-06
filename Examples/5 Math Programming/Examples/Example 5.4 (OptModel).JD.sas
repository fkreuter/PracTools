/*********************************************************************/
/* Program: Example 5.4 (OptModel).sas                               */
/* Date:    03/12/10                                                 */
/* Author:  J.Dever                                                  */
/* Purpose: Reproduce optimization results generated with Solver.    */
/* Revised:                                                          */
/*********************************************************************/
options nocenter orientation=portrait;

TITLE1 "Example 5.4";

**********************************************************************;
Title2 "Load Excel Information";
**********************************************************************;
DATA Example_54;
  LENGTH Stratum 3 FrameCts UnitCost Revenue Emplyees Revnu_SD Emply_SD 
         RCredit OffShore 8;
  LABEL Stratum  = "Stratum ID"
        FrameCts = "Sampling Frame Counts per Stratum"
        UnitCost = "Unit-specific Data CollectionCost"
        Revenue  = "Pop. Mean Revenue (Millions)"
        Emplyees = "Pop. Mean Employees"
        Revnu_SD = "Pop. Standard Deviation Revenue (Millions)"
        Emply_SD = "Pop. Standard Deviation Employees"
        RCredit  = "Pop. Proportion Claimed Research Credits"
        OffShore = "Pop. Proportion Had Offshore Affiliates";
  INPUT Stratum FrameCts UnitCost Revenue Emplyees Revnu_SD Emply_SD 
        RCredit OffShore;
  CARDS;
  1   6221  120   85  511  170.0  255.50  0.8  0.06
  2  11738   80   11   21    8.8    5.25  0.2  0.03
  3   4333   80   23   70   23.0   35.00  0.5  0.03
  4  22809   90   17   32   25.5   32.00  0.3  0.21
  5   5467  150  126  157  315.0  471.00  0.9  0.77
  ;
RUN;
                                  *Standard deviations for proportions;
DATA Example_54;
  SET Example_54;
  ARRAY p_s  RCredit  OffShore;
  ARRAY sd_s RCrdt_SD OffSh_SD;

  DO OVER p_s;
    sd_s = SQRT(p_s * (1 - p_s) * FrameCts / (FrameCts - 1));
  END;
RUN;

PROC PRINT DATA=Example_54 UNIFORM NOOBS;  RUN;

**********************************************************************;
Title2 "Sample Allocation - Initial Solution";
**********************************************************************;
PROC OPTMODEL;

 *_____ LOAD PARAMETERS _____*;
                                                 *Stratum frame counts;
   NUMBER FrameCts{1..5};
   READ DATA Example_54 INTO [_n_] FrameCts;
   PRINT FrameCts;
                                                        *Per Unit Cost;
   NUMBER UnitCost{1..5};
   READ DATA Example_54 INTO [_n_] UnitCost;
   PRINT UnitCost;
                                *Population means & standard deviations;
   NUMBER Revenue{1..5},  Emplyees{1..5}, RCredit{1..5},  OffShore{1..5},
          Revnu_SD{1..5}, Emply_SD{1..5}, RCrdt_SD{1..5}, OffSh_SD{1..5};
   READ DATA Example_54 INTO [_n_]
                             Revenue Emplyees RCredit OffShore 
                             Revnu_SD Emply_SD RCrdt_SD OffSh_SD;
   PRINT Revenue Revnu_SD;

 
  *_____ DECISION VARIABLES _____*;
                                                  *Stratum sample sizes with initial value assigments;
   VAR NSamp{i in 1..5} init 100;
   PRINT NSamp;

  *_____ CONSTRAINTS _____*;

                                  *Stratum sizes >= 100, <= Frame Sizes;
   CON SampSize{i in 1..5}: 100 <= NSamp[i] <= FrameCts[i];

                                                         *Survey Budget;
   CON Budget:  (SUM{i in 1..5} UnitCost[i] * NSamp[i]) <= 300000; 

                              *Relvariance for Mean Number of Employees;
   CON RelVar1: 
       (SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * Emply_SD[i]^2) 
        / ((SUM{i in 1..5} FrameCts[i] * Emplyees[i])^2)
       <= (0.05^2);
   PRINT ((SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * Emply_SD[i]^2) 
          / ((SUM{i in 1..5} FrameCts[i] * Emplyees[i])^2));

                *Relvariance for Proportion of Claimed Research Credits;
   CON RelVar2: 
       (SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * RCrdt_SD[i]^2) 
        / ((SUM{i in 1..5} FrameCts[i] * RCredit[i])^2)
       <= (0.03^2);

                 *Relvariance for Proportion Having Offshore Affiliates;
   CON RelVar3: 
       (SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * OffSh_SD[i]^2) 
        / ((SUM{i in 1..5} FrameCts[i] * OffShore[i])^2)
       <= (0.03^2);

  *_____ OBJECTIVE FUNCTION _____*;

   MIN f = (SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * Revnu_SD[i]^2) 
            / ((SUM{i in 1..5} FrameCts[i] * Revenue[i])^2);


  *_____ SOLUTION _____*;

   SOLVE;
     PRINT NSamp;
     PRINT (SUM{i in 1..5} NSamp[i]);
     PRINT (SQRT((SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * Revnu_SD[i]^2) 
                 / ((SUM{i in 1..5} FrameCts[i] * Revenue[i])^2)));
     PRINT (SQRT((SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * Emply_SD[i]^2) 
                 / ((SUM{i in 1..5} FrameCts[i] * Emplyees[i])^2)));
     PRINT (SQRT((SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * RCrdt_SD[i]^2) 
                 / ((SUM{i in 1..5} FrameCts[i] * RCredit[i])^2)));
     PRINT (SQRT((SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * OffSh_SD[i]^2) 
                 / ((SUM{i in 1..5} FrameCts[i] * OffShore[i])^2)));
   
   SOLVE with nmsimp / TECH=QUANEW;
     PRINT NSamp;
     PRINT (SUM{i in 1..5} NSamp[i]);
     PRINT (SQRT((SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * Revnu_SD[i]^2) 
                 / ((SUM{i in 1..5} FrameCts[i] * Revenue[i])^2)));
     PRINT (SQRT((SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * Emply_SD[i]^2) 
                 / ((SUM{i in 1..5} FrameCts[i] * Emplyees[i])^2)));
     PRINT (SQRT((SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * RCrdt_SD[i]^2) 
                 / ((SUM{i in 1..5} FrameCts[i] * RCredit[i])^2)));
     PRINT (SQRT((SUM{i in 1..5} FrameCts[i] * (FrameCts[i]/NSamp[i] - 1) * OffSh_SD[i]^2) 
                 / ((SUM{i in 1..5} FrameCts[i] * OffShore[i])^2)));

   QUIT;
RUN;

/*********************************************************************/
