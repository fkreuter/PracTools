/*********************************************************************/
/* Program: Project 1 (OptModel).sas                                 */
/* Date:    02/17/10                                                 */
/* Author:  J.Dever                                                  */
/* Purpose: Reproduce optimization results generated with Solver.    */
/* Revised: 07/19/10 Updated SEs for strata.                         */
/*********************************************************************/
options nocenter orientation=portrait;

TITLE1 "VNUV Bi-Annual Survey Division Climate Survey (Sample Allocation)";

**********************************************************************;
** Variable Formats.                                                **;
**********************************************************************;
PROC FORMAT;
  VALUE salgrad_ 1="A1-A3"
                 2="R1-R5"
                 3="M1-M3";
  VALUE tenure_  1="Less than 5 Years"
                 2="5+ Years";
  VALUE busunit_ 1="Survey Research Unit (SR)"
                 2="Computing Research Unit (CR)"
                 3="Field Operations (FO)";
RUN;

**********************************************************************;
** Macro Variables.                                                 **;
**********************************************************************;
%let maxsize =500;                            *Max overall sample size;
%let minsz_SR=125.6;              *Min size for SR domain (power calc); 
%let minsz_CR= 76.4;              *Min size for CR domain (power calc); 
%let minsz_FO=210.2;              *Min size for FO domain (power calc); 

**********************************************************************;
Title2 "Load Chapter 2 Tables 2.1, 2.4";
**********************************************************************;
DATA SampFrame;
  LENGTH StratID 3 SalGrade Tenure BusUnit 3 FrameCts PopEst1-PopEst4 
         SE_Est4 8;
  LABEL StratID  = "Stratum ID"
        BusUnit  = "Business Unit (3) within VUV-SD"
        SalGrade = "Salary Grade (3)"
        Tenure   = "Years of Tenure at VUV"
        FrameCts = "Sampling Frame Counts per Stratum"
        PopEst1  = "Proportion Answering (Strongly) Agree to Q5 by Stratum"
        PopEst2  = "Proportion Answering (Strongly) Agree to Q12 by Stratum"
        PopEst3  = "Proportion Answering (Strongly) Agree to Q15 by Stratum"
        PopEst4  = "Average Number of Classes Attended by Stratum"
        SE_Est4  = "SE for Average Number of Classes Attended by Stratum";
  StratID = _n_;

  INPUT BusUnit SalGrade Tenure FrameCts 
        PopEst1 PopEst2 PopEst3 PopEst4 SE_Est4;
  CARDS;
  1 1 1  28 0.93 0.88 0.77  8.20 0.82
  1 1 2  40 0.75 0.71 0.62 12.40 1.24
  1 2 1  96 0.84 0.80 0.69 22.30 2.23
  1 2 2 230 0.80 0.76 0.66 24.00 1.92
  1 3 1  70 0.91 0.86 0.75  8.30 0.83
  1 3 2  40 0.95 0.90 0.79  3.60 0.36
  2 1 1  86 0.99 0.94 0.92  7.22 0.72
  2 1 2  42 0.80 0.76 0.74 10.91 1.09
  2 2 1  36 0.82 0.78 0.76 19.62 1.96
  2 2 2  40 0.90 0.86 0.84 21.12 2.11
  2 3 1  12 0.97 0.92 0.90  7.30 0.73
  2 3 2  28 0.97 0.92 0.90  3.17 0.32
  3 1 1 200 0.50 0.48 0.45  4.59 0.37
  3 1 2 100 0.52 0.49 0.47  6.94 0.56
  3 2 1 280 0.75 0.71 0.68 12.49 1.00
  3 2 2 118 0.70 0.67 0.63 13.44 1.08
  3 3 1  40 0.93 0.88 0.84  4.65 0.46
  3 3 2  40 0.94 0.89 0.85  2.02 0.20
  ;
RUN;

DATA SampFrame;
  SET SampFrame;

  LENGTH SE_Est1-SE_Est4 8;
                                                  *SEs for proportions;
  ARRAY se_s SE_Est1-SE_Est3;
  ARRAY p_s  PopEst1-PopEst3;
  DO OVER p_s;
    se_s = SQRT(p_s * (1 - p_s) * FrameCts / (FrameCts - 1));
  END;

  LABEL SE_Est1  = "SE for Proportion Answering (Strongly) Agree to Q5 by Stratum"
        SE_Est2  = "SE for Proportion Answering (Strongly) Agree to Q12 by Stratum"
        SE_Est3  = "SE for Proportion Answering (Strongly) Agree to Q15 by Stratum";
RUN;

PROC PRINT DATA=SampFrame UNIFORM NOOBS;
  FORMAT SalGrade salgrad_. Tenure tenure_. BusUnit busunit_.;
  SUM FrameCts;
RUN cancel;

**********************************************************************;
Title2 "Add Domain Indicators by Stratum to Sampling Frame File";
**********************************************************************;
DATA SampFrame;
  SET SampFrame;

  LENGTH Domain1-Domain18 3;
                                      *Business Unit Reporting Domains;
  Domain1 = ( 1<=StratID<= 6);
  Domain2 = ( 7<=StratID<=12);
  Domain3 = (13<=StratID<=18);
                      *Business Unit by Salary Grade Reporting Domains;
  Domain4  = (StratID in (1,2));  
  Domain5  = (StratID in (3,4));  
  Domain6  = (StratID in (5,6));  
  Domain7  = (StratID in (7,8));  
  Domain8  = (StratID in (9,10)); 
  Domain9  = (StratID in (11,12));
  Domain10 = (StratID in (13,14));
  Domain11 = (StratID in (15,16));
  Domain12 = (StratID in (17,18));
                      *Business Unit by Salary Grade Reporting Domains;
  Domain13 = (StratID in (1,3,5));
  Domain14 = (StratID in (2,4,6));
  Domain15 = (StratID in (7,9,11));
  Domain16 = (StratID in (8,10,12));
  Domain17 = (StratID in (13,15,17));
  Domain18 = (StratID in (14,16,18));

  LABEL Domain1  = "Survey Research (SR) Reporting Domain Indicator"
        Domain2  = "Computing Research (CR) Reporting Domain Indicator"
        Domain3  = "Field Operations (FO) Reporting Domain Indicator"
        Domain4  = "Survey Research (SR) by A1-A3 Reporting Domain Indicator"
        Domain5  = "Survey Research (SR) by R1-R5 Reporting Domain Indicator"
        Domain6  = "Survey Research (SR) by M1-M3 Reporting Domain Indicator"
        Domain7  = "Computing Research (CR) by A1-A3 Reporting Domain Indicator"
        Domain8  = "Computing Research (CR) by R1-R5 Reporting Domain Indicator"
        Domain9  = "Computing Research (CR) by M1-M3 Reporting Domain Indicator"
        Domain10 = "Field Operations (FO) by A1-A3 Reporting Domain Indicator"
        Domain11 = "Field Operations (FO) by R1-R5 Reporting Domain Indicator"
        Domain12 = "Field Operations (FO) by M1-M3 Reporting Domain Indicator"
        Domain13 = "Survey Research (SR) by <5-yr Tenure Reporting Domain Indicator"
        Domain14 = "Survey Research (SR) by 5+-yr Tenure Reporting Domain Indicator"
        Domain15 = "Computing Research (CR) by <5-yr Tenure Reporting Domain Indicator"
        Domain16 = "Computing Research (CR) by 5+-yr Tenure Reporting Domain Indicator"
        Domain17 = "Field Operations (FO) by <5 yr Tenure Reporting Domain Indicator"
        Domain18 = "Field Operations (FO) by 5+ yr Tenure Reporting Domain Indicator";
RUN;

PROC PRINT DATA=SampFrame UNIFORM NOOBS;  
  VAR StratID BusUnit SalGrade Tenure Domain1-Domain18;
  FORMAT SalGrade salgrad_. Tenure tenure_. BusUnit busunit_.;
  SUM Domain1-Domain18;
RUN cancel;

**********************************************************************;
Title2 "Load Chapter 2 Table 2.5";
**********************************************************************;
DATA DomainEst;
  LENGTH DomainID 3 DomEst1 DomEst2 DomEst3 DomEst4 8;
  LABEL DomainID = "Reporting Domain ID"
        DomEst1  = "Proportion Answering (Strongly) Agree to Q5"
        DomEst2  = "Proportion Answering (Strongly) Agree to Q12"
        DomEst3  = "Proportion Answering (Strongly) Agree to Q15"
        DomEst4  = "Average Number of Classes Attended";
  DomainID = _n_;

  INPUT DomEst1 DomEst2 DomEst3 DomEst4;
  CARDS;
  0.84  0.80  0.69  18.08
  0.91  0.87  0.85  11.50
  0.67  0.63  0.60   8.95
  0.82  0.78  0.68  10.67
  0.81  0.77  0.67  23.50
  0.92  0.88  0.76   6.59
  0.93  0.88  0.86   8.43
  0.86  0.82  0.80  20.41
  0.97  0.92  0.90   4.41
  0.51  0.48  0.46   5.38
  0.74  0.70  0.66  12.77
  0.94  0.89  0.84   3.33
  0.88  0.83  0.73  15.21
  0.81  0.77  0.67  19.87
  0.94  0.90  0.88  10.56
  0.88  0.84  0.82  12.65
  0.67  0.63  0.60   8.85
  0.67  0.63  0.60   9.15
  ;
RUN;

PROC PRINT DATA=DomainEst UNIFORM NOOBS;  RUN cancel;

**********************************************************************;
Title2 "Create Population Domain Counts & Load Table 2.6";
**********************************************************************;
DATA AllRecs DomainTotals(KEEP=DomainCt1-DomainCt18);
  RETAIN DomainCt1-DomainCt18 0;
  LENGTH DomainCt1-DomainCt18 8;
  SET SampFrame(KEEP=FrameCts Domain1-Domain18) END=last;

  ARRAY dominds Domain1-Domain18;
  ARRAY domcts  DomainCt1-DomainCt18;
  DO OVER dominds;
    domcts = SUM(domcts, dominds * FrameCts);
  END;
  
  IF last THEN OUTPUT DomainTotals;
  OUTPUT AllRecs;
RUN;

PROC PRINT DATA=AllRecs;  RUN cancel;

PROC PRINT DATA=DomainTotals NOOBS;  RUN cancel;

PROC TRANSPOSE DATA=DomainTotals OUT=DomainCts;  RUN;

DATA DomainCts;
  LENGTH DomainID 3;
  SET DomainCts(DROP=_NAME_ RENAME=(COL1=DomainCts));
  LABEL DomainID  = "Reporting Domain ID"
        DomainCts = "Frame Counts per Domain";
  DomainID = _n_;

                              *Constraints on coefficient of variation;
  IF      1<=DomainID<=3  THEN DomainCV=0.10;
  ELSE IF 4<=DomainID<=18 THEN DomainCV=0.30;
RUN;

PROC PRINT DATA=DomainCts;  RUN cancel;

PROC SORT DATA=DomainEst;  BY DomainID;  RUN;
PROC SORT DATA=DomainCts;  BY DomainID;  RUN;

DATA DomainEst;
  MERGE DomainEst(in=f1) DomainCts(in=f2);
  BY DomainID;  
  IF f1 & f2;
RUN;

PROC PRINT DATA=DomainEst UNIFORM NOOBS;  RUN cancel;

**********************************************************************;
Title2 "Create proportional allocation for initial values";
**********************************************************************;
PROC FREQ DATA=SampFrame noprint;
  TABLES StratID / OUT=InitValsDS(KEEP=StratID Percent);
  WEIGHT FrameCts;
RUN;

DATA InitValsDS;
  SET InitValsDS;
  InitVals = ROUND(&maxsize. * Percent / 100);
RUN;

PROC PRINT DATA=InitValsDS UNIFORM NOOBS;
  SUM PERCENT InitVals;  
RUN cancel;

**********************************************************************;
Title2 "Sample Allocation - Initial Solution";
**********************************************************************;
PROC OPTMODEL;

  *_____ OPTIMIZATION CONSTANTS _____*;
   NUMBER nStrata  = 18;                    *Number of sampling strata;
   NUMBER nDomains = 18;                  *Number of reporting domains;
   NUMBER nMeans   = 4;                   *Number of means/proportions;

  *_____ CONSTRAINED SIZES _____*;
   NUMBER minStrat = 2;                        *Min sample per stratum; 
   NUMBER maxSamp  = &maxsize.;               *Max overall sample size;

  *_____ LOAD CHAPTER 2 TABLES _____*;
                                                 *Stratum frame counts;
   NUMBER FrameCts{1..nStrata};
   READ DATA SampFrame INTO [_n_] FrameCts;
   *PRINT FrameCts;

                                           *Population means by stratum;
   SET<NUMBER> StratID;
   NUMBER PopMeans{StratID, 1..nMeans};
   READ DATA SampFrame INTO StratID=[StratID] 
        {c in 1..nMeans} <PopMeans[StratID,c]=COL("PopEst"||c)>;
   *PRINT PopMeans;

                                 *Population standard errors by stratum;
   NUMBER PopSEs{StratID, 1..nMeans};
   READ DATA SampFrame INTO StratID=[StratID] 
        {c in 1..nMeans} <PopSEs[StratID,c]=COL("SE_Est"||c)>;
   *PRINT PopSEs;
   *PRINT {i in 1..18} PopSEs[i,1];
                                          *Domain indicators by stratum;
   NUMBER DomInds{StratID, 1..nDomains};
   READ DATA SampFrame INTO StratID=[StratID]  
        {c in 1..nDomains} <DomInds[StratID,c]=COL("Domain"||c)>;
   *PRINT DomInds;

                                                  *Domain frame counts;
   NUMBER DomainCts{1..nDomains};
   READ DATA DomainEst INTO [_n_] DomainCts;
   *PRINT DomainCts;

                                                  *Load means by domain;
   SET<NUMBER> DomainID;
   NUMBER DomMeans{DomainID, 1..nMeans};
   READ DATA DomainEst INTO DomainID=[DomainID] 
        {d in 1..nMeans} <DomMeans[DomainID,d]=COL("DomEst"||d)>;
   *PRINT DomMeans;

  *_____ OPTIMIZATION CONSTRAINTS _____*;
                                                            *Domain CVs;
   NUMBER DomainCV{1..nDomains};
   READ DATA DomainEst INTO [_n_] DomainCV;
   *PRINT DomainCV;

                                          *Initial stratum sample sizes;
   NUMBER InitVals{1..nStrata};
   READ DATA InitValsDS INTO [_n_] InitVals;
   *PRINT InitVals;
   *PRINT (SUM{i in 1..nStrata} InitVals[i]);

                                                  *Stratum sample sizes;
   *VAR NSamp{i in 1..nStrata};
   VAR NSamp{i in 1..nStrata} init InitVals[i];
   *VAR NSamp{i in 1..nStrata} init 2;

   *CONSTRAINT MinSmpSz:  minStrat <= MIN{i in 1..nStrata} NSamp[i];
   *CONSTRAINT MaxSmpFrc: 1 <= MAX{i in 1..nStrata} (NSamp[i]  / FrameCts[i]);

   CONSTRAINT SampSize{i in 1..nStrata}: 
       minStrat <= NSamp[i] <= FrameCts[i];

   *PRINT NSamp;
   *PRINT (SUM{i in 1..nStrata} NSamp[i]);

                                    *Overall sample size (study budget);
   CONSTRAINT TotSmpSize: 
       (SUM{i in 1..nStrata} NSamp[i]) <= maxSamp;

                                           *Sample size per unit domain;
   CONSTRAINT SRsmpSize: 
       (SUM{i in 1..nStrata} DomInds[i,1] * NSamp[i]) >= &minsz_SR.; 
   CONSTRAINT CRsmpSize: 
       (SUM{i in 1..nStrata} DomInds[i,2] * NSamp[i]) >= &minsz_CR.; 
   CONSTRAINT FOsmpSize: 
       (SUM{i in 1..nStrata} DomInds[i,2] * NSamp[i]) >= &minsz_FO.; 

                                                         *CVs by domain;
   CONSTRAINT DomainCVs{d in 1..nDomains, j in 1..nMeans}:
       ((SQRT(SUM{i in 1..nStrata} DomInds[i,d] * FrameCts[i] * (FrameCts[i] / NSamp[i] - 1) * PopSEs[i,j]**2)
		/ (SUM{i in 1..nStrata} DomInds[i,d] * FrameCts[i])) / DomMeans[d,j])
	   <= DomainCV[d];
   *PRINT {d in 1..nDomains, j in 1..nMeans}        
         ((SQRT(SUM{i in 1..nStrata} DomInds[i,d] * FrameCts[i] * (FrameCts[i] / NSamp[i] - 1) * PopSEs[i,j]**2)
		 / (SUM{i in 1..nStrata} DomInds[i,d] * FrameCts[i])) / DomMeans[d,j]);


  *_____ OBJECTIVE FUNCTION _____*;

                                *Importance weights for study estimates;
   NUMBER impwt{1..nMeans} = 1;
   *PRINT impwt;

                                           *Minimize sum of overall CVs;
   MIN f = SUM{j in 1..nMeans} impwt[j] * 
           (SQRT(SUM{i in 1..nStrata} FrameCts[i] * (FrameCts[i] / NSamp[i] - 1) * PopSEs[i,j]**2) 
            / (SUM{i in 1..nStrata} FrameCts[i] * PopMeans[i,j]) );
   *PRINT {j in 1..nMeans}        
         (SQRT(SUM{i in 1..nStrata} FrameCts[i] * (FrameCts[i] / NSamp[i] - 1) * PopSEs[i,j]**2) 
          / (SUM{i in 1..nStrata} FrameCts[i] * PopMeans[i,j]) );
   *PRINT (SUM{j in 1..nMeans} impwt[j] *        
          (SQRT(SUM{i in 1..nStrata} FrameCts[i] * (FrameCts[i] / NSamp[i] - 1) * PopSEs[i,j]**2) 
           / (SUM{i in 1..nStrata} FrameCts[i] * PopMeans[i,j])));

   SOLVE;
     PRINT NSamp;
     PRINT (SUM{i in 1..nStrata} NSamp[i]);
     PRINT (SUM{j in 1..nMeans} impwt[j] *        
            (SQRT(SUM{i in 1..nStrata} FrameCts[i] * (FrameCts[i] / NSamp[i] - 1) * PopSEs[i,j]**2) 
            / (SUM{i in 1..nStrata} FrameCts[i] * PopMeans[i,j])));
   
   SOLVE with NLPC / TECH=QUANEW;
     PRINT NSamp;
     PRINT (SUM{i in 1..nStrata} NSamp[i]);
     PRINT (SUM{j in 1..nMeans} impwt[j] *        
            (SQRT(SUM{i in 1..nStrata} FrameCts[i] * (FrameCts[i] / NSamp[i] - 1) * PopSEs[i,j]**2) 
            / (SUM{i in 1..nStrata} FrameCts[i] * PopMeans[i,j])));

   QUIT;
RUN;

/*********************************************************************/
