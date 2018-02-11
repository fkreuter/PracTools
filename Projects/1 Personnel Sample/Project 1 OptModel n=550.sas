/*********************************************************************/
/* Program: Project 1 OptModel n=550.sas                             */
/* Date:    02/09/12                                                 */
/* Author:  J.Dever                                                  */
/* Purpose: Reproduce optimization results generated with Solver.    */
/* Revised:                                                          */
/*********************************************************************/
options nocenter orientation=portrait;

TITLE1 "VNUV Annual Survey Division Climate Survey (Sample Allocation)";
FOOTNOTE "Program: Project 1 OptModel n=550.sas";

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
%let maxsize   =550;                       *Max respondent sample size;
%let minsz_SR  =179.2;            *Min size for SR domain (power calc); 
%let minsz_CR  = 90.4;            *Min size for CR domain (power calc); 
%let minsz_FO  =216.0;            *Min size for FO domain (power calc);

%let domaincv1 =0.06;                       *Max cv for domain group 1;
%let domaincv2 =0.10;                       *Max cv for domain group 2;

**********************************************************************;
Title2 "Load Chapter 2 Tables 2.1, 2.3, 2.4";
**********************************************************************;
DATA OptParms;
  LENGTH StratID 3 SalGrade Tenure BusUnit 3 FrameCts PopEst1-PopEst4 
         STD_Est4 InEligRt ERespRt 8;
  LABEL StratID  = "Stratum ID"
        BusUnit  = "Business Unit (3) within VUV-SD"
        SalGrade = "Salary Grade (3)"
        Tenure   = "Years of Tenure at VUV"
        FrameCts = "Sampling Frame Counts per Stratum"
        PopEst1  = "Pop Proportion Answering (Strongly) Agree to Q5 by Stratum"
        PopEst2  = "Pop Proportion Answering (Strongly) Agree to Q12 by Stratum"
        PopEst3  = "Pop Proportion Answering (Strongly) Agree to Q15 by Stratum"
        PopEst4  = "Pop Average Number of Classes Attended by Stratum"
        STD_Est4 = "Pop STD for Number of Classes Attended by Stratum"
        InEligRt = "Ineligibility Rate for Cycle 7 by Stratum"
        ERespRt  = "Response Rate among Eligible Sample Members by Stratum";
  StratID = _n_;

  INPUT BusUnit SalGrade Tenure FrameCts 
        PopEst1 PopEst2 PopEst3 PopEst4 STD_Est4 
        InEligRt ERespRt;
  CARDS;
  1 1 1  30 0.93 0.88 0.77  8.20  2.4778 0.0000 0.8890
  1 1 2  44 0.75 0.71 0.62 12.40  4.4608 0.0000 0.8460
  1 2 1 106 0.84 0.80 0.69 22.30  8.5840 0.0970 0.5156
  1 2 2 253 0.80 0.76 0.66 24.00 15.7212 0.0130 0.7866
  1 3 1  77 0.91 0.86 0.75  8.30  4.1686 0.0000 0.9130
  1 3 2  44 0.95 0.90 0.79  3.60  1.1257 0.1540 0.8460
  2 1 1 118 0.99 0.94 0.92  7.22  3.3256 0.0710 0.5861
  2 1 2  89 0.80 0.76 0.74 10.91  4.0639 0.0000 0.4998
  2 2 1  86 0.82 0.78 0.76 19.62  7.3284 0.0000 0.5000
  2 2 2  73 0.90 0.86 0.84 21.12  7.6007 0.0000 0.5380
  2 3 1  12 0.97 0.92 0.90  7.30  1.4608 0.0000 1.0000
  2 3 2  40 0.97 0.92 0.90  3.17  1.0796 0.1110 0.8890
  3 1 1 230 0.50 0.48 0.45  4.59  1.5739 0.3030 0.3938
  3 1 2 115 0.52 0.49 0.47  6.94  2.0334 0.0000 0.3940
  3 2 1 322 0.75 0.71 0.68 12.49  8.1365 0.0220 0.6523
  3 2 2 136 0.70 0.67 0.63 13.44  5.9507 0.0260 0.7178
  3 3 1  48 0.93 0.88 0.84  4.65  1.9626 0.0000 1.0000
  3 3 2  46 0.94 0.89 0.85  2.02  0.6274 0.1540 0.8460
  ;
RUN;

DATA OptParms;
  LENGTH VarU_Est1-VarU_Est4 InflateRts 8;
  SET OptParms;
                            *Unit varianace for population proportions;
  ARRAY varU_s VarU_Est1-VarU_Est3;
  ARRAY p_s    PopEst1-PopEst3;
  DO OVER p_s;
    varU_s = p_s * (1 - p_s) * FrameCts / (FrameCts - 1);
  END;

  VarU_Est4 = STD_Est4**2;

  InflateRts = (1 - InEligRt) * ERespRt;

  LABEL VarU_Est1  = "Pop Unit Variance for Proportion Answering (Strongly) Agree to Q5 by Stratum"
        VarU_Est2  = "Pop Unit Variance for Proportion Answering (Strongly) Agree to Q12 by Stratum"
        VarU_Est3  = "Pop Unit Variance for Proportion Answering (Strongly) Agree to Q15 by Stratum"
        VarU_Est4  = "Pop Unit Variance for Number of Classes Attended by Stratum"
        InflateRts = "Inflation Rate for Allocation Sample Size";
RUN;

PROC PRINT DATA=OptParms UNIFORM NOOBS;
  FORMAT SalGrade salgrad_. Tenure tenure_. BusUnit busunit_.;
  SUM FrameCts;
RUN cancel;

PROC MEANS DATA=OptParms MIN MAX;
  VAR InflateRts;
RUN;

**********************************************************************;
Title2 "Add Domain Indicators by Stratum to Sampling Frame File";
**********************************************************************;
DATA OptParms;
  LENGTH Domain1-Domain18 3;
  SET OptParms;
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

PROC PRINT DATA=OptParms UNIFORM NOOBS;  
  VAR StratID BusUnit SalGrade Tenure Domain1-Domain18;
  FORMAT SalGrade salgrad_. Tenure tenure_. BusUnit busunit_.;
  SUM Domain1-Domain18;
RUN cancel;

**********************************************************************;
Title2 "Calculate Population Totals";
**********************************************************************;
DATA OptParms;
  LENGTH PopTotal1-PopTotal4 8;
  SET OptParms;

  ARRAY p_s   PopEst1-PopEst4;
  ARRAY tot_s PopTotal1-PopTotal4;
  DO OVER p_s;
    tot_s = FrameCts * p_s;
  END;

  LABEL PopTotal1 = "Pop Total Answering (Strongly) Agree to Q5 by Stratum"
        PopTotal2 = "Pop Total Answering (Strongly) Agree to Q12 by Stratum"
        PopTotal3 = "Pop Total Answering (Strongly) Agree to Q15 by Stratum"
        PopTotal4 = "Pop Total Number of Classes Attended by Stratum"
RUN;

PROC PRINT DATA=OptParms UNIFORM NOOBS;  
  VAR StratID PopTotal1-PopTotal4;
  SUM PopTotal1-PopTotal4;
RUN cancel;

**********************************************************************;
Title2 "Create proportional allocation for initial values";
**********************************************************************;
PROC FREQ DATA=OptParms noprint;
  TABLES StratID / OUT=InitValsDS(KEEP=StratID Percent);
  WEIGHT FrameCts;
RUN;

DATA InitValsDS;
  SET InitValsDS;
  InitVals = ROUND(&maxsize. * Percent / 100);
  *InitVals = ROUND((&maxsize. - 100) * Percent / 100);
RUN;

PROC PRINT DATA=InitValsDS UNIFORM NOOBS;
  SUM PERCENT InitVals;  
RUN cancel;

**********************************************************************;
Title2 "Create Domain Constraints";
**********************************************************************;
PROC TRANSPOSE DATA=OptParms(obs=1 KEEP=Domain1-Domain18) 
                OUT=DConstraints(DROP=_NAME_ _LABEL_);
RUN;

DATA DConstraints;
  LENGTH DomainID 3 DomainRelVar 8;
  SET DConstraints(DROP=COL1);

  DomainID = _n_;
                              *Constrained domain-specific relvariance;
  IF      1<=DomainID<=3  THEN DomainRelVar=(&domaincv1.)**2;
  ELSE IF 4<=DomainID<=18 THEN DomainRelVar=(&domaincv2.)**2;

  LABEL DomainID     = "Reporting Domain ID"
        DomainRelVar = "Constrained Relvariance per Domain";
RUN;

PROC PRINT DATA=DConstraints;  RUN cancel;

**********************************************************************;
Title2 "Sample Allocation - Initial Solution";
**********************************************************************;
PROC OPTMODEL initvar;

  *_____ OPTIMIZATION CONSTANTS _____*;
   NUMBER nStrata  = 18;                    *Number of sampling strata;
   NUMBER nDomains = 18;                  *Number of reporting domains;
   NUMBER nMeans   = 4;                   *Number of means/proportions;


  *_____ CONSTRAINED SIZES _____*;
   NUMBER minStrat = 2;                        *Min sample per stratum; 
   NUMBER maxSamp  = &maxsize.;           *Max overall respondent size;


  *_____ LOAD CHAPTER 2 PARAMETER TABLES _____*;
                                                 *Stratum frame counts;
   NUMBER FrameCts{1..nStrata};
   READ DATA OptParms INTO [_n_] FrameCts;
   *PRINT FrameCts;

                                         *Population totals by stratum;
   SET<NUMBER> StratID;
   NUMBER PopTotals{StratID, 1..nMeans};
   READ DATA OptParms INTO StratID=[StratID] 
        {c in 1..nMeans} <PopTotals[StratID,c]=COL("PopTotal"||c)>;
   *PRINT PopTotals;

                                  *Population unit variance by stratum;
   NUMBER PopUVars{StratID, 1..nMeans};
   READ DATA OptParms INTO StratID=[StratID] 
        {c in 1..nMeans} <PopUVars[StratID,c]=COL("VarU_Est"||c)>;
   *PRINT PopUVars;
   *PRINT {i in 1..18} PopUVars[i,1];

                                    *Sample allocation inflation rates;
   NUMBER InflateRts{1..nStrata};
   READ DATA OptParms INTO [_n_] InflateRts;
   *PRINT InflateRts;

                                         *Domain indicators by stratum;
   NUMBER DomInds{StratID, 1..nDomains};
   READ DATA OptParms INTO StratID=[StratID]  
        {c in 1..nDomains} <DomInds[StratID,c]=COL("Domain"||c)>;
   *PRINT DomInds;


  *_____ OPTIMIZATION CONSTRAINTS _____*;

                                       *Domain relvariance constraints;
   NUMBER DomainRelVar{1..nDomains};
   READ DATA DConstraints INTO [_n_] DomainRelVar;
   *PRINT DomainRelVar;

                                         *Initial stratum sample sizes;
   NUMBER InitVals{1..nStrata};
   READ DATA InitValsDS INTO [_n_] InitVals;
   *PRINT InitVals;
   *PRINT (SUM{i in 1..nStrata} InitVals[i]);

                                                 *Stratum sample sizes;
   *VAR NResp{i in 1..nStrata} init 2;
   VAR NResp{i in 1..nStrata} init InitVals[i];
   *VAR NResp{i in 1..nStrata};

   CONSTRAINT MaxSSize{i in 1..nStrata}: (NResp[i] / InflateRts[i]) <= FrameCts[i];
   CONSTRAINT MinSSize{i in 1..nStrata}: minStrat <= NResp[i];
   *PRINT NResp;
   *PRINT (SUM{i in 1..nStrata} NResp[i]);

                        *Overall respondent sample size (study budget);
   CONSTRAINT TotSmpSize: 
       (SUM{i in 1..nStrata} NResp[i]) <= maxSamp;

                         *Minimum sample size per business unit domain;
   CONSTRAINT SRsmpSize: 
       (SUM{i in 1..nStrata} DomInds[i,1] * NResp[i]) >= &minsz_SR.; 
   CONSTRAINT CRsmpSize: 
       (SUM{i in 1..nStrata} DomInds[i,2] * NResp[i]) >= &minsz_CR.; 
   CONSTRAINT FOsmpSize: 
       (SUM{i in 1..nStrata} DomInds[i,2] * NResp[i]) >= &minsz_FO.; 

                                                *Relvariance by domain;
   CONSTRAINT DomainRelVars{d in 1..nDomains, j in 1..nMeans}:
       ((SUM{i in 1..nStrata} DomInds[i,d] * FrameCts[i] * (FrameCts[i] / NResp[i] - 1) * PopUVars[i,j]) 
        / (SUM{i in 1..nStrata} DomInds[i,d] * PopTotals[i,j])^2)
	   <= DomainRelVar[d];
   *PRINT {d in 1..nDomains, j in 1..nMeans}
       ((SUM{i in 1..nStrata} DomInds[i,d] * FrameCts[i] * (FrameCts[i] / NResp[i] - 1) * PopUVars[i,j]) 
        / (SUM{i in 1..nStrata} DomInds[i,d] * PopTotals[i,j])^2);


  *_____ OBJECTIVE FUNCTION _____*;

                               *Importance weights for study estimates;
   NUMBER impwt{1..nMeans} = 1;
   *PRINT impwt;

                                          *Minimize sum of overall CVs;
   MIN f = SUM{j in 1..nMeans} impwt[j] * 
           ((SUM{i in 1..nStrata} FrameCts[i] * (FrameCts[i] / NResp[i] - 1) * PopUVars[i,j]) 
            / (SUM{i in 1..nStrata} PopTotals[i,j])^2 );
   *PRINT {j in 1..nMeans}        
         (SQRT(SUM{i in 1..nStrata} FrameCts[i] * (FrameCts[i] / NResp[i] - 1) * PopUVars[i,j]) 
          / (SUM{i in 1..nStrata} PopTotals[i,j])^2 );
   *PRINT (SUM{j in 1..nMeans} impwt[j] *        
          (SQRT(SUM{i in 1..nStrata} FrameCts[i] * (FrameCts[i] / NResp[i] - 1) * PopUVars[i,j]) 
           / (SUM{i in 1..nStrata} PopTotals[i,j]))^2);

   SOLVE;
     PRINT NResp InflateRts FrameCts;
	 *PRINT ({i in 1..nStrata} (NResp[i] / InflateRts[i]));
     PRINT (SUM{i in 1..nStrata} NResp[i]);
     PRINT (SUM{j in 1..nMeans} impwt[j] *        
            (SQRT(SUM{i in 1..nStrata} FrameCts[i] * (FrameCts[i] / NResp[i] - 1) * PopUVars[i,j]) 
            / (SUM{i in 1..nStrata} PopTotals[i,j])^2));
   
   QUIT;
RUN;

/*********************************************************************/
