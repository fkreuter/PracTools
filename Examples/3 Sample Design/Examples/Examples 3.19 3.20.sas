/***********************************************************************/
/* PROGRAM: Examples 3.19 3.20.sas                                     */
/* PROJECT: Practical Tools for Designing and Weighting Survey Samples */
/* DATE:    07/10/10                                                   */
/* AUTHOR:  J.Dever                                                    */
/* PURPOSE: Produce results for Examples 3.19 and 3.20.                */
/* REVISED:                                                            */
/***********************************************************************/
options nocenter pageno=1 errors=1 orientation=portrait nofmterr ls=200;
LIBNAME inxp xport "\\Jpsmnds\Users\Share\Practical tools\Class Material\Data\smho98.xpt";

************************************************************************;
** Load SAS Transport Data File.                                      **;
************************************************************************;
DATA SMHO98(KEEP=STRATUM HospID BEDS);  
  SET inxp.SMHO98;
  HospID = _n_;
RUN;

PROC SORT DATA=SMHO98 nodupkey;  BY HospID;  RUN;

PROC CONTENTS DATA=SMHO98;  RUN cancel;

************************************************************************;
TITLE2 "Example 3.19";
************************************************************************;
DATA SMHO98;
  LENGTH stratum6 3;
  SET SMHO98;
                                        *Create 6-level stratum variable;
  IF       1<=STRATUM<=2  THEN stratum6=1;
  ELSE IF  3<=STRATUM<=4  THEN stratum6=2;
  ELSE IF  5<=STRATUM<=8  THEN stratum6=3;
  ELSE IF  9<=STRATUM<=10 THEN stratum6=4;
  ELSE IF 11<=STRATUM<=13 THEN stratum6=5;
  ELSE IF 14<=STRATUM<=16 THEN stratum6=6;
RUN;

PROC FREQ DATA=SMHO98;
  TABLES stratum6*STRATUM stratum6
    / LIST MISSING;
RUN;

PROC SURVEYSELECT DATA=SMHO98 OUT=SampData
     METHOD=SRS SAMPSIZE=(10 10 10 10 10 10) SEED=82841;
  STRATA stratum6;
RUN;  

PROC FREQ DATA=SampData;
  TABLES stratum6 / LIST MISSING;
RUN;

PROC CONTENTS DATA=SampData;  RUN;

PROC SORT DATA=SampData;  BY stratum6;  RUN;

PROC MEANS DATA=SampData MIN MEAN MAX SUM;
  VAR SelectionProb SamplingWeight;
  BY stratum6;
RUN;

PROC MEANS DATA=SampData MIN MEAN MAX SUM;
  VAR SelectionProb SamplingWeight;
RUN;

************************************************************************;
TITLE2 "Example 3.20";
************************************************************************;
DATA SMHO98inp DROPCASE;
  LENGTH stratum5 3;
  SET SMHO98;
                                        *Eliminate outpatient facilities;
  IF BEDS<1 THEN OUTPUT DROPCASE;
  ELSE DO; 
                                        *Create 5-level stratum variable;
    IF       1<=STRATUM<=2  THEN stratum5=1;
    ELSE IF  3<=STRATUM<=4  THEN stratum5=2;
    ELSE IF  5<=STRATUM<=8  THEN stratum5=3;
    ELSE IF  9<=STRATUM<=13 THEN stratum5=4;
    ELSE IF 14<=STRATUM<=16 THEN stratum5=5;
                                                           *Size measure;
    sqrtBEDS = sqrt(BEDS);
	OUTPUT SMHO98inp;
  END;
RUN;

PROC FREQ DATA=SMHO98inp;
  TABLES stratum5*STRATUM stratum5
    / LIST MISSING;
RUN;

/*PROC SORT DATA=SMHO98inp;  BY stratum5;  RUN;*/

PROC SURVEYSELECT DATA=SMHO98inp OUT=StratSiz N=50;
  STRATA stratum5 / ALLOC=PROP NOSAMPLE;
RUN;  

PROC PRINT DATA=StratSiz;  
  VAR stratum5 SampleSize;
  SUM SampleSize;
RUN;

PROC CONTENTS DATA=StratSiz;  RUN;

PROC SORT DATA=StratSiz(KEEP=stratum5 SampleSize);
  BY stratum5;  
RUN;

PROC SURVEYSELECT DATA=SMHO98inp OUT=SampDat2
     METHOD=PPS_SYS SAMPSIZE=StratSiz SEED=4297005;
  STRATA stratum5;
  SIZE sqrtBEDS;
  ID HospID;
RUN;  
                                          *Identify duplicate selections;
PROC SORT DATA=SampDat2 OUT=DUPCHECK nodupkey;
  BY HospID;
RUN;

PROC CONTENTS DATA=SampDat2;  RUN;

/***********************************************************************/
