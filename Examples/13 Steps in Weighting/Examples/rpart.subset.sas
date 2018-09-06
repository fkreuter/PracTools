/********************************************************************************/
/* FILE:    rpart.subset.sas                                                    */
/* TOPIC:   2003 May SOFS - Reserve Component (SOFR)                            */
/* DATE:    10/07/07                                                            */
/* AUTHOR:  Valliant                                                            */
/* PURPOSE: Create subset of Reserve file to use in rpart example to create     */
/*          nonresponse adjustment cells					*/
/* REVISED:                                                                     */
/********************************************************************************/
options nocenter pageno=1 errors=1 orientation=portrait nofmterr ls=200;

libname sofr "C:\JPSM\SURV 699-Wting case studies\DMDC-SOFReserves\SOFR May 2003\DATA_SAS";
libname out "C:\JPSM\SURV 699-Wting case studies\Lecture Notes\6 Steps in Weighting\Examples";

TITLE1 "Create Marine subset of MAY 2003 DMDC-SOFReserves SURVEY";
FOOTNOTE9 "Program: C:\JPSM\SURV 699-Wting case studies\Lecture Notes\6 Steps in Weighting\rpart.subset.sas";

proc contents data = sofr.SFR0305P;
run;

data out.marines
  (keep =
  	ELIGFLGW 
  	FINALWGT
  	XACT2R  		/* Imputed Activated 30 Days - 3 Level	 */                                                                                                                   
   	XACT3AR  		/* Imputed Activated Volunt - 3 Level 	 */                                                                                                                  
   	XACT3BR  		/* Imputed Activated Involunt - 3 Level	 */
   	XACT4AR  		/* Imputed Activated CONUS - 3 Level   	 */                                                                                                                   
   	XACT4BR  		/* Imputed Activated OCONUS - 3 Level    */                                                                                                                 
   	XACT4CR  		/* Imputed Activated Deployed - 3 Level  */                                                                                                                 
   	XACTR    		/* Imputed Activated - 2 Levels          */                                                                                                                 
   	XCPAY1R  		/* Imputed Paygrade Group 1              */                                                                                                                 
   	XPRIORR  		/* Imputed Dual Service Spouse - 2 Level */                                                                                                                 
   	XPROG2R  		/* Imputd Program - 3 Levels             */                                                                                                                 
   	XPROG3R  		/* Imputed Program - 2 Levels            */                                                                                                                 
   	XPROGR   		/* Imputed Program - 3 Levels            */                                                                                                                 
   	XRCPY3R  		/* Imputed Paygrade 24 Levels            */                                                                                                                 
   	XRCPY4R  		/* Imputed Paygrade 12 Levels            */                                                                                                                 
   	XRETH4R  		/* Imputed Race/Ethnicity - 2 Level      */                                                                                                                 
   	XRETH5R  		/* Imputed Race/Ethnicity - 3 Level      */                                                                                                                 
   	XSEXR    		/* Imputed Gender                        */                                                                                                                 
   	XSRRCR   		/* Imputed Service                       */)
   	;
  set sofr.SFR0305P;
  if XSRRCR=4;          /* select Marines */
   ;
run;

proc contents data = out.marines;
run;

LIBNAME outxp xport "C:\JPSM\SURV 699-Wting case studies\Lecture Notes\6 Steps in Weighting\\Examples\marines.xpt";

*********************************************************************************;
** Create SAS Transport Data File.                                             **;
*********************************************************************************;

DATA outxp.SFR0305P;
  SET out.marines;
RUN;

