/********************************************************************************/
/* FILE:    marine.tabs.sas                                                     */
/* DATE:    10/07/07                                                            */
/* AUTHOR:  Valliant                                                            */
/* PURPOSE: Tabulations related to nonresponse cell example to create           */
/* REVISED:                                                                     */
/********************************************************************************/
options nocenter pageno=1 errors=1 orientation=portrait nofmterr ls=200;

libname dat "C:\JPSM\SURV 699-Wting case studies\Lecture Notes\6 Steps in Weighting\Examples";

TITLE1 "Tabulations on Marine subset of MAY 2003 DMDC-SOFReserves SURVEY";
FOOTNOTE9 "Program: C:\JPSM\SURV 699-Wting case studies\Lecture Notes\6 Steps in Weighting\marine.tabs.sas";

/*****************************************************************/
/*   XACT2R  		 Imputed Activated 30 Days - 3 Level	 */                                                                                                                   
/*   XACT3AR  		 Imputed Activated Volunt - 3 Level 	 */                                                                                                                  
/*   XACT3BR  		 Imputed Activated Involunt - 3 Level	 */
/*   XACT4AR  		 Imputed Activated CONUS - 3 Level   	 */                                                                                                                   
/*   XACT4BR  		 Imputed Activated OCONUS - 3 Level    */                                                                                                                 
/*   XACT4CR  		 Imputed Activated Deployed - 3 Level  */                                                                                                                 
/*   XACTR    		 Imputed Activated - 2 Levels          */                                                                                                                 
/*   XCPAY1R  		 Imputed Paygrade Group 1              */                                                                                                                 
/*   XPRIORR  		 Imputed Dual Service Spouse - 2 Level */                                                                                                                 
/*   XPROG2R  		 Imputd Program - 3 Levels             */                                                                                                                 
/*   XPROG3R  		 Imputed Program - 2 Levels            */                                                                                                                 
/*   XPROGR   		 Imputed Program - 3 Levels            */                                                                                                                 
/*   XRCPY3R  		 Imputed Paygrade 24 Levels            */                                                                                                                 
/*   XRCPY4R  		 Imputed Paygrade 12 Levels            */                                                                                                                 
/*   XRETH4R  		 Imputed Race/Ethnicity - 2 Level      */                                                                                                                 
/*   XRETH5R  		 Imputed Race/Ethnicity - 3 Level      */                                                                                                                 
/*   XSEXR    		 Imputed Gender                        */                                                                                                                 
/*   XSRRCR   		 Imputed Service                       */      
/*****************************************************************/

proc freq data = dat.marines;
   tables respstat
          respstat * XRCPY4R
          ;
run;

libname full "C:\JPSM\SURV 699-Wting case studies\Projects\Project 2 - Weighting\Background Material";
proc freq data = full.sofr0507full;
   tables respstat
        XCPAY1R  
        xreth4r
        xsexr
        xsrrcr
        gender;
run;

proc freq data = full.sofr0507full;
   tables v_strat;
run;


proc freq data = full.sofr0507;
   tables respstat
        XCPAY1R  
        xreth4r
        xsexr
        xsrrcr
         ;
run;

TITLE1 "Pop counts for MAY 2003 DMDC-SOFReserves SURVEY";
FOOTNOTE9 "Program: C:\JPSM\SURV 699-Wting case studies\Lecture Notes\6 Steps in Weighting\marine.tabs.sas";

proc print data = full.rccpds57;
run;

proc freq data = full.sofr0507;
   tables
        XRCPY4R * xsrrcr
        XRCPY3R  
         ;
run;

options nocenter pageno=1 errors=1 orientation=portrait 
        nofmterr ls=200
        pagesize = 80;

data respondents;
  set full.sofr0507;
  if respstat = 1;
run;

proc freq data = respondents;
   tables xsrrcr * xsexr * srgrade5 * xprogr / list missing;
run;


  