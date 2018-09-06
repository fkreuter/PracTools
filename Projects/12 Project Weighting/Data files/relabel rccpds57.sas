/********************************************************************************/
/* FILE:    relabel rccpds57.sas                                                */
/* TOPIC:   2003 May SOFS - Reserve Component (SOFR)                            */
/* DATE:    01/27/2012                                                          */
/* AUTHOR:  R. Valliant                                                         */
/* PURPOSE: Relabel some fields in the rccpds57 data file.                      */
/********************************************************************************/

options nocenter pageno=1 errors=1 orientation=portrait 
        nofmterr ls=200
        pagesize = 80;

libname myfmtlib "C:\Projects\Practical Tools Book\Book Chapters\12 Project Weighting\Data files";
libname a "C:\Projects\Practical Tools Book\Book Chapters\12 Project Weighting\Data files";

options fmtsearch=(myfmtlib);
   
data rccpds57;
  set a.rccpds57safe;
  format _all_;
  label 
  	count    = "Count of persons"
  	service  = "Service (xsrrcr)"
	gender   = "Gender (xsexr)"
  	educcat  = "Highest level of school (sred)"
  	marit    = "Marital status (srmarst)"
  	activatd = "Length of activation in last 24 months (xact2r)"
        pg_group = "Pay grade (xcpay1r)"
	raceth   = "Race/ethnicity (xreth4r)"
	;
run;

proc contents data = rccpds57;
run;

/*
proc format library = myfmtlib;
  value activatd_
  	1 = "More than 30 days"
  	2 = "Less than 30 days"
  	3 = "Not activated" ;
run;
*/

data rccpds57;
  set rccpds57;
  format _all_;
  format educcat sred_.  ; 
  format marit srmarst_.;
  format activatd  xact2r_. ;
  format pg_group xcpay1r_.;
  format raceth xreth4r_.;
  format gender   xsexr_.  ;
  format service  xsrrcr_.;
run;

proc contents data = rccpds57;
run;

proc freq data = rccpds57;
  weight count;
  table 
  	service 
  	gender  
  	educcat 
  	marit   
  	activatd
  	pg_group
  	raceth ;
run;