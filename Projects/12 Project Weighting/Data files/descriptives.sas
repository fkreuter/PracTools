/********************************************************************************/
/* FILE:    descriptives.sas                                                    */
/* TOPIC:   2003 May SOFS - Reserve Component (SOFR)                            */
/* DATE:    10/29/2010                                                          */
/* AUTHOR:  R. Valliant                                                         */
/* PURPOSE: Basic tabulations on data files for ch. 13 weighting project        */
/* REVISED:                                                                     */
/********************************************************************************/

options nocenter pageno=1 errors=1 orientation=portrait 
        nofmterr ls=200
        pagesize = 80;

libname library "C:\Projects\Practical Tools Book\Book Chapters\13 Project Weighting\Data files";
libname a "C:\Projects\Practical Tools Book\Book Chapters\13 Project Weighting\Data files";

title "Contents of SOFR file";
proc contents data = a.sofr;
run;

title "Contents of RCCPDS pop count file";
proc contents data = a.rccpds57;
run;

title "Frequencies on SOFR file variables";
proc freq data = a.sofr;
  tables ra008 			/* reenlist         	*/
  		ra115 			/* prepared for job 	*/
  		ra118 			/* stress           	*/
  		ra006a 			/* compensation			*/
  		ra006b 			/* type of work			*/
  		ra112ra 		/* days compensated		*/
        sred 			/* education			*/
        srmarst 		/* marital status		*/
        xact2r 			/* activated 30 days	*/
        xcpay1r 		/* paygrade				*/
        xreth4r 		/* race-ethnicity		*/
        xsexr 			/* gender				*/
        xsrrcr			/* service				*/
        respstat		/* response status		*/
     ;
run;

proc freq data = a.sofr;
	tables  stratum
			v_strat
			stratum * v_strat / list
	;
run;

title "List of pop counts from RCCPDS57";
proc print data = a.rccpds57;
run;
         
data sofr45;
	set a.sofr;
	if (stratum = 4 | stratum = 5);
run;

title "Records from sofr45";
proc print data = sofr45;
run;
