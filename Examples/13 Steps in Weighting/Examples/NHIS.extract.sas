*********************************************************************************;
* Extract a subset of NHIS 2003 public use file to use as illustration          *;
* in binary regression for nonresponse.                                         *;
* Date: 10/13/07                                                                *;
*********************************************************************************;

libname nhis "C:\Projects\NCHS-Adj to Svy Ests\NHIS\Data2003";
libname out "C:\JPSM\SURV 699-Wting case studies\Lecture Notes\6 Steps in Weighting\Examples";
LIBNAME  LIBRARY  'C:\Projects\NCHS-Adj to Svy Ests\NHIS\Data2003';

options nofmterr;

*********************************************************************************;
** Select subset of NHIS 2003 public use file                                  **;
*********************************************************************************;

data tmp1 
   (keep = 
   	   rectype hhx px        /* ID vars                                  	*/
   	   stratum psu
   	   sex age_p r_age1
   	   origin_I		/* Hispanic, Y 1, N = 2			     	*/
           racerpi2 MRACRPI2 
           RACRECI2		/* W = 1, B = 2, Other = 3			*/		
           R_MARITL LG_MSTAT CDCMSTAT
           PARENTS 
           WTFA                 /* final annual weight                       	*/
           EDUC_R1              /* highest level of education                	*/
           rat_cat              /* ratio of family income to poverty level   	*/
           INCGRP		/* income group					*/
           );
   set nhis.personsx;
   
run;

data out.nhispart;
  set tmp1;
  retain seed1 1298573062;
  call ranuni (Seed1,X1);
*  if rat_cat in (96, 99) then resp = 0;
*	else resp = 1;
  if X1 < 0.2;
  if age_p >= 18;
   if stratum <= 100;
   if educ_r1 < 97;		* drop unknown education
  				* Income groups (refused, not ascertained, DK) + no detail;
  if INCGRP in (97, 98, 99, 12, 13) then resp = 0;
	else resp = 1;
	
  if parents = 9 then parents = 4;	* Collapse UNK with neither mother nor father;
  if educ_r1 in (1,2) then educ_r1 = 1;
  if educ_r1 in (3,4) then educ_r1 = 3;
  if educ_r1 in (6,7,8) then educ_r1 = 6;
run;
   
LIBNAME outxp xport "C:\JPSM\SURV 699-Wting case studies\Lecture Notes\6 Steps in Weighting\Examples\nhispart.xpt";

*********************************************************************************;
** Create SAS Transport Data File.                                             **;
*********************************************************************************;

DATA outxp.nhispart (drop = X1 seed1);
  SET out.nhispart;
RUN;

proc freq data = out.nhispart;
   tables stratum psu
   	   sex age_p r_age1
   	   origin_I
           racerpi2 MRACRPI2 RACRECI2
           R_MARITL LG_MSTAT CDCMSTAT
           PARENTS 
           EDUC_R1 
           rat_cat 
           resp
     ;
run;

proc univariate data = out.nhispart;
   var age_p;
run;

proc freq data = out.nhispart;
   tables resp * sex 
   	  resp * r_age1
   	  resp * origin_I
   	  resp * racreci2
   	  resp * cdcmstat
   	  resp * parents
   	  resp * educ_r1
   	  / norow
   ;
run;


libname out "C:\JPSM\SURV 699-Wting case studies\Lecture Notes\6 Steps in Weighting\Examples";
proc contents data = out.nhispart;
run;
