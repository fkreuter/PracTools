/********************************************************************************/
/* FILE:    relabel sofr.sas                                                    */
/* TOPIC:   2003 May SOFS - Reserve Component (SOFR)                            */
/* DATE:    01/27/2012                                                          */
/* AUTHOR:  R. Valliant                                                         */
/* PURPOSE: Relabel some fields in the sofr data file. Write out new format     */
/*          catalog.                                                            */
/********************************************************************************/

options nocenter pageno=1 errors=1 orientation=portrait 
        nofmterr ls=200
        pagesize = 80;

libname myfmtlib "C:\Projects\Practical Tools Book\Book Chapters\12 Project Weighting\Data files";
libname a "C:\Projects\Practical Tools Book\Book Chapters\12 Project Weighting\Data files";
   
data sofr;
  set a.sofrsafe;
  label 
  	ra008   = "Retention intention"
  	ra115   = "Preparation rating"
  	ra118   = "Stress rating"
  	ra006a  = "Satisfaction with compensation"
  	ra006b  = "Satisfaction with type of work"
  	ra112ra = "Days in compensated status"
  	sred    = "Highest level of school"
  	srmarst = "Marital status"
  	xact2r  = "Length of activation in last 24 months"
        xcpay1r = "Pay grade"
	xreth4r = "Race/ethnicity"
	xsexr   = "Gender"
	xsrrcr  = "Service"
        ;
run;

proc contents data = sofr;
run;


proc format library = myfmtlib;
  value ra008_
  	1 = "Very unlikely"
  	2 = "Unlikely"
  	3 = "Neither likely nor unlikely"
  	4 = "Likely"
  	5 = "Very likely";
  value ra115_
  	1 = "Very poorly prepared"
  	2 = "Poorly prepared"
  	3 = "Neither well nor poorly prepared"
	4 = "Well prepared"
  	5 = "Very well prepared";
  value ra118_
  	1 = "Much less than usual"
  	2 = "Less than usual"
  	3 = "About the same as usual"
	4 = "More than usual"
  	5 = "Much more than usual";  
  value ra006a_ 
  	1 = "Very dissatisfied"
	2 = "Dissatisfied"
	3 = "Neither satisfied nor dissatisfied"
	4 = "Satisfied"
	5 = "Very satisfied";
  value ra006b_ 
  	1 = "Very dissatisfied"
	2 = "Dissatisfied"
	3 = "Neither satisfied nor dissatisfied"
	4 = "Satisfied"
	5 = "Very satisfied";
  value respstat_
	1  = "Questionnaire Returned-Completed"
	2  = "Questionnaire Returned-(Sufficient) Partial Complete"
	3  = "Questionnaire Returned-(Insufficient) Partial Complete"
	4  = "Questionnaire Returned-Ineligible"
	5  = "Questionnaire Returned-Blank"
	18 = "No Return-Deceased"
	19 = "No Return-Incarcerated"
	22 = "No Return-Separated/Retired"
	23 = "No Return-Active Refusal"
	25 = "No Return-Other"
	26 = "No Return-Eligible based on administrative records"
	27 = "Postal Non-delivery"
	29 = "Not Locatable"
	35 = "Ineligible-No Questionnaire Sent" ;

  value sred_
        1 = "12 years or less of school (no diploma)"
	2 = "High school graduate, high school diploma or equivalent"
	3 = "Some college credit, but less than 1 year"
	4 = "1 or more years of college, no degree"
	5 = "Associate degree (e.g., AA, AS)"
	6 = "Bachelor’s degree (e.g., BA, AB, BS)"
	7 = "Master’s, doctoral or professional school degree";
  value srmarst_
  	1 = "Married"
	2 = "Separated"
	3 = "Divorced"
	4 = "Widowed"
	5 = "Never Married";
  value xact2r_
	1 = "Activated 30 Days or Less"
	2 = "Activated More Than 30 Days"
	3 = "Not activated";
  value xcpay1r_
  	1 = "E1-E3"
  	2 = "E4"
	3 = "E5-E6"
	4 = "E7-E9" 
	5 = "W1-W5"  
	6 = "O1-O3" 
	7 = "O4-O6";
  value xreth4r_
  	1 = "NonHispanic White" 
	2 = "Minority";
  value xsexr_
  	1 = "Male"
  	2 = "Female";
  value xsrrcr_
  	1 = "Army National Guard"
	2 = "Army Reserve"
	3 = "Navy Reserve"
	4 = "Marine Corp Reserve"
	5 = "Air National Guard"
	6 = "Air Force Reserve"
	8 = "No reserve component"; 
run;

options fmtsearch=(myfmtlib);

data a.sofr;
  set sofr;
  format _all_;
  format ra008   ra008_.;
  format ra115   ra115_.;  
  format ra118   ra118_.;  
  format ra006a  ra006a_.;
  format ra006b  ra006b_.; 
  format respstat respstat_. ;
  format sred    sred_.  ; 
  format srmarst srmarst_.;
  format xact2r  xact2r_. ;
  format xcpay1r xcpay1r_.;
  format xreth4r xreth4r_.;
  format xsexr   xsexr_.  ;
  format xsrrcr  xsrrcr_.;
/*  
  if ra008 = N then ra008 = .;
  if ra008 = B then ra008 = .;
  if ra115 = N then ra115 = .;
  if ra115 = B then ra115 = .;
  if ra118 = N then ra118 = .;
  if ra118 = B then ra118 = .;
  if ra006a = N then ra006a = .;
  if ra006a = B then ra006a = .;
  if ra006b = N then ra006b = .;
  if ra006b = B then ra006b = .;
  if ra112ra = N then ra112ra = .;
  if ra112ra = B then ra112ra = .;
  if sred = N then sred = .;
  if sred = B then sred = .;
  if srmarst = N then srmarst = .;
  if srmarst = B then srmarst = .;
  if xact2r = N then xact2r = .;
  if xact2r = B then xact2r = .;
  if xcpay1r = N then xcpay1r = .;
  if xcpay1r = B then xcpay1r = .;
  if xreth4r = N then xreth4r = .;
  if xreth4r = B then xreth4r = .;
  if xsexr = N then xsexr = .;
  if xsexr = B then xsexr = .;
  if xsrrcr = N then xsrrcr = .;
  if xsrrcr = B then xsrrcr = .;
*/
run;

proc contents data = a.sofr;
run;

proc freq data = a.sofr;
  table ra008   
	ra115   
	ra118   
	ra006a  
	ra006b  
	respstat
	sred    
	srmarst 
	xact2r  
	xcpay1r 
	xreth4r 
	xsexr   
	xsrrcr  ;
run;

proc univariate data = a.sofr;
  var ra112ra;
run;