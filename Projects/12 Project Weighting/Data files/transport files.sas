/********************************************************************************/
/* FILE:    transpot files.sas                                                  */
/* TOPIC:   2003 May SOFS - Reserve Component (SOFR)                            */
/* DATE:    01/28/2012                                                          */
/* AUTHOR:  R. Valliant                                                         */
/* PURPOSE: Create SAS transport files from sofr and rccpds57.                  */
/********************************************************************************/

options nocenter pageno=1 errors=1 orientation=portrait 
        nofmterr ls=200
        pagesize = 80;
        

* libname in '<pathname where SAS data file exists>'; * USE FOR SD2 DATASETS *;
* libname in v604 '<pathname where SAS data file exists>'; * USE FOR SSD DATASETS *;
* libname out xport '<pathname where you want to write the file, including FILENAME>';

libname in1 "C:\Projects\Practical Tools Book\Book Chapters\12 Project Weighting\Data files";
libname in2 "C:\Projects\Practical Tools Book\Book Chapters\12 Project Weighting\Data files\tmp";

libname out1 xport "C:\Projects\Practical Tools Book\Book Chapters\12 Project Weighting\Data files\sofr.xpt";
libname out2 xport "C:\Projects\Practical Tools Book\Book Chapters\12 Project Weighting\Data files\rccpds57.xpt";

options fmtsearch=(in);

data in2.sofr;
   set in1.sofr;
   format _all_;
run;

data in2.rccpds57;
   set in1.rccpds57;
   format _all_;
run;

proc copy in=in2 out=out1;
   select sofr;
run;

proc copy in=in2 out=out2;
   select rccpds57;
run;


