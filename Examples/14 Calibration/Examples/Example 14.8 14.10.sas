/***************************************************************/
/* PROGRAM: Example 14.8 14.10.sas                             */ 
/* DATE:    03/11/11                                           */ 
/* AUTHOR:  R Valliant                                         */
/* PURPOSE: Reproduce example 14.7 for weight trimming using   */
/*          SUDAAN.                                            */
/* REVISED: 02/12/18 JD Updated location and program comments. */
/***************************************************************/
options nocenter ls=250;
LIBNAME io "/rtpnfil03/rtpnfil03_vol4/NSCAW_III/Users/JDever/";

FOOTNOTE "Program: Example 14.8 14.10.sas";

****************************************************************;
TITLE "Ex. 14.8 - Import SAS transport file";
****************************************************************;
LIBNAME smho_xpt XPORT ".../Users/JDever/VDK/Ex14_7.xpt";

PROC COPY in=smho_xpt OUT=io;  RUN; 

PROC CONTENTS DATA=io.SMHO_80 ORDER=IGNORECASE;  RUN; 

PROC FREQ DATA=io.SMHO_80;
  TABLES HOSP_TYP / LIST MISSING;
RUN;

DATA SMHO_80;
  LENGTH ID 3;
  SET io.SMHO_80;
  ID = _n_;
    LABEL ID = "Unique Identification Number";
RUN;

PROC PRINT DATA=SMHO_80(obs=5) UNIFORM NOOBS;  RUN;

PROC SORT DATA=SMHO_80 OUT=TEST nodupkey;  BY ID;  RUN;

****************************************************************;
TITLE "Analysis with calibrate weights, unbounded";
** This portion of code/results is not included in Ex. 14.8 **;
****************************************************************;
PROC WTADJUST DATA=SMHO_80 DESIGN=WR ADJUST=POST;
  NEST _one_;     * No stratification or clustering;
  WEIGHT DWT;
  CLASS HOSP_TYP;
  MODEL _one_ = SEENCNT EOYCNT HOSP_TYP*BEDS;
                      * Corresponds to pop.tots in R program;
  POSTWGT 725  1349241  505345  37978  13066  9573  10077;
  IDVAR ID SEENCNT EOYCNT HOSP_TYP BEDS; 
  OUTPUT /*ADJFACTOR WTFINAL*/ 
     / PREDICTED=ALL FILENAME=CAL_WTS FILETYPE=SAS REPLACE; 
RUN; 

DATA CAL_WTS;
  SET CAL_WTS(DROP=_one_ 
              RENAME=(WTFINAL=CAL_WT ADJFACTOR=CAL_ADJ));
RUN;

PROC CONTENTS DATA=CAL_WTS;  RUN;

PROC DESCRIPT DATA=CAL_WTS DESIGN=WR;
  NEST _one_;
  WEIGHT CAL_WT;
  CLASS HOSP_TYP;
  VAR _one_ SEENCNT EOYCNT BEDS;
  TABLES HOSP_TYP;
  PRINT TOTAL SETOTAL / STYLE=NCHS;
RUN;

****************************************************************;
TITLE "Ex. 14.8 - Analysis with calibrate weights, bounded";
****************************************************************;
PROC WTADJUST DATA=CAL_WTS DESIGN=WR ADJUST=POST;
  NEST _one_;     * No stratification or clustering;
  WEIGHT DWT;
  LOWERBD 0.4;
  UPPERBD 3.0;
  CLASS HOSP_TYP;
  MODEL _one_ = SEENCNT EOYCNT HOSP_TYP*BEDS;
                      * Corresponds to pop.tots in R program;
  POSTWGT 725  1349241  505345  37978  13066  9573  10077;
  IDVAR ID SEENCNT EOYCNT HOSP_TYP BEDS CAL_WT CAL_ADJ; 
  OUTPUT 
     / PREDICTED=ALL FILENAME=BCAL_WTS /*FILETYPE=SAS*/ REPLACE; 
RUN; 

DATA BCAL_WTS;
  SET BCAL_WTS(DROP=_one_ 
              RENAME=(WTFINAL=BCAL_WT ADJFACTOR=BCAL_ADJ));

  LABEL BCAL_WT  = "Calibrated base weights w/bounded adjustments"
        BCAL_ADJ = "Bounded calibration adjustments";
RUN;

PROC CONTENTS DATA=BCAL_WTS;  RUN;

PROC DESCRIPT DATA=BCAL_WTS DESIGN=WR;
  NEST _one_;
  WEIGHT BCAL_WT;
  CLASS HOSP_TYP;
  VAR _one_ SEENCNT EOYCNT BEDS;
  TABLES HOSP_TYP;
  PRINT TOTAL SETOTAL / STYLE=NCHS;
RUN;

****************************************************************;
TITLE "Ex. 14.10 - Analysis w/bounded calibrate weights, trimmed";
****************************************************************;
PROC WTADJUST DATA=BCAL_WTS DESIGN=WR ADJUST=POST;
  NEST _one_;     * No stratification or clustering;
  WEIGHT BCAL_WT;
*  LOWERBD 0.4;
*  UPPERBD 3.0;
  LOWERBD 0;
  UPPERBD 18;
  WTMIN   2;
  WTMAX   18;
  CLASS HOSP_TYP;
  MODEL _one_ = SEENCNT EOYCNT HOSP_TYP*BEDS;
                      * Corresponds to pop.tots in R program;
  POSTWGT 725  1349241  505345  37978  13066  9573  10077;
  IDVAR ID SEENCNT EOYCNT HOSP_TYP BEDS DWT CAL_WT CAL_ADJ 
        BCAL_WT BCAL_ADJ; 
  OUTPUT 
     / PREDICTED=ALL FILENAME=TBCAL_WTS REPLACE; 
RUN; 

DATA TBCAL_WTS;
  SET TBCAL_WTS(DROP=_one_ 
              RENAME=(WTFINAL=TBCAL_WT ADJFACTOR=TBCAL_ADJ));
RUN;

PROC DESCRIPT DATA=TBCAL_WTS DESIGN=WR;
  NEST _one_;
  WEIGHT TBCAL_WT;
  CLASS HOSP_TYP;
  VAR _one_ SEENCNT EOYCNT BEDS;
  TABLES HOSP_TYP;
  PRINT TOTAL SETOTAL / STYLE=NCHS;
RUN;

****************************************************************;
** Trim weights.                                              **;
****************************************************************;
PROC WTADJUST DATA=TBCAL_WTS DESIGN=WR ADJUST=POST;
  NEST _one_;     * No stratification or clustering;
  WEIGHT DWT;
  *LOWERBD 0.4;
  *UPPERBD 3.0;
  LOWERBD 0;
  UPPERBD 18;
  WTMIN   2;
  WTMAX   18;
  CLASS HOSP_TYP;
  MODEL _one_ = SEENCNT EOYCNT HOSP_TYP*BEDS;
                      * Corresponds to pop.tots in R program;
  POSTWGT 725  1349241  505345  37978  13066  9573  10077;
  IDVAR ID SEENCNT EOYCNT HOSP_TYP BEDS DWT CAL_WT CAL_ADJ 
        BCAL_WT BCAL_ADJ TBCAL_WT TBCAL_ADJ; 
  OUTPUT / PREDICTED=ALL FILENAME=TCAL_WTS2 REPLACE; 
RUN; 

DATA TCAL_WTS2;
  SET TCAL_WTS2(DROP=_one_ 
              RENAME=(WTFINAL=TCAL2_WT ADJFACTOR=TCAL2_ADJ));
RUN;

PROC DESCRIPT DATA=TCAL_WTS2 DESIGN=WR;
  NEST _one_;
  WEIGHT TCAL2_WT;
  CLASS HOSP_TYP;
  VAR _one_ SEENCNT EOYCNT BEDS;
  TABLES HOSP_TYP;
  PRINT TOTAL SETOTAL / STYLE=NCHS;
RUN;

PROC MEANS DATA=TCAL_WTS2 NOLABELS MIN P25 P50 MEAN P75 MAX SUM CV;
  VAR DWT BCAL_WT /*CAL_WT*/ TBCAL_WT /*TCAL2_WT*/;
  TITLE "Ex. 14.8 14.10 - Summary tables with SUDAAN";
RUN;

/***************************************************************/
