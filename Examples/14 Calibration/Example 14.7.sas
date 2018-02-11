/***************************************************************/
/* PROGRAM: Example 15.7.sas                                   */ 
/* DATE:    03/11/11                                           */ 
/* PURPOSE: Reproduce example in section 15.3.2 for weight     */
/*          trimming using SUDAAN.                             */
/* REVISED:                                                    */
/***************************************************************/
options nocenter ls=250;
LIBNAME io  "\\jpsmnds\users\share\practical tools\book\data\";
LIBNAME tmp "c:\";

FOOTNOTE "Program: Example 15.7.sas";

****************************************************************;
** Create comparable analysis file.                           **;
****************************************************************;
LIBNAME smho_xpt XPORT "\\jpsmnds\users\share\Practical Tools\Book\Data\Ex15_7.dat";

PROC COPY in=smho_xpt OUT=tmp;  RUN; 

PROC CONTENTS DATA=tmp.SMHO_80;  RUN; 

PROC FREQ DATA=tmp.SMHO_80;
  TABLES HOSP_TYP / LIST MISSING;
RUN;

DATA SMHO_80;
  LENGTH ID 3;
  SET tmp.SMHO_80;
  ID = _n_;
RUN;

PROC PRINT DATA=SMHO_80(obs=5) UNIFORM NOOBS;  RUN;

PROC SORT DATA=SMHO_80 OUT=TEST nodupkey;  BY ID;  RUN;

****************************************************************;
** Calibrate weights - unbounded.                             **;
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

PROC MEANS DATA=CAL_WTS NOLABELS MIN P25 P50 MEAN P75 SUM MAX VAR;
  VAR DWT CAL_WT;
RUN;

****************************************************************;
** Calibrate weights - bounded.                               **;
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
  OUTPUT /*ADJFACTOR WTFINAL*/ 
     / PREDICTED=ALL FILENAME=BCAL_WTS FILETYPE=SAS REPLACE; 
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

PROC MEANS DATA=BCAL_WTS NOLABELS MIN P25 P50 MEAN P75 MAX SUM VAR;
  VAR DWT BCAL_WT;
RUN;

****************************************************************;
** Trim weights.                                              **;
****************************************************************;
PROC WTADJUST DATA=BCAL_WTS DESIGN=WR ADJUST=POST;
  NEST _one_;     * No stratification or clustering;
  WEIGHT BCAL_WT;
  LOWERBD 0.4;
  UPPERBD 3.0;
  WTMIN   2;
  WTMAX   18;
  CLASS HOSP_TYP;
  MODEL _one_ = SEENCNT EOYCNT HOSP_TYP*BEDS;
                      * Corresponds to pop.tots in R program;
  POSTWGT 725  1349241  505345  37978  13066  9573  10077;
  IDVAR ID SEENCNT EOYCNT HOSP_TYP BEDS DWT CAL_WT CAL_ADJ 
        BCAL_WT BCAL_ADJ; 
  OUTPUT /*ADJFACTOR WTFINAL*/ 
     / PREDICTED=ALL FILENAME=TCAL_WTS REPLACE; 
RUN; 

DATA TCAL_WTS;
  SET TCAL_WTS(DROP=_one_ 
              RENAME=(WTFINAL=TCAL_WT ADJFACTOR=TCAL_ADJ));
RUN;

PROC DESCRIPT DATA=TCAL_WTS DESIGN=WR;
  NEST _one_;
  WEIGHT TCAL_WT;
  CLASS HOSP_TYP;
  VAR _one_ SEENCNT EOYCNT BEDS;
  TABLES HOSP_TYP;
  PRINT TOTAL SETOTAL / STYLE=NCHS;
RUN;

PROC MEANS DATA=TCAL_WTS NOLABELS MIN P25 P50 MEAN P75 MAX SUM VAR;
  VAR TCAL_WT;
RUN;

****************************************************************;
** Trim weights.                                              **;
****************************************************************;
PROC WTADJUST DATA=TCAL_WTS DESIGN=WR ADJUST=POST;
  NEST _one_;     * No stratification or clustering;
  WEIGHT DWT;
  LOWERBD 0.4;
  UPPERBD 3.0;
  WTMIN   2;
  WTMAX   18;
  CLASS HOSP_TYP;
  MODEL _one_ = SEENCNT EOYCNT HOSP_TYP*BEDS;
                      * Corresponds to pop.tots in R program;
  POSTWGT 725  1349241  505345  37978  13066  9573  10077;
  IDVAR ID SEENCNT EOYCNT HOSP_TYP BEDS DWT CAL_WT CAL_ADJ 
        BCAL_WT BCAL_ADJ TCAL_WT TCAL_ADJ; 
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

PROC MEANS DATA=TCAL_WTS2 NOLABELS MIN P25 P50 MEAN P75 MAX SUM VAR;
  VAR TCAL_WT TCAL2_WT;
RUN;

/***************************************************************/
