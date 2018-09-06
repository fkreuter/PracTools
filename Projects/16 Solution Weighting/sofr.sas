* Written by R;
*  write.foreign(df = sofr.cal[, fields], datafile = paste(file_loc2,  ;

PROC FORMAT;
value p_cls_10 
     1 = "[0.0981,0.166]" 
     2 = "(0.166,0.213]" 
     3 = "(0.213,0.264]" 
     4 = "(0.264,0.3]" 
     5 = "(0.3,0.36]" 
     6 = "(0.36,0.446]" 
     7 = "(0.446,0.519]" 
     8 = "(0.519,0.569]" 
     9 = "(0.569,0.627]" 
     10 = "(0.627,0.735]" 
;

DATA  rdata ;
INFILE  "C:\Projects\Practical Tools Book\Book Chapters\18 Solution Weighting\sofr.cal.sas.csv" 
     DSD 
     LRECL= 220 ;
INPUT
 rec_id
 nr_class
 respstat
 stratum
 nsamp
 nstrat
 v_strat
 srmarst
 sred
 xsrrcr
 xact2r
 xreth4r
 xsexr
 xcpay1r
 ra006a
 ra006b
 ra008
 ra115
 ra118
 ra112ra
 pred_logit
 p_class_10
 unwt_rr
 wt_rr
 d0
 d1
 a1
 d2
 a2
 d3
;
LABEL  rec_id = "rec.id" ;
LABEL  nr_class = "nr.class" ;
LABEL  v_strat = "v.strat" ;
LABEL  pred_logit = "pred.logit" ;
LABEL  p_class_10 = "p.class.10" ;
LABEL  unwt_rr = "unwt.rr" ;
LABEL  wt_rr = "wt.rr" ;
FORMAT p_class_10 p_cls_10. ;
RUN;
