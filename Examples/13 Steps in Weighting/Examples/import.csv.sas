proc import out= work.NHIS 
            datafile= "c:\pROJECTS\pRACTICAL tOOLS bOOK\dATA\NHIS.CSV" 
            dbms=csv replace;
     getnames=yes;
     datarow=2; 
run;
