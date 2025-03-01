
*Calculation for linear Regression using SAS ;
  /* First do a simple linear regression */
 /* Sample size 130 */ 

LIBNAME GBD "D:\Consulation";
PROC IMPORT OUT= GBD.Data 
            DATAFILE = "D:\Consulation\GBD\Data.sav" 
            DBMS=SPSS REPLACE;
RUN;
ODS RTF FILE = "D:\Consulation\GBD\Multiple Linear regression.RTF";
PROC REG DATA = GBD.Data ;
model JMEStunting2022 = Unsafewater Unsafesanitation UHC2021  SDI2021 HAQindex2021 Malaria  Nematode Diarrhoea HIV2021 TB Noncerealplantprotein
TotalanimalProteingmcapitaday Totalkcalpdayavail  Wheatandproducts  Riceandproducts Maizeandproducts  Milletandproducts  Sorghumandproducts
Cassavaandproducts /clb alpha=0.05;
output out = t student=res cookd = cookd h = lev;
run;
quit;
data t; set t;
  resid_sq = res*res;
run;

proc sgplot data = t;
  scatter y = lev x = resid_sq / datalabel =  S_No;
run;
quit;
proc print data = t;
  where cookd > 4/130;
  var S_No Region Country_ 
  JMEStunting2022 Unsafewater Unsafesanitation UHC2021  SDI2021 HAQindex2021 Malaria  Nematode Diarrhoea HIV2021 TB Noncerealplantprotein
TotalanimalProteingmcapitaday Totalkcalpdayavail  Wheatandproducts  Riceandproducts Maizeandproducts  Milletandproducts  Sorghumandproducts
Cassavaandproducts 
cookd;
run;

data t2; set t;
  rabs = abs(res);
run;
proc sort data  = t2;
  by descending rabs;
run;
proc print data = t2 (obs=15);
run;
ODS RTF CLOSE;

*-------------------------- Robust Regression -------------------;
    * M: M-estimation, balances robustness and efficiency.
    * MM: High efficiency and robust to outliers. 
    * S: Highly robust but computationally intensive.
    * LTS: Least Trimmed Squares, focuses on minimizing the effect of outliers.;

/* https://support.sas.com/resources/papers/proceedings/proceedings/sugi27/p265-27.pdf */
    

ODS RTF FILE = "D:\Consulation\GBD\Robust Regression for stunting with all estimation procedures - 9 Dec 2024.RTF";
* Title : Method M estimation;
PROC ROBUSTREG DATA = GBD.Data  METHOD = M  seed=9122024;
model JMEStunting2022 = Unsafewater Unsafesanitation UHC2021  SDI2021 HAQindex2021 Malaria  Nematode Diarrhoea HIV2021 TB Noncerealplantprotein
TotalanimalProteingmcapitaday Totalkcalpdayavail  Wheatandproducts  Riceandproducts Maizeandproducts  Milletandproducts  Sorghumandproducts
Cassavaandproducts /diagnostics leverage;
RUN;
* Title : Method MM estimation;
PROC ROBUSTREG DATA = GBD.Data  METHOD = MM seed=9122024;
model JMEStunting2022 = Unsafewater Unsafesanitation UHC2021  SDI2021 HAQindex2021 Malaria  Nematode Diarrhoea HIV2021 TB Noncerealplantprotein
TotalanimalProteingmcapitaday Totalkcalpdayavail  Wheatandproducts  Riceandproducts Maizeandproducts  Milletandproducts  Sorghumandproducts
Cassavaandproducts /diagnostics leverage;
Run;
* Title : Method S estimation;
PROC ROBUSTREG DATA = GBD.Data  METHOD = S ;
model JMEStunting2022 = Unsafewater Unsafesanitation UHC2021  SDI2021 HAQindex2021 Malaria  Nematode Diarrhoea HIV2021 TB Noncerealplantprotein
TotalanimalProteingmcapitaday Totalkcalpdayavail  Wheatandproducts  Riceandproducts Maizeandproducts  Milletandproducts  Sorghumandproducts
Cassavaandproducts /diagnostics leverage;
Run;
* Title : Method LTS estimation;
* h = [   n[1-0.5] + p  ]; p = number of variables;
* fwls : Final Weighted Least Squares;
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS(h=85) fwls;
model JMEStunting2022 = Unsafewater Unsafesanitation UHC2021  SDI2021 HAQindex2021 Malaria  Nematode Diarrhoea HIV2021 TB Noncerealplantprotein
TotalanimalProteingmcapitaday Totalkcalpdayavail  Wheatandproducts  Riceandproducts Maizeandproducts  Milletandproducts  Sorghumandproducts
Cassavaandproducts / diagnostics leverage ;
RUN;
ODS RTF CLOSE;



ODS RTF FILE = "D:\Consulation\GBD\Robust Regression for wasting with all estimation procedures - 9 Dec 2024.RTF";
PROC ROBUSTREG DATA = GBD.Data  METHOD = M  ;
model JMEWastingJME2022 = Unsafewater Unsafesanitation UHC2021  SDI2021 HAQindex2021 Malaria  Nematode Diarrhoea HIV2021 TB Noncerealplantprotein
TotalanimalProteingmcapitaday Totalkcalpdayavail  Wheatandproducts  Riceandproducts Maizeandproducts  Milletandproducts  Sorghumandproducts
Cassavaandproducts /diagnostics leverage;
RUN;

* Title : Method MM estimation;
PROC ROBUSTREG DATA = GBD.Data  METHOD = MM ;
model JMEWastingJME2022 = Unsafewater Unsafesanitation UHC2021  SDI2021 HAQindex2021 Malaria  Nematode Diarrhoea HIV2021 TB Noncerealplantprotein
TotalanimalProteingmcapitaday Totalkcalpdayavail  Wheatandproducts  Riceandproducts Maizeandproducts  Milletandproducts  Sorghumandproducts
Cassavaandproducts /diagnostics leverage;
Run;

* Title : Method S estimation;
PROC ROBUSTREG DATA = GBD.Data  METHOD = S ;
model JMEWastingJME2022 = Unsafewater Unsafesanitation UHC2021  SDI2021 HAQindex2021 Malaria  Nematode Diarrhoea HIV2021 TB Noncerealplantprotein
TotalanimalProteingmcapitaday Totalkcalpdayavail  Wheatandproducts  Riceandproducts Maizeandproducts  Milletandproducts  Sorghumandproducts
Cassavaandproducts /diagnostics leverage;
Run;

* Title : Method LTS estimation;
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS(h=85) fwls;
model JMEWastingJME2022 = Unsafewater Unsafesanitation UHC2021  SDI2021 HAQindex2021 Malaria  Nematode Diarrhoea HIV2021 TB Noncerealplantprotein
TotalanimalProteingmcapitaday Totalkcalpdayavail  Wheatandproducts  Riceandproducts Maizeandproducts  Milletandproducts  Sorghumandproducts
Cassavaandproducts / diagnostics leverage;
RUN;
ODS RTF CLOSE;
*------------------------------  --------------------------------------------------------;

ODS RTF FILE = "D:\Consulation\GBD\Univariate final RR for stunting - 14 Dec 2024.RTF";

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Unsafewater ;
RUN;
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Unsafesanitation  ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = UHC2021  ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = SDI2021   ;
RUN;
  
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = HAQindex2021   ;
RUN;
  
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Malaria   ;
RUN;
   
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Nematode   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Diarrhoea   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = HIV2021   ;
RUN;
 
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = TB   ;
RUN;
   
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Noncerealplantprotein   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = TotalanimalProteingmcapitaday   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Totalkcalpdayavail   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Wheatandproducts   ;
RUN;
  
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Riceandproducts   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Maizeandproducts   ;
RUN;
   
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Milletandproducts   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Sorghumandproducts   ;
RUN;
  
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunting2022 = Cassavaandproducts   ;
RUN;

ods rtf close;
  
 



















ODS RTF FILE = "D:\Consulation\GBD\Univariate final RR for wasting - 14 Dec 2024.RTF";

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Unsafewater ;
RUN;
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Unsafesanitation  ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = UHC2021  ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = SDI2021   ;
RUN;
  
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = HAQindex2021   ;
RUN;
  
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Malaria   ;
RUN;
   
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Nematode   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEStunJMEWastingJME2022ting2022 = Diarrhoea   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = HIV2021   ;
RUN;
 
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = TB   ;
RUN;
   
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Noncerealplantprotein   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = TotalanimalProteingmcapitaday   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Totalkcalpdayavail   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Wheatandproducts   ;
RUN;
  
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Riceandproducts   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Maizeandproducts   ;
RUN;
   
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Milletandproducts   ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Sorghumandproducts   ;
RUN;
  
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS fwls;
model JMEWastingJME2022 = Cassavaandproducts   ;
RUN;
ods rtf close;
 
 
*Robust Regression;
ODS RTF FILE = "D:\Consulation\GBD\RR for stunt and waste - 24 Dec 2024.RTF";
PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS(h=83) fwls;
model JMEStunting2022 = zUHC zSDI zmal zdia ztb ztotanipro ztotcalday zwheat zrice zmaize  zmilt zsorgam zcasava / diagnostics leverage ;
RUN;

PROC ROBUSTREG DATA = GBD.Data  METHOD = LTS(h=83) fwls;
model JMEWastingJME2022 = zUHC zSDI zmal zdia ztb ztotanipro ztotcalday zwheat zrice zmaize  zmilt zsorgam zcasava / diagnostics leverage ;
RUN;
ODS RTF CLOSE;



















