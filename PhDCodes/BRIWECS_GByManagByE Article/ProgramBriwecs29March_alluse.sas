/* Generated Code (IMPORT) */
/* Source File: Genodata_200.xlsx */
/* Source Path: /folders/myshortcuts/SAS */
/* Code generated on: 2/9/18, 3:10 PM */

%web_drop_table(WORK.IMPORT);


FILENAME REFFILE '/folders/myshortcuts/SAS/Genodata_200.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=RAWDATA.IMPORT;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.IMPORT; RUN;


%web_open_table(WORK.IMPORT);

/* ?jeudi ?15 ?février ?2018*/
 PROC IMPORT DATAFILE="/folders/myshortcuts/SAS/Genodata_200.xlsx"
	DBMS=XLSX
	OUT=RAWDATA.IMPORT;
	GETNAMES=YES;
RUN;
 
 /* PCA Analysis*/
proc princomp data=Rawdata.genodrought;
run;

/* Data Briwecs
/* Let us import and copy the data into SAS */
PROC IMPORT DATAFILE= "/folders/myshortcuts/SAS/Geno Data/Briwecs_2015.xlsx"
	DBMS=XLSX
	OUT=RAWDATA.Briwecs_2015;
	GETNAMES=YES;
RUN;
	
proc reg;
      model Biomass_bio=Plantheight;
   run;

/*proc mixed data=heights;
      class Family Gender;
      model Height = Gender Family Family*Gender;
   run;   (As an example of model) from R modbri1=lmer(Biomass_bio~Treatment+(1|BRISONr)+(1|Block/Row)+(1|Block/Column), data=Briwecs_2015)*/
  
  
  PROC MIXED DATA=rawdata.briwecs_2015;
  		CLASS Treatment BRISONr; 
  		MODEL Plantheight= Treatment BRISONr  Treatment*BRISONr;
  		RUN; /* here there is no random effect* /
  
  /*let us run the now with genotypes as ramdom effect */
  
   PROC MIXED DATA=rawdata.briwecs_2015;
  		CLASS Block Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Treatment*BRISONr;
  		LSMEANS BRISONr / Block ; 
  		RUN; /* this model has a big Bic Fit Statistics
-2 Res Log Likelihood	22491.6
AIC (Smaller is Better)	22495.6
AICC (Smaller is Better)	22495.6
BIC (Smaller is Better)	22504.6 */ 

  	
  	PROC GLM DATA=rawdata.briwecs_2015;
  		CLASS Block Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr;
  		RANDOM Block;
  		MEANS BRISONr;
  		LSMEANS BRISONr; 
  		RUN;
  /*Let constuct a Split plot design; The SPLIT PLOT design just incopore BLOCKING in the model
  
   * proc anova data=Split;
      class Block A B;
      model Response = Block A Block*A B A*B;
      test h=A e=Block*A;
   run;*/ 
  	
  PROC ANOVA DATA=rawdata.briwecs_2015;
  		CLASS Block Treatment BRISONr;
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr Block*BRISONr*Treatment;
  		MEA
  		TEST H=BRISONr E=Treatment*BRISONr;
  	RUN;
  	
  /* Let us Construct a model where Row and Column nested in Bloc or rep are Random effects. */
 /* proc nested data=Turnip;
      classes plant leaf;
      var calcium;
   run;*/
 
 PROC NESTED DATA=rawdata.briwecs_2015;
  		CLASS Block Treatment BRISONr; 
  		CLASSES Treatment BRISONr;
  		VAR Plantheight;
 	RUN; /*  This not what I want to construct as nested model.*/
  	
  	
  /* let us import the new data*/
  PROC IMPORT DATAFILE="/folders/myfolders/Briwecs_2015.xlsx"
	DBMS=XLSX
	OUT=WORK.Br_2015;
	GETNAMES=YES;
RUN;
  
  PROC GLM DATA=Work.briwecs_2015;
  		CLASS Block Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Block;
  		LSMEANS BRISONr / Block ; 
  		RUN;
  		
   PROC MIXED DATA=Work.briwecs_2015;
  		CLASS Block Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Block;
  		LSMEANS BRISONr; 
  		RUN;/* This model has a smaller Bic Fit Statistics
-2 Res Log Likelihood 	4087.5
AIC (Smaller is Better) 	4089.5
AICC (Smaller is Better) 	4089.5
BIC (Smaller is Better) 	4088.2 */ 

/* let us include Row Column as Randomn effect */
PROC MIXED DATA=Work.briwecs_2015;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Block Row Column;
  		LSMEANS BRISONr; 
 	    RUN; /* This one is more better, the residual has decreased and the BIC also Fit Statistics
-2 Res Log Likelihood 	3991.8
AIC (Smaller is Better) 	3997.8
AICC (Smaller is Better) 	3997.8
BIC (Smaller is Better) 	3993.9
  		*/ 
  
PROC MIXED DATA=Work.briwecs_2015;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Block Row Column Row*Column;
  		LSMEANS BRISONr; /*this one is not that good like the previous Fit Statistics
-2 Res Log Likelihood 	3991.8
AIC (Smaller is Better) 	3999.8
AICC (Smaller is Better) 	3999.9
BIC (Smaller is Better) 	3994.6 */

PROC NESTED DATA=Work.briwecs_2015;
  		CLASS Block Row Column Treatment BRISONr; 
  		VAR Plantheight;
  		BY descending BRISONr  descending Treatment descending Block ;
  		RUN; /* ERROR: Data set WORK.BRIWECS_2015 is not sorted in descending sequence. The current BY group has Treatment = HN_NF and the next BY 
        group has Treatment = HN_WF.*/
  		
  		
PROC NESTED DATA=Work.briwecs_2015;
  		CLASS Block Row Column Treatment BRISONr; 
  		VAR Plantheight;
  	    BY BRISONr;
  		RUN; 
  
   /* let us import the new data*/
PROC IMPORT DATAFILE="/folders/myfolders/Briwecs_2017.xlsx"
	DBMS=XLSX
	OUT=WORK.briwecs_2017;
	GETNAMES=YES;
RUN;
  
PROC NESTED DATA=Work.briwecs_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		VAR Plantheight;
  		BY descending BRISONr  descending Treatment descending Block ;
  		RUN;
  		
  		
/* let´s work on 2017 */
PROC MIXED DATA=Work.briwecs_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Block Row Column;
  		LSMEANS BRISONr; 
 	    RUN; 
 	    
 	    
 	    
 	    
PROC MIXED  DATA=Work.briwecs_2017;
 	    BY notsorted  Block;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Block Row Column;
  		LSMEANS Treatment*BRISONr; 
 	    RUN; 
 	    
 	 /* Sort and print*/ 
 PROC SORT data=Work.briwecs_2017 out=Work.briwecs_2017sorted;
 		BY Block Treatment BRISONr;
 PROC PRINT data=Work.briwecs_2017sorted;
 RUN;
 
 /*let´s try afgain*/
PROC NESTED DATA=Work.briwecs_2017sorted;
  		CLASS Block Row Column Treatment BRISONr; 
  		VAR Plantheight;
  		BY BRISONr ;
  		RUN; /* Till then the nested procedure doesn´t help*/
  		
 
	
  /* let us import the new data*/
PROC IMPORT DATAFILE="/folders/myfolders/sasuser.v94/Briwecs_2017sortedbysasdescendin.xlsx"
	DBMS=XLSX
	OUT=WORK.Briwecs_2017sortedbysas;
	GETNAMES=YES;
RUN;
 
 PROC MIXED DATA=Briwecs_2017sortedbysas; 	
 		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Block Row Column;
 Proc Means;   BY  Block; BY Treatment;
  		RUN;
/* 20  March 2018*/

PROC IMPORT OUT= WORK.Briwecs_2017 
            DATAFILE= "C:\Users\Koua\Desktop\data file majid\Briwecs_Fin
al\Briwecs_Bonnanalysis\Briwecs_2015.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
 		
PROC MIXED DATA=Work.briwecs_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Block Row Column;
  		Store Work.Outmodel1; 
 	    RUN; 	/*Parfect worked well*/
/*let us run the PLM Procedure*/
PROC PLM Restore= Work.Outmodel1;
 LSMEANS Treatment*BRISONr;
 BYLEVEL Block; 
 RUN; /* The PLM worked perfect */ /*But the Matter is we can´t generate from the Threelevel BLOCK-> Treatment-> Briso*/

/* The Predicted value has been corrected by the Model´*/

/*let us run the PLM Procedure for LMESTIMATE*/
PROC PLM Restore= Work.Outmodel1;
 LSMESTIMATE Treatment*BRISONr;
 RUN;
PROC MIXED DATA=Work.briwecs_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Block*Treatment*BRISONr;
  		RANDOM Row Column;
  		Store Work.Outmodel2; 
 	    RUN; /*WARNING: Stopped because of infinite likelihood*/

PROC MIXED DATA=Work.briwecs_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= ;
  		RANDOM Row Column;
  		Store Work.Outmodel2; 
 	    RUN; /*WARNING: Stopped because of infinite likelihood*/

PROC MIXED DATA=Work.briwecs_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Block*Treatment*BRISONr;
  		RANDOM Row Column;
  		Store Work.Outmodel2; 
 	    RUN;

		PROC MIXED DATA=Work.briwecs_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Block (Row) Column;
	 	Store Work.Outmodel3; 
 	    RUN; 	/* SUPER!!! THIS model Row nested in Block can better correct the medel rather than row column Fit Statistics 
-2 Res Log Likelihood 3809.8 
AIC (Smaller is Better) 3815.8 
AICC (Smaller is Better) 3815.8 
BIC (Smaller is Better) 3822.4 VERY GOOD MODEL /*RANDOM Block (Row) Column

		Covariance Parameter Estimates 
Cov Parm Estimate 
Block(Row) 1.4937 
Column 1.3260 
Residual 7.9027 



Fit Statistics
-2 Res Log Likelihood	3822.0
AIC (Smaller is Better)	3828.0
AICC (Smaller is Better)	3828.0
BIC (Smaller is Better)	3832.5

If we design the experiment design like previously it can be very good*/		/*Parfect worked well*/

/*let us run the PLM Procedure*/
PROC PLM Restore= Work.Outmodel3;
 LSMEANS Treatment*BRISONr;
 BY  Block;
 RUN; /* The PLM worked perfect */ /*But the Matter is we can´t generate from the Threelevel BLOCK-> Treatment-> Briso*/

 /*the LSMEANS is consequently slity diffrent*/
 Indeed the creation of unique cordinates xy is justs the heatmap the TRAIT and LOOK at the positional variation. 

 /*You schould take into consideration ROW COL from the disign*/

 PROC PLM Restore= Work.Outmodel3;
 LSMEANS Block*Treatment*BRISONr;
 RUN;


 PROC MIXED DATA=Work.briwecs_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr Block (Treatment*BRISONr);
  		RANDOM Row Column;
	 	Store Work.Outmodel4; 
 	    RUN; 
 PROC PLM Restore= Work.Outmodel4;
 LSMEANS Block(Treatment*BRISONr);
 RUN; 

 /* Let us learn PROC Means */
 /*proc means data=GradeBySection min max median;

 Note about code 	

   by Section;

 Note about code 	

   var Score;

 Note about code 	

   class Status Year;

 Note about code 	

   title1 'Final Exam Scores for Student Status and Year of Graduation';
   title2 ' Within Each Section';
run;*/

 PROC MEANS DATA= Work.briwecs_2017 mean min max median; 
 		BY Block;
		VAR Plantheight;
		CLASS Treatment BRISONr; 
	RUN;


/* LET US SEE WITH the Previous BRiwecs data. */

PROC MIXED DATA=Work.Briwecs_1st_2017;
  		CLASS Rep Row Column Treatment Genotype; 
  		MODEL PH= Rep Treatment Genotype Rep*Treatment*Genotype;
  		RANDOM Row Column;
  		Store Work.Outmodel4; 
 	    RUN; /*WARNING: Stopped because of infinite likelihood; The same case as with briwecs_2017*/

PROC PLM Restore= Work.Outmodel4;
 	LSMEANS Block*Treatment*BRISONr;
 	RUN; 

	PROC MIXED DATA=Work.Briwecs_1st_2017;
  		CLASS Rep Row Column Treatment Genotype; 
  		MODEL PH= Rep Treatment Genotype Treatment*Genotype;
  		RANDOM Row Column;
  		Store Work.Outmodel4; 
 	    RUN; /* Worked good*/

PROC PLM Restore= Work.Outmodel4;
 	LSMEANS Block*Treatment*BRISONr;
 	RUN; 



PROC MIXED DATA=Work.Briwecs_1st_2017;
  		CLASS Rep Row Column Treatment Genotype; 
  		MODEL PH= Rep Treatment Genotype Treatment*Genotype;
  		RANDOM Rep(Row) Rep(Column);
  		Store Work.Outmodel5; 
 	    RUN; /* Good than Treatment(Row) Treatment(Column) [make sense] Covariance Parameter Estimates 
Cov Parm Estimate 
Rep(Row) 0.4174 
Rep(Column) 1.2124 
Residual 9.2418 



Fit Statistics 
-2 Res Log Likelihood 3858.8 
AIC (Smaller is Better) 3864.8 
AICC (Smaller is Better) 3864.9 
BIC (Smaller is Better) 3868.1 

*/

PROC PLM Restore= Work.Outmodel5;
 	LSMEANS Treatment*Genotype;
 	RUN; 
Block (Row) Column

PROC MIXED DATA=Work.Briwecs_1st_2017;
  		CLASS Rep Row Column Treatment Genotype; 
  		MODEL PH= Rep Treatment Genotype Treatment*Genotype;
  		RANDOM Treatment(Row) Treatment(Column);
  		Store Work.Outmodel6; 
 	    RUN; /*Covariance Parameter Estimates 
Cov Parm Estimate 
Treatment(Row) 0.6124 
Treatment(Column) 0.8324 
Residual 9.4879 



Fit Statistics 
-2 Res Log Likelihood 3875.1 
AIC (Smaller is Better) 3881.1 
AICC (Smaller is Better) 3881.2 
BIC (Smaller is Better) 3885.6 

*/

PROC PLM Restore= Work.Outmodel6;
 	LSMEANS Block*Treatment*BRISONr;
 	RUN; 


/* LET US SEE WITH the Very New plan BRiwecs data*/

PROC MIXED DATA = Work.Briwecs_newcode_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Row Column;
	 	Store Work.Outmodel7; 
 	    RUN; /*Covariance Parameter Estimates 
Cov Parm Estimate 
Row 1.1125 
Column 0.7333 
Residual 9.1033 



Fit Statistics 
-2 Res Log Likelihood 3851.8 
AIC (Smaller is Better) 3857.8 
AICC (Smaller is Better) 3857.8 
BIC (Smaller is Better) 3862.2 
*/

 PROC PLM Restore= Work.Outmodel7;
 LSMEANS Treatment*BRISONr;
 RUN; 


 PROC MIXED DATA = Work.Briwecs_newcode_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Block(Row) Block(Column);
	 	Store Work.Outmodel8; 
 	    RUN;

 /* THIS is te best model this is not right the Block can´t be nested in ROW
		Covariance Parameter Estimates 
Cov Parm Estimate 
Block(Row) 1.4937 
Block(Column) 1.3260 
Residual 7.9027 



Fit Statistics 
-2 Res Log Likelihood 3809.8 
AIC (Smaller is Better) 3815.8 
AICC (Smaller is Better) 3815.8 
BIC (Smaller is Better) 3822.4 

		/* IT IS THE SAME LIKE Block(Row) Column 

		Covariance Parameter Estimates 
Cov Parm Estimate 
Block(Row) 1.4937 
Column 1.3260 
Residual 7.9027 



Fit Statistics 
-2 Res Log Likelihood 3809.8 
AIC (Smaller is Better) 3815.8 
AICC (Smaller is Better) 3815.8 
BIC (Smaller is Better) 3822.4 */

PROC PLM Restore= Work.Outmodel8;
 LSMEANS Treatment*BRISONr;
 RUN;


PROC MIXED DATA = Work.Briwecs_newcode_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Row(Block) Column(Block);
		REPEATED Block/ SUBJECT= Treatment TYPE=CS; 
	 	Store Work.Outmodel12; 
 	    RUN; 
		/* 
		Including Repeated that not change much the resuclt 
		Covariance Parameter Estimates 
Cov Parm Subject Estimate 
Row(Block)   1.4936 
Column(Block)   1.3260 
CS Treatment 0.08269 
Residual   7.9027 

type = cs -> compound symmetry; type=un -> unstructered 

Fit Statistics 
-2 Res Log Likelihood 3809.8 
AIC (Smaller is Better) 3817.8 
AICC (Smaller is Better) 3817.9 
BIC (Smaller is Better) 3826.6 
*/
PROC MIXED DATA = Work.Briwecs_newcode_2017;
  		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr/solution;
  		RANDOM Row(Block) Column(Block);
		RANDOM int/subject = BRISONr type = cs;
	 	Store Work.Outmodel12; 
 	    RUN; 

		/*Title 'Analysis with random intercept.' ;
Proc Mixed data = lib.WickselllongMiss;
    class group time subject ;
    model dv = group time group*time/solution;
    random int /subject = subject type = cs;
run;*/

PROC PLM Restore= Work.Outmodel12;
 LSMEANS Treatment*BRISONr;
 RUN;


PROC MIXED DATA = Work.Briwecs_newcode_2017;
		CLASS Block Row Column Treatment BRISONr; 
  		MODEL Plantheight= Block Treatment BRISONr Treatment*BRISONr;
  		RANDOM Row(Block) Column(Block);
	 	Store Work.Outmodel10; 
 	    RUN;

PROC PLM Restore= Work.Outmodel10;
 LSMEANS Treatment*BRISONr;
 RUN;

 PROC MIXED DATA = Work.Briwecs_newcode_2017;
  		CLASS BRISONr Treatment Block Row Column ; 
  		MODEL Plantheight= BRISONr Treatment Block  BRISONr*Treatment*Block;
  		RANDOM Row(Block) Column(Block);
		Store Work.Outmodel13; 
 	    RUN;

PROC PLM Restore= Work.Outmodel11;
 LSMEANS Treatment*BRISONr/ BYLEVEL B1 B2; 
 RUN;/* AT variable = value ;AT (variable-list) = (value-list); AT MEANS */
 PROC PLM Restore= Work.Outmodel11;
 LSMEANS Treatment/pdiff=ALL;
 RUN; /* Good post hoc*/ 

PROC PLM Restore= Work.Outmodel11;
 LSMEANS Treatment*BRISONr /adjust=smm ADJDFE=Source; 
 RUN;

PROC MIXED DATA=Work.Briwecs_newcode_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RUN;
 

	PROC MIXED DATA=Work.Briwecs_newcode_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM Row(Block) Column(Block)
	RUN; 

	/* New Running 29 March 2018*/

	
	PROC IMPORT DATAFILE= "/folders/myshortcuts/SAS/Geno Data/Briwecs_2015.xlsx"
	DBMS=XLSX
	OUT=RAWDATA.Briwecs_2015;
	GETNAMES=YES;
RUN;

/* Let usimport the Hiararchical data*/

PROC IMPORT DATAFILE= "/folders/myshortcuts/SAS/Geno Data/Briwecs_2015.xlsx"
	DBMS=XLSX
	OUT=RAWDATA.Briwecs_2015;
	GETNAMES=YES;
RUN;

/*Let usimport the Hierarchical data*/

PROC IMPORT OUT= Briwecs.Briwecs_Hierac_2017 
            DATAFILE= "M:\PatriceKoua\SAS\Datt_Bri__hierac2017.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= Briwecs.Briwecs_Hierac_2016 
            DATAFILE= "C:\Users\Koua\Desktop\data file majid\Briwecs_Final\Briwecs_Bonnanalysis\DattallBri_Bonn_hierachical\Datt_Bri__hierac2016.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= Briwecs.Briwecs_Hierac_2015 
            DATAFILE= "C:\Users\Koua\Desktop\data file majid\Briwecs_Final\Briwecs_Bonnanalysis\DattallBri_Bonn_hierachical\Datt_Bri__hierac2015.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= Briwecs.Briwecs_Hierac_all 
            DATAFILE= "C:\Users\Koua\Desktop\data file majid\Briwecs_Final\Briwecs_Bonnanalysis\DattallBri_Bonn_hierachical\Datt_Bri__hierac15_16_17.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
 		
/* Let us run the last model*/
PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM Row(Block) Column;
	RUN;
/*let us run the PLM*/
PROC PLM Restore= Work.Outmodel14;
 LSMEANS Block*Treatment*BRISONr; 
 RUN;

 PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM Row(Block) Column;
	Store Work.Outmodel17;
	RUN;

	PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM Block Row*Column(Block);
	LSMEANS  Block*Treatment*BRISONr;
	RUN; /* Super Alles läuft im Glatt*/

	PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM Block Row*Column(Block);
	LSMEANS  Block*Treatment*BRISONr;
	RUN; /* Block is not a factor let us make it as only random effect/* 
	/*166  PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
NOTE: PROCEDURE MIXED used (Total process time):
      real time           0.00 seconds
      cpu time            0.00 seconds

ERROR: Effects used in the LSMEANS statement must have appeared previously in the MODEL statement.
NOTE: The SAS System stopped processing this step because of errors.
167      CLASS Block Row Column Treatment BRISONr;
168      MODEL Plantheight = Treatment|BRISONr;
169      RANDOM Block Row*Column(Block);
170      LSMEANS  Block*Treatment*BRISONr;
171      RUN;

*/ /* It doesn´t work, in fact benedict it like i did in SPSS*/ 

PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM Block Row*Column(Block);
	LSMEANS  Block*Treatment*BRISONr;
	RUN; /* This model is the last good one; Block is take as random effect and you can also asses the predicted values of Block*Treatment*BRISONr ; But it popped out Convergence criteria met but final hessian is not positive definite. So the LSMEANS
	is simply the Mean given */ 

	/* Let us removed Block as random*/
PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight =  Treatment|BRISONr;
	RANDOM Row(Block) Column;
	LSMEANS  Treatment*BRISONr;
	RUN; 
/* At the end I will generate with SPSS*/

PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight =  Treatment|BRISONr;
	RANDOM Row(Block) Column;
	LSMEANS  Treatment*BRISONr;
	RUN;
/* Let us see if there are the same model. Plantheight =  Treatment|BRISONr and Plantheight =  Treatment BRISONr Treatment*BRISONr
	Conclusion: there are same*/
PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight =  Treatment BRISONr Treatment*BRISONr;
	RANDOM Row(Block) Column;
	LSMEANS  Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM Block Row(Block)Column;
	LSMEANS  Treatment*BRISONr;/* the values are same like means*/
	LSMEANS  Block*Treatment*BRISONr;
	RUN; 

PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	 Row Column Column*Row(Block);
	LSMEANS  Treatment*BRISONr;/* the values are same like means*/
	RUN;

	PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	 Row(Block) Column(Block) Column*Row(Block);
	LSMEANS  Treatment*BRISONr;/* the values are same like means*/
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM 	 Block Row Column Column*Row(Block);
	LSMEANS  Treatment*BRISONr;
	LSMEANS  Block*Treatment*BRISONr;/* the values are same like means*/
	RUN;

	/*Random model:	Rep + Rep.Row + Rep.Column + Rep.Row.Column;*/
PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM 	 Block Row(Block) Column(Block) Column*Row(Block);
	LSMEANS  Treatment*BRISONr;
	LSMEANS  Block*Treatment*BRISONr;/* the values are same like means yes in deed*/
	RUN;

	PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM 	 Block Row Column Column*Row(Block);
	LSMEANS  Treatment*BRISONr;
	LSMEANS  Block*Treatment*BRISONr;/* */
	RUN;

	PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM 	 Block Row Block*Row Column Block*Column Column*Row(Block);
	LSMEANS  Treatment*BRISONr;
	LSMEANS  Block*Treatment*BRISONr;/* also the values are same like means yes in deed*/
	RUN;

	PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	 Block Row Block*Row Column Block*Column Column*Row(Block);
	LSMEANS  Treatment*BRISONr;/* also the values are same like means yes in deed*/
	RUN;
PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	 Block Row(Block) Column(Block) Row*Column(Block);
	LSMEANS  Treatment*BRISONr;/*  values are not the same like means yes in deed*/
	RUN;
	PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM 	 Row Block*Row Column Block*Column Column*Row(Block);
	LSMEANS  Block*Treatment*BRISONr;/* also the values are same like means yes in deed*/
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	 Block Row(Block) Column Column*Row(Block);
	LSMEANS  Treatment*BRISONr;/* */
	RUN;
	/*Convergence criteria met but final hessian is not positive definite. 



Covariance Parameter Estimates 
Cov Parm Estimate 
Block 3.6187 
Row(Block) 1.4937 It can be change depending on the Parameter of the model.
Column(Block) 1.3260 is the same like Column 1.3260 because there is not in this design one full Clomn in a Block. 
Row*Column(Block) 4.3229 
Residual 3.5798 



Fit Statistics 
-2 Res Log Likelihood 3814.7 
AIC (Smaller is Better) 3824.7 
AICC (Smaller is Better) 3824.8 
BIC (Smaller is Better) 3818.1 
MEANS is different
*/

PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Block|Treatment|BRISONr;
	RANDOM 	Row Column Column*Row;
	LSMEANS  Block*Treatment*BRISONr;/* */
	RUN;/* Block as fixed effect change everything the MEANS ARE THE SAME
	Convergence criteria met but final hessian is not positive definite. 



Covariance Parameter Estimates 
Cov Parm Estimate 
Row 0.000240 
Column 0 
Row*Column 0 
Residual 2.673E-8 



Fit Statistics 
-2 Res Log Likelihood -12.8 
AIC (Smaller is Better) -8.8 
AICC (Smaller is Better) 3.2 
BIC (Smaller is Better) -5.8 

*/

PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	Row(Block) Column Column*Row;
	LSMEANS Treatment*BRISONr;/* */
	RUN;/*Convergence criteria met but final hessian is not positive definite. 



Covariance Parameter Estimates 
Cov Parm Estimate 
Row(Block) 1.7484 
Column 2.7992 
Row*Column 1.4813 
Residual 6.3336 



Fit Statistics 
-2 Res Log Likelihood 3833.8 
AIC (Smaller is Better) 3841.8 
AICC (Smaller is Better) 3841.8 
BIC (Smaller is Better) 3850.5 
*/

PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Column*Row(Block);
	LSMEANS Treatment*BRISONr;/* */
	RUN; /*Convergence criteria met but final hessian is not positive definite. THIS is so sa the most realistic design



Covariance Parameter Estimates 
Cov Parm Estimate 
Block 3.6187 
Row(Block) 1.4937 
Column 1.3260 
Row*Column(Block) 4.3229 
Residual 3.5798 



Fit Statistics 
-2 Res Log Likelihood 3814.7 
AIC (Smaller is Better) 3824.7 
AICC (Smaller is Better) 3824.8 
BIC (Smaller is Better) 3818.1 
*/
PROC MIXED DATA=Rawdata.Briwecs_Hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column(Block) Row*Column(Block);
	LSMEANS Treatment*BRISONr;/* */
	RUN; /*Convergence criteria met but final hessian is not positive definite. 



Covariance Parameter Estimates 
Cov Parm Estimate 
Block 3.6187 
Row(Block) 1.4937 
Block*Row 0  [no interaction because no replication]
Column 1.3260 
Row*Column(Block) 4.3229 
Residual 3.5798 



Fit Statistics 
-2 Res Log Likelihood 3814.7 
AIC (Smaller is Better) 3824.7 
AICC (Smaller is Better) 3824.8 
BIC (Smaller is Better) 3818.1 

*/
PROC MIXED DATA=Rawdata.Briwecs_hierac_2015;
	CLASS Year Block Row Column Treatment BRISONr; 
	MODEL MC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;/* Very good for Mc same prediction with Genstat although AIC are diff */
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2015;
	CLASS Year Block Row Column Treatment BRISONr; 
	MODEL MC = Block|Treatment|BRISONr;
	RANDOM 	Row(Block) Column Row*Column(Block);
	LSMEANS Block*Treatment*BRISONr;/* Block has to be randon*/
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2015;
	CLASS Year Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;/* Very good for NDF, Plantheight, ... same prediction with GenstatAkaike information coefficient	 2786.29, SAS AIC (Smaller is Better) 4001.3  although AIC are diff */
	RUN;
PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Year Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;/* Very good for NDF, Plantheight, ... same prediction with GenstatAkaike information coefficient	 2786.29, SAS AIC (Smaller is Better) 4001.3  although AIC are diff */
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;/* Very good for 2016 also, ... same prediction with GenstatAkaike information coefficient	 2786.29, SAS AIC (Smaller is Better) 4001.3  although AIC are diff */
	RUN;


PROC MIXED DATA=Rawdata.Briwecs_hierac_all;
	CLASS Year Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Year|Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Year*Treatment*BRISONr;/* Very good for 2016 also, ... same prediction with GenstatAkaike information coefficient	 2786.29, SAS AIC (Smaller is Better) 4001.3  although AIC are diff */
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_all;
	CLASS Year Block Row Column Treatment BRISONr; 
	MODEL AC = Year|Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Year*Treatment*BRISONr;/* Very good for 2016 also, ... same prediction with GenstatAkaike information coefficient	 2786.29, SAS AIC (Smaller is Better) 4001.3  although AIC are diff */
	RUN;

	/* LET US run all the Traits with SAS*/

	/*2016*/
PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Emergence_date = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;/* Very good for 2016 also, ... same prediction with GenstatAkaike information coefficient	 2786.29, SAS AIC (Smaller is Better) 4001.3  although AIC are diff */
	RUN;
/* there is a problem on this trait*/
	PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL BBCH59__HD = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;
/* I forgot that*/
PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL BBCH87__YM = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN; 

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

	PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Seedyield = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;


PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Seedyield_bio = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Biomass_bio = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;


PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL TKW_plot = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;
PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL TKW_bio = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Spike_number = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Stripe_rust = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Sedimentation = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Falling_number = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Raw_Protein = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Raw_Protein = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL MC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL PC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL SC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL CFC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL NDF = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Hardness = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL AC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2016;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL HI = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

	/*2017*/
PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Emergence_date = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;/* Very good for 2016 also, ... same prediction with GenstatAkaike information coefficient	 2786.29, SAS AIC (Smaller is Better) 4001.3  although AIC are diff */
	RUN;

	PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL BBCH59__HD = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;
PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL BBCH87_YM = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;
PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Plantheight = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

	PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Seedyield = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;


PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Seedyield_bio = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Biomass_bio = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;


PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL TKW_plot = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;
PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL TKW_bio = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Spike_number = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;


PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Sedimentation = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Falling_number = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Raw_Protein = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Raw_Protein = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL MC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL PC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL SC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL CFC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL NDF = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL Hardness = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL AC = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;

PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL HI = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;
	RUN;


PROC MIXED DATA=Briwecs.Briwecs_hierac_2017 method=REML; 
CLASS Block Row Column Treatment BRISONr; 
MODEL HI=;
RANDOM Treatment BRISONr;
RUN; 

PROC MIXED DATA=Briwecs.Briwecs_hierac_2017 method=REML; 
CLASS Block Row Column Treatment BRISONr; 
MODEL HI=;
RANDOM Treatment BRISONr Treatment*BRISONr;
RUN;
	

	/*B*/
Proc mixed data=Rawdata2.Briwecs_bluesall;
Class Year Rep Treatment BRISONr;
Model Plantheight = Year*Treatment*BRISONr; /*<---: Fixed effects*/
Random Rep/solution; /*<---: Random effects*/
Estimate "Overall mean of variable" intercept 1 | BRISONr 0;
Run; 

Proc mixed data=Rawdata2.Briwecs_bluesall;
Class Year Rep Treatment BRISONr;
Model Plantheight = Year|Treatment|BRISONr; /*<---: Fixed effects*/
Random Rep/solution; /*<---: Random effects*/
LSMEANS Year*Treatment*BRISONr;
Run;

Proc mixed data=Rawdata.Briwecs_hierac_2015;
Class Block Treatment BRISONr;
Model CFC = Treatment|BRISONr; /*<---: Fixed effects*/
RANDOM 	Block Row(Block) Column Row*Column(Block);
LSMEANS Treatment*BRISONr;
RUN; /*<---: Random effects*/

/* Verification of importance of the predicted value*/

/* The predicted values worked well and it has corrected the data; same prediction with Genstat*/
PROC MIXED DATA=Briwecs.Briwecs_hierac_2015;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL BBCH59_HD = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;/* Very good for 2016 also, ... same prediction with GenstatAkaike information coefficient	 2786.29, SAS AIC (Smaller is Better) 4001.3  although AIC are diff */
	RUN;

	PROC MIXED DATA=Briwecs.Briwecs_hierac_2015;
	CLASS Block Row Column Treatment BRISONr; 
	MODEL BBCH87_YM = Treatment|BRISONr;
	RANDOM 	Block Row(Block) Column Row*Column(Block);
	LSMEANS Treatment*BRISONr;/* Very good for 2016 also, ... same prediction with GenstatAkaike information coefficient	 2786.29, SAS AIC (Smaller is Better) 4001.3  although AIC are diff */
	RUN;


	/* Running Var component*/
PROC MIXED DATA=Briwecs.Briwecs_hierac_2017 method=REML; 
CLASS Block Row Column Treatment BRISONr; 
MODEL HI=;
RANDOM Treatment BRISONr;
RUN; 

PROC MIXED DATA=Briwecs.Briwecs_hierac_2017 method=REML; 
CLASS Block Row Column Treatment BRISONr; 
MODEL HI=;
RANDOM Block Treatment BRISONr Treatment*BRISONr Block*Treatment*BRISONr;
RUN;


PROC MIXED DATA=Briwecs.Briwecs_hierac_2017 method=REML; 
CLASS Block Row Column Treatment BRISONr; 
MODEL HI=;
RANDOM Block Treatment BRISONr Treatment*BRISONr Block*Treatment*BRISONr;
RUN;
PROC MIXED DATA=Briwecs.Briwecs_hierac_2017 method=REML; 
CLASS Block Treatment BRISONr; 
MODEL Plantheight=;
RANDOM Block Treatment BRISONr Treatment*BRISONr Block*BRISONr;
RUN;


PROC MIXED DATA=Briwecs.Briwecs_hierac_2017 method=REML; 
CLASS Block Treatment BRISONr; 
MODEL Plantheight=;
RANDOM Block Treatment BRISONr Treatment*BRISONr Block*BRISONr Block*Treatment*BRISONr;
RUN;

/*Let see with the Blues data*/
PROC MIXED DATA=Briwecs.Pheno_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Plantheight=;
RANDOM Rep Treatment Genotype Genotype*Treatment Genotype*Rep;
RUN;

/* H_Seedyield_bio*/

PROC MIXED DATA=Briwecs.Pheno_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Seedyield_bio=;
RANDOM Rep Treatment Genotype Genotype*Treatment Genotype*Rep;
RUN;

/* Biomass_bio*/

PROC MIXED DATA=Briwecs.Pheno_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio=;
RANDOM Rep Treatment Genotype Genotype*Treatment Genotype*Rep;
RUN;

PROC MIXED DATA=Briwecs.Pheno_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio=;
RANDOM Rep Treatment Genotype Genotype*Treatment Genotype*Rep Genotype*Rep*Treatment;
RUN;
/*New Model after talking to Prof*/

PROC MIXED DATA=Briwecs.Pheno_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio=;
RANDOM Rep Treatment Genotype Genotype*Treatment ; /* Remove Genotype*Rep in ther model*/
RUN;

PROC MIXED DATA=Briwecs.Pheno_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment ; /* Let us include rep*treatment*/
RUN; /*Retain this model*/

/* let us write treatment within rep*/
PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Plantheight=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment ; /* Let us include rep by treatment*/
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Seedyield_bio=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment ; /* Let us include rep by treatment*/
RUN;

PROC MIXED DATA=Briwecs.Pheno_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio=;
RANDOM Treatment Rep(Treatment) Genotype Rep*Treatment Genotype*Treatment ; /* Let us include rep by treatment*/
RUN;
/* Tet Us learn about the PROC CARCOMP*/

proc varcomp method=reml data=cure;
class temp lab batch;
model cure=temp|lab batch(lab temp) / fixed=1;
run;

PROC VARCOMP DATA=Briwecs.Pheno_2017 method=REML;
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio= Rep Treatment Genotype Genotype*Treatment Genotype*Rep / Fixed=0;   
RUN;

PROC VARCOMP DATA=Briwecs.Pheno_2017 method=REML;
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio= Rep Treatment Genotype Genotype*Treatment Genotype*Rep;   
RUN;

PROC VARCOMP DATA=Briwecs.Pheno_2017 method=REML;
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio= Rep Treatment Genotype Genotype*Treatment Genotype*Rep / Fixed=0;   
RANDOM Rep Treatment Genotype Genotype*Treatment Genotype*Rep;
RUN;

PROC VARCOMP DATA=Briwecs.Pheno_2017 method=REML;
CLASS Rep Treatment Genotype; 
MODEL Plantheight= Rep Treatment Genotype Genotype*Treatment Genotype*Rep;   
RUN;


* heritability estimation: ;
		proc mixed  Data=Briwecs.Pheno_2017 asycov; 

		class Rep env &treat genotype; 
		model Plantheight =  	%if &treatment eq 	1 						%then %do; 	&treat 				%end; ; 
		random 		%if &Environment eq 1 						%then %do; 	Env  				%end;
					%if &Environment eq 1 	and &numrep gt 1	%then %do; 	rep(Env) 			%end;
			 																genotype 
	 				%if &treatment eq 	1 						%then %do; 	genotype*&treat 	%end;
					%if &Environment eq 1 						%then %do; 	Env*genotype  		%end;
					%if &treatment eq 1 and &Environment eq 1 	%then %do; 	genotype*&treat*Env %end;
		;
		ods listing exclude AsyCov CovParm; ods output asycov = covmat covparms = estmat; run;

		proc varcomp data=data method=REML; 
				class rep &forceClassVar ; 
				model y = 
				%if &Environment eq 1 						%then %do; 	Env  				%end;
				%if &Environment eq 1 	and &numrep gt 1	%then %do; 	rep(Env) 			%end; 
				%if &treatment eq 	1 						%then %do; 	&treat 				%end;
																		genotype 
				%if &treatment eq 	1 						%then %do; 	genotype*&treat 	%end;
				%if &Environment eq 1 						%then %do; 	Env*genotype  		%end;
		 		; run;
proc iml;
			start seh(V, C, LG, LP, H, SE);
			Vp = LP`*V;
			Vg = LG`*V;
			H = VG/Vp;
			d = (1/Vp)*(LG - (LP*H));
			VH = d`*C*d;
			SE = sqrt(VH);
			finish seh;

			use estmat; read all into v; use covmat; read all into c;
			* Note that SAS introduces an extra first column into the matrix which must be removed;
			C = C(|1:nrow(C), 2:ncol(C)|);
			*order of variance components in v and c matrices is V(R), V(G), V(error);
			%if &numrep gt 1  %then %do; 
				%if &treatment eq 0 and &Environment eq 1 %then %do; 
				LG = {0, 0, 1, 0, 0};
				LP = {0, 0, 1, 1, 1};
				%end;
				%if &treatment eq 1 and &Environment eq 1 %then %do; 
				LG = {0, 0, 1, 0, 0, 0, 0};
				LP = {0, 0, 1, 1, 1, 1, 1};
				%end;
			%end; 
			%else; %do; 
				%if &treatment eq 0 and &Environment eq 1 %then %do; 
				LG = {0, 1, 0, 0};
				LP = {0, 1, 1, 1};
				%end;
				%if &treatment eq 1 and &Environment eq 1 %then %do; 
				LG = {0, 1, 0, 0, 0, 0};
				LP = {0, 1, 1, 1, 1, 1};
				%end;
			%end; 

			%if &treatment eq 1 and &Environment eq 0 %then %do; 
			LG = {1, 0, 0};
			LP = {1, 1, 1};
			%end;
			%if &treatment eq 0 and &Environment eq 0 %then %do; 
			LG = {1, 0};
			LP = {1, 1};
			%end;

			e=&numEnv;
			t=&numT;
			r = &numrep;
			%if &numrep gt 1  %then %do; 
				%if &treatment eq 0 and &Environment eq 1 %then %do; 
				r = &numrep * &numT;
				LP = 0//0//1//(1/e*r)//(1/r);
				%end;
				%if &treatment eq 1 and &Environment eq 1 %then %do; 
				LP = 0//0//1//(1/e*r)//(1/t*r)//(1/t*e*r)//(1/r);
				%end;
			%end;
			%else; %do; 
				%if &treatment eq 0 and &Environment eq 1 %then %do; 
				r = &numrep * &numT;
				LP = 0//1//(1/e*r)//(1/r);
				%end;
				%if &treatment eq 1 and &Environment eq 1 %then %do; 
				LP = 0//1//(1/e*r)//(1/t*r)//(1/t*e*r)//(1/r);
				%end;
			%end;


			%if &treatment eq 1 and &Environment eq 0 %then %do; 
			r = &numrep * &numEnv;
			LP = 1//(1/t*r)//(1/r);
			%end;
			%if &treatment eq 0 and &Environment eq 0 %then %do; 
			r = &numrep * &numEnv * &numT;
			LP = 1//(1/r);
			%end;

			call seh(V, C, LG, LP, H, SE);
			create heritability var{H SE}; append;  show contents; close heritability;
			quit; run;
	
			/* Let us import all the BLUES Briwecs*/

PROC IMPORT OUT= Briwecs.BluesBriwecs_2015 
            DATAFILE= "C:\SASUniversityEdition\SAS\BluesBriwecs_2015.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= Briwecs.BluesBriwecs_2016 
            DATAFILE= "C:\SASUniversityEdition\SAS\BluesBriwecs_2016.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
PROC IMPORT OUT= Briwecs.BluesBriwecs_2017 
            DATAFILE= "C:\SASUniversityEdition\SAS\BluesBriwecs_2017.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/* Let us import all the BLUES Briwecs*/ AFTER!!!
PROC IMPORT OUT= Briwecs.BluesBriwecs_201 
            DATAFILE= "C:\SASUniversityEdition\SAS\BluesBriwecs_2017.txt" 
            DBMS=TAB REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

/* Let Us run the covariance*/

/*2015
AC,BBCH59,BBCH87,Biomass_bio,Emergence_date,FC, Hardness,HI,MC,NDF,Plantheight,Raw_Protein,SC, Sedimentation,Seedyield,
Seedyield_bio,Spike_number,Stripe_rust */ 

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL AC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment; 
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL BBCH59=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL BBCH87=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Emergence_date=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL FC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Hardness=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL HI=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL MC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL NDF=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Plantheight=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Raw_Protein=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;
PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL SC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Sedimentation=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Seedyield=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Seedyield_bio=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Spike_number=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2015 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Stripe_rust=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;



/* 2016
AC	BBCH59	BBCH87	Biomass_bio	Emergence_date	FC	Falling_number	Hardness	Raw_Protein	MC	NDF	Plantheight	SC	Sedimentation	
Seedyield	Seedyield_bio	Sowing_date	Spad	Spike_number	Stripe_rust	TKW_plot
*/

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL AC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment; 
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL BBCH59=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL BBCH87=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Emergence_date=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL FC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Hardness=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL HI=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL MC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL NDF=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Plantheight=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Raw_Protein=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;
PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL SC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Sedimentation=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Seedyield=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Seedyield_bio=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Spike_number=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Stripe_rust=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL TKW_plot=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2016 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Spad=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

/*2017*/

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL AC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment; 
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL BBCH59=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL BBCH87=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Biomass_bio=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Emergence_date=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL FC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Hardness=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL HI=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL MC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL NDF=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Plantheight=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Raw_Protein=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;
PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL SC=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Sedimentation=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Seedyield=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Seedyield_bio=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Spad=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL Spike_number=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Bluesbriwecs_2017 method=REML; 
CLASS Rep Treatment Genotype; 
MODEL TKW_plot=;
RANDOM Rep Treatment Genotype Rep*Treatment Genotype*Treatment;
RUN;

/* Let us run the Anova of the Yield to see the source of variation. */
PROC MIXED DATA=Briwecs.Briwecs_bluesall method=REML; 
CLASS Treatment BRISONr; 
MODEL Seedyield= Treatment BRISONr BRISONr*Treatment;
RUN;

PROC MIXED DATA=Briwecs.Briwecs_bluesall method=REML; 
CLASS Year Treatment BRISONr;
MODEL Seedyield= Year Treatment BRISONr Year*Treatment BRISONr*Treatment;
RUN;
PROC MIXED DATA=Briwecs.Briwecs_bluesall method=REML; 
CLASS Year Treatment BRISONr;
MODEL Seedyield= Year Treatment BRISONr Year*Treatment BRISONr*Treatment Year*BRISONr*Treatment;
RUN;
/* Let us see with Proc Anova COMBINE Analysis*/
PROC ANOVA DATA=Briwecs.Briwecs_bluesall; 
CLASS Year Treatment BRISONr;
MODEL Seedyield= Year Treatment BRISONr Year*Treatment BRISONr*Treatment;
RUN;
PROC ANOVA DATA=Briwecs.Briwecs_bluesall; 
CLASS Year Treatment BRISONr;
MODEL Seedyield= Year Treatment BRISONr Year*Treatment Treatment*BRISONr  Year*Treatment*BRISONr;
RUN;


/*Let us see with Proc Anova for the three Year*/
PROC ANOVA DATA=Briwecs.Bluesbriwecs_2015; 
CLASS Treatment Genotype;
MODEL Seedyield= Treatment Genotype  Genotype*Treatment;
RUN;

PROC ANOVA DATA=Briwecs.Bluesbriwecs_2016; 
CLASS Treatment Genotype;
MODEL Seedyield= Treatment Genotype  Genotype*Treatment;
RUN;

PROC ANOVA DATA=Briwecs.Bluesbriwecs_2017; 
CLASS Treatment Genotype;
MODEL Seedyield= Treatment Genotype  Genotype*Treatment;
RUN;




RUN;PROC MIXED DATA=Briwecs.Briwecs_bluesall method=REML; 
CLASS Year Treatment BRISONr;
MODEL Seedyield= Year Treatment BRISONr Year*Treatment BRISONr*Treatment Year*BRISONr*Treatment;
RUN;

