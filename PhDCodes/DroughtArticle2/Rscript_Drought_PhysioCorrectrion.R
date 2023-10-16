################ Drought Article 2 Correction for Resubmission ######################

# Monday 31st January 2021

# Perform an ANOVA test to show that thre is year effect even within Gemany.

# Load the Data with year of release

install.packages("readxl")
library(readxl)

ancestr <- read_excel("Data/ancestr.xlsx", sheet = "PopStructure")

head(ancestr,5) # Okay

# Load the data with yield mean (Blue Data)

head(Drought_Adapted_year_release,5) # Not the Good one

head(Blues_17_18_article1_2,5)

ancestr[1:5, 10:22]

sub_ancestr <- ancestr[, c(2:4,7,9)]
head(sub_ancestr)

# Merge the two files

BluesDataOrigine <- merge(Blues_17_18_article1_2, sub_ancestr,by.x = "Genotypes", 
                   by.y = "Genotype", all = TRUE, all.x = TRUE, all.y = TRUE)
Anova(AnovaModel.5)
head(BluesDataOrigine,5)

library(Rcmdr)

# Let us run the ANOVA combined all Data

AnovaModel <- lm(GY ~ Origine*Year_Realease, data=BluesDataOrigine, 
                 contrasts=list(Origine ="contr.Sum", Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: GY
# Sum Sq  Df F value Pr(>F)
# Origine                 129463356  17  0.2890 0.9979
# Year_Realease           157721069  45  0.1330 1.0000
# Origine:Year_Realease    87356532  20  0.1658 1.0000
# Residuals             18498552469 702               



AnovaModel <- lm(YII ~ Origine*Year_Realease, data=BluesDataOrigine, 
                 contrasts=list(Origine ="contr.Sum", Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value Pr(>F)
# Origine               0.0900  17  0.7920 0.7036
# Year_Realease         0.0697  45  0.2318 1.0000
# Origine:Year_Realease 0.0442  20  0.3303 0.9977
# Residuals             4.7817 715               


# Let us make the ANOVA with subset Data
View(subset(BluesDataOrigine, Year == "2017" & TreatEnv == "D_stress"))

AnovaModel <- lm(GY ~ Origine*Year_Realease, data = subset(BluesDataOrigine, Year == "2017" & TreatEnv == "D_stress") , 
                 contrasts=list(Origine ="contr.Sum", Year_Realease ="contr.Sum"))

Anova(AnovaModel)

Anova Table (Type II tests)

# Response: GY
# Sum Sq  Df F value Pr(>F)
# Origine                3755821  13  0.6602 0.7975
# Year_Realease         23467784  43  1.2471 0.1791
# Origine:Year_Realease  9926728  16  1.4177 0.1461
# Residuals             49013007 112   

AnovaModel <- lm(GY ~ Origine*Year_Realease, data = subset(BluesDataOrigine, Year == "2018" & TreatEnv == "D_stress") , 
                 contrasts=list(Origine ="contr.Sum", Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: GY
# Sum Sq  Df F value   Pr(>F)   
# Origine                37088288  17  2.1867 0.007829 **
#   Year_Realease          87161712  45  1.9414 0.002502 **
#   Origine:Year_Realease  30653508  20  1.5362 0.082249 . 
# Residuals             114735483 115                    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

AnovaModel <- lm(YII ~ Origine*Year_Realease, data = subset(BluesDataOrigine, Year == "2017" & TreatEnv == "D_stress") , 
                 contrasts=list(Origine ="contr.Sum", Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value       Pr(>F)    
# Origine               0.0179864  17  4.7629 0.0000001497 ***
#   Year_Realease         0.0135325  45  1.3538       0.1011    
# Origine:Year_Realease 0.0053787  20  1.2107       0.2585    
# Residuals             0.0255458 115                         
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

AnovaModel <- lm(YII ~ Origine*Year_Realease, data = subset(BluesDataOrigine, Year == "2018" & TreatEnv == "D_stress") , 
                 contrasts=list(Origine ="contr.Sum", Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value       Pr(>F)    
# Origine               0.30935  17  5.2766 0.0000000186 ***
#   Year_Realease         0.20321  45  1.3094     0.127765    
# Origine:Year_Realease 0.15514  20  2.2492     0.003937 ** 
#   Residuals             0.39659 115                         
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Do the analysis with the cultivars only from Germany

library("dplyr")

BluesDataOrigine <- BluesDataOrigine %>% 
  
mutate(Origine = recode(Origine, Frankreich = "France"
                  ))


BluesDataOrigine <- mutate(BluesDataOrigine, Origine = recode(Origine, "NE; Germany" = "Germany"))

BluesDataOrigine$Origine <- as.factor(BluesDataOrigine$Origine)

levels(BluesDataOrigine$Origine)

BluesDataGermany <- subset(BluesDataOrigine, Origine == "Germany")


############# Resume the Anova with Cultivars from Germany ##########################

# For drought -------

AnovaModel <- lm(YII ~ Year_Realease, data = subset(BluesDataGermany, TreatEnv == "D_stress") , 
                 contrasts=list( Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value Pr(>F)
# Year_Realease 0.05389  39  0.2606      1
# Residuals     1.36825 258               


AnovaModel <- lm(YII ~ Year_Realease, data = subset(BluesDataGermany, Year == "2017" & TreatEnv == "D_stress") , 
                 contrasts=list( Year_Realease ="contr.Sum"))

Anova(AnovaModel)
# Anova Table (Type II tests)

# Response: YII
# Sum Sq  Df F value Pr(>F)
# Year_Realease 0.010258  39  1.3474 0.1165
# Residuals     0.021277 109  # Interesting results

AnovaModel <- lm(YII ~ Year_Realease, data = subset(BluesDataGermany, Year == "2018" & TreatEnv == "D_stress") , 
                 contrasts=list( Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value Pr(>F)
# Year_Realease 0.10275  39  0.8224 0.7532
# Residuals     0.34917 109 

# For control ---------------

AnovaModel <- lm(YII ~ Year_Realease, data = subset(BluesDataGermany, TreatEnv == "Control") , 
                 contrasts=list( Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value Pr(>F)
# Year_Realease 0.02596  39  0.1602      1
# Residuals     1.07212 258 # Not that interresting (BESIDE YOU CAN CONCETRATE ON THE CONTRASTED Year of release)


AnovaModel <- lm(YII ~ Year_Realease, data = subset(BluesDataGermany, Year == "2017" & TreatEnv == "Control") , 
                 contrasts=list( Year_Realease ="contr.Sum"))

Anova(AnovaModel)
# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value Pr(>F)
# Year_Realease 0.016396  39  1.3313  0.126
# Residuals     0.034420 109               

AnovaModel <- lm(YII ~ Year_Realease, data = subset(BluesDataGermany, Year == "2018" & TreatEnv == "Control") , 
                 contrasts=list( Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value  Pr(>F)  
# Year_Realease 0.029142  39  1.4038 0.08776 .
# Residuals     0.058020 109                  


############### For Grain yield (not the focus) ###############
AnovaModel <- lm(GY ~ Year_Realease, data = subset(BluesDataGermany, Year == "2017" & TreatEnv == "D_stress") , 
                 contrasts=list( Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: GY
# Sum Sq  Df F value  Pr(>F)  
# Year_Realease 24441145  39  1.5397 0.04271 *
#   Residuals     43958065 108                  


AnovaModel <- lm(GY ~ Year_Realease, data = subset(BluesDataGermany, Year == "2018" & TreatEnv == "D_stress") , 
                 contrasts=list( Year_Realease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: GY
# Sum Sq  Df F value   Pr(>F)   
# Year_Realease  69142490  39  1.8597 0.006442 **
#   Residuals     103910738 109                    

BluesDataGermany$Year_Realease <- as.factor(BluesDataGermany$Year_Realease)

levels(BluesDataGermany$Year_Realease)

table(BluesDataGermany$Year_Realease)

head(BluesDataGermany)

# Creating the newest and oldest variable in the Data
# using something like if statement
BluesDataGermany$RelYear <- as.numeric(as.character(BluesDataGermany$Year_Realease))

table(BluesDataGermany$RelYear)
str(BluesDataGermany)

BluesDataGermany$TimeRelease[BluesDataGermany$RelYear >= 1980 | BluesDataGermany$RelYear< 2010] <- "Medium"

BluesDataGermany$TimeRelease[BluesDataGermany$RelYear<1980] <- "Oldest" 

BluesDataGermany$TimeRelease[BluesDataGermany$RelYear> 2010] <- "Newest"

levels(as.factor(BluesDataGermany$TimeRelease))
str(BluesDataGermany)
View(BluesDataGermany)


############ Check the effect of release time ############################


AnovaModel <- lm(YII ~ TimeRelease, data = subset(BluesDataGermany, Year == "2017" & TreatEnv == "Control") , 
                 contrasts=list( TimeRelease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value  Pr(>F)  
# TimeRelease 0.002739   2  4.1582 0.01753 *
#   Residuals   0.048077 146                  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

AnovaModel <- lm(YII ~ TimeRelease, data = subset(BluesDataGermany, Year == "2018" & TreatEnv == "Control") , 
                 contrasts=list( TimeRelease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value    Pr(>F)    
# TimeRelease 0.008336   2  7.7197 0.0006502 ***
#   Residuals   0.078826 146                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

AnovaModel <- lm(YII ~ TimeRelease, data = subset(BluesDataGermany, Year == "2017" & TreatEnv == "D_stress") , 
                 contrasts=list( TimeRelease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value Pr(>F)  
# TimeRelease 0.001397   2  3.3838 0.0366 *
#   Residuals   0.030138 146                 
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1     


AnovaModel <- lm(YII ~ TimeRelease, data = subset(BluesDataGermany, Year == "2018" & TreatEnv == "D_stress") , 
                 contrasts=list( TimeRelease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value Pr(>F)
# TimeRelease 0.00136   2   0.221  0.802
# Residuals   0.45056 146  


############ Check the effect of contrasted release time #########################

BluesDataGermanyOlNe <- BluesDataGermany[BluesDataGermany$TimeRelease == "Oldest" | BluesDataGermany$TimeRelease == "Newest",]
dim(BluesDataGermany)
dim(BluesDataGermanyOlNe)
View(BluesDataGermanyOlNe)

AnovaModel <- lm(YII ~ TimeRelease, data = subset(BluesDataGermanyOlNe, Year == "2017" & TreatEnv == "D_stress") , 
                 contrasts=list( TimeRelease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq Df F value   Pr(>F)   
# TimeRelease 0.0013371  1  8.2867 0.005947 **
#   Residuals   0.0077447 48                         

AnovaModel <- lm(YII ~ TimeRelease, data = subset(BluesDataGermanyOlNe, Year == "2018" & TreatEnv == "D_stress") , 
                 contrasts=list( TimeRelease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq Df F value Pr(>F)
# TimeRelease 0.00023  1  0.0998 0.7535
# Residuals   0.11072 48               

AnovaModel <- lm(YII ~ TimeRelease, data = subset(BluesDataGermanyOlNe, Year == "2017" & TreatEnv == "Control") , 
                 contrasts=list( TimeRelease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq Df F value   Pr(>F)   
# TimeRelease 0.0025715  1    10.4 0.002269 **
#   Residuals   0.0118680 48                    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

AnovaModel <- lm(YII ~ TimeRelease, data = subset(BluesDataGermanyOlNe, Year == "2018" & TreatEnv == "Control") , 
                 contrasts=list( TimeRelease ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq Df F value   Pr(>F)   
# TimeRelease 0.006023  1  10.612 0.002066 **
#   Residuals   0.027244 48                    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# abline(AnovaModel)



########### Conduct ANOVA including growing season as a factor #############
install.packages("Rcmdr")
library(Rcmdr)

AnovaModel.1 <- lm(YII ~ Genotypes*TreatEnv*Year, 
                   data=Blues_17_18_article1_2, contrasts=list(Genotypes ="contr.Sum", 
                                                               TreatEnv ="contr.Sum", Year ="contr.Sum"))
Anova(AnovaModel.1) # You cannot use the Blues for ANOVA because it doesn´t contain REPETITION


# From The SAS model

# PROC MIXED DATA=Rawdata.Briwecs_hierac_2017;
# CLASS Block Row Column Treatment BRISONr; 
# MODEL Hardness = Treatment|BRISONr;
# RANDOM 	Block Row(Block) Column Row*Column(Block);
# LSMEANS Treatment*BRISONr;
# RUN;

library(lme4)

head(Data_forHeritab_Physio_2)
dim(Data_forHeritab_Physio_2)


mod <-lmer(YII ~ Year*TreatEnv*Genotypes + (1|Repetition) +
             (1|Row/Repetition) + (1|Column/Repetition),
             data = Data_forHeritab_Physio_2)

Anova(mod)

# Analysis of Deviance Table (Type II Wald chisquare tests)

# Response: YII
# Chisq  Df  Pr(>Chisq)    
# Year                    1971.92   1   < 2.2e-16 ***
#   TreatEnv                 700.54 200   < 2.2e-16 ***
#   Genotypes                838.30 398   < 2.2e-16 ***
#   Year:TreatEnv             10.57   1    0.001149 ** 
#   Year:Genotypes           298.46 199 0.000006271 ***
#   TreatEnv:Genotypes       526.52 199   < 2.2e-16 ***
#   Year:TreatEnv:Genotypes  259.30 199    0.002598 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

mod <-lm(YII ~ Year*TreatEnv*Genotypes + (1|Repetition) +
             (1|Row) + (1|Column),
           data = Data_forHeritab_Physio_2)

Anova(mod)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq   Df   F value       Pr(>F)    
# Year                    9.7811    1 2827.3116    < 2.2e-16 ***
#   TreatEnv                0.4961    1  143.3983    < 2.2e-16 ***
#   Genotypes               1.1326  199    1.6452 0.0000001828 ***
#   1 | Repetition                    0                           
# 1 | Row/Repetition                0                           
# 1 | Column/Repetition             0                           
# Year:TreatEnv           0.0404    1   11.6847    0.0006432 ***
#   Year:Genotypes          1.0835  199    1.5739 0.0000020334 ***
#   TreatEnv:Genotypes      1.7753  199    2.5787    < 2.2e-16 ***
#   Year:TreatEnv:Genotypes 0.8000  199    1.1621    0.0689412 .  
# Residuals               6.6803 1931         

mod <-lm(SPAD ~ Year*TreatEnv*Genotypes + (1|Repetition) +
           (1|Row) + (1|Column),
         data = Data_forHeritab_Physio_2)

Anova(mod)

# Anova Table (Type II tests)
# 
# Response: SPAD
# Sum Sq   Df  F value        Pr(>F)    
# Year                       19.4    1   1.4929       0.22192    
# TreatEnv                 3049.6    1 234.6840     < 2.2e-16 ***
#   Genotypes               13413.8  199   5.1873     < 2.2e-16 ***
#   1 | Repetition                     0                           
# 1 | Row                            0                           
# 1 | Column                         0                           
# Year:TreatEnv              17.1    1   1.3139       0.25183    
# Year:Genotypes           3127.4  199   1.2094       0.02976 *  
#   TreatEnv:Genotypes       5707.6  199   2.2072     < 2.2e-16 ***
#   Year:TreatEnv:Genotypes  4356.7  199   1.6848 0.00000004242 ***
#   Residuals               25975.8 1999                           
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


mod <-lmer(SPAD ~ Year*TreatEnv*Genotypes + (1|Repetition) +
             (1|Row/Repetition) + (1|Column/Repetition),
           data = Data_forHeritab_Physio_2)

Anova(mod)

install.packages("lsmeans")
library(lsmeans)

ls = lsmeans(mod,list("Genotypes"))
ls = lsmeans(mod,list("TreatEnv*Genotypes"))

em = emmeans (mod, ~ Genotypes | TreatEnv, adjust = "sidak", options = get_emm_option("emmeans"))
aa

install.packages("xlsx")
library(xlsx)
write.xlsx(
  BluesDataGermany,
  "Data/BluesDataGermany.xlsx",
  sheetName = "BluesDataGermany",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE,
  showNA = TRUE,
  password = NULL
)


###################### Correlation based only on Germany ####################

# Combine Chl_SC_Trs_Phot and origine of release data

dim(Chl_SC_Trs_Phot)
head(sub_ancestr,2)
ChlSCTrsPhotOrigine <- merge(Chl_SC_Trs_Phot, sub_ancestr,by.x = "Genotypes", 
                          by.y = "Genotype", all = FALSE, all.x = FALSE, all.y = FALSE)
dim(ChlSCTrsPhotOrigine)

head(ChlSCTrsPhotOrigine,5)

ChlSCTrsPhotOrigineGermany <- subset(ChlSCTrsPhotOrigine, Origine == "Germany")
dim(ChlSCTrsPhotOrigineGermany)

head(ChlSCTrsPhotOrigineGermany,5)

chart.Correlation(ChlSCTrsPhotOrigineGermany[,c(6:16)])


chart.Correlation(ChlSCTrsPhotOrigineGermany[ChlSCTrsPhotOrigineGermany$TreatEnv %in% "D_stress",][,c(6:16)])

chart.Correlation(ChlSCTrsPhotOrigineGermany[ChlSCTrsPhotOrigineGermany$TreatEnv %in% "Control",][,c(6:16)])

########################### NPQ June 17 18 ##################
library(readxl)
CFP_Data <- read_excel("Data/CFP_Data.xlsx", 
                       sheet = "CFP_NPQJune")
View(CFP_Data)

str(CFP_Data)

CFP_Data$Year <- as.factor(CFP_Data$Year)
CFP_Data$TreatEnv <- as.factor(CFP_Data$TreatEnv)
CFP_Data$Genotypes <- as.factor(CFP_Data$Genotypes)
CFP_Data$Genotypes <- as.factor(CFP_Data$Genotypes)

AnovaModel.2 <- lm(NPQ ~ Genotypes*TreatEnv*Year, data=CFP_Data, 
                 contrasts=list(Genotypes ="contr.Sum", TreatEnv ="contr.Sum", 
                      Year="contr.Sum"))

Anova(AnovaModel.2)

# 
# Anova Table (Type II tests)
# 
# Response: NPQ
# Sum Sq  Df F value    Pr(>F)    
# Genotypes                1.1209  19  0.7394   0.77426    
# TreatEnv                 7.9294   1 99.3866 < 2.2e-16 ***
#   Year                     5.8483   1 73.3024 5.871e-15 ***
#   Genotypes:TreatEnv       2.3682  19  1.5623   0.07068 .  
# Genotypes:Year           0.8589  19  0.5666   0.92554    
# TreatEnv:Year            0.3546   1  4.4442   0.03646 *  
#   Genotypes:TreatEnv:Year  1.5165  18  1.0560   0.40092    
# Residuals               13.8025 173                      


AnovaModel.2 <- lm(FvoverFm ~ Genotypes*TreatEnv*Year, data=CFP_Data, 
                   contrasts=list(Genotypes ="contr.Sum", TreatEnv ="contr.Sum", 
                                  Year="contr.Sum"))

Anova(AnovaModel.2)

# Anova Table (Type II tests)
# 
# Response: FvoverFm
# Sum Sq  Df F value    Pr(>F)    
# Genotypes               0.05539  19  1.4958  0.091809 .  
# TreatEnv                0.07048   1 36.1611 1.042e-08 ***
#   Year                    0.15499   1 79.5211 6.399e-16 ***
#   Genotypes:TreatEnv      0.08319  19  2.2464  0.003289 ** 
#   Genotypes:Year          0.04974  19  1.3431  0.162149    
# TreatEnv:Year           0.00387   1  1.9873  0.160411    
# Genotypes:TreatEnv:Year 0.04454  18  1.2695  0.213118    
# Residuals               0.33914 174                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


AnovaModel.2 <- lm(YII ~ Genotypes*TreatEnv*Year, data=CFP_Data, 
                   contrasts=list(Genotypes ="contr.Sum", TreatEnv ="contr.Sum", 
                                  Year="contr.Sum"))

Anova(AnovaModel.2)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df F value      Pr(>F)    
# Genotypes               0.16301  19  3.6291 0.000002913 ***
#   TreatEnv                0.00071   1  0.3024     0.58309    
# Year                    0.21269   1 89.9660   < 2.2e-16 ***
#   Genotypes:TreatEnv      0.04764  19  1.0607     0.39568    
# Genotypes:Year          0.14259  19  3.1744 0.000030819 ***
#   TreatEnv:Year           0.03904   1 16.5119 0.000073228 ***
#   Genotypes:TreatEnv:Year 0.07369  18  1.7316     0.03791 *  
#   Residuals               0.40899 173                        
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


########### use the Subset 20 Genotypes for SPAD and YII at anthesis #######################################################

Geno <- data.frame(unique(CFP_Data$Genotypes))

colnames(Geno)[1] <- "Genotypes"
head(Geno)
head(Data_forHeritab_Physio_2)

SubData_forHeritab_Physio_2 <- merge(Data_forHeritab_Physio_2,Geno, by.x = "Genotypes", by.y = "Genotypes", all = FALSE)

head(SubData_forHeritab_Physio_2,50)

levels(as.factor(SubData_forHeritab_Physio_2$Genotypes)) #ok

options(max.print = 150)

str(SubData_forHeritab_Physio_2)
SubData_forHeritab_Physio_2$Genotypes <- as.factor(SubData_forHeritab_Physio_2$Genotypes)
SubData_forHeritab_Physio_2$TreatEnv <- as.factor(SubData_forHeritab_Physio_2$TreatEnv)
SubData_forHeritab_Physio_2$Year <- as.factor(SubData_forHeritab_Physio_2$Year)


AnovaModel.2 <- lm(YII ~ Genotypes*TreatEnv*Year, data=subset(SubData_forHeritab_Physio_2, TreatN=="Voll"), 
                   contrasts=list(Genotypes ="contr.Sum", TreatEnv ="contr.Sum", 
                                  Year="contr.Sum"))
Anova(AnovaModel.2)

# Anova Table (Type II tests)
# 
# Response: YII
# Sum Sq  Df  F value       Pr(>F)    
# Genotypes               0.05470  19   1.4322    0.1250629    
# TreatEnv                0.06452   1  32.0981 0.0000001068 ***
#   Year                    0.76748   1 381.8087    < 2.2e-16 ***
#   Genotypes:TreatEnv      0.11078  19   2.9005    0.0002332 ***
#   Genotypes:Year          0.08998  19   2.3560    0.0027741 ** 
#   TreatEnv:Year           0.01010   1   5.0260    0.0268552 *  
#   Genotypes:TreatEnv:Year 0.08205  19   2.1484    0.0069462 ** 
#   Residuals               0.23518 117                          
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

AnovaModel.2 <- lm(SPAD ~ Genotypes*TreatEnv*Year, data=subset(SubData_forHeritab_Physio_2, TreatN=="Voll"), 
                   contrasts=list(Genotypes ="contr.Sum", TreatEnv ="contr.Sum", 
                                  Year="contr.Sum"))
Anova(AnovaModel.2)



# Anova Table (Type II tests)
# 
# Response: SPAD
# Sum Sq  Df F value       Pr(>F)    
# Genotypes               1822.53  19  4.3250 0.0000003129 ***
#   TreatEnv                 254.82   1 11.4893    0.0009479 ***
#   Year                     105.15   1  4.7411    0.0314070 *  
#   Genotypes:TreatEnv      1052.59  19  2.4979    0.0014220 ** 
#   Genotypes:Year           232.39  19  0.5515    0.9321979    
# TreatEnv:Year             35.88   1  1.6178    0.2058599    
# Genotypes:TreatEnv:Year  360.47  19  0.8554    0.6374420    
# Residuals               2661.43 120                         
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
