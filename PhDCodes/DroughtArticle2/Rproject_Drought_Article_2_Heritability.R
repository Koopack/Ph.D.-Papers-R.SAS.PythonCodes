# Date Sunday 20 December 2020 ----

# Heritability calculation for drought article Physio ----

install.packages("heritability")
library(heritability)

repeatability(data.vector=rep(rnorm(26),each=5) + rnorm(5*26),
              geno.vector=rep(letters,each=5))

a <- rep (rnorm(3), times=2) # a <- rep (rnorm(3), each=2) each repete the same before moving on

data(means_LDV)
head(means_LDV,5)

data(LDV)
head(LDV,7)

data(K_atwell)
head(K_atwell,5)

data(R_matrix_LDV)
head(R_matrix_LDV,5)

covariates.frame=as.data.frame(LDV[,3])
head(covariates.frame,10)

# Read the data 
MiniPamVollfinalDataDynamic2017 <- read.delim("D:/Patrice Koua/Documents, PhD Study/Documents, PhD Researches/
                                              Documents, Articles Patrice Koua/Documents, Article 2 Sub20Geno/
                                              Ready data/2017/MiniPamVollfinalDataDynamic2017.txt") 

head(MiniPamVollfinalDataDynamic2017,5)

MiniPamVollfinalDataDynamic2017S2_Boot_Ds <- subset(MiniPamVollfinalDataDynamic2017, BBCH_Range=="S2_Booting" & TreatEnv == "D_stress") # always ==
View(MiniPamVollfinalDataDynamic2017S2_Boot_Ds)

# repeatability(data.vector=rep(rnorm(26),each=5) + rnorm(5*26),
#               geno.vector=rep(letters,each=5))

# repeatability(data.vector, geno.vector, line.repeatability = FALSE,
#               covariates.frame = data.frame())

H2_Fim_ds = repeatability(data.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$Fmin,
              geno.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$SortenNr, line.repeatability = FALSE)

H2_Fim_ds = repeatability(data.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$Fmin,
                          geno.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$SortenNr,
                          line.repeatability = TRUE)

H2_Fim2_ds = H2_Fim_ds$gen.variance / (H2_Fim_ds$gen.variance + H2_Fim_ds$res.variance)

repeatability(data.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$Fmin,
              geno.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$SortenNr, line.repeatability = TRUE,
              covariates.frame = data.frame(MiniPamVollfinalDataDynamic2017S2_Boot_Ds$Laufend_Nr))

# Varcomp_DS2017_PBW <-lmer(PBW ~ 1 +  (1|Genotypes)
#                           ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2017" & 
#                                                                 DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])

Varcomp_2017Ds_Fmin <-lmer(Fmin ~ 1 +  (1|SortenNr),data = MiniPamVollfinalDataDynamic2017S2_Boot_Ds)

summary(Varcomp_2017Ds_Fmin)

85.22 / 927.04
Varcomp_2017Ds_Fmin <-lmer(Fmin ~ 1 +  (1|SortenNr) + (1|wdh) + (1|SortenNr:wdh),
                           data = MiniPamVollfinalDataDynamic2017S2_Boot_Ds)

5.282e-04 / 2.513e+01
Varcomp2017 <- lmer(Grain_yield~ 1+(1|Treatment)+(1|BRISONr)+(1|Treatment:BRISONr)
                    ,data=Briwecs_BLUES_GY_151617[Briwecs_BLUES_GY_151617$Year%in%"2017",])


repeatability(data.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$Fmax,
              geno.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$SortenNr, line.repeatability = TRUE,
              covariates.frame = data.frame(MiniPamVollfinalDataDynamic2017S2_Boot_Ds$Laufend_Nr))

repeatability(data.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$YII,
              geno.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$SortenNr, line.repeatability = TRUE,
              covariates.frame = data.frame(MiniPamVollfinalDataDynamic2017S2_Boot_Ds$Laufend_Nr))

Varcomp_2017Ds_YII<-lmer(YII ~ 1 +  (1|SortenNr),data = MiniPamVollfinalDataDynamic2017S2_Boot_Ds)
0.01832 / 0.05459

82 / 7

#SeedY_QuaDTI_Data171818forAnova
SeedY_QuaDTI_Data171818forAnova_Ds <- subset (SeedY_QuaDTI_Data171818forAnova, TreatEnv=="D_stress")

repeatability(data.vector = SeedY_QuaDTI_Data171818forAnova_Ds$Seedyield_dT.ha,
              geno.vector = SeedY_QuaDTI_Data171818forAnova_Ds$Genotypes, line.repeatability = FALSE,
              covariates.frame = data.frame(SeedY_QuaDTI_Data171818forAnova_Ds$Repetition))

Varcomp_2017Ds_Seedyield_dT.ha <- lmer(Seedyield_dT.ha ~ 1 +  (1|Genotypes),
                                       data = SeedY_QuaDTI_Data171818forAnova_Ds)

### Load the data for the 4 traits

Data_forHeritab_Physio <- read.delim("Data/Data_forHeritab_Physio.txt")

head(Data_forHeritab_Physio,5)


repeatability(data.vector = Data_forHeritab_Physio$SPAD,
              geno.vector = Data_forHeritab_Physio$Genotypes, line.repeatability = TRUE,
              covariates.frame = data.frame(Data_forHeritab_Physio$Repetition))

# SPAD in 2017 DS

Data_forHeritab_Physio_2017Ds = subset(Data_forHeritab_Physio, Year =="2017" & TreatEnv =="D_stress")

Varcomp_2017Ds_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                                       data = Data_forHeritab_Physio_2017Ds)
summary(Varcomp_2017Ds_SPAD)
class(Varcomp_2017Ds_SPAD)
#13.89 / 24.71 =0.5621206

# SPAD in 2017 control

Varcomp_2017K_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                            data = subset(Data_forHeritab_Physio, Year =="2017" & TreatEnv =="Control"))
summary(Varcomp_2017K_SPAD)
# 3.625 / 12.748 = 0.2843583

# SPAD in 2018 DS

Varcomp_2018Ds_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                            data = subset(Data_forHeritab_Physio, Year =="2018" & TreatEnv =="D_stress"))
summary(Varcomp_2018Ds_SPAD)

# 4.332 / 9.629 = 0.449891

# SPAD in 2018 K

Varcomp_2018K_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                            data = subset(Data_forHeritab_Physio, Year =="2018" & TreatEnv =="Control"))
summary(Varcomp_2018K_SPAD)
# 2.018 / 8.710 = 0.2316877

####

# SPAD in 2017 DS

Data_forHeritab_Physio_2017Ds = subset(Data_forHeritab_Physio, Year =="2017" & TreatEnv =="D_stress")

Varcomp_2017Ds_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                            data = Data_forHeritab_Physio_2017Ds)
summary(Varcomp_2017Ds_SPAD)
class(Varcomp_2017Ds_SPAD)
#13.89 / 24.71 =0.5621206

# SPAD in 2017 control

Varcomp_2017K_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                           data = subset(Data_forHeritab_Physio, Year =="2017" & TreatEnv =="Control"))
summary(Varcomp_2017K_SPAD)
# 3.625 / 12.748 = 0.2843583

# SPAD in 2018 DS

Varcomp_2018Ds_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                            data = subset(Data_forHeritab_Physio, Year =="2018" & TreatEnv =="D_stress"))
summary(Varcomp_2018Ds_SPAD)

# 4.332 / 9.629 = 0.449891

# SPAD in 2018 K

Varcomp_2018K_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                           data = subset(Data_forHeritab_Physio, Year =="2018" & TreatEnv =="Control"))
summary(Varcomp_2018K_SPAD)
# 2.018 / 8.710 = 0.2316877

str(ranef(Varcomp_2018K_SPAD))

Df_SPAD_K18 <- as.data.frame(VarCorr(Varcomp_2018K_SPAD))

H_SPAD_K18 <- Df_SPAD_K18$vcov[1] / Df_SPAD_K18$vcov[2] 

####

# Monday 21th December 2020 -----

# Heritability including genetic relatedness (kinship matrix) -----

#load the kinship matrix calculated in Tassel Kinship_IBS_Briw2dDrought200genoHomoAll
Kinship_IBS <- read.delim("Data/Kinship_IBS_Briw2dDrought200genoHomoAll.txt")
head(Kinship_IBS,2)

colnames(Kinship_IBS)[1] <- " "
View(Kinship_IBS[1:5,1:5])

# Phenotyipic data 
head(Data_forHeritab_Physio_2017Ds,6)

Data_forHeritab_Physio_2017Ds <- Data_forHeritab_Physio_2017Ds[order(-Data_forHeritab_Physio_2017Ds$Genotypes),] 
# Work with number

Data_forHeritab_Physio_2017Ds <- Data_forHeritab_Physio_2017Ds[with(Data_forHeritab_Physio_2017Ds, order(Genotypes)),] 
#  with is the magical function
head(Data_forHeritab_Physio_2017Ds,6) 

# out <- marker_h2(data.vector=LD$LD,geno.vector=LD$genotype,
#                  covariates=LD[,4:8],K=K_atwell)

out <- marker_h2(data.vector = Data_forHeritab_Physio_2017Ds$SPAD,
                 geno.vector = Data_forHeritab_Physio_2017Ds$Genotypes,
                 covariates  = Data_forHeritab_Physio_2017Ds[,10],
                         K   = Kinship_IBS_Mat)

str(K_atwell)
str(Kinship_IBS)
row.names(Kinship_IBS) <- Kinship_IBS[,1]
Kinship_IBS <- Kinship_IBS[,-1]
#xy.list <- as.list(as.data.frame(t(xy.df)))
Kinship_IBS.list <- as.list(as.data.frame(Kinship_IBS))
str(xy.list)
remove(xy.list)

Kinship_IBS_Mat <- as.matrix(Kinship_IBS) # Well you yust had to convert the dataframe into matrix

marker_h2(data.vector = Data_forHeritab_Physio_2017Ds$SPAD,
          geno.vector = Data_forHeritab_Physio_2017Ds$Genotypes,
          covariates  = Data_forHeritab_Physio_2017Ds$Repetition,
          K   = Kinship_IBS_Mat) # With covariate gives better results

head(Data_forHeritab_Physio_2017Ds,5)
tail(Data_forHeritab_Physio_2017Ds,5)

Data_forHeritab_Physio_2017K = subset(Data_forHeritab_Physio, Year =="2017" & TreatEnv =="Control")
Data_forHeritab_Physio_2017K <- Data_forHeritab_Physio_2017K[with(Data_forHeritab_Physio_2017K, order(Genotypes)),]
head(Data_forHeritab_Physio_2017K,5)

marker_h2(data.vector = Data_forHeritab_Physio_2017K$SPAD,
          geno.vector = Data_forHeritab_Physio_2017K$Genotypes,
          covariates  = Data_forHeritab_Physio_2017K$Laufend_Nr,
          K   = Kinship_IBS_Mat) 

# So odering the data doesn´t affect the rusult, however good to oroder the data

# Load the new Data type

Data_forHeritab_Physio_2 <- read.delim("Data/Data_forHeritab_Physio_2.txt")

Data_forHeritab_Physio_2_2017K = subset(Data_forHeritab_Physio_2, Year =="2017" & TreatEnv =="Control")

head(Data_forHeritab_Physio_2_2017K,5)

marker_h2(data.vector = Data_forHeritab_Physio_2_2017K$Fmin,
          geno.vector = Data_forHeritab_Physio_2_2017K$Genotypes,
          covariates  = Data_forHeritab_Physio_2_2017K$Repetition,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = Data_forHeritab_Physio_2_2017K$Fmin,
          geno.vector = Data_forHeritab_Physio_2_2017K$Genotypes,
          covariates  = Data_forHeritab_Physio_2_2017K[,11:13],
          K   = Kinship_IBS_Mat)

# Varcomp2017_PH <-lmer(PH ~ 1 + (1|TreatEnv) + (1|Genotypes) + (1|Repetition)+
#                         (1|TreatEnv:Genotypes) + (1|TreatEnv:Repetition)
#                       ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2017",])

# Data with mean SPAD

Data_forHeritab_Physio_Mean <- read.delim ("Data/Data_forHeritab_Physio_Mean.txt")

Data_forHeritab_Physio_Mean_K <- subset(Data_forHeritab_Physio_Mean, TreatEnv == "Control")
head(Data_forHeritab_Physio_Mean_K,5)
marker_h2(data.vector = subset(Data_forHeritab_Physio_Mean, TreatEnv == "Control")$SPAD,
          geno.vector = Data_forHeritab_Physio_Mean_K$Genotypes,
          covariates  = Data_forHeritab_Physio_Mean_K$Repetition,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = subset(Data_forHeritab_Physio_Mean, TreatEnv == "D_stress")$SPAD,
          geno.vector = Data_forHeritab_Physio_Mean_K$Genotypes,
          covariates  = Data_forHeritab_Physio_Mean_K$Repetition,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = subset(Data_forHeritab_Physio_Mean, TreatEnv == "Control")$Fmin,
          geno.vector = Data_forHeritab_Physio_Mean_K$Genotypes,
          covariates  = Data_forHeritab_Physio_Mean_K$Repetition,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = subset(Data_forHeritab_Physio_Mean, TreatEnv == "D_stress")$Fmin,
          geno.vector = Data_forHeritab_Physio_Mean_K$Genotypes,
          covariates  = Data_forHeritab_Physio_Mean_K$Repetition,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = subset(Data_forHeritab_Physio_Mean, TreatEnv == "Control")$Fmax,
          geno.vector = Data_forHeritab_Physio_Mean_K$Genotypes,
          covariates  = Data_forHeritab_Physio_Mean_K$Repetition,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = subset(Data_forHeritab_Physio_Mean, TreatEnv == "D_stress")$Fmax,
          geno.vector = Data_forHeritab_Physio_Mean_K$Genotypes,
          covariates  = Data_forHeritab_Physio_Mean_K$Repetition,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = subset(Data_forHeritab_Physio_Mean, TreatEnv == "Control")$YII,
          geno.vector = Data_forHeritab_Physio_Mean_K$Genotypes,
          covariates  = Data_forHeritab_Physio_Mean_K$Repetition,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = subset(Blues_17_18_article1_2, TreatEnv == "Control")$YII,
          geno.vector = Blues_17_18_article1_2$Genotypes,
                  K   = Kinship_IBS_Mat)

# Varcomp2017_PH <-lmer(PH ~ 1 + (1|TreatEnv) + (1|Genotypes) + (1|Repetition)+
#                         (1|TreatEnv:Genotypes) + (1|TreatEnv:Repetition)
#                       ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2017",])

# SPAD
Varcomp_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                           data = subset(Data_forHeritab_Physio_Mean, TreatEnv =="Control"))
summary(Varcomp_SPAD)
3.446 / 7.873

Varcomp_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                     data = subset(Data_forHeritab_Physio_Mean, TreatEnv =="D_stress"))
summary(Varcomp_SPAD)
9.066 / 16.102

# Fmin
Varcomp_Fmin <- lmer(Fmin ~ 1 +  (1|Genotypes),
                     data = subset(Data_forHeritab_Physio_Mean, TreatEnv =="Control"))
summary(Varcomp_Fmin)


Varcomp_Fmin <- lmer(Fmin ~ 1 +  (1|Genotypes),
                     data = subset(Data_forHeritab_Physio_Mean, TreatEnv =="D_stress"))
summary(Varcomp_Fmin)


# Fmax
Varcomp_Fmax <- lmer(Fmax ~ 1 +  (1|Genotypes),
                     data = subset(Data_forHeritab_Physio_Mean, TreatEnv =="Control"))
summary(Varcomp_Fmax)

Varcomp_Fmax <- lmer(Fmax ~ 1 +  (1|Genotypes),
                     data = subset(Data_forHeritab_Physio_Mean, TreatEnv =="D_stress"))
summary(Varcomp_Fmax)


# YII
Varcomp_YII <- lmer(YII ~ 1 +  (1|Genotypes),
                     data = subset(Data_forHeritab_Physio_Mean, TreatEnv =="Control"))
summary(Varcomp_YII)

Varcomp_YII <- lmer(YII ~ 1 +  (1|Genotypes),
                     data = subset(Data_forHeritab_Physio_Mean, TreatEnv =="D_stress"))
summary(Varcomp_YII)


# Ds and control

Varcomp_SPAD <-lmer(SPAD ~ 1 + (1|TreatEnv) + (1|Genotypes) + (1|Repetition)+
                        (1|TreatEnv:Genotypes) + (1|TreatEnv:Repetition)
                      ,data = Data_forHeritab_Physio_Mean)
summary(Varcomp_SPAD)

Varcomp_YII <-lmer(YII ~ 1 + (1|TreatEnv) + (1|Genotypes) + (1|Repetition)+
                      (1|TreatEnv:Genotypes) + (1|TreatEnv:Repetition)
                    ,data = Data_forHeritab_Physio_Mean)
summary(Varcomp_YII)


Varcomp_Fmin <-lmer(Fmin ~ 1 + (1|TreatEnv) + (1|Genotypes) + (1|Repetition)+
                     (1|TreatEnv:Genotypes) + (1|TreatEnv:Repetition)
                   ,data = Data_forHeritab_Physio_Mean)
summary(Varcomp_Fmin)

Varcomp_Fmax <-lmer(Fmax ~ 1 + (1|TreatEnv) + (1|Genotypes) + (1|Repetition)+
                      (1|TreatEnv:Genotypes) + (1|TreatEnv:Repetition)
                    ,data = Data_forHeritab_Physio_Mean)
summary(Varcomp_Fmax)

# Let us use the Final Blue Data

Blues_17_18_article1_2 <- read.delim("Data/Blues_17_18_article1_2.txt")
head(Blues_17_18_article1_2,5)
# Ds and control

Varcomp_SPAD <-lmer(SPAD ~ 1 + (1|TreatEnv) + 
                      (1|Genotypes) + 
                      (1|TreatEnv:Genotypes), 
                      data = Blues_17_18_article1_2)
summary(Varcomp_SPAD)

Varcomp_YII <-lmer(YII ~ (1|TreatEnv) + 
                      (1|Genotypes) + 
                      (1|TreatEnv:Genotypes), 
                    data = Blues_17_18_article1_2)
summary(Varcomp_YII)

head(Blues_17_18_article1_2,5)

marker_h2(data.vector = subset(Blues_17_18_article1_2, TreatEnv == "Control")$YII,
          geno.vector =subset(Blues_17_18_article1_2, TreatEnv == "Control")$Genotypes,
          K   = Kinship_IBS_Mat)
marker_h2(data.vector = subset(Blues_17_18_article1_2, TreatEnv == "D_stress")$YII,
          geno.vector =subset(Blues_17_18_article1_2, TreatEnv == "D_stress")$Genotypes,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = subset(Blues_17_18_article1_2, TreatEnv == "Control")$Fmax,
          geno.vector =subset(Blues_17_18_article1_2, TreatEnv == "Control")$Genotypes,
          K   = Kinship_IBS_Mat)
marker_h2(data.vector = subset(Blues_17_18_article1_2, TreatEnv == "D_stress")$Fmax,
          geno.vector =subset(Blues_17_18_article1_2, TreatEnv == "D_stress")$Genotypes,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = subset(Blues_17_18_article1_2, TreatEnv == "Control")$Fmin,
          geno.vector =subset(Blues_17_18_article1_2, TreatEnv == "Control")$Genotypes,
          K   = Kinship_IBS_Mat)
marker_h2(data.vector = subset(Blues_17_18_article1_2, TreatEnv == "D_stress")$Fmin,
          geno.vector =subset(Blues_17_18_article1_2, TreatEnv == "D_stress")$Genotypes,
          K   = Kinship_IBS_Mat)

marker_h2(data.vector = subset(Blues_17_18_article1_2, TreatEnv == "Control")$SPAD,
          geno.vector =subset(Blues_17_18_article1_2, TreatEnv == "Control")$Genotypes,
          K   = Kinship_IBS_Mat)
marker_h2(data.vector = subset(Blues_17_18_article1_2, TreatEnv == "D_stress")$SPAD,
          geno.vector =subset(Blues_17_18_article1_2, TreatEnv == "D_stress")$Genotypes,
          K   = Kinship_IBS_Mat)


# BSH under each water regimes with Blues values
Varcomp_YII <- lmer(YII ~ 1 +  (1|Genotypes),
                    data = subset(Blues_17_18_article1_2, TreatEnv =="Control"))
summary(Varcomp_YII)

Varcomp_YII <- lmer(YII ~ 1 +  (1|Genotypes),
                    data = subset(Blues_17_18_article1_2, TreatEnv =="D_stress"))
summary(Varcomp_YII)


Varcomp_Fmax <- lmer(Fmax ~ 1 +  (1|Genotypes),
                    data = subset(Blues_17_18_article1_2, TreatEnv =="Control"))
summary(Varcomp_Fmax)
223.6 / 4713.3

Varcomp_Fmax <- lmer(Fmax ~ 1 +  (1|Genotypes),
                     data = subset(Blues_17_18_article1_2, TreatEnv =="D_stress"))
summary(Varcomp_Fmax)
3080 / 10385


Varcomp_Fmin <- lmer(Fmin ~ 1 +  (1|Genotypes),
                     data = subset(Blues_17_18_article1_2, TreatEnv =="Control"))
summary(Varcomp_Fmin)

Varcomp_Fmin <- lmer(Fmin ~ 1 +  (1|Genotypes),
                     data = subset(Blues_17_18_article1_2, TreatEnv =="D_stress"))
summary(Varcomp_Fmin)


Varcomp_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                     data = subset(Blues_17_18_article1_2, TreatEnv =="Control"))
summary(Varcomp_SPAD)

3.699 / 3.996

Varcomp_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                     data = subset(Blues_17_18_article1_2, TreatEnv =="D_stress"))
summary(Varcomp_SPAD)
6.514 / 9.712

Blues_17_18_article1_2$Year = as.factor(Blues_17_18_article1_2$Year)
Blues_17_18_article1_2$TreatEnv = as.factor(Blues_17_18_article1_2$TreatEnv)
Blues_17_18_article1_2$Genotypes = as.factor(Blues_17_18_article1_2$Genotypes)

######### 

# Use the data with mean

Heritab_Physio_Mean <- read.delim("Data/Heritab_Physio_Mean.txt")
str(Heritab_Physio_Mean)

Heritab_Physio_Mean$TreatEnv <- as.factor(Heritab_Physio_Mean$TreatEnv)
Heritab_Physio_Mean$Genotypes <- as.factor(Heritab_Physio_Mean$Genotypes)

Varcomp_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                     data = subset(Heritab_Physio_Mean, TreatEnv =="Control"))
summary(Varcomp_SPAD)

Varcomp_SPAD <- lmer(SPAD ~ 1 +  (1|Genotypes),
                     data = subset(Heritab_Physio_Mean, TreatEnv =="D_stress"))
summary(Varcomp_SPAD)


Varcomp_YII <- lmer(YII ~ 1 +  (1|Genotypes),
                     data = subset(Heritab_Physio_Mean, TreatEnv =="Control"))
summary(Varcomp_YII)

Varcomp_YII <- lmer(YII ~ 1 +  (1|Genotypes),
                    data = subset(Heritab_Physio_Mean, TreatEnv =="D_stress"))
summary(Varcomp_YII)


Varcomp_Fmin <- lmer(Fmin ~ 1 +  (1|Genotypes),
                    data = subset(Heritab_Physio_Mean, TreatEnv =="Control"))
summary(Varcomp_Fmin)

Varcomp_Fmin <- lmer(Fmin ~ 1 +  (1|Genotypes),
                    data = subset(Heritab_Physio_Mean, TreatEnv =="D_stress"))
summary(Varcomp_Fmin)

Varcomp_Fmax <- lmer(Fmax ~ 1 +  (1|Genotypes),
                     data = subset(Heritab_Physio_Mean, TreatEnv =="Control"))
summary(Varcomp_Fmax)
3080 /  10385

Varcomp_Fmax <- lmer(Fmax ~ 1 +  (1|Genotypes),
                     data = subset(Heritab_Physio_Mean, TreatEnv =="D_stress"))
summary(Varcomp_Fmax)

3080 / 10385

Varcomp_GY <- lmer(GY ~ 1 +  (1|Genotypes), data = subset(Blues_17_18_article1_2, TreatEnv =="Control"))
summary(Varcomp_GY)


Varcomp_GY <- lmer(GY ~ 1 +  (1|Genotypes),
                     data = subset(Heritab_Physio_Mean, TreatEnv =="D_stress"))
