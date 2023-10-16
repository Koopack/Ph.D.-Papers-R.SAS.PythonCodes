# Heritability and Repetabilty

# Same Like the CV amd descriptive Stats

# The heritability is run with the Blue Data 

#H2_Fim_ds = repeatability(data.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$Fmin,

# H2_Fim_ds = repeatability(data.vector = ndyncorrected1718forbluesfin_17$Fmin,
#                           geno.vector = ndyncorrected1718forbluesfin_17$SortenNr, line.repeatability = FALSE)
# 
# H2_Fim_ds = repeatability(data.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$Fmin,
#                           geno.vector = MiniPamVollfinalDataDynamic2017S2_Boot_Ds$SortenNr,
#                           line.repeatability = TRUE)
# 
# H2_Fim2_ds = H2_Fim_ds$gen.variance / (H2_Fim_ds$gen.variance + H2_Fim_ds$res.variance)
View(DataBlue17)
str(DataBlue17)
DataBlue17 <- as.data.frame(DataBlue17)

DataBlue17$Water <- as.factor(DataBlue17$Water)
DataBlue17$Genotypes <- as.factor(DataBlue17$Genotypes)

Varcomp2017_Biomasse <- lmer(Biomasse ~ 1+(1|Water)+(1|Genotypes)+(1|Water:Genotypes)
                                        ,data = DataBlue17)
# Error: number of levels of each grouping factor must be < number of observations (problems: Water:Genotypes)
# Because there is no repetition

# Take the Data with repetition DataBlue1718

Varcomp_Biomasse <- lmer(Biomasse ~ 1+(1|Water)+(1|Genotypes)+(1|Water:Genotypes) + (1|Repetition) +(1|Repetition:Water)
                             ,data = DataBlue1718)

summary(Varcomp_Biomasse)

Df_Biomasse <- as.data.frame(VarCorr(Varcomp_Biomasse))

H_Biomasse <- Df_Biomasse$vcov[2] / (Df_Biomasse$vcov[2] + Df_Biomasse$vcov[1]/2 + Df_Biomasse$vcov[6]/4)

H_Biomasse
# 0.4217419

# For Seedyield

Varcomp_Seedyield <- lmer(Seedyield ~ 1+(1|Water)+(1|Genotypes)+(1|Water:Genotypes) + (1|Repetition) +(1|Repetition:Water)
                         ,data = DataBlue1718)

summary(Varcomp_Seedyield)

Df_Seedyield <- as.data.frame(VarCorr(Varcomp_Seedyield))

H_Seedyield <- Df_Seedyield$vcov[2] / (Df_Seedyield$vcov[2] + Df_Seedyield$vcov[1]/2 + Df_Seedyield$vcov[6]/4)

H_Seedyield 
#  0.3705988
# The heritability is so moderate because from drought to control the variation is very high

# Let us check the repeatability

# Varcomp_Rainout_Seedyield <- lmer(Seedyield ~ 1+ (1|Genotypes)+(1|Repetition) +(1|Repetition:Genotypes)
#                           ,data = subset(DataBlue1718, Water=="Rainout"))
# 
# summary(Varcomp_Rainout_Seedyield)
# 
# Df_R_Seedyield <- as.data.frame(VarCorr(Varcomp_Seedyield))
# 
# H_Seedyield <- Df_Seedyield$vcov[2] / (Df_Seedyield$vcov[2] + Df_Seedyield$vcov[1]/2 + Df_Seedyield$vcov[6]/4)
# 
# H_Seedyield  # Not interesting
