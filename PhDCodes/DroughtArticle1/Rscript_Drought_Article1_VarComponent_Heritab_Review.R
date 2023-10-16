# Saturday, 01st May 2021 

# Calculate the heritability for drought article 1

str(DataRepDsExp1718_YearRL_Abb)

# Change several characters into factors

DataRepDsExp1718_YearRL_Abb[, c("Year", "TreatEnv", "Repetition","Genotypes" )] <-
  lapply(DataRepDsExp1718_YearRL_Abb[, c("Year", "TreatEnv", "Repetition","Genotypes" )],factor)

str(DataRepDsExp1718_YearRL_Abb)
# Varcomp_DS2017_PH <-lmer(PH ~ 1 +  (1|Genotypes)
#                          ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2017" & 
#                                                                DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])
Varcomp_DS2017_PH <-lmer(PH ~ 1 +  (1|Genotypes) + (1|Repetition)
                         ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2017" & 
                                                               DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])

summary(Varcomp_DS2017_PH)

Varcomp_K2017_PH <-lmer(PH ~ 1 +  (1|Genotypes) + (1|Repetition)
                         ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2017" & 
                                                               DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "Control",])
summary(Varcomp_K2017_PH)


# SDW17
Varcomp_DS2017_SDW <-lmer(SDW ~ 1 +  (1|Genotypes) + (1|Repetition)
                          ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2017" & 
                                                                DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])

summary(Varcomp_DS2017_SDW)


Varcomp_K2017_SDW <-lmer(SDW ~ 1 +  (1|Genotypes) + (1|Repetition)
                          ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2017" & 
                                                                DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "Control",])

summary(Varcomp_K2017_SDW)


######################

Varcomp_DS2017_HGr <-lmer(HGr ~ 1 +  (1|Genotypes) + (1|Repetition)
                          ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2017" & 
                                                                DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])

summary(Varcomp_DS2017_HGr)


Varcomp_K2017_HGr <-lmer(HGr ~ 1 +  (1|Genotypes) + (1|Repetition)
                         ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2017" & 
                                                               DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "Control",])

summary(Varcomp_K2017_HGr)

#########################
# in 2018

Varcomp_DS2018_PH <-lmer(PH ~ 1 +  (1|Genotypes) + (1|Repetition)
                           ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                                                                 DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])
summary(Varcomp_DS2018_PH)

Varcomp_K2018_PH <-lmer(PH ~ 1 +  (1|Genotypes) + (1|Repetition)
                         ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                                                               DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "Control",])
summary(Varcomp_K2018_PH)


Varcomp_DS2018_SNms <-lmer(SNms ~ 1 +  (1|Genotypes) + (1|Repetition)
                         ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                           DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])
summary(Varcomp_DS2018_SNms)

Varcomp_K2018_SNms <-lmer(SNms ~ 1 +  (1|Genotypes) + (1|Repetition)
                        ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                         DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "Control",])
summary(Varcomp_K2018_SNms)



Varcomp_DS2018_SDW <-lmer(SDW ~ 1 +  (1|Genotypes) + (1|Repetition)
                           ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                            DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])
summary(Varcomp_DS2018_SDW)

Varcomp_K2018_SDW <-lmer(SDW ~ 1 +  (1|Genotypes) + (1|Repetition)
                          ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                           DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "Control",])
summary(Varcomp_K2018_SDW)


Varcomp_DS2018_NDF <-lmer(NDF ~ 1 +  (1|Genotypes) + (1|Repetition)
                          ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                                                                DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])
summary(Varcomp_DS2018_NDF)

Varcomp_K2018_NDF <-lmer(NDF ~ 1 +  (1|Genotypes) + (1|Repetition)
                         ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                                                               DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "Control",])
summary(Varcomp_K2018_NDF)


Varcomp_DS2018_HI <-lmer(HI ~ 1 +  (1|Genotypes) + (1|Repetition)
                          ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                                                                DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])
summary(Varcomp_DS2018_HI)

Varcomp_K2018_HI <-lmer(HI ~ 1 +  (1|Genotypes) + (1|Repetition)
                         ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                                                               DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "Control",])
summary(Varcomp_K2018_HI)

############################

Varcomp_DS2018_KNms <-lmer(KNms ~ 1 +  (1|Genotypes) + (1|Repetition)
                         ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                                                               DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "D_stress",])
summary(Varcomp_DS2018_KNms)

Varcomp_K2018_KNms <-lmer(KNms ~ 1 +  (1|Genotypes) + (1|Repetition)
                        ,data = DataRepDsExp1718_YearRL_Abb[DataRepDsExp1718_YearRL_Abb$Year%in%"2018" & 
                                                              DataRepDsExp1718_YearRL_Abb$TreatEnv %in% "Control",])
summary(Varcomp_K2018_KNms)


#####################################
# GY for 2 years

Varcomp_GY_g_row <-lmer(GY_g_row ~ 1 + (1|Year)+ (1|Genotypes) + (1|Repetition)+ (1|TreatEnv)+ (1|TreatEnv:Genotypes)+ (1|Repetition:TreatEnv)
                               ,data = DataRepDsExp1718_YearRL_Abb)
summary(Varcomp_GY_g_row)

# Create New rep variable 

View(DataRepDsExp1718_YearRL_Abb)

DataRepDsExp1718_YearRL_Abb$RepNew <- DataRepDsExp1718_YearRL_Abb$Repetition

# Replace the level R4 and R5 by R1 and R2

DataRepDsExp1718_YearRL_Abb <- DataRepDsExp1718_YearRL_Abb %>%
  mutate(RepNew = recode(RepNew, R4 = "R1", R5 = "R2"))

Varcomp2_GY_g_row <-lmer(GY_g_row ~ 1 + (1|Year)+ (1|Genotypes) + (1|RepNew)+ (1|TreatEnv)+ (1|TreatEnv:Genotypes)+ (1|RepNew:TreatEnv)
                        ,data = DataRepDsExp1718_YearRL_Abb)
summary(Varcomp2_GY_g_row)


# Heritabibility across water regimes

Varcomp18_GY_g_row <-lmer(GY_g_row ~ 1 + (1|Genotypes) + (1|Repetition)+ (1|TreatEnv)+ (1|TreatEnv:Genotypes)+ (1|Repetition:TreatEnv)
                        ,data = subset(DataRepDsExp1718_YearRL_Abb, Year=="2017"))

summary(Varcomp18_GY_g_row) 