# Anova GS duration

DataFinal_GS_Duration <- read.delim("Data/DataFinal_GS_Duration.txt")

str(DataFinal_GS_Duration)

DataFinal_GS_Duration[, c("Sowing_date", "Year", "Meas_session", "Date", "Messdate","TreatEnv", "SortenNr", "Growth_Stage")] <- 
  lapply(DataFinal_GS_Duration[, c("Sowing_date", "Year", "Meas_session", "Date", "Messdate", "TreatEnv", "SortenNr", "Growth_Stage")], factor)


Data_IntAll[, c("Trait", "Chr", "M_code_peak", "Treatment", "Allele")] <- 
  lapply (Data_IntAll[, c("Trait", "Chr", "M_code_peak", "Treatment", "Allele")], factor )

head(DataFinal_GS_Duration,2)

AnovaModel.1 <- lm(DayToTheGrowthSatge ~ Growth_Stage*TreatEnv, 
       data = subset (DataFinal_GS_Duration, Year =="2017"), contrasts=list(Growth_Stage ="contr.Sum", 
                                                                        TreatEnv ="contr.Sum"))

Anova(AnovaModel.1)
#Anova Table (Type II tests)

# Response: DayToTheGrowthSatge
# Sum Sq   Df F value    Pr(>F)    
# Growth_Stage          157079    4 3718.40 < 2.2e-16 ***
#   TreatEnv               66739    1 6319.45 < 2.2e-16 ***
#   Growth_Stage:TreatEnv   3196    2  151.31 < 2.2e-16 ***
#   Residuals              34344 3252                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

AnovaModel.1 <- lm(DayToTheGrowthSatge ~ Growth_Stage*TreatEnv, 
                   data = subset (DataFinal_GS_Duration, Year =="2018"), contrasts=list(Growth_Stage ="contr.Sum", 
                                                                                        TreatEnv ="contr.Sum"))
Anova(AnovaModel.1)
