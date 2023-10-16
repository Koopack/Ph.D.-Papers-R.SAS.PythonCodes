# MOnady 21 December 2020 -----

# Slopes comparisons for drought article 2 -----


# Comparison of Regression coefficients of preeding progress under the two water regimes
# How to interpret the slopes
#https://blog.minitab.com/blog/adventures-in-statistics-2/how-to-compare-regression-lines-between-different-models

head(SPADData17GrowthStageRYMeanKnownRelYear,5)

Reg_SPAD_Booting <- lm (SPAD_Booting ~ Jahr.der.Zulass.*Treatment, data = SPADData17GrowthStageRYMeanKnownRelYear)
summary(Reg_SPAD_Booting)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             2077.4267   422.3513   4.919 1.30e-06 ***
#   Year_Rel                  -0.7975     0.2116  -3.768 0.000190 ***
#   WaterD_stress          -2405.4437   597.2950  -4.027 6.82e-05 *** 
# the diference between the two constant is siignificant
#   Year_Rel:WaterD_stress     1.0514     0.2993   3.513 0.000497 *** 
# The difference between the two slopes is significant
#   ---

Reg_SPAD_Heading <- lm (SPAD_Heading ~ Jahr.der.Zulass.*Treatment, data = SPADData17GrowthStageRYMeanKnownRelYear)
summary(Reg_SPAD_Heading)

Reg_SPAD_Anthesis <- lm (SPAD_Anthesis ~ Jahr.der.Zulass.*Treatment, data = SPADData17GrowthStageRYMeanKnownRelYear)
summary(Reg_SPAD_Anthesis) # No intesting results

# For mini Pam 

head(PamDynamic2018MeanKnownRelYear,5)

Reg_YII_Booting <- lm (YII_Booting ~ Jahr.der.Zulass.*Treatment, data = PamDynamic2018MeanKnownRelYear)
summary(Reg_YII_Booting)

Reg_YII_Heading <- lm (YII_Heading ~ Jahr.der.Zulass.*Treatment, data = PamDynamic2018MeanKnownRelYear)
summary(Reg_YII_Heading)

Reg_YII_Anthesis <- lm (YII_Anthesis ~ Jahr.der.Zulass.*Treatment, data = PamDynamic2018MeanKnownRelYear)
summary(Reg_YII_Anthesis)  # No intesting results

# Regression A and LSCl done in exell
head(Chl_SC_Trs_Phot,5)

Reg_A_LSCl <- lm (A ~ LSCl*TreatEnv, data = Chl_SC_Trs_Phot)
summary(Reg_A_LSCl)

Reg_A_E <- lm (A ~ E*TreatEnv, data = Chl_SC_Trs_Phot)
summary(Reg_A_E)



