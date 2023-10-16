
library(mvtnorm, pos=17)
library(survival, pos=17)
library(MASS, pos=17)
library(TH.data, pos=17)
library(multcomp, pos=17)
library(abind, pos=22)
AnovaModel.1 <- aov(ShootDryMater_Kg_ha ~ Water_N, 
  data=ndyncorrected1718forbluesfin_18)
summary(AnovaModel.1)
with(ndyncorrected1718forbluesfin_18, numSummary(ShootDryMater_Kg_ha, 
  groups=Water_N, statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.1, linfct = mcp(Water_N = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.2 <- aov(NUpE ~ Water_N, data=ndyncorrected1718forbluesfin_18)
summary(AnovaModel.2)
with(ndyncorrected1718forbluesfin_18, numSummary(NUpE, groups=Water_N, 
  statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.2, linfct = mcp(Water_N = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.3 <- aov(NUtE ~ Water_N, data=ndyncorrected1718forbluesfin_18)
summary(AnovaModel.3)
with(ndyncorrected1718forbluesfin_18, numSummary(NUtE, groups=Water_N, 
  statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.3, linfct = mcp(Water_N = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.4 <- aov(NABHarvest ~ Water_N, 
  data=ndyncorrected1718forbluesfin_18)
summary(AnovaModel.4)
with(ndyncorrected1718forbluesfin_18, numSummary(NABHarvest, groups=Water_N,
   statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.4, linfct = mcp(Water_N = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.5 <- aov(LeafN_Content ~ Water_N, 
  data=ndyncorrected1718forbluesfin_18)
summary(AnovaModel.5)
with(ndyncorrected1718forbluesfin_18, numSummary(LeafN_Content, 
  groups=Water_N, statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.5, linfct = mcp(Water_N = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.6 <- aov(NRE ~ Water_N, data=ndyncorrected1718forbluesfin_18)
summary(AnovaModel.6)
with(ndyncorrected1718forbluesfin_18, numSummary(NRE, groups=Water_N, 
  statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.6, linfct = mcp(Water_N = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.7 <- aov(SPAD3 ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.7)
with(YII3_SPAD3_NDVI3_Agro, numSummary(SPAD3, groups=Env, 
  statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.7, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.8 <- aov(YII3 ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.8)
with(YII3_SPAD3_NDVI3_Agro, numSummary(YII3, groups=Env, 
  statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.8, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.9 <- aov(NDVI3 ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.9)
with(YII3_SPAD3_NDVI3_Agro, numSummary(NDVI3, groups=Env, 
  statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.9, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.10 <- aov(FSW ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.10)
with(YII3_SPAD3_NDVI3_Agro, numSummary(FSW, groups=Env, statistics=c("mean",
   "sd")))
local({
  .Pairs <- glht(AnovaModel.10, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.11 <- aov(SDW ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.11)
with(YII3_SPAD3_NDVI3_Agro, numSummary(SDW, groups=Env, statistics=c("mean",
   "sd")))
local({
  .Pairs <- glht(AnovaModel.11, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.12 <- aov(SWaP ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.12)
with(YII3_SPAD3_NDVI3_Agro, numSummary(SWaP, groups=Env, 
  statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.12, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.13 <- aov(NLf ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.13)
with(YII3_SPAD3_NDVI3_Agro, numSummary(NLf, groups=Env, statistics=c("mean",
   "sd")))
local({
  .Pairs <- glht(AnovaModel.13, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.14 <- aov(RA ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.14)
with(YII3_SPAD3_NDVI3_Agro, numSummary(RA, groups=Env, statistics=c("mean", 
  "sd")))
local({
  .Pairs <- glht(AnovaModel.14, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.15 <- aov(FRW ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.15)
with(YII3_SPAD3_NDVI3_Agro, numSummary(FRW, groups=Env, statistics=c("mean",
   "sd")))
local({
  .Pairs <- glht(AnovaModel.15, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.16 <- aov(RDW ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.16)
with(YII3_SPAD3_NDVI3_Agro, numSummary(RDW, groups=Env, statistics=c("mean",
   "sd")))
local({
  .Pairs <- glht(AnovaModel.16, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
AnovaModel.17 <- aov(RWaP ~ Env, data=YII3_SPAD3_NDVI3_Agro)
summary(AnovaModel.17)
with(YII3_SPAD3_NDVI3_Agro, numSummary(RWaP, groups=Env, 
  statistics=c("mean", "sd")))
local({
  .Pairs <- glht(AnovaModel.17, linfct = mcp(Env = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})

