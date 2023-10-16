# Resume ANOVA Graphs Breeding Progress for review article 1

# Check the Data

head(DataRepDsExp1718_YearRL_AbbOnlyNew_old,2)

# Change D_stress by Drought

# DataBlue_all18_recode <- DataBlue_all18 %>%
#   mutate(Water = recode(Water, Control = "Rainfed",
#                         Rainout = "Drought"))

DataRepDsExp1718_YearRL_AbbOnlyNew_old <- DataRepDsExp1718_YearRL_AbbOnlyNew_old %>%
  mutate(TreatEnv = recode(TreatEnv, D_stress = "Drought"))

DataRepDsExp1718_YearRL_AbbOnlyNew_old <- DataRepDsExp1718_YearRL_AbbOnlyNew_old %>%
  mutate(Release_Time = recode(Release_Time, Newest = "After 2010", Oldest = "Before 1980"))


head(DataRepDsExp1718_YearRL_AbbOnlyNew_old,2)

windows(5,4)
# For yield related traits
ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= PH,  fill = Release_Time))+
  xlab("Release_Time") + ylab("Plant height (cm)")+
  ggtitle("Plant height of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= GY_g_row,  fill = Release_Time))+
  xlab("Release_Time") + ylab("Grain yield (g/row)")+
  ggtitle("GY of contrasted released time genotypes")+
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= SDW_g_row,  fill = Release_Time))+
  xlab("Release_Time") + ylab("Shoot dry  weight (g/row)")+
  ggtitle("Dry shoot weight of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= PBW_g_row,  fill = Release_Time))+
  xlab("Release_Time") + ylab("Plant biomass weight (g/row)")+
  ggtitle("Biomass weight of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 



ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= TKW,  fill = Release_Time))+
  xlab("Release_Time") +   ylab("TKW (g)")+
  ggtitle("TKW weight of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= SNms,  fill = Release_Time))+
  xlab("Release_Time") + ylab("Spike number per meter square")+
  ggtitle("SNms of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= KNms,  fill = Release_Time))+
  xlab("Release_Time") + ylab("Kernels number per meter square")+
  ggtitle("KNms of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= KNSp,  fill = Release_Time))+
  xlab("Release_Time") + ylab("Kernels number per spike")+
  ggtitle("KNSp of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= HI,  fill = Release_Time))+
  xlab("Release_Time") + ylab("Harvest index")+
  ggtitle("Harvest index contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= GPC,  fill = Release_Time))+
  xlab("Release_Time") + ylab("GPC (%)")+
  ggtitle("Protein content contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= GSC,  fill = Release_Time))+
  xlab("Release_Time") + ylab("GSC (%)")+
  ggtitle("Starch content of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= NDF,  fill = Release_Time))+
  xlab("Release_Time") + ylab("NDF (%)")+
  ggtitle("NDF of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


# Response to reviewer 2 comments

AnovaModel <- lm(GY_g_row ~ Genotypes*RepNew, 
                data= subset (DataRepDsExp1718_YearRL_Abb, TreatEnv =="D_stress"), 
                contrasts=list(Genotypes ="contr.Sum", RepNew ="contr.Sum"))

Anova(AnovaModel)
# Anova Table (Type II tests)
# 
# Response: GY_g_row
# Sum Sq  Df F value    Pr(>F)    
# Genotypes        154050 199  0.2751         1    
# RepNew           186705   2 33.1691 7.979e-14 ***
#   Genotypes:RepNew 183073 363  0.1792         1    
# Residuals        906248 322                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


AnovaModel <- lm( GY ~ Genotypes*RepNew, 
                 data= subset (DataRepDsExp1718_YearRL_Abb, TreatEnv =="Control"), 
                 contrasts=list(Genotypes ="contr.Sum", RepNew ="contr.Sum"))

Anova(AnovaModel)

# Anova Table (Type II tests)
# 
# Response: GY_g_row
# Sum Sq  Df F value    Pr(>F)    
# Genotypes         714050 199  0.9186    0.7478    
# RepNew            227564   2 29.1290 1.759e-12 ***
#   Genotypes:RepNew  581911 394  0.3781    1.0000    
# Residuals        1456990 373               

# Saved Data from R for Supplementary files

write.table(DataRepDsExp1718_YearRL_Abb, "Data/DatafromR/DataRepDsExp1718_YearRL_Abb.txt", row.names=F,
            quote = FALSE, sep = "\t")

# Monday 10th Mai 2021

# Change the KNms into KN and SNms into SN 

DataRepDsExp1718_YearRL_AbbOnlyNew_old$KN <- (DataRepDsExp1718_YearRL_AbbOnlyNew_old$KNms*0.9)/4.76
DataRepDsExp1718_YearRL_AbbOnlyNew_old$SN <- (DataRepDsExp1718_YearRL_AbbOnlyNew_old$SNms*0.9)/4.76


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= SN,  fill = Release_Time))+
  xlab("Release_Time") + ylab("Spikes number per row")+
  ggtitle("SN of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(DataRepDsExp1718_YearRL_AbbOnlyNew_old, aes(x=Release_Time, y= KN,  fill = Release_Time))+
  xlab("Release_Time") + ylab("Kernels number per row")+
  ggtitle("KN of contrasted released time genotypes") +
  scale_fill_manual(values = c("#52854C", "#E7B800"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_wrap(~TreatEnv)+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("After 2010", "Before 1980")),map_signif_level=TRUE,test = "t.test")+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 
