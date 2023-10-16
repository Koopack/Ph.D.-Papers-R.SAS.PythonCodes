# Rscript_Drought_Article3_AllGraph----

# Friday 29th May 2021 # Modify on 26th August 2023 ----

library(ggplot2)
library(ggsignif)
library(data.table)
library(dplyr)
library(tidyverse)

# Rerun analysis for drought article 3 (N dynamic)
windows(5,5)
ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= Spad,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SPAD") + ggtitle("SPAD value") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
 # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=12)) +
  theme(axis.text.y = element_text (color="black", size=12))  # Now Good


# change the nitrogen levels name in the Data
str(DataBlue_all18_recode)
DataBlue_all18_recode$Nitrogen <- as.factor(DataBlue_all18_recode$Nitrogen)
DataBlue_all18_recode$Genotypes <- as.factor(DataBlue_all18_recode$Genotypes)

DataBlue_all18_recode <- DataBlue_all18 %>%
  mutate(Water = recode(Water, Control = "Rainfed",
                        Rainout = "Drought"),
        Nitrogen = recode(Nitrogen, Voll = "HN",
                          Null = "MN"))
head(DataBlue_all18_recode)
colnames(DataBlue_all18_recode)[9] <- "NUE_Bio"
colnames(DataBlue_all18_recode)[10] <- "NUE_Grain"

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= NUE_Grain,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("NUE_Grain") + ggtitle ("NUE for grain production") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c()) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
 # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=12)) +
  theme(axis.text.y = element_text (color="black", size=12))


ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= NHI,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("NHI") + ggtitle ("Nitrogen harvested index") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c()) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  #theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=12)) +
  theme(axis.text.y = element_text (color="black", size=12))

head(merge_RY_lab_Blue_recode_new,2)

# let us change Rainfed_Voll... into Rainfed_HN

merge_RY_lab_Blue_recode_new <- merge_RY_lab_Blue_recode_new %>%
  mutate(Water_N = recode(Water_N, Rainfed_Voll = "Rainfed_HN",
                                   Rainfed_Null = "Rainfed_MN",
                                   Drought_Voll = "Drought_HN",
                                   Drought_Null = "Drought_MN"))

p2017 = ggplot(merge_RY_lab_Blue_recode_new) +
  aes(x=Water_N, y = NUE_Grain, fill = factor(Water_N, 
                                              levels = c("Rainfed_HN", "Rainfed_MN","Drought_HN","Drought_MN"))) +
  stat_boxplot(geom ='errorbar') + expand_limits(y = c())+
  theme_bw()+geom_boxplot(outlier.shape = NA) + 
  scale_fill_manual("Water_N",values=c("chartreuse","forestgreen","brown3", "#619CFF" ))+
  geom_text(x = 1, y = 175, label = "c",colour = "black")+
  geom_text(x = 2, y = 175, label = "d",colour = "black")+
  geom_text(x = 3, y = 175, label = "a",colour = "black")+
  geom_text(x = 4, y = 175, label = "b",colour = "black")+
  ggtitle("NUE for GY production [kg/kg N]")+ theme(axis.title.x = element_text(size=10))+
  theme(legend.position = "none")+
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(plot.title = element_text(color="black", size=10,face="bold", hjust = 0.5))+ 
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=12)) +
  theme(axis.text.y = element_text (color="black", size=12))# +
 # theme(plot.background = element_rect(fill = "gray80"))

# Saturday 26th August 2023 ----

# Transform table 2 and 4 into boxplot ----

# for ndyncorrected1718forbluesfin_18 

View(ndyncorrected1718forbluesfin_18)
head(ndyncorrected1718forbluesfin_18)
colnames(ndyncorrected1718forbluesfin_18)

View(DataBlue_all18_recode)
head(DataBlue_all18_recode)
colnames(DataBlue_all18_recode)

windows(4,4)
ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= Biomasse,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Biomass (g/row)") + # ggtitle("Biomass weight") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y = Seedyield,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Seedyield (g/row)") + # ggtitle("Biomass weight") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

# ggplot(subset(ndyncorrected1718forbluesfin_18, Year == "2018") , aes(x = TreatN , y = Seedyield_Kg_ha,  fill = TreatN))+
#   xlab("Nitrogen") + ylab("Seedyield (Kg/ha)") +
#   scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
#   geom_boxplot(outlier.shape = NA) + facet_wrap (TreatEnv~.) + expand_limits(y = c(40, 60)) +
#   theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
#   geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
#   theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
#   theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
#   # theme(plot.background = element_rect(fill = "gray80")) +
#   theme(legend.position = "none") +
#   theme(axis.text.x = element_text (color="black", size=10)) +
#   theme(axis.text.y = element_text (color="black", size=10))

# Let us recode the names of the treatments 

ndyncorrected1718forbluesfin_18_recode <- ndyncorrected1718forbluesfin_18 %>%
  mutate(TreatEnv = recode(TreatEnv, Control = "Rainfed",
                        D_stress = "Drought"),
         TreatN = recode(TreatN, Voll = "HN",
                           Null = "MN"))

ndyncorrected1718forbluesfin_18_recode$TreatN <- factor(
  ndyncorrected1718forbluesfin_18_recode$TreatN, 
  levels = c("HN", "MN")  # Specify the desired order
)


# 
# ggplot(subset(ndyncorrected1718forbluesfin_18, Year == "2018") , aes(x = TreatN , y = ShootDryMater_Kg_ha,  fill = TreatN))+
#   xlab("Nitrogen") + ylab("SDW (g/row)") +
#   scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
#   geom_boxplot(outlier.shape = NA) + facet_wrap (TreatEnv~.) + expand_limits(y = c(40, 60)) +
#   theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
#   geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
#   theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
#   theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
#   # theme(plot.background = element_rect(fill = "gray80")) +
#   theme(legend.position = "none") +
#   theme(axis.text.x = element_text (color="black", size=10)) +
#   theme(axis.text.y = element_text (color="black", size=10))


# Working methodical will save you!!!

# So for 2018 SDW, generate the Data from Biomass and Seedyield (those are in g/row)

head(DataBlue_all18_recode)

DataBlue_all18_recode$SDW <- DataBlue_all18_recode$Biomasse - DataBlue_all18_recode$Seedyield

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y = SDW,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SDW (g/row)") + # ggtitle("Biomass weight") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y = NUE_Bio,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("NUE_Bio") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y = NHI,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("NHI") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))


ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y = YII,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Effective quantum yield of PSII (YII)") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))



# Now let´s run all the other NUe related traits with data table
# ndyncorrected1718forbluesfin_18

head(ndyncorrected1718forbluesfin_18)
tail(ndyncorrected1718forbluesfin_18)
dim(ndyncorrected1718forbluesfin_18)

ggplot(ndyncorrected1718forbluesfin_18_recode, aes(x = TreatN, y = NUpE,  fill = TreatN))+
  xlab("Nitrogen") + ylab("NUpE") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (TreatEnv~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(ndyncorrected1718forbluesfin_18_recode, aes(x = TreatN, y = NRE,  fill = TreatN))+
  xlab("Nitrogen") + ylab("NRE") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (TreatEnv~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(ndyncorrected1718forbluesfin_18_recode, aes(x = TreatN, y = NUtE,  fill = TreatN))+
  xlab("Nitrogen") + ylab("NUtE") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (TreatEnv~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))


ggplot(ndyncorrected1718forbluesfin_18_recode, aes(x = TreatN, y = NABHarvest,  fill = TreatN))+
  xlab("Nitrogen") + ylab("NAB") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (TreatEnv~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(ndyncorrected1718forbluesfin_18_recode, aes(x = TreatN, y = LeafN_Content,  fill = TreatN))+
  xlab("Nitrogen") + ylab("Leaf N content (NLf)") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (TreatEnv~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(ndyncorrected1718forbluesfin_18_recode, aes(x = TreatN, y = GrainN_Content,  fill = TreatN))+
  xlab("Nitrogen") + ylab("Grain N content (NGr)") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (TreatEnv~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(ndyncorrected1718forbluesfin_18_recode, aes(x = TreatN, y = StrohN_Content,  fill = TreatN))+
  xlab("Nitrogen") + ylab("Shoot N content (NSt)") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (TreatEnv~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))


# other way around
ggplot(ndyncorrected1718forbluesfin_18_recode, aes(x = TreatEnv, y = NUtE,  fill = TreatEnv))+
  xlab("Water regimes") + ylab("NUtE") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (TreatN~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Rainfed", "Drought")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(ndyncorrected1718forbluesfin_18_recode, aes(x = TreatEnv, y = NUtE,  fill = TreatEnv))+
  xlab("Water regimes") + ylab("NUtE") +
  scale_fill_manual(values = c("cyan1", "coral3")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (TreatN~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Rainfed", "Drought")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(DataBlue_all18_recode, aes(x = Water, y = YII,  fill = Water))+
  xlab("Water regimes") + ylab("Effective quantum yield of PSII (YII)") +
  scale_fill_manual(values = c("cyan1", "coral3")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Nitrogen~.) + expand_limits(y = c(0.40, 0.84)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Rainfed", "Drought")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

# Boxplot of the table 4 traits (Pot experiment)

#YII3_SPAD3_NDVI3_Agro

# Let us recode the variable levels
YII3_SPAD3_NDVI3_Agro[, c("Water", "Nitrogen")] <- lapply (YII3_SPAD3_NDVI3_Agro[,
          c("Water","Nitrogen")], factor )
str(YII3_SPAD3_NDVI3_Agro)
levels(YII3_SPAD3_NDVI3_Agro$Water)
levels(YII3_SPAD3_NDVI3_Agro$Nitrogen)
head(YII3_SPAD3_NDVI3_Agro)
tail(YII3_SPAD3_NDVI3_Agro)

YII3_SPAD3_NDVI3_Agro_recode <- YII3_SPAD3_NDVI3_Agro %>%
  mutate(Water = recode(Water, 
                        Kontrolle = "Control",
                        stress = "Drought"),
         Nitrogen  = recode(Nitrogen , 
                           voll = "HN",
                         reduziert = "MN"))
dim(YII3_SPAD3_NDVI3_Agro_recode)

# Remove the row without Data
# subset the factor level that is not NA

YII3_SPAD3_NDVI3_Agro_recode <- YII3_SPAD3_NDVI3_Agro_recode %>%
  filter(!is.na(Nitrogen))

YII3_SPAD3_NDVI3_Agro_recode$Nitrogen <- factor(
  YII3_SPAD3_NDVI3_Agro_recode$Nitrogen, 
  levels = c("HN", "MN")  # Specify the desired order
)

windows(4,4)
ggplot(YII3_SPAD3_NDVI3_Agro_recode, aes(x = Nitrogen, y = SPAD3,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SPAD value") +
  scale_fill_manual(values = c("forestgreen","chartreuse")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))


ggplot(YII3_SPAD3_NDVI3_Agro_recode, aes(x = Nitrogen, y = YII3,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Effective quantum yield of PSII (YII)") +
  scale_fill_manual(values = c("forestgreen","chartreuse")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))


ggplot(YII3_SPAD3_NDVI3_Agro_recode, aes(x = Nitrogen, y = FSW,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Fresh shoot weight (FSW)") +
  scale_fill_manual(values = c("forestgreen","chartreuse")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(YII3_SPAD3_NDVI3_Agro_recode, aes(x = Nitrogen, y = FSW,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Fresh shoot weight (FSW in g)") +
  scale_fill_manual(values = c("forestgreen","chartreuse")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

colnames(YII3_SPAD3_NDVI3_Agro_recode)[colnames(YII3_SPAD3_NDVI3_Agro_recode)== "SDW"] <- "DSW"

ggplot(YII3_SPAD3_NDVI3_Agro_recode, aes(x = Nitrogen, y = DSW,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Dry shoot weight (DSW in g)") +
  scale_fill_manual(values = c("forestgreen","chartreuse")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))


ggplot(YII3_SPAD3_NDVI3_Agro_recode, aes(x = Nitrogen, y = SWaP,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Shoot water potential (g)") +
  scale_fill_manual(values = c("forestgreen","chartreuse")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(YII3_SPAD3_NDVI3_Agro_recode, aes(x = Nitrogen, y = NLf,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Leaf N content (NLf)") +
  scale_fill_manual(values = c("forestgreen","chartreuse")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(YII3_SPAD3_NDVI3_Agro_recode, aes(x = Nitrogen, y = RA,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Root angle (°)") +
  scale_fill_manual(values = c("forestgreen","chartreuse")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))


# Take the root speccific data for root traits 

# ShootRoot

head(ShootRoot,5)
# # A tibble: 5 × 10
# Nitrogen Env            BRISONr FRW_g FSW_g DSW_g SWaP__g   NLf   DRW  RWaP
# <fct>    <fct>          <fct>   <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>
#   1 voll     Kontrolle_Voll 84      111.   230.  91.8   139.   3.35  21.9  88.7
# 2 voll     Kontrolle_Voll 98      146.   205.  82.3   123.   3.04  32.6 114. 
# 3 voll     Kontrolle_Voll 206      95.2  151.  65.6    85.6  3.00  19.5  75.6
# 4 voll     Kontrolle_Voll 172      69.8  192.  72.7   119.   2.78  13.1  56.

ShootRoot <-separate(
  ShootRoot,
  Env,
  c("Water","Nitrogen"),
  "_"
  )
head(ShootRoot,5)
tail(ShootRoot)
levels(ShootRoot$Water)
str(ShootRoot)

ShootRoot$Env <- paste0(ShootRoot$Water,"_",ShootRoot$Nitrogen)


ShootRoot[,c("Water", "Nitrogen", "Env")] <- lapply(ShootRoot[,c("Water", "Nitrogen","Env")], 
                                             factor)

ShootRoot_recode <- ShootRoot %>%
  mutate(Water = recode(Water, 
                        Kontrolle = "Control",
                        Stress = "Drought"),
         Nitrogen  = recode(Nitrogen , 
                            Voll = "HN",
                            Reduziert = "MN"))
dim(ShootRoot_recode)
head(ShootRoot_recode)

ggplot(ShootRoot_recode, aes(x = Nitrogen, y = FRW_g ,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Fresh root weight (g)") +
  scale_fill_manual(values = c("forestgreen","chartreuse")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("HN", "MN")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

# Th Trait only under Voll

ggplot(subset(YII3_SPAD3_NDVI3_Agro_recode, Nitrogen == "HN"), aes(x = Water, y = NDVI3,  fill = Water))+
  xlab("Water regimes") + ylab("NDVI value") +
  scale_fill_manual(values = c("cyan1", "coral3")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + #facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Control", "Drought")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(subset(ShootRoot_recode, Nitrogen == "HN"), aes(x = Water, y = DRW,  fill = Water))+
  xlab("Water regimes") + ylab("Dry root weight (g)") +
  scale_fill_manual(values = c("cyan1", "coral3")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + #facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Control", "Drought")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))


ggplot(subset(ShootRoot_recode, Nitrogen == "HN"), aes(x = Water, y = RWaP,  fill = Water))+
  xlab("Water regimes") + ylab("Root water potential (g)") +
  scale_fill_manual(values = c("cyan1", "coral3")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + #facet_wrap (Water ~.) + #expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Control", "Drought")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=10,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=10,face="bold")) +
  # theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))