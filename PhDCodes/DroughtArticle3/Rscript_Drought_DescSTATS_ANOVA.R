# ANOVA and Descriptive stats for Drought Article 3 ----

# Read the data 
library(car)
library(haven)
library(lme4)
install.packages("summarytools")
library(summarytools)
install.packages("pastecs")
library(pastecs)
library(readxl)

ndyncorrected1718forbluesfin <- read_sas("Data/ndyncorrected1718forbluesfin.sas7bdat")

head(ndyncorrected1718forbluesfin)
tail(ndyncorrected1718forbluesfin)
ndyncorrected1718forbluesfin <- as.data.frame(ndyncorrected1718forbluesfin)
dim(ndyncorrected1718forbluesfin)
str(ndyncorrected1718forbluesfin)

ndyncorrected1718forbluesfin$Genotypes <- as.factor(ndyncorrected1718forbluesfin$Genotypes)
ndyncorrected1718forbluesfin$Year <- as.factor(ndyncorrected1718forbluesfin$Year)
ndyncorrected1718forbluesfin$TreatEnv <- as.factor(ndyncorrected1718forbluesfin$TreatEnv)
ndyncorrected1718forbluesfin$TreatN <- as.factor(ndyncorrected1718forbluesfin$TreatN)
ndyncorrected1718forbluesfin$Repetition <- as.factor(ndyncorrected1718forbluesfin$Repetition)

ndyncorrected1718forbluesfin_17 <- subset(ndyncorrected1718forbluesfin, Year=="2017")
dim(ndyncorrected1718forbluesfin_17)
levels(ndyncorrected1718forbluesfin_17$Year)
View(ndyncorrected1718forbluesfin_17)

ndyncorrected1718forbluesfin_18 <- subset(ndyncorrected1718forbluesfin, Year=="2018")
dim(ndyncorrected1718forbluesfin_18)
levels(ndyncorrected1718forbluesfin_18$Year)
View(ndyncorrected1718forbluesfin_18)

AnovaModel18 <- lm(NUpE ~ Genotypes*TreatEnv*TreatN,
                   data=ndyncorrected1718forbluesfin_18, contrasts=list(Genotypes ="contr.Sum",
                                                                        TreatEnv ="contr.Sum", TreatN ="contr.Sum"))
Anova(AnovaModel18)

head(ndyncorrected1718forbluesfin_18,2)
tail(ndyncorrected1718forbluesfin_18,2)

str(ndyncorrected1718forbluesfin_18$ShootDryMater_Kg_ha)

AnovaModel17 <- lm(NABHarvest ~ Genotypes*TreatEnv,
                   data=ndyncorrected1718forbluesfin_17, contrasts=list(Genotypes ="contr.Sum",
                                                                        TreatEnv ="contr.Sum"))
Anova(AnovaModel17)

head(ndyncorrected1718forbluesfin_17,2)


AnovaModel_blue <- lm(Seedyield ~ Water,
                      data= subset( DataBlue_all18_recode, Nitrogen =="Null"), contrasts=list(
                        Water ="contr.Sum"))
Anova(AnovaModel_blue)

head(ndyncorrected1718forbluesfin_17,2)

# Load the Blues Data
remove(Daten_20172018_For_GWAS)


Kont17 <- read_excel("Data/Daten 20172018 For GWAS.xlsx", 
                     sheet = "2017 Kontrolle")
head(Kont17)

DS17 <- read_excel("Data/Daten 20172018 For GWAS.xlsx", 
                   sheet = "2017_Rainout")

head(DS17)
colnames(DS17)==colnames(Kont17)


DataBlue17 <- rbind.data.frame(Kont17,DS17)
View(DataBlue17)
head(DataBlue17)

Kont18_Voll <- read_excel("Data/Daten 20172018 For GWAS.xlsx", 
                          sheet = "2018_Kontrolle_Voll")

DS18_Voll <- read_excel("Data/Daten 20172018 For GWAS.xlsx", 
                        sheet = "2018 Rainout Voll")

colnames(Kont18_Voll)==colnames(DS18_Voll)

colnames(DS17[,c(2:15)])==colnames(DS18_Voll[,c(3:16)])
DataBlue_Voll18 <- rbind.data.frame(Kont18_Voll,DS18_Voll)

head(DataBlue_Voll18,5)

# So you can Rbind all the voll data

# Include Rep1 in the data of 2017 and Rep2 in the data of 2018

DataBlue17$Repetition <- "Rep1"
head(DataBlue17)
tail(DataBlue17)

DataBlue_Voll18$Repetition <- "Rep2"
head(DataBlue_Voll18)
tail(DataBlue_Voll18)

DataBlue_Voll18 <- as.data.frame(DataBlue_Voll18)
head(DataBlue_Voll18)
tail(DataBlue_Voll18)

DataBlue18 <- DataBlue_Voll18[, -2]


head(DataBlue18) # Check also for head(DataBlue17)# so biomasse in 2017
                 # is smaller than Seedyield 
tail(DataBlue18)

colnames(DataBlue17)==colnames(DataBlue18)

DataBlue1718 <- rbind(DataBlue17,DataBlue18)

# Prepare the nULL DATA.

Kont18_Null <- read_excel("Data/Daten 20172018 For GWAS.xlsx", 
                          sheet = "2018 Kontrolle Null")
Kont18_Null <- as.data.frame(Kont18_Null)
head(Kont18_Null)
tail(Kont18_Null)

DS18_Null <- read_excel("Data/Daten 20172018 For GWAS.xlsx", 
                        sheet = "2018 Rainout Null")
DS18_Null <- as.data.frame(DS18_Null)
head(DS18_Null)
tail(DS18_Null)

colnames(Kont18_Null)==colnames(DS18_Null)

Data_Null18 <- rbind(Kont18_Null,DS18_Null)
head(Data_Null18)
tail(Data_Null18) # Use this Data for boxplot including TreatEnv and TreattN

DataBlue_all18 <- rbind(DataBlue_Voll18,Data_Null18)

head(DataBlue_Voll18)
dim(DataBlue_Voll18)

DataBlue_all18 <- rbind(DataBlue_Voll18[,-17],Data_Null18)

# Boxplot in 2018 voll nulll  and both water treatments

head(DataBlue_all18)

# ggplot(BriwecsAverage3Yearsall_GYState, aes(x=GY_Status, y= Spad,  fill = GY_Status))+
#   xlab("GY_Status") + ylab("SPAD")+ggtitle("SPAD of contrasted GY status") +
#   scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+ stat_boxplot(geom ='errorbar')+
#   geom_boxplot(outlier.shape = NA)+facet_grid(Treatment~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
#   geom_signif(comparisons = list(c("High Yielding", "Low Yielding")), map_signif_level=TRUE)


ggplot(DataBlue_all18, aes(x = Nitrogen, y= Spad,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SPAD") + ggtitle("SPAD value") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_grid(Water~.)+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) # Very Good, Now make it compelling


# #aggregate(cbind(PDW,PH,SD,Gday,Hday,Yday, Spad)~BRISO_Nr+Treatment,data=Datall1_151617,FUN= mean)
# 
# # This one compute means with two levels"Genotype" and treatment
# 
# #Then change 3a into 31
# levels(scoreYIID17cop3A$Chr)[scoreYIID17cop3A$Chr=="3A"] <- "31"
# head(scoreYIID17cop3A,3)
# 
# # Then change the names of the column
# 
# setnames(scoreYIID17cop3A, old=c("Marker","Chr", "Pos", "YII"), new=c("SNP", "CHR","BP", "P"),skip_absent=TRUE)
# head(scoreYIID17cop3A,3)## per
# 
# Rename by index in levels list: change third item, "gamma", to "three".
# levels(x)[3] <- "three"
# 
# Rename by index in names vector: change third item, "gamma", to "three"
# names(d)[3] <- "three"
# 

# Chagne the levels of water regimes

levels(DataBlue_all18[1:400,]$Water)[1] <- "Rainfed"
str(DataBlue_all18)
dim(DataBlue_all18)
head(DataBlue_all18)
tail(DataBlue_all18)

# Change the character into factor 
DataBlue_all18$Water <- as.factor(DataBlue_all18$Water)


levels(DataBlue_all18$Water)[levels(DataBlue_all18$water)=="Rainout"] <- "Drought"

# mutate(dat, x = fct_recode(x, "B" = "A"))
mutate(DataBlue_all18, Water = fct_recode(Water, "Rainout" = "Drought"))

# df <- iris %>%
#   mutate(Species = recode(Species, setosa = "SETOSA",
#                           versicolor = "VERSICOLOR",
#                           virginica = "VIRGINICA"
#   )
#   )

DataBlue_all18_recode <- DataBlue_all18 %>%
  mutate(Water = recode(Water, Control = "Rainfed",
                        Rainout = "Drought"))

head(DataBlue_all18_recode)
tail(DataBlue_all18_recode)

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= Spad,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SPAD") + ggtitle("SPAD value") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_grid(Water~.)+ expand_limits(y = c(40, 60))+
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) # Very Good, Now make it compelling

min(DataBlue_all18_recode$Spad)
#28

#replaces the negetive values with NA's
data1<-replace(df$entry2,df$entry2<0,NA)
data1

DataBlue_all18_recode$Spad <- replace(DataBlue_all18_recode$Spad, DataBlue_all18_recode$Spad <30, NA)

min(DataBlue_all18_recode$Spad, na.rm = TRUE)

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= Spad,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SPAD") + ggtitle("SPAD value") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_grid(Water~.)+ expand_limits(y = c(40, 60))+
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) # Very Good, Now make it compelling

# Now make the digits in bigger size

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= Spad,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SPAD") + ggtitle("SPAD value") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+ stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA)+facet_grid(Water~.)+ expand_limits(y = c(40, 60))+
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5))+
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold"))+
  theme(axis.title.y =element_text (color="black", size=12,face="bold"))

# Remove the legend and add gray color in the back ground

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= Spad,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SPAD") + ggtitle("SPAD value") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA)+facet_grid(Water~.) + expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none")

# increas the x-axis and y-axis size 
ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= Spad,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SPAD") + ggtitle("SPAD value") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_grid(Water~.) + expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  # Now Good

# For SPAD
ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= Spad,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SPAD") + ggtitle("SPAD value") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c(40, 60)) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  # Now Good

# For YII not significant # Do for NUEBio and NUE

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= YII,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("SPAD") + ggtitle("SPAD value") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c()) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

head(DataBlue_all18_recode)
colnames(DataBlue_all18_recode)[9] <- "NUE_Bio"
colnames(DataBlue_all18_recode)[10] <- "NUE_Grain"

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= NHI,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("NHI") + ggtitle ("Nitrogen harvested index") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c()) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= NUE_Grain,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("NUE_Grain") + ggtitle ("NUE for grain production") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c()) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))

# For Grain yield
ggplot(DataBlue_all18_recode, aes(x = Nitrogen, y= Seedyield,  fill = Nitrogen))+
  xlab("Nitrogen") + ylab("Grain yield") + ggtitle ("Grain yield ") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07")) + stat_boxplot(geom ='errorbar') +
  geom_boxplot(outlier.shape = NA) + facet_wrap (Water~.) + expand_limits(y = c()) +
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5)) +
  geom_signif(comparisons = list(c("Voll", "Null")), map_signif_level=TRUE) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(plot.background = element_rect(fill = "gray80")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))


# Add letter to boxplots for NUE grain that showed interesting result in Katrin Thesis


# p2017 = ggplot(Subdata_2017) +
#   aes(x=Treatment, y=Gr_N_Yld, fill = factor(Treatment, levels = c("LN_NF", "HN_NF", "HN_WF"))) +
#   stat_boxplot(geom ='errorbar')+expand_limits(y = c(0, 4))+ 
#   theme_bw()+geom_boxplot(outlier.shape = NA)+scale_fill_manual("Treatment",values=c("#619CFF","forestgreen","brown3" ))+
#   geom_text(x = 1, y = 3.8, label = "b",colour = "black")+
#   geom_text(x = 2, y = 3.8, label = "a",colour = "black")+
#   geom_text(x = 3, y = 3.8, label = "c",colour = "black")+
#   ggtitle("Harvested N (dt/ha) in 2017")+ theme(axis.title.x = element_text(size=10))+ 
#   theme(legend.position = "none")+
#   theme(axis.title.y=element_blank())+theme(plot.title = element_text(color="black", size=10,face="bold", hjust = 0.5))
# 

head(merge_RY_lab_Blue_recode)
colnames(merge_RY_lab_Blue_recode) [13] <- "NUE_Bio"
colnames(merge_RY_lab_Blue_recode) [14] <- "NUE_Grain"

p2017 = ggplot(merge_RY_lab_Blue_recode) +
  aes(x=TreatEnV_N, y = NUE_Grain, fill = factor(TreatEnV_N, 
                                                 levels = c("Rainfed_Voll", "Rainfed_Null","Drought_Voll","Drought_Null"))) +
  stat_boxplot(geom ='errorbar') + expand_limits(y = c())+
  theme_bw()+geom_boxplot(outlier.shape = NA) + scale_fill_manual("TreatEnV_N",values=c("#619CFF","forestgreen","brown3", "yellow" ))+
  #geom_text(x = 1, y = 3.8, label = "b",colour = "black")+
  #geom_text(x = 2, y = 3.8, label = "a",colour = "black")+
  #geom_text(x = 3, y = 3.8, label = "c",colour = "black")+
  ggtitle("NUE for GY production [kg/kg N]")+ theme(axis.title.x = element_text(size=10))+
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank())+
  theme(plot.title = element_text(color="black", size=10,face="bold", hjust = 0.5))

# reorder 

data_new <- data                             # Duplicate data
data_new$group <- factor(data_new$group,     # Reorder factor levels
                         c("C", "B", "D", "A"))

merge_RY_lab_Blue_recode_new <- merge_RY_lab_Blue_recode                            # Duplicate data
merge_RY_lab_Blue_recode_new$TreatEnV_N <- factor(merge_RY_lab_Blue_recode_new$TreatEnV_N,     # Reorder factor levels
                                                  c("Rainfed_Voll", "Rainfed_Null", "Drought_Voll","Drought_Null"))

head(merge_RY_lab_Blue_recode_new)

ggplot(merge_RY_lab_Blue_recode_new) +
  aes(x=TreatEnV_N, y = NUE_Grain, fill = factor(TreatEnV_N, 
                                                 levels = c("Rainfed_Voll", "Rainfed_Null","Drought_Voll","Drought_Null"))) +
  stat_boxplot(geom ='errorbar') + expand_limits(y = c())+
  theme_bw()+geom_boxplot(outlier.shape = NA) + scale_fill_manual("TreatEnV_N",values=c("#619CFF","forestgreen","brown3", "yellow" ))+
  #geom_text(x = 1, y = 3.8, label = "b",colour = "black")+
  #geom_text(x = 2, y = 3.8, label = "a",colour = "black")+
  #geom_text(x = 3, y = 3.8, label = "c",colour = "black")+
  ggtitle("NUE for GY production [kg/kg N]")+ theme(axis.title.x = element_text(size=10))+
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank())+
  theme(plot.title = element_text(color="black", size=10,face="bold", hjust = 0.5))


# As Kostas said, create a *new* group variable according to your needs:
#   plot(Petal.Width~Species,iris)
# iris$Species2<-factor(iris$Species,levels=levels(iris$Species)[c(3,1,2)])
# plot(Petal.Width~Species2,iris)

merge_RY_lab_Blue_recode_new$Water_N <- factor(merge_RY_lab_Blue_recode_new$TreatEnV_N,
                                               levels=levels(merge_RY_lab_Blue_recode_new$TreatEnV_N)[c(1,2,4,2)])

head(merge_RY_lab_Blue_recode_new)
tail(merge_RY_lab_Blue_recode_new)

levels(merge_RY_lab_Blue_recode_new$TreatEnV_N)
str(merge_RY_lab_Blue_recode_new$TreatEnV_N)
windows(5,5)

p2017 = ggplot(merge_RY_lab_Blue_recode_new) +
  aes(x=Water_N, y = NUE_Grain, fill = factor(Water_N, 
                                              levels = c("Rainfed_Voll", "Rainfed_Null","Drought_Voll","Drought_Null"))) +
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
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) +
  theme(plot.background = element_rect(fill = "gray80"))

# ANOVA Root  ANAlysis

# Load the data 

Root_Niko <- read_excel("Data/Wurzel_Werte_Aussortiert_Niko.xlsx", 
                        sheet = "Root")

Root_Niko <- as.data.frame(Root_Niko)
head(Root_Niko,2)
str(Root_Niko)
dim(Root_Niko)

Root_Kat <- read_excel("Data/DataRoot_Shoot_Kat.xlsx", 
                       sheet = "Root")

Root_Kat <- as.data.frame(Root_Kat)
head(Root_Kat,2)
str(Root_Kat)
dim(Root_Kat)

# Topf Nummer is Unique from the Experiment So I can merge both Data using that

Root_ShootKat_Niko_new <- merge(Root_Kat, Root_Niko,by.x = "TopfNr", by.y = "TopfNr", 
                            all = TRUE, all.x = TRUE, all.y = TRUE)

dim(Root_ShootKat_Niko_new)

head(Root_ShootKat_Niko_new)

# dim(Root_ShootKat_Niko)
# head(Root_ShootKat_Niko)

write.table(Root_ShootKat_Niko_new, "Data/Root_ShootKat_Niko_new.txt")


Root_ShootKat_Niko_select <- Root_ShootKat_Niko[, c(1:6, 8,10:13,18, 23:26)]
View(head(Root_ShootKat_Niko_select))

Root_ShootKat_Niko_sel_ord <- Root_ShootKat_Niko_select[, c(1:6,12, 8:11, 7,13:16 )]
View(head(Root_ShootKat_Niko_sel_ord))
View(Root_ShootKat_Niko_sel_ord[,12:13])

Root_ShootKat_Niko_sel_ord_N <- Root_ShootKat_Niko_sel_ord[,-13]

head(Root_ShootKat_Niko_sel_ord_N)

# Change the abbreviations of the variables

setnames(Root_ShootKat_Niko_sel_ord_N, old = c("TopfNr","WDH.x","Water.x","Nitrogen.x","Env","BRISONr.x",
                                               "BBCH_Klasse","FreshBiomass_g","Dry_Biomass_g","__g",
                                               "N-Gehalt_Blatt","Rootarea","Wfrisch","Wtrocken","RWaP"),
         new = c("TopfNr","WDH","Water","Nitrogen","Env","BRISONr",
                 "BBCH_Stage","FSW","SDW","",
                 "NLf","RA","FRW","RDw",""))

# The probleme here is, instead of taking FreshRootmass_g variable from Katrin Data, 
# you took Wfrisch from Niko Data which doesn´t contain all data including MN (medium N)
str(Root_ShootKat_Niko_sel_ord_N)

# Convert many chr into factors
Data_IntAll[, c("Trait", "Chr", "M_code_peak", "Treatment", "Allele")] <- lapply (Data_IntAll[, 
                                                                                              c("Trait", "Chr", "M_code_peak", "Treatment", "Allele")], factor )

Root_ShootKat_Niko_sel_ord_N[, c("WDH", "Water", "Nitrogen", "Env", "BRISONr", "BBCH_Stage" )] <- lapply (Root_ShootKat_Niko_sel_ord_N[, 
                                                                                                                                       c("WDH", "Water", "Nitrogen", "Env", "BRISONr", "BBCH_Stage" )], factor )

str(Root_ShootKat_Niko_sel_ord_N)
Root_ShootKat_Niko_sel_ord_N$WDH <- as.factor(Root_ShootKat_Niko_sel_ord_N$WDH)

AnovaModel.5 <- lm(SWaP ~ BRISONr*Water, data=Root_ShootKat_Niko_sel_ord_N, contrasts=list(BRISONr ="contr.Sum",  Water ="contr.Sum"))
Anova(AnovaModel.5)

AnovaModel.3 <- lm(RDw ~ BRISONr*Nitrogen*Water, data=Root_ShootKat_Niko_sel_ord_N, contrasts=list(BRISONr ="contr.Sum", Nitrogen 
                                                                                                   ="contr.Sum", Water ="contr.Sum"))
Anova(AnovaModel.3)

head(Root_ShootKat_Niko_sel_ord_N,2)

# Physiological traits from Pot experiment accross growing seasons

DataMT_Kat_MT1 <- read_excel("Data/DataMT_Kat.xlsx", sheet = "MT1")
DataMT_Kat_MT2 <- read_excel("Data/DataMT_Kat.xlsx", sheet = "MT2")
DataMT_Kat_MT3 <- read_excel("Data/DataMT_Kat.xlsx", sheet = "MT3")

head(DataMT_Kat_MT1)
head(DataMT_Kat_MT2)
head(DataMT_Kat_MT3)

DataMT_Kat_MT1$Env <- paste0(DataMT_Kat_MT1$Water, "_", DataMT_Kat_MT1$Nitrogen)
head(DataMT_Kat_MT1)

DataMT_Kat_MT2$Env <- paste0(DataMT_Kat_MT2$Water, "_", DataMT_Kat_MT2$Nitrogen)
head(DataMT_Kat_MT2)

DataMT_Kat_MT3$Env <- paste0(DataMT_Kat_MT3$Water, "_", DataMT_Kat_MT3$Nitrogen)
head(DataMT_Kat_MT3)

colnames(DataMT_Kat_MT1)== colnames(DataMT_Kat_MT2)

DataMT_Kat_MT1_ord <- DataMT_Kat_MT1[, c(1:4, 12, 5,6, 8, 9, 7,10:11 )]
head(DataMT_Kat_MT1_ord)

DataMT_Kat_MT2_ord <- DataMT_Kat_MT2[, c(1:6, 8,9, 7, 10:12 )]
head(DataMT_Kat_MT2_ord)

DataMT_Kat_MT2_ord <- DataMT_Kat_MT2_ord[, c(1:8, 10, 9, 11,12)]
head(DataMT_Kat_MT2_ord)

DataMT_Kat_MT3_ord <- DataMT_Kat_MT3[, c(1:7, 9:10, 8, 11,12)]
head(DataMT_Kat_MT3_ord)

colnames(DataMT_Kat_MT2_ord)== colnames(DataMT_Kat_MT3_ord)


# Create the data from MT1 to MT2 -----

DataMT_Kat_MT2_ord_sel <- DataMT_Kat_MT2_ord[, c(6,10:12)]
head(DataMT_Kat_MT2_ord_sel)

DataMT_Kat_MT3_ord_sel <- DataMT_Kat_MT3_ord[, c(6,10:12)]
head(DataMT_Kat_MT3_ord_sel)

str(DataMT_Kat_MT3_ord_sel)
# Merge all the three
# You could try this if you have 3 dataframes
# https://stackoverflow.com/questions/23668427/pandas-three-way-
#   joining-multiple-dataframes-on-columns#:~:text=You%20can%20join
# %20any%20number%20of%20DataFrames%20together%20with%20it.&text=To%20
# work%20with%20multiple%20DataFrames,joining%20columns%20in%20the%20index.

# dfs = [df1, df2, df3]
# dfs = [df.set_index('name') for df in dfs]
# dfs[0].join(dfs[1:])

install.packages("pandas")


Merge_MT <- merge(DataMT_Kat_MT1_ord, DataMT_Kat_MT2_ord_sel,by.x = "TopfNr", by.y = "TopfNr", 
                  all = TRUE, all.x = TRUE, all.y = TRUE)

head(Merge_MT,2)

Merge_DataMT <- merge(Merge_MT, DataMT_Kat_MT3_ord_sel,by.x = "TopfNr", by.y = "TopfNr", 
                      all = TRUE, all.x = TRUE, all.y = TRUE)
head(Merge_DataMT,2)

Merge_DataMT <- Merge_DataMT[, c(1:9, 11,12, 14,15, 17,18)]
head(Merge_DataMT,2)

# TopfNr Termin WDH     Water Nitrogen            Env BRISONr    Sorte       BBCH SPAD1 YII1 SPAD2 YII2 SPAD3 YII3
# 1    241      1   2 Kontrolle     voll Kontrolle_voll      84 MarisHun Prebooting    54 0.67    56 0.45    52 0.50
# 2    242      1   2 Kontrolle     voll Kontrolle_voll      98  Severin Prebooting    46 0.48    51 0.38    46 0.46

tail(Merge_DataMT,2)

# let us load the polypen NDVI Data

NDVI1 <- read_excel("Data/NDVI_NikPat.xlsx", sheet = "NDVI1")
NDVI2 <- read_excel("Data/NDVI_NikPat.xlsx", sheet = "NDVI2")
NDVI3 <- read_excel("Data/NDVI_NikPat.xlsx", sheet = "NDVI3")

head(NDVI1,5)
head(NDVI2,5)
head(NDVI3,5)

NDVI12 <- merge(NDVI1, NDVI2,by.x = "TopfNr", by.y = "TopfNr", 
                all = TRUE, all.x = TRUE, all.y = TRUE)
head(NDVI12)

NDVI_123 <- merge(NDVI12, NDVI3,by.x = "TopfNr", by.y = "TopfNr", 
                  all = TRUE, all.x = TRUE, all.y = TRUE)

View(head(NDVI_123))

NDVI_123_sel <- NDVI_123[, c(1,5,6,7,8,9, 13,22)]
head(NDVI_123_sel)
dim(NDVI_123_sel)

# Merge with the other Photos data

Merge_YII_SPAD_NDVI_123 <- merge(Merge_DataMT, NDVI_123_sel,by.x = "TopfNr", by.y = "TopfNr", 
                                 all = TRUE, all.x = TRUE, all.y = TRUE)

View(head(Merge_YII_SPAD_NDVI_123,2))

unique(Merge_YII_SPAD_NDVI_123$Water==Merge_YII_SPAD_NDVI_123$Water.y)

View(Merge_YII_SPAD_NDVI_123)

dim(Merge_YII_SPAD_NDVI_123)

Merge_YII_SPAD_NDVI_123_sel <- Merge_YII_SPAD_NDVI_123[, c(1:15, 18,21,22)]

dim(Merge_YII_SPAD_NDVI_123_sel)
head(Merge_YII_SPAD_NDVI_123_sel)

# now remove the data of plant that have been eaten by rat
# TopfNr 246, 248

Merge_YII_SPAD_NDVI_123_selN <- Merge_YII_SPAD_NDVI_123_sel[Merge_YII_SPAD_NDVI_123_sel$TopfNr != c("246","248"),] #did not work
Merge_YII_SPAD_NDVI_123_selN <- Merge_YII_SPAD_NDVI_123_sel[-c(16:18,22:24),]

head(Merge_YII_SPAD_NDVI_123_selN,25)

tail(Merge_YII_SPAD_NDVI_123_selN,25)


# Read the Stacked Photos Data for ANOVA

DataMT_stack <- read_excel("Data/DataMT_stack.xlsx", sheet = "MT_Stack")
head(DataMT_stack,5)
str(DataMT_stack)

DataMT_stack[, c("WDH", "Water", "Nitrogen", "Env", "BRISONr", "BBCH" )] <- lapply (DataMT_stack[, 
                                                                                                 c("WDH", "Water", "Nitrogen", "Env", "BRISONr", "BBCH" )], factor )
str(DataMT_stack)

DataMT_stack_Anthesis <- subset(DataMT_stack, BBCH=="Anthesis")

AnovaModel.3 <- lm(YII ~ BRISONr*Nitrogen*Water, data=DataMT_stack_Anthesis, contrasts=list(BRISONr ="contr.Sum", Nitrogen 
                                                                                            ="contr.Sum", Water ="contr.Sum"))
Anova(AnovaModel.3)



NDVI_stack <- read_excel("Data/NDVI_NikPat.xlsx", sheet = "NDVI_stack")

head(NDVI_stack,5)
str(NDVI_stack)

NDVI_stack[, c("WDH", "Water", "Nitrogen", "Env", "BRISONr", "BBCH_Klasse" )] <- lapply (NDVI_stack[, 
                                                                                                    c("WDH", "Water", "Nitrogen", "Env", "BRISONr", "BBCH_Klasse" )], factor )
str(NDVI_stack)

NDVI_stack_Anthesis <- subset(NDVI_stack, BBCH_Klasse=="Anthesis")

AnovaModel.5 <- lm(NDVI ~ BRISONr*Water, data=NDVI_stack_Anthesis, contrasts=list(BRISONr ="contr.Sum",  Water ="contr.Sum"))
Anova(AnovaModel.5)

# Now merge the photos traits and morphological traits for correlation ----

head(Merge_YII_SPAD_NDVI_123_selN,2)

head(Root_ShootKat_Niko_sel_ord_N,2)
dim(Root_ShootKat_Niko_sel_ord_N)

YII3_SPAD3_NDVI3_Agro <- merge(Merge_YII_SPAD_NDVI_123_selN[,c(1:9, 16:18)], Root_ShootKat_Niko_sel_ord_N[,c(1,7:15)],by.x = "TopfNr", by.y = "TopfNr", 
                               all = TRUE, all.x = TRUE, all.y = TRUE)

head(YII3_SPAD3_NDVI3_Agro,2)

#YII3_SPAD3_NDVI3_Agro <- YII3_SPAD3_NDVI3_Agro[, -c(9,13)]


YII3_SPAD3_NDVI3_Agro <- YII3_SPAD3_NDVI3_Agro %>%
  select(-c(BBCH, BBCH_Stage))
head(YII3_SPAD3_NDVI3_Agro)


# Saturday 03rd April 2021

# Compare the anova Tuckey test combination of levels. # donne with rcmdr 
merge_RY_lab_Blue_recode_new 

unique(merge_RY_lab_Blue_recode_new$TreatEnV_N==merge_RY_lab_Blue_recode_new$Water_N)

# Recheck the Tuckey classification with the whole dataset

head(ndyncorrected1718forbluesfin_18,2)
tail(ndyncorrected1718forbluesfin_18,2)

# Add the new factor variable to the dataset
ndyncorrected1718forbluesfin$Water_N <- paste0(ndyncorrected1718forbluesfin$TreatEnv,"_",ndyncorrected1718forbluesfin$TreatN)

head(ndyncorrected1718forbluesfin,2)
tail(ndyncorrected1718forbluesfin,2)
str(ndyncorrected1718forbluesfin)

ndyncorrected1718forbluesfin$Water_N <- as.factor(ndyncorrected1718forbluesfin$Water_N)
levels(ndyncorrected1718forbluesfin$Water_N)
# Replace all the small voll by big Voll

ndyncorrected1718forbluesfin_cop <- ndyncorrected1718forbluesfin

ndyncorrected1718forbluesfin_cop[ndyncorrected1718forbluesfin_cop$TreatN == "voll",] <- "Voll" #didnt work

levels(ndyncorrected1718forbluesfin_cop$TreatEnv)
levels(ndyncorrected1718forbluesfin_cop$TreatN)
# [1] "Null" "voll" "Voll"

# Rename by index in levels list: change third item, "gamma", to "three".
# levels(x)[3] <- "three"

levels(ndyncorrected1718forbluesfin_cop)[1] <- "Voll"

# Rename by name: change "beta" to "two"
# levels(x)[levels(x)=="beta"] <- "two"

levels(ndyncorrected1718forbluesfin_cop$TreatN)[levels(ndyncorrected1718forbluesfin_cop$TreatN) == "voll"] <- "Voll"

ndyncorrected1718forbluesfin <- ndyncorrected1718forbluesfin_cop
rm(ndyncorrected1718forbluesfin_cop)

# Descriptives statistic for Pot Experiment drought

head(YII3_SPAD3_NDVI3_Agro)

YII3_SPAD3_NDVI3_Agro_Mean <- read_excel("Data/YII3_SPAD3_NDVI3_Agro_Mean.xlsx", sheet = "DataMean")

head(YII3_SPAD3_NDVI3_Agro_Mean,5)

YII3_SPAD3_NDVI3_Agro_Mean <- as.data.frame(YII3_SPAD3_NDVI3_Agro_Mean)


# Descriptives statistics ----

# stat.desc(cropyield, basic=F)
# 
# stat.desc(Briwecs_2016_spad, basic=F)
# options(digits =   )
# 
# print(aggregate(cbind(Gday)~Year+Treatment,data=Datall2_151617[,1:3-8],FUN = function(x) c(mean = mean(x), sd = sd(x), max=max(x), min=min(x) )))
# sink()  

# let us do the kurtosis and skewness
sink("outputallGday.txt")
print(aggregate(cbind(Gday)~Year+Treatment,data=Datall2_151617[,1:3-8],FUN = function(x) c(kurt = kurtosis(x), skew=skewness(x) )))
sink()

levels(YII3_SPAD3_NDVI3_Agro_Mean$Env)
str(YII3_SPAD3_NDVI3_Agro_Mean)
YII3_SPAD3_NDVI3_Agro_Mean$Env <- as.factor(YII3_SPAD3_NDVI3_Agro_Mean$Env)
YII3_SPAD3_NDVI3_Agro_Mean$Genotypes <- as.factor(YII3_SPAD3_NDVI3_Agro_Mean$Genotypes)



descr(subset(YII3_SPAD3_NDVI3_Agro_Mean, Env =="Kontrolle_voll"))
Kontrolle_voll <- stat.desc(subset(YII3_SPAD3_NDVI3_Agro_Mean, Env =="Kontrolle_voll"), basic=F)
Kontrolle_reduziert <- stat.desc(subset(YII3_SPAD3_NDVI3_Agro_Mean, Env =="Kontrolle_reduziert"), basic=F)
stress_voll <- stat.desc(subset(YII3_SPAD3_NDVI3_Agro_Mean, Env =="stress_voll"), basic=F)

DMN <- as.data.frame(stat.desc(subset(YII3_SPAD3_NDVI3_Agro_Mean,Env =="stress_reduziert"), basic=F))
data_DMN <- data.frame(Env = rep("DMN",times = 7))
Desc_DMN <- cbind(stress_reduziert,data_DMN)

DHN <- as.data.frame(stat.desc(subset(YII3_SPAD3_NDVI3_Agro_Mean,Env =="stress_voll"), basic=F))
data_DHN <- data.frame(Env = rep("DHN",times = 7))
Desc_DHN <- cbind(stress_reduziert,data_DHN)

CHN <- as.data.frame(stat.desc(subset(YII3_SPAD3_NDVI3_Agro_Mean,Env =="Kontrolle_voll"), basic=F))
data_CHN <- data.frame(Env = rep("CHN",times = 7))
Desc_CHN <- cbind(stress_reduziert,data_CHN)

CMN <- as.data.frame(stat.desc(subset(YII3_SPAD3_NDVI3_Agro_Mean,Env =="Kontrolle_reduziert"), basic=F))
data_CMN <- data.frame(Env= rep("CMN",times = 7))
Desc_CMN <- cbind(stress_reduziert,data_CMN)

# rbind(Desc_CHN,Desc_DHN,Desc_CMN,Desc_DMN) # Copy Paste 


df <- data.frame(DMN = rep("DMN", times = 7))
stats_table <- as.table(Kontrolle_voll)

# select just some few

Kontrolle_voll <- descr(subset(YII3_SPAD3_NDVI3_Agro_Mean, Env =="Kontrolle_voll"),stats = c("mean", "sd", "min", "max","se"))
Kontrolle_reduziert <- descr(subset(YII3_SPAD3_NDVI3_Agro_Mean, Env =="Kontrolle_reduziert"), stats = c("mean", "sd", "min", "max","se"))
stress_voll <- descr(subset(YII3_SPAD3_NDVI3_Agro_Mean, Env =="stress_voll"), stats = c("mean", "sd", "min", "max","se"))
stress_reduziert <- descr(subset(YII3_SPAD3_NDVI3_Agro_Mean, Env =="stress_reduziert"), stats = c("mean", "sd", "min", "max","se"))

# group by Env

data("YII3_SPAD3_NDVI3_Agro_Mean")

descr <- with(YII3_SPAD3_NDVI3_Agro_Mean, stby (Env,descr, stats = c("mean", "sd", "min", "max","se")))

grouped_stats <- descr(YII3_SPAD3_NDVI3_Agro_Mean, group.by = "Env", stats = c("mean", "sd", "cv"))

# Check in the original Data which has replications


data("tobacco")
with(tobacco, stby(BMI, gender, descr))
head(YII3_SPAD3_NDVI3_Agro,5)

stat.desc(subset(YII3_SPAD3_NDVI3_Agro, Env =="Kontrolle_voll"), basic=F)
stat.desc(subset(YII3_SPAD3_NDVI3_Agro, Env =="Kontrolle_reduziert"), basic=F)
stat.desc(subset(YII3_SPAD3_NDVI3_Agro, Env =="stress_reduziert"), basic=F)
stat.desc(subset(YII3_SPAD3_NDVI3_Agro, Env =="stress_voll"), basic=F)

# Do the descriptives stats for the remaining traits under field conditions
head(ndyncorrected1718forbluesfin)
View(head(ndyncorrected1718forbluesfin_18))
dim(ndyncorrected1718forbluesfin_18)
levels(ndyncorrected1718forbluesfin_18$Water_N)

stat.desc(subset(ndyncorrected1718forbluesfin_18[, c(12,14,15, 18, 20,21,22,23,27)], Water_N =="Control_Voll"), basic=F)
stat.desc(subset(ndyncorrected1718forbluesfin_18[, c(12,14,15, 18, 20,21,22,23,27)], Water_N =="Control_Null"), basic=F)
stat.desc(subset(ndyncorrected1718forbluesfin_18[, c(12,14,15, 18, 20,21,22,23,27)], Water_N =="D_stress_Voll"), basic=F)
stat.desc(subset(ndyncorrected1718forbluesfin_18[, c(12,14,15, 18, 20,21,22,23,27)], Water_N =="D_stress_Null"), basic=F)

# Do the Rcmdr for this remaining traits

str(ndyncorrected1718forbluesfin_18) # Ok Means comparison completed

# Now for the pot Experiment

head(YII3_SPAD3_NDVI3_Agro,5)
str(YII3_SPAD3_NDVI3_Agro,5)

 View(YII3_SPAD3_NDVI3_Agro)


# Desc Stats (deleted from heritability R scripts)------

head(DataBlue1718)

summaryBy(Biomasse + Seedyield+ Protein ~ Water, DataBlue1718, FUN = c(mean,sd))

summaryBy(Dv_YII ~ W135_EQFH, Drought_GenoHmp[Drought_GenoHmp$W135_EQFH!="NN",])

install.packages("pastecs")
library(pastecs)
stat.desc(cropyield, basic=F)

stat.desc(subset(DataBlue1718, Water=="Control"), basic=F)
options(digits =2)

stat.desc(subset(DataBlue1718, Water=="Rainout"), basic=F)
options(digits =2)

# in 2018 Null
stat.desc(subset(Data_Null18, Water=="Rainout"), basic=F)

stat.desc(subset(Data_Null18, Water=="Control"), basic=F)

colnames(Data_Null18)[9] <- "NUE_Bio"
colnames(Data_Null18)[10] <- "NUE_grain"

# in 2018 Voll
colnames(DataBlue_Voll18)[9] <- "NUE_Bio"
colnames(DataBlue_Voll18)[10] <- "NUE_grain"

stat.desc(subset(DataBlue_Voll18, Water=="Rainout"), basic=F)

stat.desc(subset(DataBlue_Voll18, Water=="Control"), basic=F) # end script from Heritability file


# Thursday, 17 August 2023 ----


Root_ShootKat_Niko_new <- read_excel("Data/Data_Article3_Recap.xlsx", sheet = "Root_ShootKat_Niko_new")
View(Root_ShootKat_Niko_new)
head(Root_ShootKat_Niko_new)

ShootRoot <- Root_ShootKat_Niko_new %>%
  select(Nitrogen, Env, BRISONr, FRW_g, FSW_g, DSW_g, SWaP__g,NLf,DRW, RWaP)
head(ShootRoot)
dim(ShootRoot)
ShootRoot[, c("Nitrogen", "Env", "BRISONr")] <- 
  lapply (ShootRoot[, c("Nitrogen", "Env", "BRISONr")], factor)
str(ShootRoot)
levels(ShootRoot$Env)


Kontrolle_Voll <- stat.desc(subset(ShootRoot, Env =="Kontrolle_Voll"), basic=F)
Kontrolle_Reduziert <- stat.desc(subset(ShootRoot, Env =="Kontrolle_Reduziert"), basic=F)
Stress_Voll <- stat.desc(subset(ShootRoot, Env =="Stress_Voll"), basic=F)
Stress_Reduziert <- stat.desc(subset(ShootRoot,Env =="Stress_Reduziert"), basic=F)

head(ShootRoot,5)
