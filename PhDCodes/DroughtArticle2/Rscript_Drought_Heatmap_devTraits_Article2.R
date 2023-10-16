# Heatmap
install.packages("data.table")
library(data.table)
install.packages("tidyverse") # which include ggplot2
library(tidyverse)

HeatmapDevdatafor_R <-read.delim("Data/HeatmapDevdatafor_R.txt")
head(HeatmapDevdatafor_R)

# Genotypes       HSr       HGr       LGr       LRr      Mean
# 1     TG117 0.7000000 0.6250000 0.4055556 0.3750000 0.5263889
# 2     TG153 0.8166667 0.5750000 0.3686869 0.3928571 0.5383027
# 3     TG176 0.8388889 0.5416667 0.3025104 0.4900794 0.5432863
# 4     TG177 0.7261905 0.6666667 0.3130342 0.4750000 0.5452228
# 5     TG149 0.7416667 0.4861111 0.5373016 0.5194444 0.5711310
# 6     TG084 0.7500000 0.5833333 0.4805556 0.4956349 0.5773810

# Merge with the genotypes names 
HeatmapDevdatafor_R_Sorte <- merge(Sortename, HeatmapDevdatafor_R,  by.x = "Genoname", 
                                   by.y = "Genotypes", all = FALSE )
head(HeatmapDevdatafor_R_Sorte,5)

# order by Mean
HeatmapDevdatafor_R_Sorte <- HeatmapDevdatafor_R_Sorte[order(HeatmapDevdatafor_R_Sorte$Mean),]

HeatmapDevdatafor_R <- HeatmapDevdatafor_R_Sorte[,-c(1,7,8)]
head(HeatmapDevdatafor_R,5)
colnames(HeatmapDevdatafor_R)[6] <- "Genotypes"

HeatmapDevdatafor_R <- HeatmapDevdatafor_R[,c(6, 1:5)]
#let us melt the data
HeatmapDevdatafor_R_Melt <- melt(HeatmapDevdatafor_R) # you have to install reshape 2 and data table

head(HeatmapDevdatafor_R_Melt,2)

head(HeatmapDevdatafor_R_Melt,2)
# Genotypes variable     Score row
# 1     TG117      HSr 0.7000000   1
# 2     TG153      HSr 0.8166667   2
# Genotypes    variable     value
# 1   Pantus_1966 Healthiness 0.7000000
# 2 Kormoran_1973 Healthiness 0.8166667
colnames(HeatmapDevdatafor_R_Melt)[3]<-"Score"

HeatmapDevdatafor_R_Melt<-HeatmapDevdatafor_R_Melt%>%mutate(row=row_number())

windows(6,5)
# direction = +1 means you use the default the direction of the color.
ggplot(HeatmapDevdatafor_R_Melt, aes(x=reorder(Genotypes,row), y=variable))+geom_tile(aes(fill=Score),color = "black") + 
  scale_fill_distiller(palette = "RdBu", direction = +1)+geom_text(aes(label = round(Score, 2)))+ylab("Traits") +
  xlab("Genotypes")+theme(axis.text.y = element_text(hjust = 0, face = "plain", size = 10, color = "black"))+theme(axis.text.x = element_text(angle = 90, face = "plain", size = 10, color = "black")) +
  theme(plot.background = element_rect(fill = "gray80")) # Perfect

Drought_Adapted_year_release <- read.delim("Data/Drought_Adapted_year_release.txt")
head(Drought_Adapted_year_release,2)

dataRY <- Drought_Adapted_year_release[,c(2,4,8)]
HeatmapDevdatafor_R_Sorte <- merge(HeatmapDevdatafor_R, Drought_Adapted_year_release[,c(2,4,8)], by.x = "Genotypes", 
                                   by.y = "Genoname", all = FALSE)
rm(dataRY)

head(HeatmapDevdatafor_R_Sorte,5)

colnames(HeatmapDevdatafor_R_Sorte)[8] <- "Release_year"

HeatmapDevdatafor_R_Sorte$Release_year <- as.numeric(as.character(HeatmapDevdatafor_R_Sorte$Release_year)) # ok 


ggplot(HeatmapDevdatafor_R_Sorte, aes(x=Release_year, y=Healthiness),size=0.001)+
  geom_point(size=1) + scale_x_continuous(name="Year of release")+theme_bw()+ ylab("Mean DevTraits") +
  labs(title="DevTraits", element_text(hjust=.5)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method ="lm") + # no aes because no treamtent
  # scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+
  # scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3)+ 
  stat_cor(size=3,label.x = 1990) + 
  theme(axis.text.x = element_text(size=11), axis.title.x =element_text (color="black", size=11,face="bold")) +
  theme(axis.text.y = element_text(size=11), axis.title.y =element_text (color="black", size=11,face="bold")) +
  theme(plot.background = element_rect(fill = "gray80"))+
  theme(legend.position = "none") # ok but p value of the regresion is not significant.

HeatmapDevdatafor_R_Sorte$Sorte_RY <- paste(HeatmapDevdatafor_R_Sorte$Sorte_R, HeatmapDevdatafor_R_Sorte$Release_year, sep = "_")
SortenameSub20