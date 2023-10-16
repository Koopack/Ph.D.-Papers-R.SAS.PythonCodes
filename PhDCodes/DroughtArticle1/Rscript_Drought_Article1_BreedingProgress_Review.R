# Resume Breeding Progress for review article 1

# check the data 
head(Blues_Mean_article1_Brinov,2)

windows(5,4)

ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=SDW_g_row, color=Water),size=0.001) +
  ylab("Shoot Dry Weight (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw() + 
  labs(title="Breeding Trend in Shoot Biomass", element_text(hjust=.5)) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  


ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=PBW_g_row, color=Water),size=0.001) +
  ylab("Plant Biomass Weight (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw() + 
  labs(title="Breeding Trend in aboveground Biomass", element_text(hjust=.5)) +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  

ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=GY_g_row, color=Water),size=0.001) +
  ylab("Grain yield weight (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw() + 
  labs(title="Breeding Trend in Grain Yield", element_text(hjust=.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  


ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=KNms, color=Water),size=0.001) +
  ylab("Kernels number per meter square")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+ theme_bw() + 
  labs(title="Breeding Trend in Kernels number per meter square", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  


ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=KNSp, color=Water),size=0.001)+
  ylab("Kernels number per spike")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw()+ 
  labs(title="Breeding Trend in Kernels number per Spike", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  

ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=HI, color=Water),size=0.001)+
  ylab("Harvest index")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw()+ 
  labs(title="Breeding Trend in Harvest index", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  


ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=PH, color=Water),size=0.001)+
  ylab("Plant height (cm)")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw()+
  labs(title="Breeding Trend in Plant height", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  


ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=SNms, color=Water),size=0.001)+
  ylab("Spikes number per meter square")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw()+ 
  labs(title="Spikes number per meter square", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  


ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=TKW, color=Water),size=0.001)+
  ylab("TKW (g)")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw()+ 
  labs(title="Thousand kernels weight", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  


ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=NDF, color=Water),size=0.001)+
  ylab("NDF (%)")+
  geom_point(size=1)+scale_x_continuous(name="Release Year") + theme_bw() +
  labs(title="Neutral detergent fiber", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  


ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=GPC, color=Water),size=0.001)+
  ylab("GPC (%)")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw()+ 
  labs(title="Grain protein content", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=GSC, color=Water),size=0.001)+
  ylab("GSC (%)")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw()+ 
  labs(title="Grain starch content", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  

# Date: Monday 10th May 2021 ----

# Calculate the SN and KN 
str(Blues_Mean_article1_Brinov)
Blues_Mean_article1_Brinov$KN_row <- (Blues_Mean_article1_Brinov$KNms*0.9)/4.76
Blues_Mean_article1_Brinov$SN_row <- (Blues_Mean_article1_Brinov$SNms*0.9)/4.76

ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=SN_row, color=Water),size=0.001)+
  ylab("Spikes number per row")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+theme_bw()+ 
  labs(title="Spikes number per row", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  

ggplot(Blues_Mean_article1_Brinov, aes(x=Year_Rel, y=KN_row, color=Water),size=0.001) +
  ylab("Kernels number per row")+
  geom_point(size=1)+scale_x_continuous(name="Release Year")+ theme_bw() + 
  labs(title="Kernels number per row", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 3) + 
  stat_cor(size=3,label.x = 1990,  aes(color = Water ))+
  theme(legend.position = "none") +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10))  