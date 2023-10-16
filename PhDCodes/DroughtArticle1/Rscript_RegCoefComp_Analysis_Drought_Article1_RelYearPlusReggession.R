# Comparison of Regression coefficients of preeding progress under the two water regimes
# How to interpret the slopes
#https://blog.minitab.com/blog/adventures-in-statistics-2/how-to-compare-regression-lines-between-different-models
dim(Blues_Mean_article1_Brinov)
summary(Blues_Mean_article1_Brinov)

# for PBW_g_row
Reg_PBW_g_row<-lm(PBW_g_row ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_PBW_g_row)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             2077.4267   422.3513   4.919 1.30e-06 ***
#   Year_Rel                  -0.7975     0.2116  -3.768 0.000190 ***
#   WaterD_stress          -2405.4437   597.2950  -4.027 6.82e-05 *** # the diference between the two constant is siignificant
#   Year_Rel:WaterD_stress     1.0514     0.2993   3.513 0.000497 *** # The difference between the two slopes is significant
#   ---

# For SDW_g_row
Reg_SDW_g_row<-lm(SDW_g_row ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_SDW_g_row)

# for GY_g_row
Reg_GY_g_row<-lm(GY_g_row ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_GY_g_row)

# for KNms
Reg_KNms<-lm(KNms ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_KNms)

# for KNSp
Reg_KNSp<-lm(KNSp ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_KNSp)

# for HI
Reg_HI<-lm(HI ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_HI)

# for PH
Reg_PH<-lm(PH ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_PH)

# for SNms
Reg_SNms<-lm(SNms ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_SNms)

# for TKW
Reg_TKW<-lm(TKW ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_TKW)

# for NDF
Reg_NDF<-lm(NDF ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_NDF)

# for GPC
Reg_GPC<-lm(GPC ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_GPC)

# for GSC
Reg_GSC<-lm(GSC ~ Year_Rel * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_GSC)

#################################

# Let´s us perform the regressions between GY and it´s components!
windows(7,5)
ggplot(Blues_Mean_article1_Brinov, aes(x=SDW_g_row, y=GY_g_row, color=Water),size=0.005)+ylab("GY (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Shoot dry weight (g/row)")+theme_bw()+ labs(title="GY (g/row) vs SDW (g/row)", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm")+scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4)+stat_cor(size=4,label.x = 300,  aes(color = Water ))+ expand_limits(x = c(0, 450))

# for SDW_g_row
Reg_SDW_g_row<-lm(GY_g_row ~ SDW_g_row * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_SDW_g_row)

KNSp<-ggplot(Blues_Mean_article1_Brinov, aes(x=KNSp, y=GY_g_row, color=Water),size=0.001)+ylab("GY (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Kernels number per spike")+theme_bw()+ labs(title="GY (g/row) vs KNSp", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm")+scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4)+stat_cor(size=4,label.x = 70,  aes(color = Water ))+ expand_limits(x = c(0, 100))

# for KNSp
Reg_KNSp<-lm(GY_g_row ~ KNSp * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_KNSp)

save.image("Graphs/Regression GY and Components/KNSp.png")
dev.off()
png("Graphs/Regression GY and Components/KNSp.png")
ggsave("Graphs/Regression GY and Components/KNSp.png",scale = 1.5# width = 6, height = 5, units = "cm"
       )

ggplot(Blues_Mean_article1_Brinov, aes(x=KNms, y=GY_g_row, color=Water),size=0.001)+ylab("GY (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Kernels number per meter square")+theme_bw()+ labs(title="GY (g/row) vs KNms", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm")+scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4)+stat_cor(size=4,label.x = 35000,  aes(color = Water ))+ expand_limits(x = c(0, 50000))

# for KNms
Reg_KNms<-lm(GY_g_row ~ KNms * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_KNms)

ggplot(Blues_Mean_article1_Brinov, aes(x=SNms, y=GY_g_row, color=Water),size=0.001)+ylab("GY (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Spikes number per meter square")+theme_bw()+ labs(title="GY (g/row) vs SNms", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm")+scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4)+stat_cor(size=4,label.x = 800,  aes(color = Water ))+ expand_limits(x = c(0, 1200))

# for SNms
Reg_SNms<-lm(GY_g_row ~ SNms * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_SNms)

ggplot(Blues_Mean_article1_Brinov, aes(x=TKW, y=GY_g_row, color=Water),size=0.001)+ylab("GY (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Thousand Kernels Weight (g)")+theme_bw()+ labs(title="GY (g/row) vs TKW (g)", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm")+scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4)+stat_cor(size=4,label.x = 50,  aes(color = Water ))+ expand_limits(x = c(20, 60))

# for TKW
Reg_TKW<-lm(GY_g_row ~ TKW * Water, data = Blues_Mean_article1_Brinov)
summary(Reg_TKW)

# just to check for GSC
ggplot(Blues_Mean_article1_Brinov, aes(x=GSC, y=GY_g_row, color=Water),size=0.001)+ylab("GY (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Grain starch content (%)")+theme_bw()+ labs(title="GY (g/row) vs GSC (%)", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm")+scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4)+stat_cor(size=4,label.x = 50,  aes(color = Water ))+ expand_limits(x = c(40, 80))

###############

# Date: Monday 10th May 2021 ----

# Rerun regression 

head(Blues_Mean_article1_Brinov,2)

ggplot(Blues_Mean_article1_Brinov, aes(x=SN_row, y=GY_g_row, color=Water),size=0.001)+ylab("GY (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Spikes number per row")+theme_bw()+
  labs(title="GY (g/row) vs SN per row", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm")+
  scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+  
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4)+
  stat_cor(size=4,label.x = 200,  aes(color = Water ))+ 
  expand_limits(x = c(0, 300)) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(Blues_Mean_article1_Brinov, aes(x=KN_row, y=GY_g_row, color=Water),size=0.001)+
  ylab("GY (g/row)")+
  geom_point(size=1)+scale_x_continuous(name="Kernels number per row")+
  theme_bw()+ labs(title="GY (g/row) vs KN per row", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm")+
  scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+  
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4)+
  stat_cor(size=4,label.x = 5000,  aes(color = Water ))+
  expand_limits(x = c(0, 8000))+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 

ggplot(Blues_Mean_article1_Brinov, aes(x=SDW_g_row, y=GY_g_row, color=Water),size=0.005) + 
  ylab("GY (g/row)")+ geom_point(size=1) + 
  scale_x_continuous(name="Shoot dry weight (g/row)")+
  theme_bw()+ labs(title="GY (g/row) vs SDW (g/row)", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm")+
  scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+  
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4)+
  stat_cor(size=4,label.x = 300,  aes(color = Water ))+ expand_limits(x = c(0, 450)) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 


ggplot(Blues_Mean_article1_Brinov, aes(x=KNSp, y=GY_g_row, color=Water),size=0.001)+
  ylab("GY (g/row)")+ geom_point(size=1) + 
  scale_x_continuous(name="Kernels number per spike")+theme_bw()+ 
  labs(title="GY (g/row) vs KNSp", element_text(hjust=.5))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(aes(fill=Water, color = Water), method ="lm")+
  scale_color_manual(values = c("#00AFBB",  "#FC4E07"))+  
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4) +
  stat_cor(size=4,label.x = 60,  aes(color = Water ))+ expand_limits(x = c(0, 100)) +
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 

ggplot(Blues_Mean_article1_Brinov, aes(x=TKW, y=GY_g_row, color=Water),size=0.001)+
  ylab("GY (g/row)")+ geom_point(size=1)+ 
  scale_x_continuous(name="Thousand Kernels Weight (g)") + 
  theme_bw()+ labs(title="GY (g/row) vs TKW (g)", element_text(hjust=.5)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(aes(fill=Water, color = Water), method ="lm") + 
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + scale_fill_manual(values = c("#00AFBB", "#FC4E07"))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), size= 4) +
  stat_cor(size=4,label.x = 42,  aes(color = Water ))+ expand_limits(x = c(20, 60))+
  theme(axis.title.x =element_text (color="black", size=12,face="bold")) +
  theme(axis.title.y =element_text (color="black", size=12,face="bold")) +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text (color="black", size=10)) +
  theme(axis.text.y = element_text (color="black", size=10)) 

options(di)

options(digits = 3)
