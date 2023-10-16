# Calculate Chromosomal LD

# From the script Rscript_LD_Decay_Agenome take out the dataframe for each chromosome

head(LD_Briw2dDrought200genoHomoAll_GenA[LD_Briw2dDrought200genoHomoAll_GenA$Dist_megabase<100,],2)

LDBriwecs2_Chr1A<-LD_Briw2dDrought200genoHomoAll_GenA[LD_Briw2dDrought200genoHomoAll_GenA$Locus1%in%"1A",]

dim(LDBriwecs2_Chr1A)
str(LDBriwecs2_Chr1A)
Sub_LDBriwecs2_Chr1A<-subset(LDBriwecs2_Chr1A,LDBriwecs2_Chr1A$R.2==0.1000000)
dim(Sub_LDBriwecs2_Chr1A)
head(Sub_LDBriwecs2_Chr1A,5)

# For Chr 1A LDBriwecs2_Chr1A
windows(8,6)

ggplot(data = LDBriwecs2_Chr1A[LDBriwecs2_Chr1A$Dist_megabase<100,],
       aes(x = Dist_megabase, y = R.2)) +ylim(c(0,1))+scale_x_continuous(breaks=seq(0,100,5))+#to limit to 1
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ 
  labs(title="LD Decay Chr 1A", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=10,face="bold"))+
  xlab("Distance in megabase pairs")+ylab(expression(r^2))

LDBriwecs2_Chr2A<-LD_Briw2dDrought200genoHomoAll_GenA[LD_Briw2dDrought200genoHomoAll_GenA$Locus1%in%"2A",]
ggplot(data = LDBriwecs2_Chr2A[LDBriwecs2_Chr2A$Dist_megabase<100,],
       aes(x = Dist_megabase, y = R.2)) +ylim(c(0,1))+scale_x_continuous(breaks=seq(0,100,5))+#to limit to 1
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ 
  labs(title="LD Decay Chr 2A", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=10,face="bold"))+
  xlab("Distance in megabase pairs")+ylab(expression(r^2))

LDBriwecs2_Chr3A<-LD_Briw2dDrought200genoHomoAll_GenA[LD_Briw2dDrought200genoHomoAll_GenA$Locus1%in%"3A",]
ggplot(data = LDBriwecs2_Chr3A[LDBriwecs2_Chr3A$Dist_megabase<100,],
       aes(x = Dist_megabase, y = R.2)) +ylim(c(0,1))+scale_x_continuous(breaks=seq(0,100,5))+#to limit to 1
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ 
  labs(title="LD Decay Chr 3A", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=10,face="bold"))+
  xlab("Distance in megabase pairs")+ylab(expression(r^2))

LDBriwecs2_Chr4A<-LD_Briw2dDrought200genoHomoAll_GenA[LD_Briw2dDrought200genoHomoAll_GenA$Locus1%in%"4A",]
ggplot(data = LDBriwecs2_Chr4A[LDBriwecs2_Chr4A$Dist_megabase<100,],
       aes(x = Dist_megabase, y = R.2)) +ylim(c(0,1))+scale_x_continuous(breaks=seq(0,100,5))+#to limit to 1
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ 
  labs(title="LD Decay Chr 4A", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=10,face="bold"))+
  xlab("Distance in megabase pairs")+ylab(expression(r^2))

LDBriwecs2_Chr5A<-LD_Briw2dDrought200genoHomoAll_GenA[LD_Briw2dDrought200genoHomoAll_GenA$Locus1%in%"5A",]
ggplot(data = LDBriwecs2_Chr5A[LDBriwecs2_Chr5A$Dist_megabase<100,],
       aes(x = Dist_megabase, y = R.2)) +ylim(c(0,1))+scale_x_continuous(breaks=seq(0,100,5))+#to limit to 1
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ 
  labs(title="LD Decay Chr 5A", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=10,face="bold"))+
  xlab("Distance in megabase pairs")+ylab(expression(r^2))

#######
# Let us try to do all in one
head(LD_Briw2dDrought200genoHomoAll_GenA,5)

LD_Briw2dDrought200genoHomoAll_GenA$Chromosome<-paste("Chr", LD_Briw2dDrought200genoHomoAll_GenA$Locus1, sep = " ")

ggplot(data = LD_Briw2dDrought200genoHomoAll_GenA[LD_Briw2dDrought200genoHomoAll_GenA$Dist_megabase<100,],
       aes(x = Dist_megabase, y = R.2, color=Chromosome)) +ylim(c(0,1))+scale_x_continuous(breaks=seq(0,100,5))+#to limit to 1
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ 
  labs(title="LD Decay by Chromosome of Genome A ", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=10,face="bold"))+
  xlab("Distance in megabase pairs")+ylab(expression(r^2))

# For Genome B

head(LD_Briw2dDrought200genoHomoAll_GenB,5)

LD_Briw2dDrought200genoHomoAll_GenB$Chromosome<-paste("Chr", LD_Briw2dDrought200genoHomoAll_GenB$Locus1, sep = " ")

ggplot(data = LD_Briw2dDrought200genoHomoAll_GenB[LD_Briw2dDrought200genoHomoAll_GenB$Dist_megabase<100,],
       aes(x = Dist_megabase, y = R.2, color=Chromosome)) +ylim(c(0,1))+scale_x_continuous(breaks=seq(0,100,5))+#to limit to 1
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ 
  labs(title="LD Decay by Chromosome of Genome B ", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=10,face="bold"))+
  xlab("Distance in megabase pairs")+ylab(expression(r^2))

# For Genome D

head(LD_Briw2dDrought200genoHomoAll_GenD,5)

LD_Briw2dDrought200genoHomoAll_GenD$Chromosome<-paste("Chr", LD_Briw2dDrought200genoHomoAll_GenD$Locus1, sep = " ")

ggplot(data = LD_Briw2dDrought200genoHomoAll_GenD[LD_Briw2dDrought200genoHomoAll_GenD$Dist_megabase<100,],
       aes(x = Dist_megabase, y = R.2, color=Chromosome)) +ylim(c(0,1))+scale_x_continuous(breaks=seq(0,100,5))+#to limit to 1
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ 
  labs(title="LD Decay by Chromosome of Genome D ", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=10,face="bold"))+
  xlab("Distance in megabase pairs")+ylab(expression(r^2))

summary(LD_Briw2dDrought200genoHomoAll_GenD$Chromosome)
str(LD_Briw2dDrought200genoHomoAll_GenD$Chromosome)

dim(LD_Briw2dDrought200genoHomoAll_GenD)

dim(LD_Briw2dDrought200genoHomoAll_GenA[LD_Briw2dDrought200genoHomoAll_GenA$Chromosome%in%"Chr 7A",])

dim(LD_Briw2dDrought200genoHomoAll_GenB[LD_Briw2dDrought200genoHomoAll_GenB$Chromosome%in%"Chr 7B",])

dim(LD_Briw2dDrought200genoHomoAll_GenD[LD_Briw2dDrought200genoHomoAll_GenD$Chromosome%in%"Chr 7D",])
nrow(LD_Briw2dDrought200genoHomoAll_GenD[LD_Briw2dDrought200genoHomoAll_GenD$Chromosome%in%"Chr 7D",])

dim(LD_Briw2dDrought200genoHomoAll_all)

windows(8,6)
ggplot(data = LD_Briw2dDrought200genoHomoAll_all[LD_Briw2dDrought200genoHomoAll_all$Dist_megabase<100,],
       aes(x = Dist_megabase, y = R.2)) +ylim(c(0,1))+scale_x_continuous(breaks=seq(0,100,5))+#to limit to 1
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ 
  labs(title="LD Decay 150K", element_text(hjust=.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=10,face="bold"))+
  xlab("Distance in megabase pairs")+ylab(expression(r^2))
