#   LD decay Bgenome 
install.packages("ggplot2")
library(ggplot2)

LDBriwecs2dGenodataHomo213<-read.delim("Data/LDBriwecs2dGenodataHomo213.txt")
dim(LDBriwecs2dGenodataHomo213)
LDBriwecs2dGenodataHomo213[1:10,1:17]

LDBriwecs2dGenodataHomo213c<-LDBriwecs2dGenodataHomo213
attach(LDBriwecs2dGenodataHomo213c)

# combine the final data of the 3 genomes
# First add new variable having the genome name
LDBri2d213genodata15k_GenA$Genome<-rep("Genome A", nrow(LDBri2d213genodata15k_GenA))
length(LDBri2d213genodata15k_GenA$Genome)

LDBri2d213genodata15k_GenB$Genome<-rep("Genome B", nrow(LDBri2d213genodata15k_GenB))
length(LDBri2d213genodata15k_GenB$Genome)

LDBri2d213genodata15k_GenD$Genome<-rep("Genome D", nrow(LDBri2d213genodata15k_GenD))
length(LDBri2d213genodata15k_GenD$Genome)

# now row bind theme 
LDBriwecs2dGenodataHomo213_15Kall<-rbind.data.frame(LDBri2d213genodata15k_GenA,LDBri2d213genodata15k_GenB, LDBri2d213genodata15k_GenD)

# We will Run it the whole 3 genome
dim(LDBriwecs2dGenodataHomo213_15Kall)
head(LDBriwecs2dGenodataHomo213_15Kall, 10)
detach(LDBriwecs2dGenodataHomo213c)
attach(LDBriwecs2dGenodataHomo213_15Kall)

# Let us take the subset distance in basepair (without NA) and rqsquare (less than 0.9)
# already done
dim(LDBriwecs2dGenodataHomo213_15Kall)
str(LDBriwecs2dGenodataHomo213_15Kall)
head(LDBriwecs2dGenodataHomo213_15Kall, 10)
#LDBriwecs2dGenodataHomo213_15Kall[554645:554650,1:17]

# #Change the dist_bp into numeric
# LDBriwecs2dGenodataHomo213_15Kall$Dist_bp=as.numeric(as.character(LDBriwecs2dGenodataHomo213_15Kall$Dist_bp))
# dim(LDBriwecs2dGenodataHomo213_15Kall)
# head(LDBriwecs2dGenodataHomo213_15Kall, 10)
# LDBriwecs2dGenodataHomo213_15Kall[554645:554650,1:17]

#Subseting by removing the r2 more than 0.9 (meaning those snp are more lickely to be the same marker)
#and | Dist_bp is not NA
 LDBriwecs2dGenodataHomo213_15Kall<-subset(LDBriwecs2dGenodataHomo213_15Kall,LDBriwecs2dGenodataHomo213_15Kall$R.2<=1 & LDBriwecs2dGenodataHomo213_15Kall$Dist_bp!="NA")

# dim(LDBriwecs2dGenodataHomo213_15Kall)
# head(LDBriwecs2dGenodataHomo213_15Kall, 10)
# str(LDBriwecs2dGenodataHomo213_15Kall)

# ggplot(data = LDBri2d213genodata15k_GenA,
#        aes(x = Dist_bp, y = R.2, color = "white")) +
#   geom_point(color = "gray" ,size = .1) +
#   geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+ggtitle("LD Decay Genome A") ####ref GOOOD

# https://ropensci.github.io/plotly/ggplot2/geom_smooth.html 
# https://stackoverflow.com/questions/40711980/r-ggplot2-fit-curve-to-scatter-plot 

# ggplot(data = LDBriwecs2dGenodataHomo213_15Kall,
#        aes(x = Dist_bp, y = R.2)) +
#   geom_point(color = "gray" ,size = .1) +
#   geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ labs(title="LD Decay Genome B", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=.1, linetype="dashed", color = "red", size=1) # For Dashed horiz line

ggplot(data = LDBriwecs2dGenodataHomo213_15Kall,
       aes(x = Dist_bp, y = R.2, color=Genome)) +
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ labs(title="LD Decay all 3 Genome", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=.1, color = "red",size=1) # Well you can reduce basepaire if you want


#############################

# reduce the distance
ggplot(data = LDBriwecs2dGenodataHomo213_15Kall[LDBriwecs2dGenodataHomo213_15Kall$Dist_bp<200000000,],
       aes(x = Dist_bp, y = R.2, color=Genome)) +
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ labs(title="LD Decay  all 3 Genome", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=.1, color = "red",size=1)

# 2 horizont lines
ggplot(data = LDBriwecs2dGenodataHomo213_15Kall,
       aes(x = Dist_bp, y = R.2, color=Genome)) +
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ labs(title="LD Decay all 3 Genomes", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=.1, color = "red",size=1)+geom_hline(yintercept=.125, linetype="dashed",color = "purple",size=1)

ggplot(data = LDBriwecs2dGenodataHomo213_15Kall,
       aes(x = Dist_bp, y = R.2, color=Genome)) +
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ labs(title="LD Decay all ABD Genomes", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)# no need for two line

# Subset the genetic distance

ggplot(data = LDBriwecs2dGenodataHomo213_15Kall[LDBriwecs2dGenodataHomo213_15Kall$Dist_bp<100000000,],
       aes(x = Dist_bp, y = R.2, color=Genome)) +
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ labs(title="LD Decay all ABD Genomes", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)

# Create Distance in megabase 

head(LDBriwecs2dGenodataHomo213_15Kall,2)

LDBriwecs2dGenodataHomo213_15Kall$Dist_megabase=LDBriwecs2dGenodataHomo213_15Kall$Dist_bp/1000000

ggplot(data = LDBriwecs2dGenodataHomo213_15Kall[LDBriwecs2dGenodataHomo213_15Kall$Dist_megabase<100,],
       aes(x = Dist_megabase, y = R.2, color=Genome)) +
  geom_point(color = "gray" ,size = .1) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ log(x))+theme_bw()+ labs(title="LD Decay all ABD Genomes", element_text(hjust=.5))+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=.1, linetype="dashed",color = "purple",size=1)+xlab("Distance in megabase pairs")+ylab("r2")

dim(LDBriwecs2dGenodataHomo213_15Kall)

write.csv(LDBriwecs2dGenodataHomo213_15Kall, "Data/LDBriwecs2dGenodataHomo213_15Kall.csv")
