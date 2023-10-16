# Normalzed the briwecs key yield components

# Load the Blues Values

Blues_Mean_article1_Brinov<- read.delim("file:///C:/Users/Koua/sciebo/My_RProjects/Dought Experiment/Rproject_Drought_Article_1/Data/Blues_Mean_article1_Brinov.txt")

str(Blues_Mean_article1_Brinov)

BluData<-Blues_Mean_article1_Brinov

# create a function for normalisation between 0 to 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# create a function for normalisation between 1 to 10
normalize <- function(x) {
  return (1+(x - min(x))*(10-1) / (max(x) - min(x)))
}

# divide the Data set into 2 Control and Ds
BluData_cont<-BluData[BluData$TreatEnv%in%"Control",]
BluData_Ds<-BluData[BluData$TreatEnv%in%"D_stress",]

#For control
BluData_cont$Bio_norm<-normalize(BluData_cont$Bio)
BluData_cont$PH_norm<-normalize(BluData_cont$PH)
BluData_cont$SNms_norm<-normalize(BluData_cont$SNms)
BluData_cont$GNms_norm<-normalize(BluData_cont$GNms)
BluData_cont$GNSp_norm<-normalize(BluData_cont$GNSp)
BluData_cont$GY_norm<-normalize(BluData_cont$GY)
BluData_cont$HI_norm<-normalize(BluData_cont$HI)
BluData_cont$ShBio_norm<-normalize(BluData_cont$ShBio)
BluData_cont$TKW_norm<-normalize(BluData_cont$TKW)
BluData_cont$NDF_norm<-normalize(BluData_cont$NDF)
BluData_cont$PC_norm<-normalize(BluData_cont$PC)
BluData_cont$SC_norm<-normalize(BluData_cont$SC)
BluData_cont$HSr_norm<-normalize(BluData_cont$HSr)
BluData_cont$HGr_norm<-normalize(BluData_cont$HGr)
BluData_cont$LGr_norm<-normalize(BluData_cont$LGr)
BluData_cont$LRr_norm<-normalize(BluData_cont$LRr)

# For Drought
BluData_Ds$Bio_norm<-normalize(BluData_Ds$Bio)
BluData_Ds$PH_norm<-normalize(BluData_Ds$PH)
BluData_Ds$SNms_norm<-normalize(BluData_Ds$SNms)
BluData_Ds$GNms_norm<-normalize(BluData_Ds$GNms)
BluData_Ds$GNSp_norm<-normalize(BluData_Ds$GNSp)
BluData_Ds$GY_norm<-normalize(BluData_Ds$GY)
BluData_Ds$HI_norm<-normalize(BluData_Ds$HI)
BluData_Ds$ShBio_norm<-normalize(BluData_Ds$ShBio)
BluData_Ds$TKW_norm<-normalize(BluData_Ds$TKW)
BluData_Ds$NDF_norm<-normalize(BluData_Ds$NDF)
BluData_Ds$PC_norm<-normalize(BluData_Ds$PC)
BluData_Ds$SC_norm<-normalize(BluData_Ds$SC)
BluData_Ds$HSr_norm<-normalize(BluData_Ds$HSr)
BluData_Ds$HGr_norm<-normalize(BluData_Ds$HGr)
BluData_Ds$LGr_norm<-normalize(BluData_Ds$LGr)
BluData_Ds$LRr_norm<-normalize(BluData_Ds$LRr)

# Bind Both data
BluDataFin<-rbind(BluData_cont, BluData_Ds)

View(BluDataFin)
dim(BluDataFin)

boxplot(BluDataFin$GY_norm~BluDataFin$TreatEnv)

boxplot(BluDataFin[,c(23:38)], horizontal=TRUE)

boxplot(BluDataFin$GY_norm~BluDataFin$Bio_norm)

boxplot(BluDataFin$GY_norm~BluDataFin$TreatEnv)

#######################

PC_contNor=PCA(BluDataFin[BluDataFin$TreatEnv%in%"Control",c(23:34)],scale=T)

PC_cont=PCA(BluDataFin[BluDataFin$TreatEnv%in%"Control",c(7:18)],scale=T)
fviz_pca_var(PC_cont)

fviz_pca_var(PC_cont,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_eig(PC_cont)

######################
PC_Ds=PCA(BluDataFin[BluDataFin$TreatEnv%in%"D_stress",c(23:38)],scale=T)

PC_Ds=PCA(BluDataFin[BluDataFin$TreatEnv%in%"D_stress",c(7:22)],scale=T)

fviz_eig(PC_Ds)

#Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.

fviz_pca_var(PC_Ds,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Biplot of individuals and variables
fviz_pca_biplot(PC_Ds, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
##########################################
#Let us check the PCA in each growing seasons

Blues_17_18_article1_Brinov<-read.delim("file:///C:/Users/Koua/sciebo/My_RProjects/Dought Experiment/Rproject_Drought_Article_1/Data/Blues_17_18_article1_Brinov.txt")
Blues_17_18_article1_Brinov[1:5,1:23]

Blues_17_18_article1_Brinov$SN <- (Blues_17_18_article1_Brinov$SNms*0.9)/4.76
Blues_17_18_article1_Brinov$KN <- (Blues_17_18_article1_Brinov$GNms*0.9)/4.76


# We will just change the names of the variables
colnames(Blues_17_18_article1_Brinov)
# [1] "Year"          "TreatEnv"      "Genotypes"     "GenoName"      "Year_Rel"      "Older_Newer"  
# [7] "Oldest_Newest" "Bio"           "GNms"          "GNSp"          "GY"            "HI"           
# [13] "PH"            "ShBio"         "SNms"          "TKW"           "NDF"           "PC"           
# [19] "SC"            "HSr"           "HGr"           "LGr"           "LRr"          

setnames(Blues_17_18_article1_Brinov, old = c("Year","TreatEnv","Genotypes","GenoName","Year_Rel","Oldest_Newest",
                                              "Bio","GNms","GNSp","GY","HI","PH","ShBio","SNms","TKW","NDF","PC","SC",
                                              "HSr","HGr","LGr","LRr"), 
         new = c("Year","TreatEnv","Genotypes","GenoName","Year_Rel","Oldest_Newest","PBW","KNms","KNSp","GY","HI",
                 "PH","SDW","SNms","TKW","NDF","GPC","GSC","HSr","HGr","LGr","LRr"))
Blues_17_18_article1_Brinov[1:5,1:23]

Blues_17_18_article1_Brinov <- Blues_17_18_article1_Brinov[,-c(9,15)]
Blues_17_18_article1_Brinov <- Blues_17_18_article1_Brinov[, c(1:17,22:23, 18:21)]

dim(Blues_17_18_article1_Brinov)
Bl_17<-Blues_17_18_article1_Brinov[1:400,]
Bl_18<-Blues_17_18_article1_Brinov[401:800,]
Blues_17_18_article1_Brinov[1:5,1:15]
dim(Blues_17_18_article1_Brinov)
View(Blues_17_18_article1_Brinov[1:5,1:23])

PC_Cont17=PCA(Bl_17[Bl_17$TreatEnv%in%"Control",c(8:19)],scale=T, ncp = 5)
windows(6,6)
# to change the name of the axes 
#http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining
#https://rpkgs.datanovia.com/factoextra/reference/fviz_pca.html 

fviz_pca_var(PC_Cont17,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
) +labs(title ="PCA Cont17", x = "PC1(31.3%)", y = "PC2(21.8%)")


fviz_eig(PC_Cont17)
PC_Cont17$eig
write.table(PC_Cont17$eig, "PCA_Results/PC_Cont17_Eig.txt", quote = FALSE, row.names = TRUE, sep = "\t")
PC_Cont17$var
write.table(PC_Cont17$var, "PCA_Results/PC_Cont17_Var.txt", quote = FALSE, row.names = TRUE, sep = "\t")


PC_Ds17=PCA(Bl_17[Bl_17$TreatEnv%in%"D_stress",c(8:19)],scale=T, ncp = 5)
fviz_pca_var(PC_Ds17,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)+labs(title ="PCA Ds17", x = "PC1(35.6%)", y = "PC2(18.7%)")

fviz_eig(PC_Ds17)
PC_Ds17$eig
write.table(PC_Ds17$eig, "PCA_Results/PC_Ds17_Eig.txt", quote = FALSE, row.names = TRUE, sep = "\t")
PC_Ds17$var
write.table(PC_Ds17$var, "PCA_Results/PC_Ds17_Var.txt", quote = FALSE, row.names = TRUE, sep = "\t")

# For 2018

PC_Cont18=PCA(Bl_18[Bl_18$TreatEnv%in%"Control",c(8:19)],scale=T, ncp = 5)
windows(6,6)
fviz_pca_var(PC_Cont18,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
) +labs(title ="PCA Cont18", x = "PC1(31.1%)", y = "PC2(24.8%)")
fviz_eig(PC_Cont18)
PC_Cont18$eig
write.table(PC_Cont18$eig, "PCA_Results/PC_Cont18_Eig.txt", quote = FALSE, row.names = TRUE, sep = "\t")
PC_Cont18$var
write.table(PC_Cont18$var, "PCA_Results/PC_Cont18_Var.txt", quote = FALSE, row.names = TRUE, sep = "\t")


PC_Ds18=PCA(Bl_18[Bl_18$TreatEnv%in%"D_stress",c(8:19)],scale=T, ncp = 5)
fviz_pca_var(PC_Ds18,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
) + labs(title ="PCA Ds18", x = "PC1(39.2%)", y = "PC2(19.2%)")
fviz_eig(PC_Ds18)
PC_Ds18$eig
write.table(PC_Ds18$eig, "PCA_Results/PC_Ds18_Eig.txt", quote = FALSE, row.names = TRUE, sep = "\t")
PC_Ds18$var
write.table(PC_Ds18$var, "PCA_Results/PC_Ds18_Var.txt", quote = FALSE, row.names = TRUE, sep = "\t")


fviz_pca_biplot(PC_Ds18, label ="var", col.ind="cos2") +
  theme_minimal()

################################

# PCA with ranking of 40 extreme tolerance level like done in TASSEL----

# Modified on 10th Mai 2021
RankingforPCA_inR<- read.delim("Data/RankingforPCA_inR.txt", header = TRUE)
head(RankingforPCA_inR,5)
dim(RankingforPCA_inR)
str(RankingforPCA_inR)
RankingforPCA_inR$Ds_Status<-as.factor(RankingforPCA_inR$Ds_Status)
View(RankingforPCA_inR)
row.names(RankingforPCA_inR)<-RankingforPCA_inR[,1]

# now include the other names
head(RankingforPCA_inR,5)
#check the names

RankingforPCA_inR$Genotypes==RankingforPCA_Labels$Genotypes

#so then
row.names(RankingforPCA_inR)<-RankingforPCA_Labels[,2]
head(RankingforPCA_inR,5)

PCA_ranking=PCA(RankingforPCA_inR[,c(3:11)],scale=T, ncp = 5)
# Start modif
colnames(RankingforPCA_inR)[4] <- "KN"
head(RankingforPCA_inR,5) # ok end Modif
# rerun PCA
PCA_ranking=PCA(RankingforPCA_inR[,c(3:11)],scale=T, ncp = 5)


windows(6,6)
fviz_pca_var(PCA_ranking,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
) +labs(title ="PCA Drought tolerance ranking", x = "PC1(50.3%)", y = "PC2(17.2%)")
fviz_eig(PCA_ranking)
PCA_ranking$eig
write.table(PCA_ranking$eig, "PCA_Results/PCA_ranking_Eig.txt", quote = FALSE, row.names = TRUE, sep = "\t")
PCA_ranking$var
write.table(PCA_ranking$var, "PCA_Results/PCA_ranking_Var.txt", quote = FALSE, row.names = TRUE, sep = "\t")

# The PCA of individuals
fviz_pca_ind(PCA_ranking, col.ind="cos2") #ok

# Let us decease the size of the file

# for legend
fviz_pca_ind(PCA_ranking, col.ind="cos2")+
  theme(
  legend.text = element_text(size = 10),
  axis.text = element_text(size = 10)
)

# For all exept individual 
#https://stackoverflow.com/questions/50411988/how-to-set-the-font-size-of-data-label-in-fviz-pca-var-of-factoextra/50412652
fviz_pca_ind(PCA_ranking, col.ind="cos2")+
  theme(text = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10)
)
# for individual or variable label in the plot (same website like upstaires)

fviz_pca_ind(PCA_ranking, col.ind="cos2", labelsize = 3)+
  theme(text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10)
  ) # Work perfect

# Add ellipses
fviz_pca_ind(PCA_ranking, labelsize = 3, habillage=RankingforPCA_inR$Ds_Status ,
                  addEllipses=TRUE, ellipse.level=0.95)+
  labs(title ="PCA Drought tolerance ranking", x = "PC1(50.3%)", y = "PC2(17.2%)")

# See the biplot

fviz_pca_biplot(PCA_ranking, labelsize = 3, habillage=RankingforPCA_inR$Ds_Status ,
                addEllipses=TRUE, ellipse.level=0.95,  col.var = "black")+
  labs(title ="PCA Drought tolerance ranking", x = "PC1(50.3%)", y = "PC2(17.2%)") # Good

fviz_pca_biplot(PCA_ranking, labelsize = 3, col.var = "contrib", # Color by contributions to the PC
                habillage=RankingforPCA_inR$Ds_Status ,
                addEllipses=TRUE, ellipse.level=0.95)+
  labs(title ="PCA Drought tolerance ranking", x = "PC1(50.3%)", y = "PC2(17.2%)")

###############################

# Pca for HighSig_SNP_YR_tforPCA_forR

HighSig_SNP_YR_tforPCA_forR <- read.delim("Data/HighSig_SNP_YR_tforPCA_forR.txt")


head(HighSig_SNP_YR_tforPCA_forR,10)
dim(HighSig_SNP_YR_tforPCA_forR)
str(HighSig_SNP_YR_tforPCA_forR)

head(RankingforPCA_inR,10)

RankingforPCA_inR$Genotypes == HighSig_SNP_YR_tforPCA_forR$Genotypes


HighSig_SNP_YR_tforPCA_forR$Ds_Status<-RankingforPCA_inR$Ds_Status


View(HighSig_SNP_YR_tforPCA_forR)
head(RankingforPCA_Labels,10)
head(HighSig_SNP_YR_tforPCA_forR,10)
row.names(HighSig_SNP_YR_tforPCA_forR)<-HighSig_SNP_YR_tforPCA_forR[,1]

#check
HighSig_SNP_YR_tforPCA_forR$Genotypes==RankingforPCA_Labels$Genotypes

#so then
row.names(HighSig_SNP_YR_tforPCA_forR)<-RankingforPCA_Labels[,2]

rownames(HighSig_SNP_YR_tforPCA_forR)
head(HighSig_SNP_YR_tforPCA_forR,10)

PCA_hisigRank=PCA(HighSig_SNP_YR_tforPCA_forR[,c(3:11)],scale=T, ncp = 5)

windows(6,6)

fviz_pca_var(PCA_hisigRank,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
) + labs(title ="PCA Drought tolerance ranking", x = "PC1(50.3%)", y = "PC2(17.2%)")
fviz_eig(PCA_hisigRank)
PCA_hisigRank$eig
write.table(PCA_hisigRank$eig, "PCA_Results/PCA_hisigRank_Eig.txt", quote = FALSE, row.names = TRUE, sep = "\t")
PCA_hisigRank$var
write.table(PCA_hisigRank$var, "PCA_Results/PCA_hisigRank_Var.txt", quote = FALSE, row.names = TRUE, sep = "\t")

fviz_pca_biplot(PCA_hisigRank, labelsize = 3, habillage=RankingforPCA_inR$Ds_Status ,
                addEllipses=TRUE, ellipse.level=0.95,  col.var = "black")+
  labs(title ="PCA Drought tolerance ranking", x = "PC1(50.3%)", y = "PC2(17.2%)")

fviz_pca_ind(PCA_hisigRank, labelsize = 3, habillage=RankingforPCA_inR$Ds_Status ,
             addEllipses=TRUE, ellipse.level=0.95)

# Add the good labels
RankingforPCA_Labels <- read.delim("Data/RankingforPCA_Labels.txt")

head(RankingforPCA_Labels,5) # ok Good now merge
