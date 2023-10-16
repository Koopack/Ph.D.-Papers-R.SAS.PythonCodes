# Let Us do the Biplot for all physiological traits at prolonged drought (Anthesis)

Chl_SC_Trs_Phot<- read.delim("D:/PATRICE KOUA/Documents, PhD Study/Documents, PhD Researches/Documents, Articles Patrice Koua/Documents, Article 2 Sub20Geno/Ready data/Data 2017-2018/For Corrtest/Chl_SC_Trs_Phot.txt")
head(Chl_SC_Trs_Phot,5)
dim(Chl_SC_Trs_Phot)
str(Chl_SC_Trs_Phot)

# Firt of all, let us change the names of the variables
colnames(Chl_SC_Trs_Phot)
setnames(Chl_SC_Trs_Phot, old = c("Year","TreatEnv","Genotypes","SPAD","YII","Fv.Fm","NPQ","SC","Trs_E", 
                                  "netCo2_A","WUEinst","IntCO2_ci","SC_gsw","DTLe_Air"),
         new = c("Year","TreatEnv","Genotypes","SPAD","YII","FV/FM","NPQ","LSCp","E",
                 "A","LWUE","Ci","LSCl","DTLA"))
colnames(Chl_SC_Trs_Phot)
head(Chl_SC_Trs_Phot,5)
dim(Chl_SC_Trs_Phot)

#inchude the genotype label
head(Devtraits_sub20Mean,5)
dim(Devtraits_sub20Mean)
SorteLabel_Ds_K <- rbind(Devtraits_sub20Mean,Devtraits_sub20Mean)
SorteLabel_Ds_K <- SorteLabel_Ds_K[,c(1,2)]

# No more merge but cbind
Chl_SC_Trs_Phot2 <- cbind(SorteLabel_Ds_K,Chl_SC_Trs_Phot)
head(Chl_SC_Trs_Phot2,5)
Chl_SC_Trs_Phot <- Chl_SC_Trs_Phot2

head(Chl_SC_Trs_Phot,5)

# Under drought condition
DsPhysioforPCA=Chl_SC_Trs_Phot[Chl_SC_Trs_Phot$TreatEnv%in% "D_stress",]

rownames(DsPhysioforPCA)=DsPhysioforPCA[,2]
head(DsPhysioforPCA,2)
View(DsPhysioforPCA)

PcaDsPhysio=PCA(DsPhysioforPCA[,6:16],graph=F)

list(PcaDsPhysio)

windows(7,6)

fviz_pca_biplot(PcaDsPhysio, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#EB6841"  # Individuals color
) + labs(title ="PCA Devtraitsboth ", x = "PC1=41.2%", y = "PC2=20.8%")

setnames(as.data.frame(PcaDsPhysio$var$cos2), old = c("Dim.1", "Dim.2", "Dim.3", "Dim.4"), new = c("PC1", "PC2", "PC3", "PC4") )
corrplot(PcaDsPhysio$var$cos2[,-c(4,5)] , is.corr=FALSE)


corrplot(PcaDsPhysio$var$cos2 , is.corr=FALSE)

corrplot(PcaDsPhysio$var$contrib , is.corr=FALSE) 

# Under Control condition
ContPhysioforPCA=Chl_SC_Trs_Phot[Chl_SC_Trs_Phot$TreatEnv%in% "Control",]

rownames(ContPhysioforPCA)=ContPhysioforPCA[,3]
head(ContPhysioforPCA,8)
View(ContPhysioforPCA)

PcaContPhysio=PCA(ContPhysioforPCA[,6:16],graph=F)

list(PcaContPhysio)

windows(7,6)

fviz_pca_biplot(PcaContPhysio, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#EB6841"  # Individuals color
) + labs(title ="PCA Devtraitsboth ", x = "PC1=41.8%", y = "PC2=19%")

setnames(as.data.frame(PcaContPhysio$var$cos2), old = c("Dim.1", "Dim.2", "Dim.3", "Dim.4"), new = c("PC1", "PC2", "PC3", "PC4") )
corrplot(PcaContPhysio$var$cos2[,-c(4,5)] , is.corr=FALSE)


corrplot(PcaContPhysio$var$cos2 , is.corr=FALSE)

corrplot(PcaContPhysio$var$contrib , is.corr=FALSE) # Per

corrplot(PcaContPhysio$var$cor , is.corr=FALSE)
