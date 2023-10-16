# Correlation plots
install.packages("corrplot") # install Corrplot
install.packages("RColorBrewer ") # install RColorBrewer
install.packages("showtext")# install showtext
install.packages("sysfonts")# install sysfonts
install.packages("showtextdb")# install showtextdb
install.packages("showtext")# install RColorBrewer
install.packages("PerformanceAnalytics")
install.packages("FactoMineR")
install.packages("factoextra")

library(corrplot) # install Corrplot
library(RColorBrewer ) # install RColorBrewer
library(showtext)# install showtext
library(sysfonts)# install sysfonts
library(showtextdb)# install showtextdb
library(showtext)# install RColorBrewer
library(data.table)
library(PerformanceAnalytics)
library(FactoMineR)
library(factoextra)

#Load the required package
lapply(c("corrplot","showtex","RColorBrewer","showtextdb","sysfonts"))


corrplot(pic01,
         
         type="lower",
         
         order="hclust",
         
         p.mat=mydatap,
         
         insig = "label_sig",
         
         sig.level = c(0.001,0.01,0.05),
         
         pch.cex = 2.5,pch.col="white",
         
         diag = FALSE,tl.srt =45,tl.col = "black",
         
         family="serif",col = COLOR01,
         
         tl.cex = 2.5,title = "miraculousdna",
         
         mar=c(0, 0, 1, 0),cl.cex = 2.5
         
)

# Check first the correlations with performance analytics
#install.packages("PerformanceAnalytics")

head(merge_RY_lab_Blue_recode,2)
dim(merge_RY_lab_Blue_recode) 

windows(7,7)

chart.Correlation(subset(merge_RY_lab_Blue_recode)[,c(8:20)] ,histogram = TRUE, method = "pearson")
chart.Correlation(subset(merge_RY_lab_Blue_recode,TreatEnV_N == "Rainfed_Voll")[,c(8:20)] ,histogram = TRUE, method = "pearson")

tail(subset(merge_RY_lab_Blue_recode,Water == "Rainfed"|Nitrogen == "Voll"))

setnames(merge_RY_lab_Blue_recode, old = c("Biomasse","Seedyield","Protein","Samen","Stroh","NUE_Bio","NUE_Grain", 
                                           "NHI","Spad","Chlorophyll","Fmin","YII","Fmax"),
                                    new = c("PBW","GY","Protein","NGr","NSt","NUEBio","NUEGrain", 
                 "NHI","SPAD","Chlorophyll","Fmin","YII","Fmax"))

merge_RY_lab_Blue_recode_forCor <- merge_RY_lab_Blue_recode[, -c(10,17,18,20)]

head(merge_RY_lab_Blue_recode_forCor)
# https://rpubs.com/cloud_wei/2107 

library(corrplot)

# M <- Hmisc::rcorr(as.matrix(mtcars))
# corrplot(M$r, p.mat = M$P, insig = "label_sig",
#          sig.level = c(.001, .01, .05), pch.cex=0.9, pch.col = "white")

# For Rainfed Voll
M <- Hmisc::rcorr(as.matrix(subset(merge_RY_lab_Blue_recode_forCor,TreatEnV_N == "Rainfed_Voll"))[,c(3,8:16)])
corrplot(M$r, p.mat = M$P, insig = "label_sig", tl.col="black", tl.cex=0.8, tl.srt=45, 
         sig.level = c(.001, .01, .05), pch.cex=0.9, pch.col = "white")

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45, 
         sig.level = c(.001, .01, .05), pch.cex=0.9, pch.col = "white")

# don´t use this
corrplot.mixed(M$r, p.mat = M$P, insig = "label_sig",
               sig.level = c(.001, .01, .05), pch.cex=0.9, pch.col = "white")


# For Rainfed null
M <- Hmisc::rcorr(as.matrix(subset(merge_RY_lab_Blue_recode_forCor,TreatEnV_N == "Rainfed_Null"))[,c(3,8:16)])
# To change the Background
par(bg = "#E8DDCB")

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45, 
         sig.level = c(.001, .01, .05), pch.cex=0.9, pch.col = "white")

# For Drought Voll
M <- Hmisc::rcorr(as.matrix(subset(merge_RY_lab_Blue_recode_forCor,TreatEnV_N == "Drought_Voll"))[,c(3,8:16)])

# change the cercle into square

corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45, 
         sig.level = c(.001, .01, .05), pch.cex=0.9, pch.col = "white")

# For Drought Null
M <- Hmisc::rcorr(as.matrix(subset(merge_RY_lab_Blue_recode_forCor,TreatEnV_N == "Drought_Null"))[,c(3,8:16)])

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45, 
         sig.level = c(.001, .01, .05), pch.cex=0.9, pch.col = "white")




# Correlation for Root traits

View(head(Root_ShootKat_Niko))
M <- Hmisc::rcorr(as.matrix(subset(Root_ShootKat_Niko,Env == "Kontrolle_Voll"))[,c(8:13, 23:26)])

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45, 
         sig.level = c(.001, .01, .05), pch.cex=0.9, pch.col = "white")

# Select the important Variable

Root_ShootKat_Niko_select <- Root_ShootKat_Niko[, c(1:6, 8,10:13,18, 23:26)]
View(head(Root_ShootKat_Niko_select))

# Resume the correlation analysis

Root_ShootKat_Niko_sel_ord_N 


M <- Hmisc::rcorr(as.matrix(subset(Root_ShootKat_Niko,Env == "Kontrolle_Voll"))[,c(8:13, 23:26)])

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45, 
         sig.level = c(.001, .01, .05), pch.cex=0.9, pch.col = "white")

# Correlation phenotypic photosynthetic traits
dim(Merge_YII_SPAD_NDVI_123_selN)

head(Merge_YII_SPAD_NDVI_123_selN,2)
Merge_YII_SPAD_NDVI_123_selN <- Merge_YII_SPAD_NDVI_123_selN[, c(1:9, 10,11,16, 12,13,17, 14,15,18 )]

#Kontrolle_voll
M <- Hmisc::rcorr(as.matrix(subset(Merge_YII_SPAD_NDVI_123_selN,Env == "Kontrolle_voll"))[,c(10:18)])

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45,  pch = 4,
         sig.level = c(.001, .01, .05), pch.cex=1.5, pch.col = "black")

windows(7,7)
str(Merge_YII_SPAD_NDVI_123_selN)
Merge_YII_SPAD_NDVI_123_selN$Env <- as.factor(Merge_YII_SPAD_NDVI_123_selN$Env)
levels(Merge_YII_SPAD_NDVI_123_selN$Env)

# Kontrolle_reduziert
M <- Hmisc::rcorr(as.matrix(subset(Merge_YII_SPAD_NDVI_123_selN,Env == "Kontrolle_reduziert"))[,c(10,11, 13:14,16:17)])

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45,  pch = 4,
         sig.level = c(.001, .01, .05), pch.cex=1.5, pch.col = "black")


#stress_voll
M <- Hmisc::rcorr(as.matrix(subset(Merge_YII_SPAD_NDVI_123_selN,Env == "stress_voll"))[,c(10:18)])

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45,  pch = 4,
         sig.level = c(.001, .01, .05), pch.cex=1.5, pch.col = "black")

# stress_reduziert
M <- Hmisc::rcorr(as.matrix(subset(Merge_YII_SPAD_NDVI_123_selN,Env == "stress_reduziert"))[,c(10,11, 13:14,16:17)])

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45,  pch = 4,
         sig.level = c(.001, .01, .05), pch.cex=1.5, pch.col = "black")

# You have to shoose the plant at the same growth stage for the Analysis
############################### 

# So create another data
# stress_reduziert
M <- Hmisc::rcorr(as.matrix(subset(Merge_YII_SPAD_NDVI_123_selN,Env == "stress_reduziert"))[,c(10,11, 13:14,16:17)])

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45,  pch = 4,
         sig.level = c(.001, .01, .05), pch.cex=1.5, pch.col = "black")

Merge_YII_SPAD_NDVI_123_selN$BBCH <- as.factor(Merge_YII_SPAD_NDVI_123_selN$BBCH)

subset(Merge_YII_SPAD_NDVI_123_selN,Env == "stress_reduziert" & BBCH == "Anthesis")

stress_reduziert <- Merge_YII_SPAD_NDVI_123_selN[Merge_YII_SPAD_NDVI_123_selN$Env %in% "stress_reduziert",]

head(stress_reduziert,5)
M <- Hmisc::rcorr(as.matrix(subset(Merge_YII_SPAD_NDVI_123_selN,BBCH == "Anthesis"))[,c(10,11, 13:14,16:17)])

# change the cercle into square
corrplot(M$r, p.mat = M$P, insig = "label_sig", method = "square",tl.col="black", tl.cex=0.8, tl.srt=45,  pch = 4,
         sig.level = c(.001, .01, .05), pch.cex=1.5, pch.col = "black")

# correlation Photo Agro
colnames(YII3_SPAD3_NDVI3_Agro)[18] <- "RDW"
str(YII3_SPAD3_NDVI3_Agro)
dim(YII3_SPAD3_NDVI3_Agro)
levels(YII3_SPAD3_NDVI3_Agro$Env)
YII3_SPAD3_NDVI3_Agro$RA_N <- 1/YII3_SPAD3_NDVI3_Agro$RA
chart.Correlation(subset(YII3_SPAD3_NDVI3_Agro,Env == "Kontrolle_voll")[,c(9:19)] ,histogram = TRUE, method = "pearson")
chart.Correlation(subset(YII3_SPAD3_NDVI3_Agro,Env == "stress_voll")[,c(9:19)] ,histogram = TRUE, method = "pearson")

chart.Correlation(subset(YII3_SPAD3_NDVI3_Agro,Env == "stress_voll")[,c(9,10,12:19)] ,histogram = TRUE, method = "pearson")
c(9,10,12:19)

write.table(YII3_SPAD3_NDVI3_Agro, "Data/YII3_SPAD3_NDVI3_Agro.txt", row.names = FALSE, sep = "\t", quote = FALSE)


# Tuesday, 28th August 2023

# Transform chart correlation graph of Figure 5 to PCA graph. 

head(YII3_SPAD3_NDVI3_Agro)
dim(YII3_SPAD3_NDVI3_Agro)
colnames(YII3_SPAD3_NDVI3_Agro)

head(YII3_SPAD3_NDVI3_Agro_Mean)
dim(YII3_SPAD3_NDVI3_Agro_Mean)

setnames(YII3_SPAD3_NDVI3_Agro, old = c("RDw"), new = c("RDW"))

# Check if all(colnames_A %in% colnames(dfB))

all(colnames(YII3_SPAD3_NDVI3_Agro) %in% colnames(YII3_SPAD3_NDVI3_Agro_Mean))

common_colnames <- intersect(colnames(YII3_SPAD3_NDVI3_Agro), 
                             colnames(YII3_SPAD3_NDVI3_Agro_Mean))

colnames_only_in_A <- setdiff(colnames(YII3_SPAD3_NDVI3_Agro), 
                              colnames(YII3_SPAD3_NDVI3_Agro_Mean))

# let us generate the means with YII3_SPAD3_NDVI3_Agro 

colnames(YII3_SPAD3_NDVI3_Agro)
head(YII3_SPAD3_NDVI3_Agro)
# Do it with aggregate 

# cdata.means <- aggregate(data[c("before","after","change")], 
#                          by = data[c("sex","condition")],
#                          FUN=mean, na.rm=TRUE)

YII3_SPAD3_NDVI3_Agro_agg <- aggregate(YII3_SPAD3_NDVI3_Agro[c("SPAD3","YII3","NDVI3","FSW",
                              "SDW","SWaP","NLf","RA","FRW","RDW","RWaP")],
                              by = YII3_SPAD3_NDVI3_Agro[c("Env","Sorte")],
                         FUN=mean, na.rm=TRUE)
# include BRISONr
YII3_SPAD3_NDVI3_Agro_agg <- aggregate(YII3_SPAD3_NDVI3_Agro[c("SPAD3","YII3","NDVI3","FSW",
                                                               "SDW","SWaP","NLf","RA","FRW","RDW","RWaP")],
                                       by = YII3_SPAD3_NDVI3_Agro[c("Env","Sorte","BRISONr")],
                                       FUN=mean, na.rm=TRUE)

head(YII3_SPAD3_NDVI3_Agro_agg)
dim(YII3_SPAD3_NDVI3_Agro_agg)
View(YII3_SPAD3_NDVI3_Agro_agg)

dim(YII3_SPAD3_NDVI3_Agro_Mean)
head(YII3_SPAD3_NDVI3_Agro_Mean) # Okay the aggregate values looks fine

# Now do the PCA 

setnames(YII3_SPAD3_NDVI3_Agro_agg,
         old = c("SPAD3","YII3","NDVI3"),
         new = c("SPAD","YII","NDVI"))

dim(YII3_SPAD3_NDVI3_Agro_agg)
head(YII3_SPAD3_NDVI3_Agro_agg)
chart.Correlation(subset(YII3_SPAD3_NDVI3_Agro_agg,Env == "Kontrolle_voll")[,c(4:14)],
                  histogram = TRUE, method = "pearson") #ok

chart.Correlation(subset(YII3_SPAD3_NDVI3_Agro_agg,Env == "stress_voll")[,c(4:14)],
                  histogram = TRUE, method = "pearson")

# Now Perform the PCA ----

head(YII3_SPAD3_NDVI3_Agro_agg)
YII3_SPAD3_NDVI3_Agro_aggPCA <- YII3_SPAD3_NDVI3_Agro_agg

# Take Sorte as roname 
YII3_SPAD3_NDVI3_Agro_aggPCA_CHN <- subset(YII3_SPAD3_NDVI3_Agro_agg,Env == "Kontrolle_voll")
YII3_SPAD3_NDVI3_Agro_aggPCA_DHN <- subset(YII3_SPAD3_NDVI3_Agro_agg,Env == "stress_voll")
dim(YII3_SPAD3_NDVI3_Agro_aggPCA_CHN)
YII3_SPAD3_NDVI3_Agro_aggPCA_CHN$Sorte

rownames(YII3_SPAD3_NDVI3_Agro_aggPCA_CHN) = YII3_SPAD3_NDVI3_Agro_aggPCA_CHN[,2]
rownames(YII3_SPAD3_NDVI3_Agro_aggPCA_DHN) = YII3_SPAD3_NDVI3_Agro_aggPCA_DHN[,2]

# for CHN ----
PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_CHN = PCA(YII3_SPAD3_NDVI3_Agro_aggPCA_CHN[,4:14],graph=F)

list(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_CHN)

windows(5,4)

fviz_pca_ind(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_CHN)

fviz_pca_var(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_CHN)

fviz_pca_biplot(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_CHN, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#EB6841"  # Individuals color) 
) + labs(title ="PCA photosynthesis and Root-shoot traits at anthesis under CHN",
         x = "PC1:38.8%", y = "PC2:20.9%") +
  theme(plot.title = element_text(hjust = 0.5))

setnames(as.data.frame(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_CHN$var$cos2), 
         old = c("Dim.1", "Dim.2", "Dim.3", "Dim.4"), 
         new = c("PC1", "PC2", "PC3", "PC4") )
corrplot(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_CHN$var$cos2[,-c(4,5)] , is.corr=FALSE)
corrplot(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_CHN$var$contrib , is.corr=FALSE) 

# For DHN ----

PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_DHN = PCA(YII3_SPAD3_NDVI3_Agro_aggPCA_DHN[,4:14],graph=F)

list(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_DHN)

windows(5,4)

fviz_pca_ind(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_DHN)
fviz_pca_var(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_DHN)

fviz_pca_biplot(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_DHN, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#EB6841"  # Individuals color) 
) + labs(title ="PCA photosynthesis and Root-shoot traits at anthesis under DHN",
         x = "PC1:35.3%", y = "PC2:32.8%") +
  theme(plot.title = element_text(hjust = 0.5))

setnames(as.data.frame(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_DHN$var$cos2), 
         old = c("Dim.1", "Dim.2", "Dim.3", "Dim.4"), 
         new = c("PC1", "PC2", "PC3", "PC4") )
corrplot(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_DHN$var$cos2[,-c(4,5)] , is.corr=FALSE)
corrplot(PCA_YII3_SPAD3_NDVI3_Agro_aggPCA_DHN$var$contrib , is.corr=FALSE) 

# Check again the charcht correlation
chart.Correlation(YII3_SPAD3_NDVI3_Agro_aggPCA_DHN[,c(4:14)] ,histogram = TRUE, method = "pearson")
chart.Correlation(subset(YII3_SPAD3_NDVI3_Agro,Env == "stress_voll")[,c(9:19)] ,histogram = TRUE, method = "pearson")

# Ok the correlation (using chart correlation was made including Rep But PCA was made using Means because the neotype should be ploted) is made 