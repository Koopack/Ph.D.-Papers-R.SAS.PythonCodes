# Epistatic interaction ofsevation

install.packages("circlize")
library(circlize)

# Load the data

Snp_anova_epistasy_YII  <- read.delim("M:/PatriceKoua/PATRICE_GWAS/SAS_Master_Student/
                                      SAS_Drought_Article_Physio/Epistatic_interaction/Traits_YII/Snp_anova_epistasy_YII.txt")

# Read data from SAS directly

library(haven)
DataInterYII <- read_sas("M:/PatriceKoua/PATRICE_GWAS/SAS_Master_Student/SAS_Drought_Article_Physio/
                         Epistatic_interaction/Traits_YII/QTLResults/snp_anova_epistasy_condi_YII.sas7bdat")
# View(data)
head(DataInterYII,3)


# The data set is huge. You can filter the most significant in SAS before loading into R. 

head(DataInterYII,3)
min(DataInterYII$ProbF)

Snp_anova_epistasy_YII_sig15 <- subset(DataInterYII, ProbF < 0.000000000000001)

Snp_anova_epistasy_YII_sig8 <- subset(DataInterYII, ProbF < 0.00000001)

head(Snp_anova_epistasy_YII_sig15,5)

Snp_anova_epistasy_YII_sig15[1:10,1:16]

max(Snp_anova_epistasy_YII_sig15$ProbF)
min(Snp_anova_epistasy_YII_sig15$ProbF)
min(Snp_anova_epistasy_YII_sig15$LogP, na.rm = TRUE)
max(Snp_anova_epistasy_YII_sig15$LogP, na.rm = TRUE)

Snp_anova_epistasy_YII_sig15 <- as.data.frame(Snp_anova_epistasy_YII_sig15)

write.table(Snp_anova_epistasy_YII_sig15, "Data/Snp_anova_epistasy_YII_sig15.txt", quote = FALSE, sep = "\t", row.names = FALSE )

# Try a test on a generated Data

InteracData <- read.delim("Data/InteracData.txt")
head(InteracData,5)
dim(InteracData)
colnames(InteracData)[2:22] <- InteracData[,1]
colnames(InteracData)[1] <- ""
head(InteracData,5)
rownames(InteracData) <- InteracData[,1]
InteracData <- InteracData[,-1]

col.pal = c("1A" ="red", "2A" ="green",	"3A" ="blue",	"4A" ="oragne",	"5A" ="grey",	"6A "="maroon",	"7A" ="chartreuse",
             "1B" ="hotpink",	"2B" ="deeppink3",	"3B" ="skyblue1", "4B" ="violetred2",	"5B" ="maroon1",
             "6B" ="orange3",	"7B" ="turquoise",	"1D" ="tomato2", "2D" ="snow4",	"3D" ="chocolate",	"4D" ="cadetblue",
             "5D" ="yellow",	"6D" ="coral2",	"7D" ="tan3")

chordDiagram(InteracData, grid.col = col.pal)# Work perfectly

# Load the real data from GWAS epistatic interaction results

InteracDataYII <- read.delim("Data/InteracDataYII.txt")
head(InteracDataYII,5)
dim(InteracDataYII)

colnames(InteracDataYII)[2:22] <- InteracDataYII[,1]
colnames(InteracDataYII)[1] <- ""
head(InteracDataYII,5)
rownames(InteracDataYII) <- InteracDataYII[,1]
InteracDataYII <- InteracDataYII[,-1]

chordDiagram(InteracDataYII, grid.col = col.pal)# Work perfectly
####

# Load the different results

# For Drought


library(haven)

DataInter_Ds_YII <- read_sas("M:/PatriceKoua/PATRICE_GWAS/SAS_Master_Student/SAS_Drought_Article_Physio/Epistatic_interaction/Drought/Traits_YII_1stTest/QTLResults/snp_anova_epistasy.sas7bdat")
DataInter_Ds_SPAD <- read_sas("M:/PatriceKoua/PATRICE_GWAS/SAS_Master_Student/SAS_Drought_Article_Physio/Epistatic_interaction/Drought/Traits_SPAD/QTLResults/snp_anova_epistasy.sas7bdat")
DataInter_Ds_Fmin <- read_sas("M:/PatriceKoua/PATRICE_GWAS/SAS_Master_Student/SAS_Drought_Article_Physio/Epistatic_interaction/Drought/Traits_Fmin/QTLResults/snp_anova_epistasy.sas7bdat")
DataInter_Ds_Fmax <- read_sas("M:/PatriceKoua/PATRICE_GWAS/SAS_Master_Student/SAS_Drought_Article_Physio/Epistatic_interaction/Drought/Traits_Fmax/QTLResults/snp_anova_epistasy.sas7bdat")


DataInter_Rf_YII <- read_sas("M:/PatriceKoua/PATRICE_GWAS/SAS_Master_Student/SAS_Drought_Article_Physio/Epistatic_interaction/Rainfed/Traits_YII/QTLResults/snp_anova_epistasy.sas7bdat")
DataInter_Rf_SPAD <- read_sas("M:/PatriceKoua/PATRICE_GWAS/SAS_Master_Student/SAS_Drought_Article_Physio/Epistatic_interaction/Rainfed/Traits_SPAD/QTLResults/snp_anova_epistasy.sas7bdat")
DataInter_Rf_Fmin <- read_sas("M:/PatriceKoua/PATRICE_GWAS/SAS_Master_Student/SAS_Drought_Article_Physio/Epistatic_interaction/Rainfed/Traits_Fmin/QTLResults/snp_anova_epistasy.sas7bdat")
DataInter_Rf_Fmax <- read_sas("M:/PatriceKoua/PATRICE_GWAS/SAS_Master_Student/SAS_Drought_Article_Physio/Epistatic_interaction/Rainfed/Traits_Fmax/QTLResults/snp_anova_epistasy.sas7bdat")

remove(DataInter_Ds_YII)
remove(DataInter_Ds_SPAD)
remove(DataInter_Ds_Fmin)
remove(DataInter_Ds_Fmax)

remove(DataInter_Rf_YII)
remove(DataInter_Rf_SPAD)
remove(DataInter_Rf_Fmin)
remove(DataInter_Rf_Fmax)

#  Thos data are too huge

# Let do for YII under drought (DataInter_Ds_YII) -----

dim(DataInter_Ds_YII)
# [1] 32177256       16
DataInter_Ds_YII[1:2, 1:16]
DataInter_Ds_YII <- as.data.frame(DataInter_Ds_YII)
DataInter_Ds_YII[1:2, 1:16]

min(DataInter_Ds_YII$ProbF, na.rm = TRUE)
max(DataInter_Ds_YII$ProbF, na.rm = TRUE)

DataInter_Ds_YII_sorted <- DataInter_Ds_YII[order(DataInter_Ds_YII$ProbF),] # sort ascending

View(DataInter_Ds_YII_sorted[1:1000, 1:16]) # ok but last a bit long 

# Sorting does not bring very good solution

DataInter_Ds_YII_sig15 <- subset(DataInter_Ds_YII, ProbF < 0.000000000000001) # very fast YOU HAVE TO SUBSET ACCORDING TO ProbF not Logp

DataInter_Ds_YII_sig15 <- subset(DataInter_Ds_YII, ProbF < 0.000000000000001)
# check the time
old <- Sys.time()
new <- Sys.time() - old # calculate difference
print(new) # print in nice format # Time difference of 10.70161 secs

dim(DataInter_Ds_YII_sig15)
# [1] 1476902      16 # you see it is still very huge with this threshold [15] # let us take 16
head(DataInter_Ds_YII_sig15,5)

DataInter_Ds_YII_sig16 <- subset(DataInter_Ds_YII_sorted, ProbF < 0.0000000000000001)
dim(DataInter_Ds_YII_sig16)
# 981044     16
head(DataInter_Ds_YII_sig16)   
tail(DataInter_Ds_YII_sig16,5)

# Let us sort according to the Fvalue (Fvalue_M_by_M)

DataInter_Ds_YII_sig16 <- DataInter_Ds_YII_sig16[order(-DataInter_Ds_YII_sig16$Fvalue_M_by_M),]

head(DataInter_Ds_YII_sig16,5)

DataInter_Ds_YII_sig16[2000:2005,]

DataInter_Ds_YII_sig16_sub10pc <- DataInter_Ds_YII_sig16[1:2364,]

write.table(DataInter_Ds_YII_sig16_sub10pc, "Data/DataInter_Ds_YII_sig16_sub10pc.txt", quote = FALSE, sep = "\t", row.names = FALSE )


# Let us do for YII under rainfed to see if drought triggered the interactions ----

# Let go for DataInter_Rf_YII
dim(DataInter_Rf_YII)
# [1] 17568672       16
DataInter_Rf_YII[1:2, 1:16]
DataInter_Rf_YII <- as.data.frame(DataInter_Rf_YII)
DataInter_Rf_YII[1:2, 1:16]

min(DataInter_Rf_YII$ProbF, na.rm = TRUE)
max(DataInter_Rf_YII$ProbF, na.rm = TRUE)

# # DataInter_Rf_YII_sorted <- DataInter_Rf_YII[order(DataInter_Rf_YII$ProbF),] # sort ascending
# 
# # View(DataInter_Rf_YII_sorted[1:1000, 1:16]) # ok but last a bit long  Sort the data so that you can take the 10% of the SNP set (2364 first rows)
# 
# # DataInter_Rf_YII_sig15 <- subset(DataInter_Rf_YII, ProbF < 0.000000000000001) # very fast YOU HAVE TO SUBSET ACCORDING TO ProbF not Logp
# 
# # min(DataInter_Rf_YII$ProbF, na.rm = TRUE)
# # [1] 2.736922e-12
# 
# # not use subset but use the firt 2364 row with highest Fvalue

# Let us sort according to the Fvalue (Fvalue_M_by_M)

DataInter_Rf_YII<- DataInter_Rf_YII[order(-DataInter_Rf_YII$Fvalue_M_by_M),]

head(DataInter_Rf_YII,5)
tail(DataInter_Rf_YII,5)

DataInter_Rf_YII_Sig_sub10pc <- DataInter_Rf_YII[1:2364,]
head(DataInter_Rf_YII_Sig_sub10pc,5)
tail(DataInter_Rf_YII_Sig_sub10pc,5)


write.table(DataInter_Rf_YII_Sig_sub10pc, "Data/DataInter_Rf_YII_Sig_sub10pc.txt", quote = FALSE, sep = "\t", row.names = FALSE )


# Let ust chech we NEED to go for Fmin and Fmax -------


# Let go for DataInter_Ds_Fmin

dim(DataInter_Ds_Fmin)
# [1] 2674860      16
DataInter_Ds_Fmin[1:2, 1:16]
DataInter_Ds_Fmin <- as.data.frame(DataInter_Ds_Fmin)
DataInter_Ds_Fmin[1:2, 1:16]

min(DataInter_Ds_Fmin$ProbF, na.rm = TRUE)
max(DataInter_Ds_Fmin$ProbF, na.rm = TRUE)

DataInter_Ds_Fmin <- DataInter_Ds_Fmin[order(-DataInter_Ds_Fmin$Fvalue_M_by_M),]

head(DataInter_Ds_Fmin,5)
tail(DataInter_Ds_Fmin,5)

DataInter_Ds_Fmin_Sig_sub10pc <- DataInter_Ds_Fmin[1:2364,]
head(DataInter_Ds_Fmin_Sig_sub10pc,5)
tail(DataInter_Ds_Fmin_Sig_sub10pc,5)


write.table(DataInter_Ds_Fmin_Sig_sub10pc, "Data/DataInter_Ds_Fmin_Sig_sub10pc.txt", quote = FALSE, sep = "\t", row.names = FALSE )


# Let go for DataInter_Rf_Fmin

dim(DataInter_Rf_Fmin)
# 14942090       16
DataInter_Rf_Fmin[1:2, 1:16]
DataInter_Rf_Fmin <- as.data.frame(DataInter_Rf_Fmin)
DataInter_Rf_Fmin[1:2, 1:16]

min(DataInter_Rf_Fmin$ProbF, na.rm = TRUE)
max(DataInter_Rf_Fmin$ProbF, na.rm = TRUE)

DataInter_Rf_Fmin <- DataInter_Rf_Fmin[order(-DataInter_Rf_Fmin$Fvalue_M_by_M),]

head(DataInter_Rf_Fmin,5)
tail(DataInter_Rf_Fmin,5)

DataInter_Rf_Fmin_Sig_sub10pc <- DataInter_Rf_Fmin[1:2364,]
head(DataInter_Rf_Fmin_Sig_sub10pc,5)
tail(DataInter_Rf_Fmin_Sig_sub10pc,5)


write.table(DataInter_Rf_Fmin_Sig_sub10pc, "Data/DataInter_Rf_Fmin_Sig_sub10pc.txt", quote = FALSE, sep = "\t", row.names = FALSE )


# Just do for SPAD and YII in the paper ----

# Let go for DataInter_Ds_SPAD

dim(DataInter_Ds_SPAD)
# 53384942       16
DataInter_Ds_SPAD[1:2, 1:16]
DataInter_Ds_SPAD <- as.data.frame(DataInter_Ds_SPAD)
DataInter_Ds_SPAD[1:2, 1:16]

min(DataInter_Ds_SPAD$ProbF, na.rm = TRUE)
max(DataInter_Ds_SPAD$ProbF, na.rm = TRUE)

DataInter_Ds_SPAD <- DataInter_Ds_SPAD[order(-DataInter_Ds_SPAD$Fvalue_M_by_M),]

head(DataInter_Ds_SPAD,5)
tail(DataInter_Ds_SPAD,5)

DataInter_Ds_SPAD_Sig_sub10pc <- DataInter_Ds_SPAD[1:2364,]
head(DataInter_Ds_SPAD_Sig_sub10pc,5)
tail(DataInter_Ds_SPAD_Sig_sub10pc,5)


write.table(DataInter_Ds_SPAD_Sig_sub10pc, "Data/DataInter_Ds_SPAD_Sig_sub10pc.txt", quote = FALSE, sep = "\t", row.names = FALSE )


# Let go for DataInter_Ds_SPAD

dim(DataInter_Ds_SPAD)
# 53384942       16
DataInter_Ds_SPAD[1:2, 1:16]
DataInter_Ds_SPAD <- as.data.frame(DataInter_Ds_SPAD)
DataInter_Ds_SPAD[1:2, 1:16]

min(DataInter_Ds_SPAD$ProbF, na.rm = TRUE)
max(DataInter_Ds_SPAD$ProbF, na.rm = TRUE)

DataInter_Ds_SPAD <- DataInter_Ds_SPAD[order(-DataInter_Ds_SPAD$Fvalue_M_by_M),]

head(DataInter_Ds_SPAD,5)
tail(DataInter_Ds_SPAD,5)

DataInter_Ds_SPAD_Sig_sub10pc <- DataInter_Ds_SPAD[1:2364,]
head(DataInter_Ds_SPAD_Sig_sub10pc,5)
tail(DataInter_Ds_SPAD_Sig_sub10pc,5)


write.table(DataInter_Ds_SPAD_Sig_sub10pc, "Data/DataInter_Ds_SPAD_Sig_sub10pc.txt", quote = FALSE, sep = "\t", row.names = FALSE )


# Let go for DataInter_Rf_SPAD

dim(DataInter_Rf_SPAD)
# 19967492       16
DataInter_Rf_SPAD[1:2, 1:16]
DataInter_Rf_SPAD <- as.data.frame(DataInter_Rf_SPAD)
DataInter_Rf_SPAD[1:2, 1:16]

min(DataInter_Rf_SPAD$ProbF, na.rm = TRUE)
max(DataInter_Rf_SPAD$ProbF, na.rm = TRUE)

DataInter_Rf_SPAD <- DataInter_Rf_SPAD[order(-DataInter_Rf_SPAD$Fvalue_M_by_M),]

head(DataInter_Rf_SPAD,5)
tail(DataInter_Rf_SPAD,5)

DataInter_Rf_SPAD_Sig_sub10pc <- DataInter_Rf_SPAD[1:2364,]
head(DataInter_Rf_SPAD_Sig_sub10pc,5)
tail(DataInter_Rf_SPAD_Sig_sub10pc,5)


write.table(DataInter_Rf_SPAD_Sig_sub10pc, "Data/DataInter_Rf_SPAD_Sig_sub10pc.txt", quote = FALSE, sep = "\t", row.names = FALSE )

